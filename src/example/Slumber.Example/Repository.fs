namespace Slumber.Example

open System
open System.IO

module Repository = 

    type Person = {
        Id : Int32;
        FullName : String;
        Age : Int32;
        CreatedBy : String;
    }
    with

        static member Empty = 
            {
                Id = 0;
                FullName = String.Empty;
                Age = 0;
                CreatedBy = String.Empty;
            }

    type IRepository =
        abstract member All : unit -> Person seq
        abstract member Find : Int32 -> Person option
        abstract member Save : Person -> Person
        abstract member Delete : Int32 -> unit
        abstract member Setup : unit -> unit

    module InMemory = 

        open System.Threading
        open System.Collections.Concurrent

        type Repository () = 

            let data = ConcurrentDictionary<Int32, Person> ()
            let current = ref 0

            let find id = 
                match (data.TryGetValue id) with
                | (true, person) -> Some person
                | _ -> None

            let all () = 
                data.Values :> seq<Person>

            let save person = 
                if (person.Id <> 0) then

                    let add _ = person
                    let update _ _ = person

                    data.AddOrUpdate (person.Id, add, update)
                    
                else
                    let id = Interlocked.Increment (current)
                    let person' = { person with Id = id; }

                    data.TryAdd (id, person')
                    |> ignore

                    person'

            let delete id = 
                data.TryRemove (id) 
                |> ignore

            static member Create () = 
                Repository () :> IRepository
            
            interface IRepository with
                member this.Setup () = ()
                member this.Find id = find id
                member this.Save person = save person
                member this.Delete id = delete id
                member this.All () = all ()

    module SqlCe = 

        open System.Data
        open System.Data.Common
        open System.Data.SqlServerCe
        open System.Configuration

        type Repository (connectionStringName : String) = 

            let [<Literal>] ProviderName = "System.Data.SqlServerCe.4.0"

            let factory = DbProviderFactories.GetFactory (ProviderName)

            let connectionString = 
                match ConfigurationManager.ConnectionStrings.[connectionStringName] with
                | null -> invalidOp (sprintf "Connection string called %s not found" connectionStringName)
                | settings when not (settings.ProviderName.Equals (ProviderName, StringComparison.OrdinalIgnoreCase)) -> invalidOp (sprintf "Connection string called %s is not for SQL CE" connectionStringName)
                | settings -> settings.ConnectionString

            let connect query parameters = 

                let connection = factory.CreateConnection ()
                connection.ConnectionString <- connectionString
                connection.Open ()

                let command = factory.CreateCommand ()
                command.Connection <- connection
                command.CommandType <- CommandType.Text
                command.CommandText <- query

                for (name, value) in parameters do
                    
                    let parameter = factory.CreateParameter ()
                    parameter.ParameterName <- name
                    parameter.Value <- value

                    parameter
                    |> command.Parameters.Add
                    |> ignore

                command

            let execRead read (command : DbCommand) = 

                use reader = command.ExecuteReader ()

                let result = 
                    seq {
                        while (reader.Read ()) do
                            yield (read reader)
                    } 
                    |> Seq.toList //Force evaluation so read is not called after reader has gone out of scope and been closed

                (result, command)

            let execNonQuery (command : DbCommand) = 

                let count = command.ExecuteNonQuery () 

                (count, command)

            let close (result, command : DbCommand) = 

                if (command.Connection.State <> ConnectionState.Closed) then
                    command.Connection.Close ()
                    command.Connection.Dispose ()

                command.Dispose ()

                result

            let readPerson (reader : IDataReader) = 
                {
                    Id = reader.GetInt32 0;
                    FullName = reader.GetString 1;
                    Age = reader.GetInt32 2;
                    CreatedBy = reader.GetString 3;
                }  
                
            let all () =               

                let query = "SELECT [ID], [FullName], [Age], [CreatedBy] FROM [Person] ORDER BY [FullName] ASC"
                    
                let people = 
                    connect query []
                    |> execRead readPerson
                    |> close
                    |> List.toSeq

                people

            let find id = 

                let query = "SELECT [ID], [FullName], [Age], [CreatedBy] FROM [Person] WHERE [ID] = @id"
                let parameters = [ ("id", id) ]

                let people = 
                    connect query parameters
                    |> execRead readPerson
                    |> close

                match people with
                | [] -> None
                | person::_ -> Some person

            let save person = 
                
                let saveQuery = 
                    if (person.Id = 0) then
                        "INSERT INTO [Person] (FullName, Age, CreatedBy) VALUES (@name, @age, @createdBy)"                   
                    else
                        "UPDATE [Person] SET [FullName] = @name, [Age] = @age WHERE [ID] = @id"

                let parameters = 
                    [
                        ("id", box person.Id);
                        ("name", box person.FullName);
                        ("age", box person.Age);
                        ("createdBy", box person.CreatedBy);
                    ]

                connect saveQuery parameters
                |> execNonQuery
                |> close
                |> ignore

                //HACK Massive simplification. Never actually do this.
                let selectQuery = "SELECT [ID], [FullName], [Age], [CreatedBy] FROM [Person] ORDER BY [ID] DESC OFFSET 0 ROWS FETCH NEXT 1 ROW ONLY"

                connect selectQuery []
                |> execRead readPerson
                |> close
                |> List.head

            let delete id = 

                let query = "DELETE FROM [Person] WHERE [ID] = @id"
                let parameters = [ ("id", id) ]

                connect query parameters
                |> execNonQuery
                |> close
                |> ignore

            let setup () = 

                use engine = new SqlCeEngine (connectionString)

                //Ensure App_Data directory exists
                let path = Path.Combine (AppDomain.CurrentDomain.BaseDirectory, "App_Data")

                if not (Directory.Exists path) then
                    Directory.CreateDirectory path
                    |> ignore
                    
                //Create database if appropriate
                if not (engine.Verify ()) then
                    engine.CreateDatabase ()

                    let query = "CREATE TABLE [Person] (\
                                                [ID] Int Identity(1, 1),\
                                                [FullName] NVarChar(250) NOT NULL,\
                                                [Age] Int NOT NULL,\
                                                [CreatedBy] NVarChar(250) NOT NULL,\
                                                CONSTRAINT [Person_PK] PRIMARY KEY ([ID])\
                                                ) "

                    connect query []
                    |> execNonQuery
                    |> close
                    |> ignore

            static member Create connectionStringName = 
                Repository (connectionStringName) :> IRepository

            interface IRepository with
                member this.All () = all ()
                member this.Find id = find id
                member this.Save person = save person
                member this.Delete id = delete id
                member this.Setup () = setup ()
                    