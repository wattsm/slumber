namespace Slumber.Example

open System
open System.Runtime.Serialization
open System.IO
open System.Collections.Generic
open Slumber
open Slumber.Common.Operations.Metadata

module People = 

    [<AutoOpen>]
    module Model = 

        [<DataContract (Name = "person-summary", Namespace = "")>]
        type PersonSummary = {

            [<field: DataMember (Name = "id")>]
            Id : Int32;

            [<field: DataMember (Name = "full-name")>]
            FullName : String;

            [<field: DataMember (Name = "age")>]
            Age : Int32;

            [<field: DataMember (Name = "url")>]
            Url : String;

            [<field: DataMember (Name = "created-by")>]
            CreatedBy : String
        }

        [<DataContract (Name = "person-catalog", Namespace = "")>]
        type PersonCatalog = {

            [<field: DataMember (Name = "self")>]
            Self : String;

            [<field: DataMember (Name = "people")>]
            People : PersonSummary seq;
        }

        [<DataContract (Name = "person", Namespace = "")>]
        type PersonMessage () = 

            [<DataMember (Name = "full-name")>]
            member val FullName = String.Empty with get, set

            [<DataMember (Name = "age")>]
            member val Age = 0 with get, set

        [<DataContract (Name = "person-created", Namespace = "")>]
        type PersonCreated = {

            [<field: DataMember (Name = "id")>]
            Id : Int32;

            [<field: DataMember (Name = "url")>]
            Url : String;
        }

    let private data = 
        Dictionary<int, PersonSummary> ()

    let private getId = 
        getParameterAs<Int32> "id"

    let private getUrl (relativeUrl : String) (meta : OperationMetadata) = 
        Uri (meta.Request.Url.BaseUrl, relativeUrl)
        |> string

    let getPeople (meta : OperationMetadata) =
        {
            Self = (getUrl "people" meta);
            People = data.Values;
        }

    let getPerson (meta : OperationMetadata) = 
        try 

            let id = 
                getId meta
        
            let success, person = 
                data.TryGetValue (id)
                
            if success then
                OperationResult.ResourceOnly person
            else
                OperationResult.StatusOnly 404
        
        with
        | :? FormatException -> OperationResult.StatusOnly 400

    let addPerson (message : PersonMessage, meta : OperationMetadata) = 
        
        let id = 
            if (data.Count = 0) then
                1
            else
                data.Keys
                |> Seq.max
                |> ((+) 1)

        let userName = 
            match meta.User with
            | Some user -> user.Id
            | _ -> String.Empty

        let url =   
            meta
            |> getUrl (String.Format ("people/{0}", id))

        let person = 
            {
                Id = id;
                FullName = message.FullName;
                Age = message.Age;
                Url = url;
                CreatedBy = userName;
            }

        data.Add (id, person)

        {
            Id = id;
            Url = url;
        }
        |> OperationResult.ResourceOnly 

    let deletePerson meta = 

        let id = 
            getId meta

        data.Remove (id) |> ignore

    let updatePerson (update : PersonMessage, meta : OperationMetadata) = 

        let id =
            getId meta
                
        let success, existing =
            data.TryGetValue (id)

        if success then
    
            let updated = 
                {
                    existing
                    with 
                        Age = update.Age;
                        FullName = update.FullName;
                }

            data.[id] <- updated

            OperationResult.StatusOnly 200
        else
            OperationResult.StatusOnly 404