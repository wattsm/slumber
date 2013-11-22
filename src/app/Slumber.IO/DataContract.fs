namespace Slumber.IO

open System
open System.IO
open System.Runtime.Serialization
open Slumber.Framework.MessageIO

module DataContract = 

    ///Helper functions for use with data data contract serialisers
    [<AutoOpen>]
    module private Helpers = 

        ///Gets the inner type of an option
        let getOptionalType (type' : Type) = 
            if (type'.IsGenericType && type'.GetGenericTypeDefinition () = typedefof<Option<_>>) then
                let args = type'.GetGenericArguments ()
                in Some args.[0]
            else
                None

        ///True if a type is optional
        let isOptionalType : Type -> bool = getOptionalType >> Option.isSome            

        ///Creates a serialisation function using the given serialiser
        let writeUsing (cleanUp : MemoryStream -> MemoryStream) (getSerialiser : Type -> #XmlObjectSerializer) (data : obj)  =

            let toList (stream : MemoryStream) = 
                stream.ToArray ()
                |> Array.toList

            match data with
            | null -> []
            | _ ->

                let serialiser = 
                    data.GetType ()
                    |> getSerialiser

                use stream = new MemoryStream ()

                serialiser.WriteObject (stream, data)

                stream
                |> cleanUp
                |> toList   

        ///Creates a deserialisation function using the given serialiser
        let readUsing (getSerialiser : Type -> #XmlObjectSerializer) (stream : Stream) (type' : Type) = 
            match stream.Length with
            | 0L -> None
            | _ ->

                let serialiser = 
                    type' 
                    |> getSerialiser

                serialiser.ReadObject (stream)
                |> Some

    ///Contains a data contract surrogate to tell the serialiser to treat Some X as X and None as null.
    module private Surrogate = 

        open System.Reflection    

        ///Surrogate which handles the conversion to/from optional types during serialisation
        type private OptionSurrogate () = 

            interface IDataContractSurrogate with

                ///Serialiser Option<'T> as 'T
                member this.GetDataContractType (currentType : Type) = 
                    match (getOptionalType currentType) with
                    | Some baseType -> baseType
                    | _ -> currentType

                ///Convert null to None and 'T to Some<'T>
                member this.GetDeserializedObject (current, currentType) = 
                    if (isOptionalType currentType) then
                        if (current <> null) then
                            Activator.CreateInstance (currentType, [| current |])
                        else
                    
                            let noneProperty = currentType.GetProperty ("None")
                            noneProperty.GetValue (null)

                    else
                        current

                ///Convert Some<'T> to 'T and None to null
                member this.GetObjectToSerialize (current, _) = 
                    if (current <> null) then
                
                        let currentType = current.GetType ()

                        if (isOptionalType currentType) then

                            let isSomeProperty = currentType.GetProperty "IsSome"
                            let isSome = isSomeProperty.GetValue (null, [| current |]) :?> bool

                            if isSome then

                                let valueProperty = currentType.GetProperty "Value"
                                valueProperty.GetValue (current)

                            else
                                null
                        else
                            current
                    else
                        current

                member this.GetCustomDataToExport (_ : MemberInfo, _ : Type) = box null
                member this.GetCustomDataToExport (_ : Type, _ : Type) = box null
                member this.GetKnownCustomDataTypes _ = ()
                member this.ProcessImportedType (decl, _) = decl
                member this.GetReferencedTypeOnImport (_, _, _) = null

        ///Gets an optional surrogate
        let get () = 
            OptionSurrogate () :> IDataContractSurrogate

    ///Contains functions for determining whether a data contract contains optional types
    module Contracts = 

        open System.Reflection
        open System.Collections.Generic

        ///True if a attribute provider is a data member
        let isDataMember (provider : ICustomAttributeProvider) = 
            provider.GetCustomAttributes (typeof<DataMemberAttribute>, true)
            |> Array.isEmpty
            |> not        

        ///Gets the type of a member
        let getMemberType (x : MemberInfo) = 
            match x with
            | :? FieldInfo as field -> field.FieldType
            | :? PropertyInfo as property -> property.PropertyType
            | _ -> invalidOp "Unsupported data member type."

        ///Gets types sued as data members 
        let getDataMemberTypes (x : Type) = 
            x.GetMembers (BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance)
            |> Array.filter isDataMember
            |> Array.map getMemberType

        ///Gets the item type of a collection
        let getCollectionType (x : Type) = 

            let getType (x' : Type) = 
                if (x'.IsGenericType && x'.GetGenericTypeDefinition () = typedefof<IEnumerable<_>>) then
                    let args = x'.GetGenericArguments ()
                    in Some args.[0]
                else
                    None

            match (getType x) with
            | Some itemType -> Some itemType
            | _ ->
                x.GetInterfaces ()
                |> Array.tryPick getType

        ///True if a type exists in the well known collection
        let isUnknownType known (x : Type) = 
            List.exists ((=) x) known
            |> not

        ///True if some data contract type contains optional data members
        let typeContainsOptionalDataMembers = 
            let rec check known (x : Type) = 
                
                let types = getDataMemberTypes x |> Array.filter (isUnknownType known)
                let exists = types |> Array.exists isOptionalType

                if exists then
                    (true, [])
                else
                    types
                    |> Array.collect (fun type' ->
                            match (getCollectionType type') with
                            | Some itemType -> [| type'; itemType; |]
                            | _ -> [| type' |]
                        )
                    |> Array.fold (fun (found, known') type' ->                             
                            if not found then
                                check known' type'
                            else
                                (true, [])
                        ) (false, x :: known)

            in check [] >> fst

        ///True if some data contract contains optional data members
        let containsOptionalDataMembers data = 
            if (data <> null) then
                data.GetType ()
                |> typeContainsOptionalDataMembers
            else
                false

    ///Contains functions for cleaning up XML produced by the data contract serialiser
    module private CleanUp =

        open System.Xml
        open System.IO
        open System.Linq

        let [<Literal>] XmlnsNamespace = "http://www.w3.org/2000/xmlns/"
        let [<Literal>] SchemaNamespace = "http://www.w3.org/2001/XMLSchema-instance"

        ///Gets an XML document from a stream
        let private getXml (stream : MemoryStream) = 

            stream.Position <- 0L

            let doc = XmlDocument ()
            doc.Load (stream)

            doc

        ///Cleans up the XML in a stream, removing unnecessary attributes added by data contract serialiser when it uses a surrogate
        let apply (input : MemoryStream) = 

            let rec removeAttrs (node : XmlNode) = 

                if (node.Attributes <> null && node.Attributes.Count > 0) then
                    node.Attributes.Cast<XmlAttribute> ()
                    |> Seq.toList
                    |> List.filter (fun attr -> not (attr.NamespaceURI = SchemaNamespace))
                    |> List.iter (fun attr -> node.Attributes.Remove (attr) |> ignore)

                if node.HasChildNodes then
                    node.ChildNodes.Cast<XmlNode> ()
                    |> Seq.iter removeAttrs

            let appendXmlns (xml : XmlDocument) = 

                let attr = xml.CreateAttribute ("xmlns", "i", XmlnsNamespace)
                attr.Value <- SchemaNamespace

                xml.DocumentElement.Attributes.Append (attr)
                |> ignore

            let xml = getXml input

            removeAttrs xml.DocumentElement
            appendXmlns xml

            let settings = XmlWriterSettings ()
            settings.OmitXmlDeclaration <- true
            settings.NamespaceHandling <- NamespaceHandling.OmitDuplicates

            let output = new MemoryStream ()
            use writer = XmlWriter.Create (output, settings)

            xml.WriteTo (writer)

            output

    ///Functions for serialising and deserialising XML using data contracts
    module Xml = 

        let private getSerialiser (type' : Type) = 
            DataContractSerializer (type', Seq.empty, Int32.MaxValue, true, false, (Surrogate.get ()))

        ///Writes an object as XML
        let write (data : obj) = 

            ///Only perform cleanup when necessary
            let cleanUp (stream : MemoryStream) =
                if (Contracts.containsOptionalDataMembers data) then
                    CleanUp.apply stream
                else
                    stream

            writeUsing cleanUp getSerialiser data

        ///Reads an XML object from a given stream
        let read : Stream -> Type -> obj option =
            readUsing getSerialiser

    ///Functions for serialising and deserialising JSON using data contracts
    module Json = 

        let private getSerialiser (type' : Type) = 
            Json.DataContractJsonSerializer (type', Seq.empty, Int32.MaxValue, true, (Surrogate.get ()), false)

        ///Writes an object as JSON
        let write : obj -> byte list = 
            writeUsing id getSerialiser

        ///Reads a JSON object from a given stream
        let read : Stream -> Type -> obj option = 
            readUsing getSerialiser