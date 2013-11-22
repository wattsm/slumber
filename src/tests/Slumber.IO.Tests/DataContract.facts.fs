namespace Slumber.IO.Tests

open System
open System.IO
open System.Runtime.Serialization
open System.Text
open FsUnit.Xunit
open Xunit
open Slumber.IO.DataContract

module ``DataContract facts`` =

    [<AutoOpen>]
    module Helpers = 

        let getString (data : byte list) = 
            
            use stream = new MemoryStream (data |> List.toArray)
            use reader = new StreamReader (stream)

            reader.ReadToEnd ()

        let getStream (data : String) = 

            let bytes = Encoding.UTF8.GetBytes (data)
            
            new MemoryStream (bytes)

    [<AutoOpen>]
    module Model = 

        [<DataContract (Name = "simple", Namespace = "")>]
        type SimpleRecord = {
            [<field: DataMember (Name = "value")>] Value : String;
        }

        [<DataContract (Name = "complex", Namespace = "")>]
        type ComplexRecord = { 
            [<field: DataMember (Name = "value", Order = 0)>] Value : String;
            [<field: DataMember (Name = "child", Order = 1)>] Child : SimpleRecord;
        }

        [<DataContract (Name = "optional", Namespace = "")>]
        type OptionalRecord = {
            [<field: DataMember (Name = "child")>] Child : SimpleRecord option;
        }

        [<DataContract (Name = "collection", Namespace = "")>]
        [<NoComparison; CustomEquality>]
        type CollectionRecord = {
            [<field: DataMember (Name = "items")>] Items : OptionalRecord seq;
        }
        with

            override this.Equals x = 
                match x with
                | :? CollectionRecord as other ->
                    this.Items
                    |> Seq.forall (fun item ->
                            other.Items
                            |> Seq.exists ((=) item)
                        )

                | _ -> false

            override this.GetHashCode () = 1

    [<AutoOpen>]
    module TestData =
        let simpleRecord = { SimpleRecord.Value = "Hello, World"; }
        let complexRecord = { ComplexRecord.Value = "Hello, World (1)"; Child = { SimpleRecord.Value = "Hello, World (2)"; }; }
        let optionalRecordSome = { OptionalRecord.Child = Some ({ SimpleRecord.Value = "Hello, World"; }); }
        let optionalRecordNone = { OptionalRecord.Child = None; }
        let collectionRecord = { CollectionRecord.Items = [ { OptionalRecord.Child = None; }; { OptionalRecord.Child = Some { SimpleRecord.Value = "Hello, World"; }; }; ]; }

    module ``Xml facts`` = 

        let [<Literal>] ModuleName = "IO.DataContract.Xml"

        let simpleData = "<simple xmlns:i=\"http://www.w3.org/2001/XMLSchema-instance\"><value>Hello, World</value></simple>"
        let complexData = "<complex xmlns:i=\"http://www.w3.org/2001/XMLSchema-instance\"><value>Hello, World (1)</value><child><value>Hello, World (2)</value></child></complex>"
        let optionalDataSome = "<optional xmlns:i=\"http://www.w3.org/2001/XMLSchema-instance\"><child><value>Hello, World</value></child></optional>"
        let optionalDataNone = "<optional xmlns:i=\"http://www.w3.org/2001/XMLSchema-instance\"><child i:nil=\"true\" /></optional>"
        let collectionData = "<collection xmlns:i=\"http://www.w3.org/2001/XMLSchema-instance\"><items><optional><child i:nil=\"true\" /></optional><optional><child><value>Hello, World</value></child></optional></items></collection>"

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``read facts`` = 

            let [<Fact>] ``Simple record is deserialised correctly`` () = 
                let stream = getStream simpleData
                in
                    Xml.read stream typeof<SimpleRecord>
                    |> Option.get
                    |> should equal simpleRecord

            let [<Fact>] ``Complex record is deserialised correctly`` () = 
                let stream = getStream complexData
                in
                    Xml.read stream typeof<ComplexRecord>
                    |> Option.get
                    |> should equal complexRecord

            let [<Fact>] ``Optional record is deserialised correctly (with Some)`` () = 
                let stream = getStream optionalDataSome
                in
                    Xml.read stream typeof<OptionalRecord>
                    |> Option.get
                    |> should equal optionalRecordSome

            let [<Fact>] ``Optional record is deserialised correctly (with None)`` () = 
                let stream = getStream optionalDataNone
                in
                    Xml.read stream typeof<OptionalRecord>
                    |> Option.get
                    |> should equal optionalRecordNone

            let [<Fact>] ``Collection record is deserialsied correclty`` () =
                let stream = getStream collectionData
                in
                    Xml.read stream typeof<CollectionRecord>
                    |> Option.get
                    |> should equal collectionRecord

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``write facts`` = 

            let [<Fact>] ``Simple record is serialised correctly`` () =
                simpleRecord
                |> Xml.write
                |> getString
                |> should equal simpleData

            let [<Fact>] ``Complex record is serialised correctly`` () = 
                complexRecord
                |> Xml.write
                |> getString
                |> should equal complexData

            let [<Fact>] ``Optional record is serialised correctly (with Some)`` () = 
                optionalRecordSome
                |> Xml.write
                |> getString
                |> should equal optionalDataSome

            let [<Fact>] ``Optional record is serialised correctly (with None)`` () = 
                optionalRecordNone
                |> Xml.write
                |> getString
                |> should equal optionalDataNone

            let [<Fact>] ``Collection record serialised correctly`` () = 
                collectionRecord
                |> Xml.write
                |> getString
                |> should equal collectionData

    module ``Json facts`` = 

        let [<Literal>] ModuleName = "IO.DataContract.Json"

        let simpleData = "{\"value\":\"Hello, World\"}"
        let complexData = "{\"value\":\"Hello, World (1)\",\"child\":{\"value\":\"Hello, World (2)\"}}"
        let optionalDataSome = "{\"child\":{\"value\":\"Hello, World\"}}"
        let optionalDataNone = "{\"child\":null}"
        let collectionData = "{\"items\":[{\"child\":null},{\"child\":{\"value\":\"Hello, World\"}}]}"

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``read facts`` = 
            
            let [<Fact>] ``Simple record is deserialised correctly`` () = 
                let stream = getStream simpleData
                in
                    Json.read stream typeof<SimpleRecord>
                    |> Option.get
                    |> should equal simpleRecord

            let [<Fact>] ``Complex record is deserialised correctly`` () = 
                let stream = getStream complexData
                in
                    Json.read stream typeof<ComplexRecord>
                    |> Option.get
                    |> should equal complexRecord

            let [<Fact>] ``Optional record is deserialised correctly (with Some)`` () = 
                let stream = getStream optionalDataSome
                in
                    Json.read stream typeof<OptionalRecord>
                    |> Option.get
                    |> should equal optionalRecordSome

            let [<Fact>] ``Optional record is deserialised correctly (with None)`` () = 
                let stream = getStream optionalDataNone
                in
                    Json.read stream typeof<OptionalRecord>
                    |> Option.get
                    |> should equal optionalRecordNone

            let [<Fact>] ``Collection record is deserialsied correclty`` () =
                let stream = getStream collectionData
                in
                    Json.read stream typeof<CollectionRecord>
                    |> Option.get
                    |> should equal collectionRecord

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``write facts`` = 

            let [<Fact>] ``Simple record is serialised correctly`` () =
                simpleRecord
                |> Json.write
                |> getString
                |> should equal simpleData

            let [<Fact>] ``Complex record is serialised correctly`` () = 
                complexRecord
                |> Json.write
                |> getString
                |> should equal complexData

            let [<Fact>] ``Optional record is serialised correctly (with Some)`` () = 
                optionalRecordSome
                |> Json.write
                |> getString
                |> should equal optionalDataSome

            let [<Fact>] ``Optional record is serialised correctly (with None)`` () = 
                optionalRecordNone
                |> Json.write
                |> getString
                |> should equal optionalDataNone

            let [<Fact>] ``Collection record serialised correctly`` () = 
                collectionRecord
                |> Json.write
                |> getString
                |> should equal collectionData

    module ``Contracts facts`` = 

        let [<Literal>] ModuleName = "IO.DataContract.Contracts"
        
        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``typeContainsOptionalDataMembers facts`` = 

            let [<Fact>] ``Returns true for types containing optional data members`` () =
                typeof<OptionalRecord>
                |> Contracts.typeContainsOptionalDataMembers 
                |> should be True

            let [<Fact>] ``Returns true for types containing data member collections of a type that contains optional data members`` () =
                typeof<CollectionRecord>
                |> Contracts.typeContainsOptionalDataMembers
                |> should be True

            let [<Fact>] ``Returns false for types containing no optional data members`` () =
                typeof<ComplexRecord>
                |> Contracts.typeContainsOptionalDataMembers
                |> should be False

            

