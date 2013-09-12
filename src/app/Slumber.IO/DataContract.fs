namespace Slumber.IO

open System
open System.IO
open System.Runtime.Serialization
open Slumber.Framework.MessageIO

module DataContract = 
    
    ///Helper functions for use with data data contract serialisers
    [<AutoOpen>]
    module private Helpers = 

        ///Creates a serialisation function using the given serialiser
        let writeUsing (getSerialiser : Type -> #XmlObjectSerializer) (data : obj)  =
            match data with
            | null -> []
            | _ ->

                let serialiser = 
                    data.GetType ()
                    |> getSerialiser

                use stream = 
                    new MemoryStream ()

                serialiser.WriteObject (stream, data)

                stream.ToArray () 
                |> Array.toList

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

    ///Functions for serialising and deserialising XML using data contracts
    module Xml = 

        let private getSerialiser (type' : Type) = 
            DataContractSerializer (type')

        ///Writes an object as XML
        let write : obj -> byte list = 
            writeUsing getSerialiser

        ///Reads an XML object from a given stream
        let read : Stream -> Type -> obj option =
            readUsing getSerialiser

    ///Functions for serialising and deserialising JSON using data contracts
    module Json = 

        let private getSerialiser (type' : Type) = 
            Json.DataContractJsonSerializer (type')

        ///Writes an object as JSON
        let write : obj -> byte list = 
            writeUsing getSerialiser

        ///Reads a JSON object from a given stream
        let read : Stream -> Type -> obj option = 
            readUsing getSerialiser