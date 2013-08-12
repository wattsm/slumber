namespace Slumber

open System
open Slumber.Common.Http
open Slumber.Execution

///Contains functions and types for describing and working with Slumber's configuration
module Configuration =

    ///Represents a verb bound operation binding
    type Binding = {
        Verb : String;
        MessageType : Type option;
        Operation : Operation;
        IsPublic : bool;
    }          
    with

        ///The empty binding
        static member Empty =
            {
                Verb = String.Empty;
                MessageType = None;
                Operation = (fun _ -> OperationResult.Empty);
                IsPublic = false;
            }

    ///Represents a URL bound endpoint exposing zero or more operations
    type Endpoint = {
        Template : String;
        Bindings : Binding list;
    }
    with

        ///The empty endpoint
        static member Empty =
            {
                Template = String.Empty;
                Bindings = [];
            }

    ///Functions for working with endpoints
    module Endpoints = 

        ///Gets an endpoint's template
        let getTemplate endpoint = 
            endpoint.Template

        ///Gets an endpoint's binding collection
        let getBindings endpoint = 
            endpoint.Bindings

        ///Tries to get a binding by verb
        let tryGetBinding verb endpoint = 
            endpoint.Bindings
            |> List.tryFind (fun binding ->
                    String.same verb binding.Verb
                )

    ///Represents supported content types 
    type IOConfig = {
        Readers : (String * MessageIO.Reader) list;
        Writers : (String * MessageIO.Writer) list;
        ForwardedTypes : (String * String) list;
    }
    with

        ///The empty content types selection
        static member Empty = 
            {
                Readers = [];
                Writers = [];
                ForwardedTypes = [];
            }

    ///Union describing possible results of user authentication
    type AuthenticationResult = 
        | Allow of (UserData option)
        | Deny

    ///Union describing the possible security modes of a container
    type SecurityMode = 
        | Public
        | Private of (Request -> AuthenticationResult)

    ///Represents a collection of endpoints and associated configuration data
    type Container = {
        Endpoints : Endpoint list;
        IO : IOConfig;
        BaseUrl : Uri;
        SecurityMode : SecurityMode;
    }
    with

        ///The empty container
        static member Empty = 
            {
                Endpoints = [];
                IO = IOConfig.Empty;
                BaseUrl = Uri ("http://localhost/", UriKind.Absolute);
                SecurityMode = Public;
            }

    ///Contains functions for working with containers
    module Containers = 

        ///Gets a container's endpoint collection
        let getEndpoints container =
            container.Endpoints

        ///Gets a container's base URL
        let getBaseUrl container = 
            container.BaseUrl

        ///Gets the container's IO configuration
        let getIO container = 
            container.IO

        ///Gets the readers configured for a container
        let getReaders container = 
            container.IO.Readers

        let private getMessageIO contentType = 
            List.tryPick (fun (contentType', io) ->
                    if (String.same contentType contentType') then
                        Some io
                    else
                        None
                )

        ///Gets the reader for the given content type
        let getReader contentType container = 
            container.IO.Readers
            |> getMessageIO contentType

        ///Gets the writers configured for a container
        let getWriters container = 
            container.IO.Writers

        ///Gets the writer for the given content type
        let getWriter contentType container = 
            container.IO.Writers
            |> getMessageIO contentType

        ///True if a type is forwarded
        let isForwarded contentType container = 
            container.IO.ForwardedTypes
            |> List.exists (fst >> String.same contentType)

        ///Applies forwarding to a content type, returning the fowrarded type if one is configured
        let applyForwarding contentType container = 

            let forwardedType = 
                container.IO.ForwardedTypes
                |> List.tryPick (fun (from, to') ->
                        if (String.same from contentType) then
                            Some to'
                        else
                            None
                    )

            match forwardedType with
            | None -> contentType
            | Some contentType -> contentType

        ///Gets the security mode of a container
        let getSecurityMode container = 
            container.SecurityMode

    ///Union describing possible modes of configuration
    type ConfigurationMode = 
        | Explicit of Container
        | Implicit
        | Mixed of (Container -> Container)

    ///Describes a service container
    type IContainerDescription =
        abstract member Describe : Uri -> Container