namespace Slumber.Tests

open System
open FsUnit
open Xunit
open Slumber

module ``Setup facts`` =
    
    open Configuration
    open Setup
    open Http

    module ``Bindings facts`` = 

        let [<Literal>] ModuleName = "Setup.Bindings"

        [<AutoOpen>]
        module Helpers =

            let noop () = 
                ()

            let getVerb (binding : Binding) = 
                binding.Verb

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``get function`` = 

            let [<Fact>] ``Creates a binding for the GET verb`` () =
                get noop |> getVerb |> should equal Verbs.Get

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``post function`` = 

            let [<Fact>] ``Creates a binding for the POST verb`` () =
                post noop |> getVerb |> should equal Verbs.Post

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``put function`` = 

            let [<Fact>] ``Creates a binding for the PUT verb`` () =
                put noop |> getVerb |> should equal Verbs.Put

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``delete function`` = 

            let [<Fact>] ``Creates a binding for the DELETE verb`` () =
                delete noop |> getVerb |> should equal Verbs.Delete

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``options function`` =

            let [<Fact>] ``Creates a binding for the OPTIONS verb`` () =
                options noop |> getVerb |> should equal Verbs.Options

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``head function`` =

            let [<Fact>] ``Creates a binding for the HEAD verb`` () =
                head noop |> getVerb |> should equal Verbs.Head

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``patch function`` =

            let [<Fact>] ``Creates a binding for the PATCH verb`` () =
                patch noop |> getVerb |> should equal Verbs.Patch

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``bind function`` = 

            let getMessageType binding = 
                binding.MessageType

            let bindAndCall f = 

                let binding = 
                    bind "VERB" f

                let context = 

                    let baseUrl = 
                        Uri ("http://localhost:8080", UriKind.Absolute)

                    {
                        Metadata = 
                            {
                                OperationMetadata.Empty
                                with
                                    Url = 
                                        {
                                            Raw = baseUrl;
                                            Path = "/";
                                            Query = [];
                                            BaseUrl = baseUrl;
                                        };
                                    Parameters = [];
                            };
                        Message = (Some (box "Hello, World")); //NOTE This will be passed to message and optional message accepting functions only
                    }

                context
                |> binding.Operation
                |> ignore

            let [<Fact>] ``Message type is set correctly for functions accepting unit`` () =
                bind "VERB" (fun () -> ()) |> getMessageType |> should be None'<Type>

            let [<Fact>] ``Message type is set correctly for functions accepting a message`` () =
                bind "VERB" (fun (_ : String) -> ()) |> getMessageType |> should be (Some' typeof<String>)

            let [<Fact>] ``Message type is set correctly for functions accepting an optional message`` () =
                bind "VERB" (fun (_ : String option) ->  ()) |> getMessageType |> should be (Some' typeof<String>)

            let [<Fact>] ``Message type is set correctly for functions accepting an OperationContext`` () =
                bind "VERB" (fun (_ : OperationContext) -> ()) |> getMessageType |> should be None'<Type>

            let [<Fact>] ``Message type is set correctly for functions accepting a metadata tuple`` () =
                bind "VERB" (fun (_ : (String * OperationMetadata)) -> ()) |> getMessageType |> should be (Some' typeof<String>)

            let [<Fact>] ``Message type is set correctly for functions accepting an optional metadata tuple`` () =
                bind "VERB" (fun (_ : (String option * OperationMetadata)) -> ()) |> getMessageType |> should be (Some' typeof<String>)

            let [<Fact>] ``Message type is set correctly for functions accepting OperationMetadata`` () =
                bind "VERB" (fun (_ : OperationMetadata) -> ()) |> getMessageType |> should be None'<Type>

            //TODO Everythign below here should refer to "input type" instead of "message type" really

            let [<Fact>] ``Message type of optional OperationContext raises NotSupportedException`` () =
                (fun () -> 
                    bind "VERB" (fun (_ : OperationContext option) -> ()) 
                    |> ignore
                ) |> should throw typeof<NotSupportedException>

            let [<Fact>] ``Resource type of optional OperationResult raises NotSupportedException`` () =
                (fun () -> 
                    bind "VERB" (fun () -> (Some OperationResult.Empty)) 
                    |> ignore
                ) |> should throw typeof<NotSupportedException>

            let [<Fact>] ``Message type of optional metadata tuple raises NotSupportedException`` () =
                (fun () ->
                    bind "VERB" (fun (_ : (String * OperationMetadata) option) -> ()) 
                    |> ignore
                ) |> should throw typeof<NotSupportedException>

            let [<Fact>] ``Message type of optional metadata raises NotSupportedException`` () =
                (fun () ->
                    bind "VERB" (fun ( _ : OperationMetadata option) -> ())
                    |> ignore
                ) |> should throw typeof<NotSupportedException>                 

            let [<Fact>] ``Functions accepting unit and returning unit are wrapped correctly`` () =
                
                let _called = ref false

                bindAndCall (fun () -> 

                    _called := true
                    
                    ()
                )

                Assert.True (_called.Value)
                
            let [<Fact>] ``Functions accepting unit and returning a resource are wrapped correctly`` () =
                
                let _called = ref false

                bindAndCall (fun () ->

                    _called := true

                    "Hello, World"
                )

                Assert.True (_called.Value)

            let [<Fact>] ``Functions accepting unit and returning an optional resource are wrapped correctly`` () =
                 
                 let _called = ref false

                 bindAndCall (fun () ->

                    _called := true

                    (Some "Hello, World")
                 )

                 Assert.True (_called.Value)

            let [<Fact>] ``Functions accepting unit and returning an OperationResult are wrapped correctly`` () =
                
                let _called = ref false

                bindAndCall (fun () ->

                    _called := true

                    OperationResult.Empty
                )

                Assert.True (_called.Value)

            let [<Fact>] ``Functions accepting a message and returning a unit are wrapped correctly`` () =
                
                let _called = ref false

                bindAndCall (fun (_ : String) -> 

                    _called := true

                    ()
                )

                Assert.True (_called.Value)

            let [<Fact>] ``Functions accepting a message and returning a resource are wrapped correctly`` () =
                
                let _called = ref false

                bindAndCall (fun (_ : String) ->

                    _called := true

                    "Hello, World"
                )

                Assert.True (_called.Value)

            let [<Fact>] ``Functions accepting a message and returning an optional resource are wrapped correctly`` () =
                
                let _called = ref false

                bindAndCall (fun (_ : String) ->
                    
                    _called := true

                    (Some "Hello, World")
                )

                Assert.True (_called.Value)

            let [<Fact>] ``Functions accepting a message and returning an OperationResult are wrapped correctly`` () =
                
                let _called = ref false

                bindAndCall (fun (_ : String) -> 
                    
                    _called := true

                    OperationResult.Empty
                )

                Assert.True (_called.Value)
                
            let [<Fact>] ``Functions accepting an optional message and returning a unit are wrapped correctly`` () =
                
                let _called = ref false

                bindAndCall (fun (_ : String option) -> 

                    _called := true

                    ()
                )

                Assert.True (_called.Value)

            let [<Fact>] ``Functions accepting an optional message and returning a resource are wrapped correctly`` () =
                
                let _called = ref false

                bindAndCall (fun (_ : String option) ->

                    _called := true

                    "Hello, World"
                )

                Assert.True (_called.Value)

            let [<Fact>] ``Functions accepting an optional message and returning an optional resource are wrapped correctly`` () =
                
                let _called = ref false

                bindAndCall (fun (_ : String option) ->
                    
                    _called := true

                    (Some "Hello, World")
                )

                Assert.True (_called.Value)

            let [<Fact>] ``Functions accepting an optional message and returning an OperationResult are wrapped correctly`` () =
                
                let _called = ref false

                bindAndCall (fun (_ : String option) -> 
                    
                    _called := true

                    OperationResult.Empty
                )

                Assert.True (_called.Value)

            let [<Fact>] ``Functions accepting an OperationContext and returning a unit are wrapped correctly`` () =
                
                let _called = ref false

                bindAndCall (fun (_ : OperationContext) -> 

                    _called := true

                    ()
                )

                Assert.True (_called.Value)

            let [<Fact>] ``Functions accepting an OperationContext and returning a resource are wrapped correctly`` () =
                
                let _called = ref false

                bindAndCall (fun (_ : OperationContext) ->

                    _called := true

                    "Hello, World"
                )

                Assert.True (_called.Value)

            let [<Fact>] ``Functions accepting an OperationContext and returning an optional resource are wrapped correctly`` () =
                
                let _called = ref false

                bindAndCall (fun (_ : OperationContext) ->
                    
                    _called := true

                    (Some "Hello, World")
                )

                Assert.True (_called.Value)

            let [<Fact>] ``Functions accepting an OperationContext and returning an OperationResult are wrapped correctly`` () =
                
                let _called = ref false

                bindAndCall (fun (_ : OperationContext) -> 
                    
                    _called := true

                    OperationResult.Empty
                )

                Assert.True (_called.Value)

            let [<Fact>] ``Functions accepting a tuple of a message and OperationMetadata and returning a unit are wrapped correctly`` () =
                
                let _called = ref false

                bindAndCall (fun (args : String * OperationMetadata) ->

                    _called := true

                    ()
                )

                Assert.True (_called.Value)

            let [<Fact>] ``Functions accepting a tuple of a message and OperationMetadata and returning a resource are wrapped correctly`` () =
                
                let _called  = ref false

                bindAndCall (fun (args : String * OperationMetadata) ->
                    
                    _called := true

                    "Hello, World"
                )

                Assert.True (_called.Value)

            let [<Fact>] ``Functions accepting a tuple of a message and OperationMetadata and returning an optional resource are wrapped correctly`` () =
                
                let _called = ref false

                bindAndCall (fun (args : String * OperationMetadata) ->

                    _called := true

                    Some ("Hello, World")
                )

                Assert.True (_called.Value)

            let [<Fact>] ``Functions accepting a tuple of a message and OperationMetadata and returning an OperationResult are wrapped correctly`` () =
                
                let _called = ref false

                bindAndCall (fun (args : String * OperationMetadata) ->

                    _called := true

                    OperationResult.Empty
                )

                Assert.True (_called.Value)

            let [<Fact>] ``Functions accepting a tuple of an optional message and OperationMetadata and returning a unit are wrapped correctly`` () =
                
                let _called = ref false

                bindAndCall (fun (args : String option * OperationMetadata) ->

                    _called := true

                    ()
                )

                Assert.True (_called.Value)

            let [<Fact>] ``Functions accepting a tuple of an optional message and OperationMetadata and returning a resource are wrapped correctly`` () =
                
                let _called  = ref false

                bindAndCall (fun (args : String option * OperationMetadata) ->
                    
                    _called := true

                    "Hello, World"
                )

                Assert.True (_called.Value)

            let [<Fact>] ``Functions accepting a tuple of an optional message and OperationMetadata and returning an optional resource are wrapped correctly`` () =

                let _called = ref false

                bindAndCall (fun (args : String option * OperationMetadata) ->

                    _called := true

                    Some ("Hello, World")
                )

                Assert.True (_called.Value)

            let [<Fact>] ``Functions accepting a tuple of an optional message and OperationMetadata and returning an OperationResult are wrapped correctly`` () =
                
                let _called = ref false

                bindAndCall (fun (args : String option * OperationMetadata) ->

                    _called := true

                    OperationResult.Empty
                )

                Assert.True (_called.Value)

            let [<Fact>] ``Functions accepting OperationMetadata and returning a unit are wrapped correctly`` () =
                
                let _called = ref false

                bindAndCall (fun (_ : OperationMetadata) ->

                    _called := true

                    ()
                )

                Assert.True (_called.Value)

            let [<Fact>] ``Functions accepting OperationMetadata and returning a resource are wrapped correctly`` () =
                
                let _called  = ref false

                bindAndCall (fun (_ : OperationMetadata) ->
                    
                    _called := true

                    "Hello, World"
                )

                Assert.True (_called.Value)

            let [<Fact>] ``Functions accepting OperationMetadata and returning an optional resource are wrapped correctly`` () =

                let _called = ref false

                bindAndCall (fun (_ : OperationMetadata) ->

                    _called := true

                    Some ("Hello, World")
                )

                Assert.True (_called.Value)

            let [<Fact>] ``Functions accepting OperationMetadata and returning an OperationResult are wrapped correctly`` () =
                
                let _called = ref false

                bindAndCall (fun (_ : OperationMetadata) ->

                    _called := true

                    OperationResult.Empty
                )

                Assert.True (_called.Value)

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``public' function`` = 

            let [<Fact>] ``Sets public flag to true on binding`` () = 

                let op _ = OperationResult.Empty
                let verb f = bind "VERB" f

                let binding = 
                    public' verb op

                binding.IsPublic |> should be True

    module ``Endpoints facts`` = 

        open Configuration.Endpoints

        let [<Literal>] ModuleName = "Setup.Endpoints"

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``endpointAt function`` = 

            let [<Fact>] ``Created endpoint has correct template`` () =
                let template = "http://localhost:8080/"
                in endpointAt template |> getTemplate |> should equal template

            let [<Fact>] ``Created endpoint has empty binding collection`` () =
                endpointAt "http://localhost:8080/" |> getBindings |> List.isEmpty |> should be True

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``supporting function`` = 

            let [<Fact>] ``Binding is added to endpoint`` () =

                let binding = 
                    {
                        Binding.Empty
                        with
                            Verb = "VERB"
                    }

                let binding' = 
                    Endpoint.Empty 
                    |> supporting binding
                    |> getBindings
                    |> List.head

                Assert.Equal<String> ("VERB", binding'.Verb)

            let [<Fact>] ``Duplicate verb raises SetupException`` () =

                let binding = 
                    {
                        Binding.Empty
                        with
                            Verb = "VERB";
                    }

                (fun () ->
                    Endpoint.Empty
                    |> supporting binding
                    |> supporting binding
                    |> ignore
                ) |> should throw typeof<SetupException>

    module ``Containers facts`` = 

        open Configuration.Containers

        let [<Literal>] ModuleName = "Setup.Containers"

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``containerAt function`` = 

            open System.Web

            let uri = 
                Uri ("http://localhost", UriKind.Absolute)

            let [<Fact>] ``Created container has correct base URL`` () =
                containerAt uri |> getBaseUrl |> should equal uri

            let [<Fact>] ``Relative URI raises NotSupportedException`` () =
                (fun () ->
                    containerAt (Uri ("/api", UriKind.Relative)) 
                    |> ignore
                ) |> should throw typeof<NotSupportedException>

            let [<Fact>] ``Created container has empty endpoints collection`` () =
                containerAt uri |> getEndpoints |> List.isEmpty |> should be True

            let [<Fact>] ``Created container has an empty reader collection`` () =
                containerAt uri |> getReaders |> List.isEmpty |> should be True

            let [<Fact>] ``Created container has an empty writer collection`` () =
                containerAt uri |> getWriters |> List.isEmpty |> should be True

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``authenticatedBy function`` = 

            let [<Fact>] ``Makes container private by default`` () =

                let isPrivate mode = 
                    match mode with
                    | Private _ -> true
                    | _ -> false

                Container.Empty
                |> authenticatedBy (fun _ -> Deny)
                |> getSecurityMode
                |> isPrivate
                |> should be True

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``with' function`` =

            let [<Fact>] ``Endpoint is added to container`` () =
                
                let endpoint = 
                    { Endpoint.Empty with Template = "/"; }

                let endpoint' = 
                    Container.Empty
                    |> with' endpoint
                    |> getEndpoints
                    |> List.head

                Assert.Equal<String> ("/", endpoint'.Template)

            let [<Fact>] ``Endpoints with equivalent templates raises SetupException`` () =
                
                let endpoint1 = 
                    { Endpoint.Empty with Template = "/{id}"; }

                let endpoint2 = 
                    { Endpoint.Empty with Template = "/{no}"; }

                (fun () ->
                    Container.Empty
                    |> with' endpoint1
                    |> with' endpoint2
                    |> ignore
                ) |> should throw typeof<SetupException>

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``all function`` = 

            let [<Fact>] ``Binding is added to all existing endpoints`` () =

                let hasCorrectBindings container = 
                    container.Endpoints
                    |> List.exists (fun endpoint ->
                            endpoint.Bindings
                            |> List.map (fun binding -> binding.Verb)
                            |> List.exists (fun verb -> verb = "OPTIONS")
                            |> not
                        )
                    |> not

                let container = 
                    {
                        Container.Empty
                        with
                            Endpoints = 
                                [
                                    Endpoint.Empty;
                                    Endpoint.Empty;
                                ]
                    }

                container
                |> all { Binding.Empty with Verb = "OPTIONS"; }
                |> hasCorrectBindings
                |> should be True

            let [<Fact>] ``NotSupportedException is thrown if any endpoints exist which already have bindings for the verb`` () =
                
                let container = 
                    {
                        Container.Empty
                        with
                            Endpoints = 
                                [
                                    {
                                        Endpoint.Empty
                                        with
                                            Bindings = 
                                                [
                                                    {
                                                        Binding.Empty
                                                        with
                                                            Verb = "OPTIONS";
                                                    }
                                                ];
                                    }
                                ];
                    }

                (fun () ->
                    container
                    |> all ({ Binding.Empty with Verb = "OPTIONS"; })
                    |> ignore

                ) |> should throw typeof<NotSupportedException>


        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``writing function`` =

            let write _ =
                []

            let [<Fact>] ``Writer is added to content types`` () =
                Container.Empty
                |> writing MediaTypes.Text.Xml write
                |> getWriter MediaTypes.Text.Xml
                |> Option.isSome
                |> should be True

            let [<Fact>] ``Duplicate content type raises SetupException`` () =
                (fun () ->

                    Container.Empty
                    |> writing MediaTypes.Text.Xml write
                    |> writing MediaTypes.Text.Xml write
                    |> ignore

                ) |> should throw typeof<SetupException>

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``reading function`` =

            let read _ _ =
                None

            let [<Fact>] ``Reader is added to content types`` () =
                Container.Empty
                |> reading MediaTypes.Text.Xml read
                |> getReader MediaTypes.Text.Xml
                |> Option.isSome
                |> should be True

            let [<Fact>] ``Duplicate content type raises SetupException`` () =
                (fun () ->

                    Container.Empty
                    |> reading MediaTypes.Text.Xml read
                    |> reading MediaTypes.Text.Xml read
                    |> ignore

                ) |> should throw typeof<SetupException>

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``forwarding function`` =

            let [<Fact>] ``Forwarded types are added to contianer`` () =
                Container.Empty
                |> forwarding MediaTypes.Text.Html MediaTypes.Text.Xml
                |> applyForwarding MediaTypes.Text.Html
                |> should equal MediaTypes.Text.Xml

            let [<Fact>] ``Duplicate from type raises SetupException`` () =
                (fun () ->

                    Container.Empty
                    |> forwarding MediaTypes.Text.Html MediaTypes.Text.Xml
                    |> forwarding MediaTypes.Text.Html MediaTypes.Application.Json
                    |> ignore

                ) |> should throw typeof<SetupException>


                

            
                

                

