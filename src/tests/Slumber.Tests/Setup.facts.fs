namespace Slumber.Tests

open System
open FsUnit
open Xunit
open Slumber

module ``Setup facts`` =
    
    open Framework
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
        module ``getTargetType function`` = 

            let private assertIsFunction target = 
                getTargetType (target.GetType ()) |> should equal Function

            let [<Fact>] ``Correct value returned for function (1)`` () =                 
                assertIsFunction (fun () -> ())

            let [<Fact>] ``Correct value returned for function (2)`` () = 
                assertIsFunction (fun (_ : Int32) -> ())

            let [<Fact>] ``Correct value returned for function (3)`` () = 
                assertIsFunction (fun (_ : Int32) -> "Hello, World")
                
            let [<Fact>] ``Correct value returned for function (4)`` () = 
                assertIsFunction (fun (arg0 : Int32) (arg1 : String) -> ())

            let [<Fact>] ``Correct value returned for function (5)`` () = 
                assertIsFunction (fun (_ : OperationMetadata) -> ())

            let [<Fact>] ``Correct value returned for function (6)`` () = 
                assertIsFunction (fun () -> OperationResult.Empty)

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``getArgumentType function`` = 

            [<AutoOpen>]
            module private Helpers = 

                let getArgumentType' (f : 'a -> unit) = 
                
                    let type' = f.GetType ()
                    let invoke = type'.GetMethod ("Invoke")
                    let parameters = invoke.GetParameters ()

                    getArgumentType (parameters.[parameters.Length - 1])

                let isParameter argType = 
                    match argType with
                    | Parameter (_, _, _) -> true
                    | _ -> false

                let isMessage argType = 
                    match argType with
                    | Message (_, _) -> true
                    | _ -> false

                let isUnit (argType : ArgumentType) = 
                    match argType with
                    | Unit' -> true
                    | _ -> false

                let isMetadata argType = 
                    match argType with
                    | Metadata -> true
                    | _ -> false

                let getParameterName argType = 
                    match argType with
                    | Parameter (name, _, _) -> name
                    | _ -> invalidOp "Not a parameter"

                let getType argType = 
                    match argType with
                    | Parameter (_, _, type') -> type'
                    | Message (_, type') -> type'
                    | _ -> invalidOp "Not a parameter or message"

                let isOptional argType = 
                    match argType with
                    | Parameter (_, isOptional, _) -> isOptional
                    | Message (isOptional, _) -> isOptional
                    | _ -> invalidOp "Not a parameter or message"

            let [<Fact>] ``Returns Parameter for value types (1)`` () = 
                getArgumentType' (fun (_ : Int32) -> ()) |> isParameter |> should be True

            let [<Fact>] ``Returns Parameter for value types (2)`` () = 
                getArgumentType' (fun (_ : Decimal) -> ()) |> isParameter |> should be True

            let [<Fact>] ``Returns Parameter for value types (3)`` () = 
                getArgumentType' (fun (_ : Double) -> ()) |> isParameter |> should be True

            let [<Fact>] ``Returns Parameter for value types (4)`` () = 
                getArgumentType' (fun (_ : DateTime) -> ()) |> isParameter |> should be True

            let [<Fact>] ``Returns Parameter for value types (5)`` () = 
                getArgumentType' (fun (_ : bool) -> ()) |> isParameter |> should be True

            let [<Fact>] ``Returns Parameter for Strings`` () = 
                getArgumentType' (fun (_ : String) -> ()) |> isParameter |> should be True

            let [<Fact>] ``Parameter names are set correclty`` () =
                getArgumentType' (fun (arg : String) -> ()) |> getParameterName |> should equal "arg"

            let [<Fact>] ``Parameter types are set correctly`` () =
                getArgumentType' (fun (_ : String) -> ()) |> getType |> should equal typeof<String>

            let [<Fact>] ``Optional flag is set correctly for optional parameter types`` () =
                getArgumentType' (fun (_ : String option) -> ()) |> isOptional |> should be True

            let [<Fact>] ``Optional flag is set correctly for non-optional parameter types`` () =
                getArgumentType' (fun (_ : String) -> ()) |> isOptional |> should be False

            let [<Fact>] ``Returns Message for complex types`` () =
                getArgumentType' (fun (_ : obj) -> ()) |> isMessage |> should be True

            let [<Fact>] ``Message types are set correctly`` () =
                getArgumentType' (fun (_ : obj) -> ()) |> getType |> should equal typeof<obj>

            let [<Fact>] ``Optional flag is set correctly for optional message types`` () =
                getArgumentType' (fun (_ : obj option) -> ()) |> isOptional |> should be True

            let [<Fact>] ``Optional flag is set correctly for non-optional message types`` () =
                getArgumentType' (fun (_ : obj) -> ()) |> isOptional |> should be False

            let [<Fact>] ``Returns Unit for unit`` () =
                getArgumentType' (fun () -> ()) |> isUnit |> should be True

            let [<Fact>] ``Returns Metadata for OperationMetadata`` () =
                getArgumentType' (fun (_ : OperationMetadata) -> ()) |> isMetadata |> should be True

            let [<Fact>] ``SetupException is thrown for optional OperationMetadata`` () =
                (fun () ->
                    getArgumentType' (fun (_ : OperationMetadata option) -> ())
                    |> ignore
                ) |> should throw typeof<SetupException>
           
        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``getReturnType function`` = 
            
            [<AutoOpen>]
            module private Helpers = 

                let getReturnType' (f : unit -> 'a) = 

                    let type' = f.GetType ()
                    let invoke = type'.GetMethod ("Invoke")

                    getReturnType invoke

                let isVoid returnType = 
                    match returnType with
                    | Void -> true
                    | _ -> false

                let isResource returnType = 
                    match returnType with
                    | ReturnType.Resource (_, _) -> true
                    | _ -> false

                let isResult returnType = 
                    match returnType with
                    | Result ->  true
                    | _ -> false

                let isOptional returnType = 
                    match returnType with
                    | ReturnType.Resource (isOptional, _) -> isOptional
                    | _ -> invalidOp "Not a resource"

                let getType returnType = 
                    match returnType with
                    | ReturnType.Resource (_, type') -> type'
                    | _ -> invalidOp "Not a resource"

            let [<Fact>] ``Returns Void for unit`` () =
                getReturnType' (fun () -> ()) |> isVoid |> should be True

            let [<Fact>] ``Returns Result for OperationResult`` () =
                getReturnType' (fun () -> OperationResult.Empty) |> isResult |> should be True

            let [<Fact>] ``SetupException is thrown for optional OperationResult`` () =
                (fun () ->
                    getReturnType' (fun () -> Some OperationResult.Empty)
                    |> ignore
                ) |> should throw typeof<SetupException>

            let [<Fact>] ``Returns Resource for other types`` () =
                getReturnType' (fun () -> "Hello, World") |> isResource |> should be True

            let [<Fact>] ``Optional flag set correctly for optional resources`` () =
                getReturnType' (fun () -> Some "Hello, World") |> isOptional |> should be True

            let [<Fact>] ``Optional flag set correctly for non-optional resources`` () =
                getReturnType' (fun  () -> "Hello, World") |> isOptional |> should be False

            let [<Fact>] ``Type set correctly for resources`` () =
                getReturnType' (fun () -> "Hello, World") |> getType |> should equal typeof<String>

            let [<Fact>] ``Returns Void for async unit`` () = 
                getReturnType' (fun () -> async { return () }) |> isVoid |> should be True

            let [<Fact>] ``Returns Result for async OperationResult`` () = 
                getReturnType' (fun () -> async { return OperationResult.Empty }) |> isResult |> should be True

            let [<Fact>] ``SetupException is thrown for async optional OperationResult`` () = 
                (fun () ->
                    getReturnType' (fun () -> async { return (Some OperationResult.Empty) })
                    |> ignore
                ) |> should throw typeof<SetupException>

            let [<Fact>] ``Returns Resource for async other types`` () = 
                getReturnType' (fun () -> async { return "Hello, World" }) |> isResource |> should be True

            let [<Fact>] ``Optional flag set correctly for async optional resources`` () = 
                getReturnType' (fun () -> async { return (Some "Hello, World") }) |> isOptional |> should be True

            let [<Fact>] ``Optional flag set correctly for async non-optional resources`` () =
                getReturnType' (fun () -> async { return "Hello, World" }) |> isOptional |> should be False

            let [<Fact>] ``Type set correctly for async resource`` () = 
                getReturnType' (fun () -> async { return "Hello, World" }) |> getType |> should equal typeof<String>

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``getOperationResult function`` = 

            [<AutoOpen>]
            module Helpers = 

                let getOperationResult' value returnType = 
                    getOperationResult (box value) returnType

            let [<Fact>] ``Returns OperationResult.Empty for Void return types`` () = 
                getOperationResult' () Void |> should equal OperationResult.Empty

            let [<Fact>] ``Returns value as OperationResult for Result return types`` () = 

                let result = { OperationResult.Empty with StatusCode = (Some 418); }

                getOperationResult' result Result |> should equal result

            let [<Fact>] ``Returns value as resource for optional Resource return type when value is Some`` () = 
                
                let returnType = ReturnType.Resource (true, typeof<String>)
                let result = getOperationResult' (Some "Hello, World") returnType

                result.StatusCode |> Option.isNone |> should be True
                result.Resource |> Option.get |> should equal (box "Hello, World")

            let [<Fact>] ``Returns None as resource for optional Resource return types when the value is None`` () = 
                
                let returnType = ReturnType.Resource (true, typeof<String>)
                let result = getOperationResult' None returnType

                result.StatusCode |> Option.isNone |> should be True
                result.Resource |> Option.isNone |> should be True

            let [<Fact>] ``Returns value as resource for non-optional Resource return types`` () = 
                
                let returnType = ReturnType.Resource (false, typeof<String>)
                let result = getOperationResult' "Hello, World" returnType

                result.StatusCode |> Option.isNone |> should be True
                result.Resource |> Option.get |> should equal (box "Hello, World")

            let [<Fact>] ``Executes async values for Void return types`` () = 

                let run = ref true

                let value = 
                    async { 
                        run := true
                        return ()
                    }

                getOperationResult' value Void |> ignore

                run.Value |> should be True

            let [<Fact>] ``Returns OperationResult.Empty for async Void return types`` () =
                getOperationResult' (async { return () }) Void |> should equal OperationResult.Empty

            let [<Fact>] ``Returns execution result of value as OperationResult for async Result return types`` () =
                
                let result = { OperationResult.Empty with StatusCode = (Some 418); }
                let asyncResult = async { return result }

                getOperationResult' asyncResult Result |> should equal result

            let [<Fact>] ``Returns execution result as resource for optional async Resource return types when execution result of value is Some`` () = 
                
                let returnType = ReturnType.Resource (true, typeof<String>)
                let value = async { return (Some "Hello, World") }
                let result = getOperationResult' value returnType

                result.StatusCode |> Option.isNone |> should be True
                result.Resource |> Option.get |> should equal (box "Hello, World")

            let [<Fact>] ``Returns None as resource for optional async Resource return types when execution result of value is None`` () = 
                
                let returnType = ReturnType.Resource (true, typeof<String>)
                let value = async { return None }
                let result = getOperationResult' value returnType

                result.StatusCode |> Option.isNone |> should be True
                result.Resource |> Option.isNone |> should be True

            let [<Fact>] ``Returns execution result of value as resource for non-optional async Resource return types`` () =
                
                let returnType = ReturnType.Resource (false, typeof<String>)
                let value = async { return "Hello, World" }
                let result = getOperationResult' value returnType

                result.StatusCode |> Option.isNone |> should be True
                result.Resource |> Option.get |> should equal (box "Hello, World")

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

            [<AutoOpen>]
            module private Helpers = 

                let getMessageType binding = 
                    binding.MessageType

                let getMode binding = 
                    binding.SecurityMode

                let bindModifyAndCall modifier f =

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
                                        Request = 
                                            { 
                                                Request.Empty 
                                                with
                                                    Url = 
                                                        {
                                                            Raw = baseUrl;
                                                            Path = "/";
                                                            Query = [];
                                                            BaseUrl = baseUrl;
                                                        };
                                            };
                                };
                            Message = None;
                        }

                    context
                    |> modifier
                    |> binding.Operation

                let bindAndCallWith message = 
                    bindModifyAndCall (fun context ->
                        {
                            context 
                            with
                                Message = message;
                        }
                    )
                    

                let bindAndCall f = 
                    bindAndCallWith (Some (box "Hello, World")) f

                let bindAndCall' f = 
                    bindAndCall f |> ignore

            let [<Fact>] ``Message type is set correctly for functions accepting unit`` () =
                bind "VERB" (fun () -> ()) |> getMessageType |> should be None'<Type>

            let [<Fact>] ``Message type is set correctly for functions accepting a message`` () =
                bind "VERB" (fun (_ : obj) -> ()) |> getMessageType |> should be (Some' typeof<obj>)

            let [<Fact>] ``Message type is set correctly for functions accepting an optional message`` () =
                bind "VERB" (fun (_ : obj option) ->  ()) |> getMessageType |> should be (Some' typeof<obj>)

            let [<Fact>] ``Verb is set correctly`` () = 
                bind "VERB" (fun () -> ()) |> getVerb |> should equal "VERB"

            let [<Fact>] ``Bindings inherit security by default`` () =
                bind "VERB" (fun () -> ()) |> getMode |> Option.isNone |> should be True

            let [<Fact>] ``Argument value retrieved correctly from URL segment`` () =
                
                let _value = ref ""

                let op (arg : String) =
                    
                    _value := arg

                    ()

                let modifier context = 
                    {
                        context
                        with
                            Metadata = 
                                {
                                    context.Metadata
                                    with
                                        Parameters = [ ("arg", "value"); ];
                                }
                    }

                bindModifyAndCall modifier op
                |> ignore

                Assert.Equal<String> ("value", _value.Value)

            let [<Fact>] ``Argument value retrieved correctly from query string`` () =

                let _value = ref ""

                let op (arg : String) =
                    
                    _value := arg

                    ()

                let modifier context = 
                    
                    let url = 
                        {
                            context.Metadata.Request.Url
                            with
                                Query = [ ("arg", "value"); ];
                        }

                    let request = { context.Metadata.Request with Url = url; }

                    { context with Metadata = { context.Metadata with Request = request; }; }

                bindModifyAndCall modifier op
                |> ignore

                Assert.Equal<String> ("value", _value.Value)

            let [<Fact>] ``Operation calls no op targets`` () =
                
                let _called = ref false

                bindAndCall' (fun () -> 

                    _called := true

                    ()
                )

                Assert.True (_called.Value)

            let [<Fact>] ``Operation calls simple targets`` () =
                
                let _called = ref false

                bindAndCall' (fun (_ : Int32 option) -> 

                    _called := true

                    ()
                )

                Assert.True (_called.Value)

            let [<Fact>] ``Operation calls complex targets`` () =
                
                let _called = ref false

                bindAndCall' (fun (meta : OperationMetadata) (_ : Int32 option) -> 

                    _called := true

                    ()
                )

                Assert.True (_called.Value)

            let [<Fact>] ``Correct result is returned for no op targets`` () =
                bindAndCall (fun () -> ()) |> should equal OperationResult.Empty

            let [<Fact>] ``Correct result is returned for resource returning targets`` () =

                let isCorrect result = 
                    match result.Resource with
                    | Some value -> value.Equals "Hello, World"
                    | _ -> false

                bindAndCall (fun () -> "Hello, World")
                |> isCorrect
                |> should be True

            let [<Fact>] ``Correct result is returned for optional resource returning targets (Some)`` () =
                bindAndCall (fun () -> Some "Hello, World") |> should equal (OperationResult.ResourceOnly "Hello, World")

            let [<Fact>] ``Correct result is returned for optional resource returning targets (None)`` () =
                bindAndCall (fun  () -> None) |> should equal OperationResult.Empty

            let [<Fact>] ``Correct result is returned for OperationResult returning targets`` () =
                bindAndCall (fun () -> OperationResult.StatusOnly 12345) |> should equal (OperationResult.StatusOnly 12345)

            let [<Fact>] ``HTTP 400 is returned if required argument is not present`` () =
                bindAndCall (fun  (arg : Int32) -> ()) |> should equal (OperationResult.StatusOnly StatusCodes.BadRequest)

            let [<Fact>] ``HTTP 400 is returned if required message is not present`` () =
                bindAndCallWith None (fun (_ : obj) -> ()) |> should equal (OperationResult.StatusOnly StatusCodes.BadRequest)

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``public' function`` = 

            let [<Fact>] ``Sets security mode to public`` () = 

                let op _ = OperationResult.Empty
                let verb f = bind "VERB" f

                let binding = 
                    public' verb op

                binding.SecurityMode |> Option.get |> should equal SecurityMode.Public

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``private' function`` = 

            let [<Fact>] ``Sets security mode to private`` () = 

                let op _ = OperationResult.Empty
                let verb f = bind "VERB" f

                let binding = 
                    private' verb op

                binding.SecurityMode |> Option.get |> should equal SecurityMode.Private

    module ``Endpoints facts`` = 

        open Framework.Core.Endpoints

        let [<Literal>] ModuleName = "Setup.Endpoints"

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``endpointAt function`` = 

            let [<Fact>] ``Created endpoint has correct template`` () =
                let template = "http://localhost:8080/"
                in endpointAt template |> getTemplate |> should equal template

            let [<Fact>] ``Created endpoint has empty binding collection`` () =
                endpointAt "http://localhost:8080/" |> getBindings |> List.isEmpty |> should be True

            let [<Fact>] ``Created endpoint is assigned a non-empty name`` () =
                endpointAt "http://localhost:8080" |> getName |> String.IsNullOrWhiteSpace |> should be False

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``named function`` = 

            let [<Fact>] ``Sets correct endpoint name`` () = 
                Endpoint.Empty |> named "Test" |> getName |> should equal "Test"

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

        open Framework.Core.Containers

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

            let auth (_ : Request) = 
                Deny

            let setDefault mode container = 
                { container with Security = { container.Security with DefaultMode = mode }; }

            let [<Fact>] ``Sets authentication function`` () =

                let config =
                    Container.Empty
                    |> authenticatedBy auth false
                    |> getSecurityConfig

                config.Authenticate |> Option.isSome |> should be True

            let [<Fact>] ``Sets default mode to private when privateByDefault is true`` () =
                
                let config = 
                    Container.Empty
                    |> setDefault Public
                    |> authenticatedBy auth true
                    |> getSecurityConfig

                config.DefaultMode |> should equal Private

            let [<Fact>] ``Sets default mode to public when privateByDefault is false`` () =

                let config = 
                    Container.Empty
                    |> setDefault Private
                    |> authenticatedBy auth false
                    |> getSecurityConfig

                config.DefaultMode |> should equal Public

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