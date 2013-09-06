namespace Slumber

open System
open Slumber.Common.Http
open Slumber.Framework

///Contains functions for generating Slumber configuration in a fluent style
module Setup = 

    ///An exception used for errors during setup
    type SetupException (message : String) = 
        inherit Exception (message)

    ///Raises a SetupException with the given message
    let invalidSetup message = 
        raise (SetupException (message))

    ///Contains functions for creating bindings
    [<AutoOpen>]
    module Bindings = 

        open System.Reflection
        open HandyFS.Types
        open HandyFS.Types.Summaries

        let [<Literal>] FunctionInvokeMethodName = "Invoke"

        ///Union describing possible binding target types
        type TargetType =
            | Function
            | Other

        ///Gets the target type for a given type
        let rec getTargetType (type' : Type) = 
            if (isGenericType<Microsoft.FSharp.Core.FSharpFunc<_, _>> type') then
                Function
            else if (isType<Object> type'.BaseType) then
                Other
            else
                getTargetType type'.BaseType

        ///Union describing supported types of function arguments
        type ArgumentType = 
            | Unit'
            | Message of (bool * Type)
            | Parameter of (String * bool * Type)
            | Metadata

        ///Gets the argument type for a given type
        let getArgumentType (parameter : ParameterInfo) = 
            
            let summary = 
                getTypeSummary parameter.ParameterType

            if (isUnit summary.BaseType) then

                Unit'

            else if (isType<OperationMetadata> summary.BaseType) then

                if summary.IsOptional then
                    invalidSetup "Optional OperationMetadata is not supported."

                Metadata

            else if (summary.BaseType.IsValueType || isType<String> summary.BaseType) then

                Parameter (parameter.Name, summary.IsOptional, summary.BaseType)

            else

                Message (summary.IsOptional, summary.BaseType)

        ///Gets the argument types for a given method
        let getArgumentTypes (method' : MethodInfo) = 
            method'.GetParameters ()
            |> Array.Parallel.map getArgumentType

        ///Gets the message type from an array of argument types
        let getMessageType =
            Array.tryPick (fun argType ->
                match argType with
                | Message (_, messageType) -> Some messageType
                | _ -> None
            )

        ///Gets the value of an argument from a given context
        let getArgumentValue context argType =

            let getMessage isOptional messageType = 
                match context.Message with
                | None -> 
                    if (not isOptional) then
                        raise (FormatException ("A message is required but is not present."))
                
                    makeNone messageType

                | Some message -> 
                    if isOptional then
                        makeSome messageType message
                    else
                        message

            let getParameter isOptional name parameterType = 
                match (tryGetParameter name context.Metadata) with
                | None -> 
                    if (not isOptional) then
                        raise (FormatException (String.Format ("The parameter {0} is required but is not present.", name)))

                    makeNone parameterType

                | Some value ->

                    ///TODO Special cases (e.g. date/times)

                    let value' =
                        Convert.ChangeType (value, parameterType)

                    if isOptional then
                        makeSome parameterType value'
                    else
                        value'

            match argType with
            | Unit' -> box ()
            | Metadata -> box context.Metadata
            | Message (isOptional, messageType) -> getMessage isOptional messageType
            | Parameter (name, isOptional, parameterType) -> getParameter isOptional name parameterType

        ///Gets the values of arugments from a given context
        let getArgumentValues context =
            Array.map (getArgumentValue context)

        ///Union describing supported return types
        type ReturnType = 
            | Void
            | Resource of (bool * Type)
            | Result

        ///Gets the return type of a method
        let getReturnType (method' : MethodInfo) = 
            
            let summary = 
                getTypeSummary method'.ReturnType

            if (isUnit summary.BaseType) then
                
                Void

            else if (isType<OperationResult> summary.BaseType) then

                if summary.IsOptional then
                    invalidSetup "Optional OperationResult is not supported."

                Result

            else

                Resource (summary.IsOptional, summary.BaseType)

        ///Gets the invoke method for a function type
        let getFunctionInvokeMethod (type' : Type) = 

            let methods = 
                type'.GetMethods ()
                |> Array.filter (fun method' -> method'.Name = FunctionInvokeMethodName)
                |> Array.sortBy (fun method' -> method'.GetParameters () |> Array.length)

            if (Array.isEmpty methods) then
                None
            else
                Some (methods.[methods.Length - 1])

        ///Converts an optional resource from Some<T> to Some<obj>
        let convertOptionalResource resourceType value = 

            //NOTE That the IsSome property of Option<T> is compiled as an
            //indexed property, and the index value is what is actually inspected.

            //value will be null if operation returned None, so cannot use value.GetType ()
            let optionalType = 
                makeGenericType typedefof<Option<_>> [ resourceType ] 

            let isSomeProperty = 
                optionalType.GetProperty ("IsSome")

            let isSome = 
                isSomeProperty.GetValue (null, [| value |]) :?> bool

            if isSome then

                let valueProperty = 
                    optionalType.GetProperty ("Value")

                Some (valueProperty.GetValue (value))
            else
                None

        ///Gets an operation result for the given value
        let getOperationResult (value : obj) returnType = 
            match returnType with
            | Void -> OperationResult.Empty
            | Result -> value :?> OperationResult
            | Resource (isOptional, resourceType) ->

                let resource = 
                    if isOptional then
                        convertOptionalResource resourceType value
                    else
                        Some value

                { OperationResult.Empty with Resource = resource; }

        ///Binds a target to the given verb
        let bind verb target = 
            
            let type' = getType target

            match (getTargetType type') with
            | Other -> invalidSetup "Only function types are supported as binding targets."
            | Function ->
                match (getFunctionInvokeMethod type') with
                | Some method' ->

                    let argTypes = getArgumentTypes method'
                    let messageType = getMessageType argTypes
                    let returnType = getReturnType method'

                    let op (context : OperationContext) =
                        try 

                            let argValues = getArgumentValues context argTypes
                            let returnValue = method'.Invoke (target, argValues)

                            getOperationResult 
                            <| returnValue 
                            <| returnType

                        with
                        | :? FormatException as e ->

                            logException e "Request was not valid"

                            OperationResult.StatusOnly StatusCodes.BadRequest

                    {
                        Binding.Empty 
                        with
                            MessageType = messageType;
                            Verb = verb;
                            Operation = op;
                    }

                | _ -> invalidOp "Could not locate Invoke method on function type."                

        ///Creates a new GET binding
        let get f =  
            bind Verbs.Get f

        ///Creates a new POST binding
        let post f = 
            bind Verbs.Post f

        ///Creates a new PUT binding
        let put f = 
            bind Verbs.Put f

        ///Creates a new DELETE binding
        let delete f = 
            bind Verbs.Delete f

        ///Creates a new OPTIONS binding
        let options f = 
            bind Verbs.Options f 

        ///Creates a new HEAD binding
        let head f = 
            bind Verbs.Head f

        ///Creates a new PATCH binding
        let patch f = 
            bind Verbs.Patch f

        ///Creates a public binding
        let public' binder f = 
            { (binder f) with IsPublic = true; }

    ///Contains functions for creating endpoints
    [<AutoOpen>]
    module Endpoints = 

        open Slumber.Framework.Core.Endpoints

        ///Creates an endpoint for the given URI template
        let endpointAt template = 
            {
                Endpoint.Empty
                with
                    Name = string (Guid.NewGuid ());
                    Template = template;
            }

        ///Assigns a name to an endpoint
        let named name endpoint = 
            { endpoint with Name = name; }

        ///Adds a binding to an endpoint
        let supporting (binding : Binding) endpoint =
            match (tryGetBinding binding.Verb endpoint) with
            | Some _ -> invalidSetup (String.Format ("The verb {0} is already bound for endpoint at {1}", binding.Verb, endpoint.Template))
            | _ -> 
                { 
                    endpoint 
                    with 
                        Bindings = (binding :: endpoint.Bindings); 
                }

    ///Contains functions for creating containers
    [<AutoOpen>]
    module Containers = 

        open System.Web
        open Slumber.Framework.Core.Containers

        ///Creates an absolute URI
        let absoluteUri (uri : String) = 
            Uri (uri, UriKind.Absolute)

        ///Creates an absolute URI from a relative URI
        let relativeUri baseUrl =
            createAbsoluteUri baseUrl

        ///Creates a new container
        let containerAt (uri : Uri) = 

            if (not uri.IsAbsoluteUri) then
                raise (NotSupportedException ("Relative base URLs are not supported. Consider using the relativeUrl function to create an absolute URL."))

            {
                Container.Empty
                with
                    BaseUrl = uri;
            }

        ///Sets the authentication function to be used by the container
        let authenticatedBy f container = 
            { container with SecurityMode = (Private f); }

        ///Adds an endpoint to a container
        let with' endpoint container = 

            let template = 
                UriTemplate (endpoint.Template)

            let existing = 
                container.Endpoints
                |> List.tryFind (fun endpoint' ->
                    
                        let template' = 
                            UriTemplate (endpoint'.Template)
                            
                        template'.IsEquivalentTo (template)
                    )

            match existing with
            | Some endpoint' -> 
                invalidSetup (String.Format ("The template {0} is equivalent to the template {1} which already exists.", endpoint.Template, endpoint'.Template))
            | _ ->
                {
                    container
                    with
                        Endpoints = (endpoint :: container.Endpoints);
                }

        ///Adds a reader to a container
        let reading (contentType : String) reader container = 
            match (getReader contentType container) with
            | Some _ -> invalidSetup (String.Format ("A reader for {0} has already been configured for this container.", contentType))
            | _ ->
                let io = 
                    {
                        container.IO
                        with
                            Readers = ((contentType, reader) :: container.IO.Readers);
                    }

                { container with IO = io; }

        ///Adds a writer to a container
        let writing (contentType : String) writer container = 
            match (getWriter contentType container) with
            | Some _ -> invalidSetup (String.Format ("A writer for {0} has already been configured for this container.", contentType))
            | _ ->
                let io = 
                    {
                        container.IO
                        with
                            Writers = ((contentType, writer) :: container.IO.Writers);
                    }

                { container with IO = io; }

        ///Sets up content type forwarding
        let forwarding (fromContentType : String) (toContentType : String) container = 
            if (isForwarded fromContentType container) then
                invalidSetup (String.Format ("The content type {0} is already being forwarded.", fromContentType))
            else
                {
                    container
                    with 
                        IO = 
                            {
                                container.IO
                                with
                                    ForwardedTypes = (fromContentType, toContentType) :: container.IO.ForwardedTypes;
                            }
                }    

        ///Applies a binding to all endpoints
        let all (binding : Binding) (container : Container) = 

            let verbAlreadyUsed = 
                container.Endpoints
                |> List.tryPick (Endpoints.tryGetBinding binding.Verb)
                |> Option.isSome

            if verbAlreadyUsed then
                raise (NotSupportedException ("One or more endpoints already has a binding for the specified HTTP verb."))

            let endpoints' = 
                container.Endpoints
                |> List.map (fun endpoint ->
                        {
                            endpoint
                            with
                                Bindings = binding :: endpoint.Bindings;
                        }
                    )

            {
                container
                with
                    Endpoints = endpoints';
            }

    