namespace Slumber

open System
open Slumber.Common.Http
open Slumber.Framework

///Contains functions for generating Slumber configuration in a fluent style
module Setup = 

    //TODO Is OperationContext binding obsolete?

    ///An exception used for errors during setup
    type SetupException (message : String) = 
        inherit Exception (message)

    ///Raises a SetupException with the given message
    let invalidSetup message = 
        raise (SetupException (message))

    ///Contains functions for creating bindings
    [<AutoOpen>]
    module Bindings = 

        ///Contains wrappers which convert functions into operations
        module private Wrappers = 

            open HandyFS.Types
            open HandyFS.Types.Summaries

            //NOTE These types exist purely as the way that functions are wrapped as operations
            //requires reflection. 

            ///Interface defining a wrapper exposing an operation
            type IOperationWrapper =                 
                abstract member Invoke : OperationContext -> OperationResult

            ///A basic wrapper
            type OperationWrapper (operation : Operation) = 
                interface IOperationWrapper  with

                    member this.Invoke context = 
                        operation context

            //TODO Revisit module and function names

            ///Contains helper functions for wrapping functions 
            [<AutoOpen>]
            module Helpers = 

                ///Casts a message to a given type
                let getMessageAs<'TMessage> (context : OperationContext) = 
                    match context.Message with
                    | None -> None
                    | Some value -> Some (value :?> 'TMessage)

                ///Calls a function with the operation message or returns the given status code
                let withMessageOr<'TMessage> context f statusCode = 
                    match (context |> getMessageAs<'TMessage>) with
                    | None -> OperationResult.StatusOnly statusCode
                    | Some message -> f message

                ///Calls a function with a message and returns the default status code
                let returningNothing f message = 
                    f message
                    OperationResult.Empty

                ///Calls a function with a message and returns a resource with the default status code
                let returningResource f message = 
                    message
                    |> f
                    |> OperationResult.ResourceOnly
                    
                ///Converts an optional resource to a result
                let getOptionalResult resource = 
                    match resource with
                    | Some resource' -> OperationResult.ResourceOnly resource'
                    | _ -> OperationResult.Empty    
                    
                ///Calls a function with a message and returns an optional resource with the default status code
                let returningOptionalResource f = 
                    f >> getOptionalResult

                ///Selects the appropriate wrapper based on the input and result types
                let getWrapperType inputType resultType (forUnit, forOptional, forResource, forResult) = 
                    match (getTypeGroup resultType) with
                    | Unit' -> forUnit inputType
                    | Optional resultType' -> 
                        if (isType<OperationResult> resultType') then
                            raise (NotSupportedException ( "Operations returning an OperationResult cannot mark them as optional."))
                        else
                            forOptional inputType resultType'
                    | Definite ->
                        if (isType<OperationResult> resultType) then
                            forResult inputType
                        else
                            forResource inputType resultType

            ///Functions for creating operations which accept no input
            [<RequireQualifiedAccess>]
            module NoMessage = 

                [<AutoOpen>]
                module Functions = 

                    ///Creates a unit to unit operation
                    let toUnit (f : unit -> unit) =                     
                        fun (_ : OperationContext) ->

                            f ()

                            OperationResult.Empty                
                    
                    ///Creates a unit to resource operation
                    let toResource<'TResource> (f : unit -> 'TResource) = 
                        fun (_ : OperationContext) ->
                            f ()
                            |> OperationResult.ResourceOnly      
                    
                    ///Creates a unit to optional resource operation
                    let toOptionalResource<'TResource> (f : unit -> 'TResource option) =     
                        fun (_ : OperationContext) ->
                            match (f ()) with
                            | Some resource -> OperationResult.ResourceOnly resource
                            | _ -> OperationResult.Empty    

                    ///Creates a unit to operation result operation
                    let toResult (f : unit -> OperationResult) = 
                        fun (_ : OperationContext) ->
                            f ()     

                [<AutoOpen>]
                module Types = 

                    ///A wrapper for unit to unit functions
                    type UnitWrapper (f) = 
                        inherit OperationWrapper (toUnit f)

                    ///A wrapper for unit to resource functions
                    type ResourceWrapper<'TResource> (f : unit -> 'TResource) = 
                        inherit OperationWrapper (toResource f)

                    ///A wrapper for unit to optional resource functions
                    type OptionalResourceWrapper<'TResource> (f : unit -> 'TResource option) = 
                        inherit OperationWrapper (toOptionalResource f)
                        
                    ///A wrapper for unit to result functions
                    type ResultWrapper (f) = 
                        inherit OperationWrapper (toResult f)             

                ///Functions which create wrapper types for operations accepting no message
                let private wrapperTypes = 
                    (
                        (fun _ -> typeof<UnitWrapper>),
                        (fun _ output -> makeGenericType typedefof<OptionalResourceWrapper<_>> [ output ]),
                        (fun _ output -> makeGenericType typedefof<ResourceWrapper<_>> [ output ]),
                        (fun _ -> typeof<ResultWrapper>)
                    )

                ///Gets the appropriate wrapper type for a given result type
                let getWrapperType' resultType = 
                    getWrapperType
                    <| typeof<Unit>
                    <| resultType
                    <| wrapperTypes

            ///Functions for creating operations which accept a message
            [<RequireQualifiedAccess>]
            module RequiredMessage = 

                [<AutoOpen>]
                module Functions = 

                    ///Creates a message to unit operation
                    let toUnit<'TMessage> (f : 'TMessage -> unit) = 
                        fun context ->
                            withMessageOr<'TMessage>
                            <| context
                            <| returningNothing f
                            <| StatusCodes.BadRequest

                    ///Creates a message to resource operation
                    let toResource<'TMessage, 'TResource> (f : 'TMessage -> 'TResource) = 
                        fun context ->
                            withMessageOr<'TMessage>
                            <| context
                            <| returningResource f
                            <| StatusCodes.BadRequest 

                    ///Creates a message to optional resource operation
                    let toOptionalResource<'TMessage, 'TResource> (f : 'TMessage -> 'TResource option) = 
                        fun context ->
                            withMessageOr<'TMessage>
                            <| context
                            <| returningOptionalResource f
                            <| StatusCodes.BadRequest

                    ///Creates a message to operation result operation
                    let toResult<'TMessage> (f : 'TMessage -> OperationResult) = 
                        fun context -> 
                            withMessageOr<'TMessage>
                            <| context
                            <| f
                            <| StatusCodes.BadRequest

                [<AutoOpen>]
                module Types = 

                    ///Wrapper for a message to unit function
                    type UnitWrapper<'TMessage> (f : 'TMessage -> unit) =
                        inherit OperationWrapper (toUnit f)

                    ///Wrapper for a message to resource function
                    type ResourceWrapper<'TMessage, 'TResource> (f : 'TMessage -> 'TResource) = 
                        inherit OperationWrapper (toResource f)

                    ///Wrapper for a message to optional resource function
                    type OptionalResourceWrapper<'TMessage, 'TResource> (f : 'TMessage -> 'TResource option) = 
                        inherit OperationWrapper (toOptionalResource f)

                    ///Wrapper for a message to result function
                    type ResultWrapper<'TMessage> (f : 'TMessage -> OperationResult) = 
                        inherit OperationWrapper (toResult f)

                ///Functions which create wrapper types for operations accepting a message
                let private wrapperTypes = 
                    (
                        (fun input -> makeGenericType typedefof<UnitWrapper<_>> [ input ]),
                        (fun input output -> makeGenericType typedefof<OptionalResourceWrapper<_, _>> [ input; output ]),
                        (fun input output -> makeGenericType typedefof<ResourceWrapper<_, _>> [ input; output ]),
                        (fun input -> makeGenericType typedefof<ResultWrapper<_>> [ input ])
                    )

                ///Gets the appropriate wrapper type for a given input and result type
                let getWrapperType' inputType resultType = 
                    getWrapperType
                    <| inputType
                    <| resultType
                    <| wrapperTypes

            ///Functions for creating operations which accept an optional message
            [<RequireQualifiedAccess>]
            module OptionalMessage =

                [<AutoOpen>]
                module Functions = 

                    ///Creates an optional message to unit operation
                    let toUnit<'TMessage> (f : 'TMessage option -> unit) = 
                        fun (context : OperationContext) ->

                            context
                            |> getMessageAs<'TMessage>
                            |> f

                            OperationResult.Empty

                    ///Creates an optional message to resource operation
                    let toResource<'TMessage, 'TResource> (f : 'TMessage option -> 'TResource) = 
                        fun context ->
                            context
                            |> getMessageAs<'TMessage>
                            |> f
                            |> OperationResult.ResourceOnly

                    ///Creates an optional message to optional resource operation
                    let toOptionalResource<'TMessage, 'TResource> (f : 'TMessage option -> 'TResource option) = 
                        fun context ->
                            context
                            |> getMessageAs<'TMessage>
                            |> f
                            |> getOptionalResult

                    ///Creates an optional message to result operation
                    let toResult<'TMessage> (f : 'TMessage option -> OperationResult) =
                        fun context ->
                            context
                            |> getMessageAs<'TMessage>
                            |> f

                [<AutoOpen>]
                module Types = 
                
                    ///Wraps an optional message to unit function
                    type UnitWrapper<'TMessage> (f : 'TMessage option -> unit) = 
                        inherit OperationWrapper (toUnit f)

                    ///Wraps an optional message to resource function
                    type ResourceWrapper<'TMessage, 'TResource> (f : 'TMessage option -> 'TResource) =
                        inherit OperationWrapper (toResource f)

                    ///Wraps an optional message to optional resource function
                    type OptionalResourceWrapper<'TMessage, 'TResource> (f : 'TMessage option -> 'TResource option) = 
                        inherit OperationWrapper (toOptionalResource f)

                    ///Wraps an optional message to result function
                    type ResultWrapper<'TMessage> (f : 'TMessage option -> OperationResult) = 
                        inherit OperationWrapper (toResult f)

                ///Functions which create wrapper types for operations accepting an optional message
                let private wrapperTypes = 
                    (
                        (fun input -> makeGenericType typedefof<UnitWrapper<_>> [ input ]),
                        (fun input output -> makeGenericType typedefof<OptionalResourceWrapper<_, _>> [ input; output ]),
                        (fun input output -> makeGenericType typedefof<ResourceWrapper<_, _>> [ input; output ]),
                        (fun input -> makeGenericType typedefof<ResultWrapper<_>> [ input ])
                    )

                ///Gets the appropriate wrapper type for a given input and result type
                let getWrapperType' inputType resultType = 
                    getWrapperType
                    <| inputType
                    <| resultType
                    <| wrapperTypes

            ///Functions and types for creates operations which accept an operation context
            [<RequireQualifiedAccess>]
            module Context = 

                [<AutoOpen>]
                module Functions = 
               
                    ///Creates a context to unit operation
                    let toUnit (f : OperationContext -> unit) = 
                        fun context ->

                            f context

                            OperationResult.Empty

                    ///Creates a context to resource operation
                    let toResource<'TResource> (f : OperationContext -> 'TResource) = 
                        fun context -> 

                            context
                            |> f
                            |> OperationResult.ResourceOnly

                    ///Creates a context to optional resource operation
                    let toOptionalResource<'TResource> (f : OperationContext -> 'TResource option) = 
                        fun context -> 
                            context
                            |> f
                            |> getOptionalResult  

                [<AutoOpen>]
                module Types = 

                    ///Wrapper for context to unit functions
                    type UnitWrapper (f : OperationContext -> unit) = 
                        inherit OperationWrapper (toUnit f)

                    ///Wrapper for context to resource functions
                    type ResourceWrapper<'TResource> (f : OperationContext -> 'TResource) = 
                        inherit OperationWrapper (toResource f)
                        
                    ///Wrapper for context to optional resource functions
                    type OptionalResourceWrapper<'TResource> (f : OperationContext -> 'TResource option) =
                        inherit OperationWrapper (toOptionalResource f)

                ///Functions which create wrapper types for operations accepting an operation context
                let private wrapperTypes = 
                    (
                        (fun _ -> typeof<UnitWrapper>),
                        (fun _ output -> makeGenericType typedefof<OptionalResourceWrapper<_>> [ output ]),
                        (fun _ output -> makeGenericType typedefof<ResourceWrapper<_>> [ output ]),
                        (fun _ -> typeof<OperationWrapper>)
                    )

                ///Gets the appropriate wrapper type for a given result type
                let getWrapperType' resultType = 
                    getWrapperType
                    <| typeof<OperationContext>
                    <| resultType
                    <| wrapperTypes

            ///Functions and types for creating operations which accept a message and metadata tuple
            [<RequireQualifiedAccess>]
            module MetadataTuple = 

                type Args<'TMessage> = 'TMessage * OperationMetadata

                [<AutoOpen>]
                module Functions = 

                    ///Creates a tuple to unit operation
                    let toUnit (f : Args<'TMessage> -> unit) =
                        fun (context : OperationContext) ->

                            let f' message = 
                                returningNothing
                                <| f
                                <| (message, context.Metadata)

                            withMessageOr<'TMessage>
                            <| context
                            <| f'
                            <| StatusCodes.BadRequest

                    ///Creates a tuple to resource operation
                    let toResource (f : Args<'TMessage> -> 'TResource) =
                        fun (context : OperationContext) ->

                            let f' message = 
                                returningResource
                                <| f
                                <| (message, context.Metadata)

                            withMessageOr<'TMessage>
                            <| context
                            <| f'
                            <| StatusCodes.BadRequest

                    ///Creates a tuple to optional resource operation
                    let toOptionalResource (f : Args<'TMessage> -> 'TResource option) = 
                        fun (context : OperationContext) ->

                            let f' message = 
                                returningOptionalResource
                                <| f
                                <| (message, context.Metadata)

                            withMessageOr<'TMessage>
                            <| context
                            <| f'
                            <| StatusCodes.BadRequest

                    ///Creates a tuple to operation result operation
                    let toResult (f : Args<'TMessage> -> OperationResult) =
                        fun (context : OperationContext) ->
                        
                            let f' message = 
                                f (message, context.Metadata)

                            withMessageOr<'TMessage>
                            <| context
                            <| f'
                            <| StatusCodes.BadRequest

                [<AutoOpen>]
                module Types = 

                    ///Wrapper for tuple to unit operations
                    type UnitWrapper<'TMessage> (f : Args<'TMessage> -> unit) =
                        inherit OperationWrapper (toUnit f)

                    ///Wrapper for tuple to resource operations
                    type ResourceWrapper<'TMessage, 'TResource> (f : Args<'TMessage> -> 'TResource) = 
                        inherit OperationWrapper (toResource f)

                    ///Wrapper for tuple to optional resource operations
                    type OptionalResourceWrapper<'TMessage, 'TResource> (f : Args<'TMessage> -> 'TResource option) = 
                        inherit OperationWrapper (toOptionalResource f)

                    ///Wrapper for tuple to operation result operations
                    type ResultWrapper<'TMessage> (f : Args<'TMessage> -> OperationResult) = 
                        inherit OperationWrapper (toResult f)

                ///Functions which create wrapper types for operations accepting an optional message and metadata
                let private wrapperTypes = 
                    (
                        (fun input -> makeGenericType typedefof<UnitWrapper<_>> [ input; ]),
                        (fun input output -> makeGenericType typedefof<OptionalResourceWrapper<_, _>> [ input; output; ]),
                        (fun input output -> makeGenericType typedefof<ResourceWrapper<_, _>> [ input; output; ]),
                        (fun input -> makeGenericType typedefof<ResultWrapper<_>> [ input; ])
                    )

                ///Gets the appropriate wrapper type for a given input type and result type
                let getWrapperType' inputType resultType = 
                    getWrapperType
                    <| inputType
                    <| resultType
                    <| wrapperTypes

            ///Functions and types for creating operations which accept an optional message and metadata tuple
            [<RequireQualifiedAccess>]
            module OptionalMetadataTuple = 

                type Args<'TMessage> = 'TMessage option * OperationMetadata

                let private usingMetadataFrom context f = 
                    fun message ->
                        f (message, context.Metadata)

                [<AutoOpen>]
                module Functions = 

                    ///Creates a tuple to unit operation
                    let toUnit (f : Args<'TMessage> -> unit) =
                        fun (context : OperationContext) ->

                            let f' = usingMetadataFrom context f

                            context
                            |> getMessageAs<'TMessage>
                            |> f'

                            OperationResult.Empty

                    ///Creates a tuple to resource operation
                    let toResource (f : Args<'TMessage> -> 'TResource) =
                        fun (context : OperationContext) ->

                            let f' = usingMetadataFrom context f

                            context
                            |> getMessageAs<'TMessage>
                            |> f'
                            |> OperationResult.ResourceOnly

                    ///Creates a tuple to optional resource operation
                    let toOptionalResource (f : Args<'TMessage> -> 'TResource option) = 
                        fun (context : OperationContext) ->

                            let f' = usingMetadataFrom context f

                            context
                            |> getMessageAs<'TMessage>
                            |> f'
                            |> getOptionalResult

                    ///Creates a tuple to operation result operation
                    let toResult (f : Args<'TMessage> -> OperationResult) =
                        fun (context : OperationContext) ->
                        
                            let f' = usingMetadataFrom context f

                            context
                            |> getMessageAs<'TMessage>
                            |> f'

                [<AutoOpen>]
                module Types = 

                    ///Wrapper for tuple to unit operations
                    type UnitWrapper<'TMessage> (f : Args<'TMessage> -> unit) =
                        inherit OperationWrapper (toUnit f)

                    ///Wrapper for tuple to resource operations
                    type ResourceWrapper<'TMessage, 'TResource> (f : Args<'TMessage> -> 'TResource) = 
                        inherit OperationWrapper (toResource f)

                    ///Wrapper for tuple to optional resource operations
                    type OptionalResourceWrapper<'TMessage, 'TResource> (f : Args<'TMessage> -> 'TResource option) = 
                        inherit OperationWrapper (toOptionalResource f)

                    ///Wrapper for tuple to operation result operations
                    type ResultWrapper<'TMessage> (f : Args<'TMessage> -> OperationResult) = 
                        inherit OperationWrapper (toResult f)

                ///Functions which create wrapper types for operations accepting an optional message and metadata
                let private wrapperTypes = 
                    (
                        (fun input -> makeGenericType typedefof<UnitWrapper<_>> [ input; ]),
                        (fun input output -> makeGenericType typedefof<OptionalResourceWrapper<_, _>> [ input; output; ]),
                        (fun input output -> makeGenericType typedefof<ResourceWrapper<_, _>> [ input; output; ]),
                        (fun input -> makeGenericType typedefof<ResultWrapper<_>> [ input; ])
                    )

                ///Gets the appropriate wrapper type for a given input type and result type
                let getWrapperType' inputType resultType = 
                    getWrapperType
                    <| inputType
                    <| resultType
                    <| wrapperTypes

            ///Functions for creating operations which accept only metadata
            [<RequireQualifiedAccess>]
            module MetadataOnly = 

                [<AutoOpen>]
                module Functions = 

                    ///Creates a metadata to unit operation
                    let toUnit (f : OperationMetadata -> unit) =                     
                        fun (ctx : OperationContext) ->

                            f ctx.Metadata

                            OperationResult.Empty                
                    
                    ///Creates a metadata to resource operation
                    let toResource<'TResource> (f : OperationMetadata -> 'TResource) = 
                        fun (ctx : OperationContext) ->
                            ctx.Metadata
                            |> f
                            |> OperationResult.ResourceOnly      
                    
                    ///Creates a metadata to optional resource operation
                    let toOptionalResource<'TResource> (f : OperationMetadata -> 'TResource option) =     
                        fun (ctx : OperationContext) ->
                            match (f ctx.Metadata) with
                            | Some resource -> OperationResult.ResourceOnly resource
                            | _ -> OperationResult.Empty    

                    ///Creates a metadata to operation result operation
                    let toResult (f : OperationMetadata -> OperationResult) = 
                        fun (ctx : OperationContext) ->
                            f ctx.Metadata     

                [<AutoOpen>]
                module Types = 

                    ///A wrapper for metadata to unit functions
                    type UnitWrapper (f : OperationMetadata -> unit) = 
                        inherit OperationWrapper (toUnit f)

                    ///A wrapper for metadata to resource functions
                    type ResourceWrapper<'TResource> (f : OperationMetadata -> 'TResource) = 
                        inherit OperationWrapper (toResource f)

                    ///A wrapper for metadata to optional resource functions
                    type OptionalResourceWrapper<'TResource> (f : OperationMetadata -> 'TResource option) = 
                        inherit OperationWrapper (toOptionalResource f)
                        
                    ///A wrapper for metadata to result functions
                    type ResultWrapper (f : OperationMetadata -> OperationResult) = 
                        inherit OperationWrapper (toResult f)             

                ///Functions which create wrapper types for operations accepting metadata
                let private wrapperTypes = 
                    (
                        (fun _ -> typeof<UnitWrapper>),
                        (fun _ output -> makeGenericType typedefof<OptionalResourceWrapper<_>> [ output ]),
                        (fun _ output -> makeGenericType typedefof<ResourceWrapper<_>> [ output ]),
                        (fun _ -> typeof<ResultWrapper>)
                    )

                ///Gets the appropriate wrapper type for a given result type
                let getWrapperType' resultType = 
                    getWrapperType
                    <| typeof<Unit>
                    <| resultType
                    <| wrapperTypes

            //Union categorising operation input types
            type InputType =                 
                | Message of (Type * bool)
                | MetadataTuple of (Type * bool)
                | Metadata
                | Context

            ///True if a type is a tuple of a message type and OperationMetadata
            let isMetadataTuple type' = 
                if (isGenericType<_ * _> type') then
                    Array.get (type'.GetGenericArguments ()) 1
                    |> isType<OperationMetadata>                    
                else
                    false

            ///Describes a type as an operation input
            let getInputType (type' : Type) = 
                match (getTypeGroup type') with
                | Unit' -> None
                | Optional optionalType -> 
                    if (isType<OperationContext> optionalType) then
                        raise (NotSupportedException ("Operations accepting an OperationContext cannot mark them as optional."))

                    if (isType<OperationMetadata> optionalType) then 
                        raise (NotSupportedException ("Operations accepting OperationMetadata cannot mark them as optional."))

                    if (isMetadataTuple optionalType) then
                        raise (NotSupportedException ("Operations accepting a metadata tuple cannot mark it as optional."))

                    Some (Message (optionalType, true))
                | Definite ->
                    if (isMetadataTuple type') then

                        let messageType = 
                            type'.GetGenericArguments ()
                            |> Array.head

                        match (getTypeGroup messageType) with
                        | Optional messageType' -> Some (MetadataTuple (messageType', true))
                        | Definite -> Some (MetadataTuple (messageType, false))
                        | _ -> raise (NotSupportedException ("Operations accepting a metadata tuple cannot use a unit as the message type."))
                    
                    else if (isType<OperationContext> type') then
                        Some Context

                    else if (isType<OperationMetadata> type') then
                        Some Metadata
                        
                    else                        
                        Some (Message (type', false))                    
            
            ///Wraps a function, creating an operation based on its parameters
            let wrap<'TInput, 'TOutput> (f : 'TInput -> 'TOutput) = 

                //TODO This function is extremely hacky and smells a lot. There must be a better way of doing this.
                //Member overloading wasn't quite up to the task due to the fact that functions matching 'x option -> 'y
                //will also match 'x -> 'y. A 'x not option would be handy.

                (**
                    NOTE This function will wrap functions as operations.

                    The following input types are explicitly supported:
                    - unit
                    - OperationContext
                    - 'TMessage
                    - 'TMessage option
                    - 'TMessage * OperationMetadata
                    - 'TMessage option * OperationMetdata

                    The following result types are supported:
                    - unit
                    - OperationResult
                    - 'TResource
                    - 'TResource option
                **)

                let inputType = typeof<'TInput>
                let resultType = typeof<'TOutput>                                               

                let wrapperType, messageType = 
                    match (getInputType inputType) with
                    | None -> (NoMessage.getWrapperType' resultType, None)
                    | Some value ->   
                        match value with
                        | Message (messageType, true) -> (OptionalMessage.getWrapperType' messageType resultType, Some messageType)
                        | Message (messageType, false) -> (RequiredMessage.getWrapperType' messageType resultType, Some messageType)
                        | MetadataTuple (messageType, true) -> (OptionalMetadataTuple.getWrapperType' messageType resultType, Some messageType)
                        | MetadataTuple (messageType, false) -> (MetadataTuple.getWrapperType' messageType resultType, Some messageType)
                        | Metadata -> (MetadataOnly.getWrapperType' resultType, None)
                        | Context -> (Context.getWrapperType' resultType, None)

                let wrapper = 

                    //TODO There must be a better way to do this. Activator.CreateInstance seems to be very literal about parameter types.

                    let constructor' = 
                        wrapperType.GetConstructor ([| f.GetType() |])

                    //TODO Exception for null constructor; "No suitable wrapper could be found for your function". Is this even possible? Types which cannot be deserialised will be trapped at runtime.

                    constructor'.Invoke ([| f |]) :?> IOperationWrapper

                let operation = 
                    fun context -> 
                        wrapper.Invoke context

                (operation, messageType)

        ///Creates a new binding based on a function
        let bind verb f = 
            let operation, messageType = Wrappers.wrap f
            in
                {
                    Binding.Empty
                    with
                        Verb = verb;
                        Operation = operation;
                        MessageType = messageType;
                }

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