### Slumber - An F# REST framework

_ALPHA RELEASE_

#### Introduction

Slumber is a framework for creating RESTful applications in a functional style using F#, inspired heavily by the excellent [OpenRasta](http://openrasta.org/).

I have tried to keep Slumber as lightweight as possible while also allowing developers as much freedom as possible. I will look to frequently expand and refine the framework.

#### Solution overview

The Slumber solution contains 5 projects:

* ``Slumber`` - the main library.
* ``Slumber.IO`` - contains predefined serialisers.
* ``Slumber.Tests`` - Xunit tests for the main Slumber library.
* ``Slumber.Example`` - contains a (very simple) example application, hosted by...
* ``Slumber.Example.Web`` - A C# web app using the default Slumber HTTP handler to handle requests.

The Slumber solution file is a Visual Studio 2012 solution, so may require tweaking to open in Visual Studio 2010.

#### A basic example

Below are the basic highlights of how Slumber works. For a more complete example see the ``Slumber.Example`` and ``Slumber.Example.Web`` projects.

Imagine a simple data model:

```fsharp
[<DataContract (Name = "widget", Namespace = "")>]
type Widget = {

  [<field: DataMember (Name = "code")>]
  Code : String;
  
  [<field: DataMember (Name = "name")>]
  Name : String;
  
  [<field: DataMember (Name = "capacity")>]
  Capacity : Int32;

}

[<DataContract (Name = "widget-catalog", Namespace = "")>]
type WidgetCatalog = {

  [<field: DataMember (Name = "widgets")>]
  Widgets : Widget list;
}
```

And some functions for working with it:

```fsharp
let storage = ... //Some storage mechanism, e.g. database

let getWidgetCatalog () = 
  let widgets = storage.GetWidgets ()
  in { Widgets = widgets; }

let addWidget (widget : Widget) =
  storage.AddWidget (widget) //unit  
```

These functions can be exposed as RESTful endpoints using Slumber:

```fsharp
open Slumber
open Slumber.Common.Http
open Slumber.Common.Http.Headers
open Slumber.Configuration
open Slumber.IO.DataContract
open Slumber.Setup

///Use this to authenticate requests
let authenticate (request : Request) = 
  match (request.Payload |> getHeaderValue "Authorization") with
  | Some username -> Allow (Some { Id = username; Properties = []; })
  | _ -> Deny

///Slumber configuration
containerAt (relativeUri baseUrl "/")
|> authenticatedBy authenticate
|> with' (
    endpointAt "/widget-catalog"
    |> supporting (get getWidgetCatalog)
    |> supporting (post addWidget)
  )
|> reading "text/xml" Xml.read
|> reading "application/json" Json.read
|> writing "text/xml" Xml.write
|> writing "application/json" Json.write
```

If you want more control over status codes and response headers you can return an OperationResult from your functions.

```fsharp
let addWidget (widget : Widget) =
  if (storage.Exists widget.Code) then
    OperationResult.StatusOnly 400
  else
    storage.AddWidget (widget)
    OperationResult.StatusOnly 201 
```

Similarly if you want access to URL segments, query string parameters etc. you can access them via OperationMetadata.

```fsharp
let getWidgetCatalog (meta : OperationMetadata) = 
  
  let search = getParameter "search" meta
  let widgets = storage.GetWidgets (search)
  
  { Widgets = widgets; }  
```

#### Operation types

Slumber supports various function signatures for operations, but all have the basic structure ``'Input -> 'Output``.

For inputs the following are supported:

* Unit
* 'TMessage
* 'TMessage option
* ('TMessage * OperationMetadata)
* ('TMessage option * OperationMetadata)
* OperationMetadata
* OperationContext

Where 'TMessage is a serialisable message type, e.g. ``Widget`` in the example above.

For outputs the following are supported:

* Unit
* 'TResource
* 'TResource option
* OperationResult

Where 'TResource is a serialisable resource type, e.g. ``WidgetCatalog`` in the example above.

#### Status codes

Status codes used by Slumber:

* ``200 OK`` - default status code for successful operations.
* ``404 Not Found`` - when no endpoint matching the request URL can be found.
* ``405 Method Not Allowed`` - when the endpoint does not support the HTTP verb used.
* ``415 Unsupported Media Type`` - when the Content-Type header contains an unsupported media type.
* ``406 Not Acceptable`` - when the Accept header contains an unsupported media type **OR** an error occurs during resource serialisation.
* ``400 Bad Request`` - when an error occurs during message deserialisation **OR** the operation expects a message but none was supplied.
* ``500 Internal Server Error`` - when an unhandled exception occurs during execution of an operation.

#### Future plans

Slumber currently supports the basic features required to create a RESTful application in F# but there is a lot of room for improvement.

* Clearer, more concise module structure.
* Better content negotiation (e.g. currently the Content-Type / Accept header has to exactly match a supported media type).
* Ability fine tune supported media types by operation.
* Ability to fine tune authentication - e.g. specify an authentication function but have all operations public by default.
* Restrict handler invocation to just URLs for which endpoints have been specified; allowing you to serve static files from the same site.
* Configuration or hooks supporting:
  * Request validation;
  * Message validation;
  * Operation authorization.
* etc.
