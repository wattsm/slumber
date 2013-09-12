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
open System
open Slumber
open Slumber.Framework
open Slumber.Setup
open Slumber.IO.DataContract

///Use this to authenticate requests
let authenticate (request : Request) = 
  match (request.Payload.Headers |> Headers.getValue "Authorization") with
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

You can access URL segments, query string parameters etc. by adding an argument to your function with the same name, e.g.

```fsharp

/// GET /widget-catalog?search=term

let getWidgetCatalog (search : String option) =   
  
  let widgets = storage.GetWidgets (search)
  
  { Widgets = widgets; }  
```

Slumber will get value type and string arugments from URL segments or the query string. If the argument type is not ``Option<T>`` and a corresponding value
cannot be found then Slumber will return an HTTP 400 response.

Functions may also accept a single argument representing the request body. The argument type must be a reference type. Again, if the argument is not ``Option<T>`` then if the request has no body
Slumber will return an ``HTTP 400`` response.

Below are some examples.

```fsharp

/// GET /widget-catalog
let getWidgets () = ...

/// POST /widget-catalog
let addWidget (widget : WidgetMessage) = ...

/// PUT /widget-cataog/{id}
let updateWidget (id : Int32) (widget : WidgetMessage) = ...

/// DELETE /widget-catalog/{id}
let deleteWidget (id : Int32) = ...

/// GET /widget-search?search=term[&orderBy=something]
let searchWidgets (search : String) (orderBy : String option) = ...
```

#### Status codes

Status codes used by Slumber:

* ``200 OK`` - default status code for successful operations.
* ``404 Not Found`` - when no endpoint matching the request URL can be found.
* ``405 Method Not Allowed`` - when the endpoint does not support the HTTP verb used.
* ``415 Unsupported Media Type`` - when the Content-Type header contains an unsupported media type.
* ``406 Not Acceptable`` - when the Accept header contains an unsupported media type **OR** an error occurs during resource serialisation.
* ``400 Bad Request`` - when an error occurs during message deserialisation **OR** the operation expects a message or parameter but none was supplied.
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
