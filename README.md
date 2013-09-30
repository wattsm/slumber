### Slumber - An F# REST framework

Slumber is a small framework for creating REST applications in a functional style using F#, inspired heavily by the excellent [OpenRasta](http://openrasta.org/).

#### Concepts

Slumber defines a RESTful application using **containers**, **endpoints** and **bindings**.

A **container** represents the top level of an application and is associated with its root URL - e.g. ``http://localhost/application/api``. Each container may contain zero or more **endpoints**. An **endpoint** is simply a URL which exposes some functionality. **Bindings** define the HTTP verbs that an endpoint supports and the F# functions which handle requests.

#### Solution overview

The Slumber solution contains 5 projects:

* ``Slumber`` - the main library.
* ``Slumber.IO`` - contains predefined serialisers.
* ``Slumber.Tests`` - Xunit tests for the main Slumber library.
* ``Slumber.Example`` - contains a (very simple) example application, hosted by...
* ``Slumber.Example.Web`` - A C# web app using the default Slumber HTTP handler to handle requests.

The Slumber solution file is a Visual Studio 2012 solution, so may require tweaking to open in Visual Studio 2010.

#### Example

(A simple working example can be found in the projects ``Slumber.Example`` and ``Slumber.Example.Web``.)

The easiest way to start working with Slumber is to register the supplied HTTP handler in your application's web.config file.

```xml
<configuration>
    ...
    <system.webServer>
      <handlers>
        <add 
          name="Slumber" 
          path="api/*" 
          verb="*" 
          type="Slumber.SlumberHandler, Slumber"
          />
      </handlers>
	</system.webServer>
</configuration>
```

_(The configuration shown here is for IIS 7.5 using an .Net 4 and integrated pipeline mode.)_

Any request made to URLs below ``api`` will now be handled by Slumber.

When the application first starts Slumber's HTTP handler will look for a type implementing the ``Slumber.Framework.Core.IContainerDescription`` interface. The handler will pass
HTTP requests to the container described by this type.

```fsharp
type Config () = 
  interface IContainerDescription with
  
    member this.Describe baseUrl =
      containerAt (relativeUrl baseUrl "/api")
      |> authenticatedBy App.Security.authenticate true
      |> with' (
          endpointAt "/"
          |> supporting (public' get App.Services.list)          
        )
      |> with' (
          endpointAt "/widgets"
          |> supporting (public' get App.Widgets.get)
          |> supporting (post App.Widgets.create)
        )
      |> with' (
          endpointAt "/widgets/{widgetId}"
          |> supporting (public' get App.Widget.get)
          |> supporting (delete App.Widget.delete)
        )
      |> reading "text/xml" Xml.read
      |> writing "text/xml" Xml.write
      |> reading "application/json" Json.read      
      |> writing "application/json" Json.write
      |> forwarding "text/html" "text/xml"      
```

This description tells Slumber where the container is based, how to authenticate requests, what functions are used to handle requests and how to read and what content types are supported.

```fsharp
containerAt (relativeUrl baseUrl "/api")
```

This line tells Slumber that the container's root URL is the web applications's base URL plus "/api". For example, if the application is a virtual directory then it's base URL
might be ``http://localhost/myapp``, meaning this container would be rooted at ``http://localhost/myapp/api``.

```fsharp
|> authenticatedBy App.Security.authenticate true
```

Here we are telling Slumber to use the function ``App.Security.authenticate`` to authenticate requests. The boolean argument tells Slumber whether all services should be 
private by default. Private services can only be accessed if the authentication function returns ``Some userData``.

```fsharp
endpointAt "/"
|> supporting (public' get App.Services.list)
```

Endpoint URLs are relative to the container's URL, so if we continue from the previous example this endpoint will be at ``http://localhost/myapp/api/``. The ``public'`` function
tells Slumber that this service can be accessed without authenticating. 

```fsharp
endpointAt "/widgets/{widgetId}"
|> supporting (public' get App.Widget.get)
```

Placeholders can be used in endpoint URLs and be accessed by name in your function signature, e.g. 

```fsharp
module Widget = 
  let get (widgetId : Int32) = ...
```

_(For more information see the section on function signatures.)_

```fsharp
|> reading "text/xml" Xml.read
|> writing "text/xml" Xml.write
```

These lines are telling Slumber how to deserialise and serialise requests using the "text/xml" media type respectively. This example uses the [data contract](http://msdn.microsoft.com/en-us/library/ms733127.aspx) based
serialisers in ``Slumber.IO.DataContract``.

```fsharp
|> forwarding "text/html" "text/xml"      
```

Finally, this line tells Slumber that when it receives a request using the "text/html" media type it should treat it as "text/xml". This can be useful for exposing services as XML
to browsers.

#### Function signatures

You can access URL segments and query string parameters by adding an argument to your function with the same name, e.g.

```fsharp
/// GET /widgets?search=term
let get (search : String option) =  ...
```

Slumber will get value type and string arugments from URL segments or the query string. If the argument type is not ``Option<T>`` and a corresponding value
cannot be found then Slumber will return an ``HTTP 400`` response.

Functions may also accept a single argument representing the request body. The argument type must be a reference type. Again, if the argument is not ``Option<T>`` then if the request has no body
Slumber will return an ``HTTP 400`` response.

Below are some examples.

```fsharp

/// GET /widgets
let list () = ...

/// POST /widgets
let add (widget : WidgetMessage) = ...

/// PUT /widgets/{id}
let update (id : Int32) (widget : WidgetMessage) = ...

/// DELETE /widgets/{id}
let delete (id : Int32) = ...

/// GET /widget-search?search=term[&orderBy=something]
let search (search : String) (orderBy : String option) = ...
```

You can also get the ``OperationMetadata`` for the current request by adding argument of that type. This contains information such as the endpoint name (if set) and details of the current
user (if authenticated).

Slumber will generate a response based on the return value of your function. If you want fine grained control over the response body, HTTP status code and headers you can return an ``OperationResult``. 
Otherwise the response will be a simple ``HTTP 200`` with the serialised form of your function's return value making up the body.

#### HTTP status codes for errors

Slumber uses various HTTP status codes to describe error conditions:

* ``404 Not Found`` - when no endpoint matching the request URL can be found.
* ``405 Method Not Allowed`` - when the endpoint does not support the HTTP verb used.
* ``415 Unsupported Media Type`` - when the Content-Type header contains an unsupported media type.
* ``406 Not Acceptable`` - when the Accept header contains an unsupported media type **OR** an error occurs during response serialisation.
* ``400 Bad Request`` - when an error occurs during request deserialisation **OR** the operation expects a request body or parameter but none was supplied.
* ``500 Internal Server Error`` - when an unhandled exception occurs during execution of an operation.

#### Future plans

* Better content negotiation. Currently the Content-Type or Accept header has to exactly match the registered media type.
* Ability to fine tune supported media types by endpoint / binding.
* Ability to fine tune which URLs are handled by Slumber and which are not - currently all or nothing below a base URL.
* Hooks to allow customisation / extension of existing functionality.
