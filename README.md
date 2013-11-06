### Slumber - An F# REST framework

Slumber is a small framework for creating REST applications in a functional style using F#, inspired heavily by the excellent [OpenRasta](http://openrasta.org/).

#### The short, short version...

The [wiki](https://github.com/wattsm/slumber/wiki) contains a more detailed overview of Slumber, but a short overview is given below.

Some business logic:


```fsharp
module People = 
  let getPeople () = ...
  let getPerson (id : Int32) = ...
  let createPerson (model : Person) = ...
  let updatePerson (id : Int32) (model : Person) = ...
  let search (term : String option) = ...
  
module Security = 
  let authenticate (req : Request) = ...
```

Exposed as REST services:

```fsharp
type Config () = 
  interface IContainerDescription with
    member this.Describe baseUrl = 
      containerAt (relativeUrl baseUrl "/api")
      |> authenticatedBy Security.authenticate true
      |> with' (
        endpointAt "/people"
        |> named "people"
        |> supporting (public' get People.getPeople)
        |> supporting (post People.createPerson)
      )
      |> with' (
        endpointAt "/people/search"
        |> named "people-search"
        |> supporting (public' get People.search)
      )
      |> with' (
        endpointAt "/people/{id}"
        |> named "person"
        |> supporting (public' get People.getPerson)
        |> supporting (put People.updatePerson)
      )
      |> reading MediaTypes.Text.Xml Xml.read
      |> writing MediaTypes.Text.Xml Xml.write
      |> reading MediaTypes.Application.Json Json.read
      |> writing MediaTypes.Application.Json Json.write
      |> forwarding MediaTypes.Text.Html MediaTypes.Text.Xml
```

### Working example

The Slumber solution contains a working example - [details can be found here](https://github.com/wattsm/slumber/wiki/running-the-example).

#### Quick links

Links to the main [wiki](https://github.com/wattsm/slumber/wiki) pages:

* [Install](https://github.com/wattsm/slumber/wiki/install)
* [Defining services](https://github.com/wattsm/slumber/wiki/defining-services)
* [Functions and arguments](https://github.com/wattsm/slumber/wiki/functions-and-arguments)
* [Running the example](https://github.com/wattsm/slumber/wiki/running-the-example)
