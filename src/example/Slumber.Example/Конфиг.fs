namespace Дрема.Пример

open System
open Дрема
open Дрема.Framework
open Дрема.Setup
open Дрема.IO.DataContract

module Config = 

    type Description () = 

        let authenticate (request : Запрос) = 

            let username = 
                request.Payload.Заголовки
                |> Заголовки.получитьЗначение "Authorization"

            match username with
            | Some username' -> Разрешено (Some { Id = username'; Свойства = []; })
            | _ -> Запрещено

        let createRepository () =

            //NOTE You can uncomment the SQL CE line below to use a very simple, very
            //fragile SQL CE repository.

            //let repository = Repository.SqlCe.Repository.Create "Default"
            let repository = Repository.InMemory.Repository.Create ()

            repository.Setup ()

            repository

        interface IContainerDescription with

            member this.Describe baseUrl =

                (**
                    NOTE The config below uses two approaches to dependencies. One is to partially apply functions, the other
                    is to use a resolver function.

                    When partially applying (e.g. supporting (public' get (someOperation dependency))) you may be tempted to create a
                    function which automatically does this for you, e.g. get' f = get (f dependency, however this creates a new function
                    which does not maintain the parameter names of your original so Slumber will not be able to populate values from 
                    URI segments or the query string. For example if you start with a function like f dependency x y and then you create
                    a new function f' = get' f then the parameters x and y will no longer be called x and y.
                **)

                let repository = createRepository ()

                let resolve type' =                     
                    if (type' = typeof<Repository.IRepository>) then
                        Some (box repository)
                    else
                        None


                containerAt (relativeUri baseUrl "/")
                |> authenticatedBy authenticate true
                |> resolveUsing resolve
                |> with' (
                        endpointAt "/"
                        |> named "service-catalog"
                        |> supporting (public' get Startup.getCatalog)
                    )            
                |> with' (
                        endpointAt "/people"
                        |> named "people"
                        |> supporting (public' get People.getPeople) //NOTE This uses the resolver function to get the IRepository parameter
                        |> supporting (post (People.addPerson repository))
                    )
                |> with' (
                        endpointAt "/people/{id}"
                        |> named "person"
                        |> supporting (public' get (People.getPerson repository))
                        |> supporting (delete (People.deletePerson repository))
                        |> supporting (put (People.updatePerson repository))
                    )
                |> all (public' options Общее.options)
                |> reading МедиаТипы.Application.Json Json.read
                |> writing МедиаТипы.Application.Json Json.write
                |> reading МедиаТипы.Text.Xml Xml.read
                |> writing МедиаТипы.Text.Xml Xml.write
                |> forwarding МедиаТипы.Text.Html МедиаТипы.Text.Xml