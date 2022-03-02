namespace Дрема.Пример

open System
open Дрема
open Дрема.Framework
open Дрема.Setup
open Дрема.IO.DataContract

module Конфиг = 

    type Описание () = 

        let авторизовать (запрос : Запрос) = 

            let username = 
                запрос.Payload.Заголовки
                |> Заголовки.получитьЗначение "Authorization"

            match username with
            | Some username' -> Разрешено (Some { Id = username'; Свойства = []; })
            | _ -> Запрещено

        let создатьРепозиторий () =

            //NOTE You can uncomment the SQL CE line below to use a very simple, very
            //fragile SQL CE repository.

            //let repository = Repository.SqlCe.Repository.Create "Default"
            let репозиторий = Repository.InMemory.Repository.Create ()

            репозиторий.Setup ()

            репозиторий

        interface ИОписаниеКонтейнера with

            member this.Описать базовыйУрл =

                (**
                    NOTE The config below uses two approaches to dependencies. One is to partially apply functions, the other
                    is to use a resolver function.

                    When partially applying (e.g. supporting (public' get (someOperation dependency))) you may be tempted to create a
                    function which automatically does this for you, e.g. get' f = get (f dependency, however this creates a new function
                    which does not maintain the parameter names of your original so Slumber will not be able to populate values from 
                    URI segments or the query string. For example if you start with a function like f dependency x y and then you create
                    a new function f' = get' f then the parameters x and y will no longer be called x and y.
                **)

                let репозиторий = создатьРепозиторий ()

                let resolve type' =                     
                    if (type' = typeof<Repository.IRepository>) then
                        Some (box репозиторий)
                    else
                        None


                containerAt (relativeUri базовыйУрл "/")
                |> authenticatedBy авторизовать true
                |> resolveUsing resolve
                |> with' (
                        endpointAt "/"
                        |> named "service-catalog"
                        |> supporting (public' get Startup.получитьКаталог)
                    )            
                |> with' (
                        endpointAt "/people"
                        |> named "people"
                        |> supporting (public' get People.getPeople) //NOTE This uses the resolver function to get the IRepository parameter
                        |> supporting (post (People.addPerson репозиторий))
                    )
                |> with' (
                        endpointAt "/people/{id}"
                        |> named "person"
                        |> supporting (public' get (People.getPerson репозиторий))
                        |> supporting (delete (People.deletePerson репозиторий))
                        |> supporting (put (People.updatePerson репозиторий))
                    )
                |> all (public' options Общее.options)
                |> чтение МедиаТипы.Application.Json Json.read
                |> запись МедиаТипы.Application.Json Json.write
                |> чтение МедиаТипы.Text.Xml Xml.read
                |> запись МедиаТипы.Text.Xml Xml.write
                |> перенаправление МедиаТипы.Text.Html МедиаТипы.Text.Xml