namespace Дрема

open System
open System.Web
open Дрема.Framework
open Дрема.Discovery
open Дрема.Общее.Http

///Contains functions used to parse the raw HTTP request and initialise Slumber's configuration
module Bootstrap = 

    ///Runs the bootstrapping phase asynchronously
    let asyncRun mode (request : Запрос) =
        async {

            let mode' = 
                match mode with
                | Explicit _ -> "explicit"
                | Implicit -> "implicit"
                | Mixed _ -> "mixed"

            журналИнфо "[%A] Bootstrapping using %A" request.Id mode'

            let container = 
                match mode with
                | Explicit container' -> container'
                | Implicit -> ImplicitConfiguration.get request.Url.BaseUrl
                | Mixed modifier -> 
                    request.Url.BaseUrl
                    |> ImplicitConfiguration.get
                    |> modifier

            return 
                {
                    Запрос = request;
                    Контейнер = container;
                }
                |> Running
        }

    ///Runs the bootstrapping phase synchronously
    let run mode request = 
        asyncRun mode request
        |> Async.RunSynchronously
    
        
