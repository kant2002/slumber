namespace Дрема

open Дрема.Framework
open Дрема.Discovery
open Дрема.Общее.Http

///Contains functions used to parse the raw HTTP request and initialise Slumber's configuration
module Bootstrap = 

    ///Runs the bootstrapping phase asynchronously
    let asyncRun режим (запрос : Запрос) =
        async {

            let mode' = 
                match режим with
                | Explicit _ -> "explicit"
                | Implicit -> "implicit"
                | Mixed _ -> "mixed"

            журналИнфо "[%A] Bootstrapping using %A" запрос.Id mode'

            let контейнер = 
                match режим with
                | Explicit контейнер' -> контейнер'
                | Implicit -> НеявнаяКонфигурация.получить запрос.Url.BaseUrl
                | Mixed modifier -> 
                    запрос.Url.BaseUrl
                    |> НеявнаяКонфигурация.получить
                    |> modifier

            return 
                {
                    Запрос = запрос;
                    Контейнер = контейнер;
                }
                |> Запущен
        }

    ///Runs the bootstrapping phase synchronously
    let run режим запрос = 
        asyncRun режим запрос
        |> Async.RunSynchronously
    
        
