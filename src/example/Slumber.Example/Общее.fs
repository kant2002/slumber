namespace Дрема.Пример

open System
open Дрема
open Дрема.Framework
open Дрема.Framework.Ядро.Containers

module Общее = 

    let options (meta : МетаданныеОперации) = 

        let endpoint = 
            meta.ContainerUrl
            |> НеявнаяКонфигурация.получить
            |> tryGetEndpointByName meta.EndpointName
            |> Option.get

        let verbs = 
            endpoint.Привязки
            |> List.map (fun binding -> binding.Verb)
            
        let accept = 
            String.Join (",", verbs)

        РезультатОперации.ТолькоСтатус (
            StatusCodes.Ok,
            [ (Заголовки.Accept, accept) ]
        )