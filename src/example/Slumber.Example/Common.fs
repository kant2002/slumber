namespace Дрема.Пример

open System
open Дрема
open Дрема.Framework
open Дрема.Framework.Core.Containers

module Common = 

    let options (meta : МетаданныеОперации) = 

        let endpoint = 
            meta.ContainerUrl
            |> ImplicitConfiguration.get
            |> tryGetEndpointByName meta.EndpointName
            |> Option.get

        let verbs = 
            endpoint.Bindings
            |> List.map (fun binding -> binding.Verb)
            
        let accept = 
            String.Join (",", verbs)

        OperationResult.StatusOnly (
            StatusCodes.Ok,
            [ (Заголовки.Accept, accept) ]
        )