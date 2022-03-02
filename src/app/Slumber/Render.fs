namespace Дрема

open System
open System.Text
open System.Web
open System.IO
open System.Collections.Specialized
open Дрема.Framework

///Contains functions used to render the HTTP response 
module Render =

    ///Content type used for textual message
    let [<Literal>] MessageContentType = МедиаТипы.Text.Plain

    ///Functions for writing to the HTTP response object
    module Writing = 

        ///Writes an optional body to the given output
        let asyncWriteBody (output : IOutput) args = 
            async {
                match args.ТипОтвета with
                | StatusCode _ -> ()
                | Ресурс (_, []) -> ()
                | Ресурс (_, bytes) -> do! output.ЗаписатьТело (bytes)                    
            }     
        
        ///Writes the response headers to the given output
        let asyncWriteHeaders (output : IOutput) args = 

            let addContentLength headers = 

                let value = 
                    match args.ТипОтвета with
                    | StatusCode _ -> 0
                    | Ресурс (_, bytes) -> List.length bytes
                    
                (Заголовки.ContentLength, (string value)) :: headers

            let addContentType headers = 

                let currentValue = headers |> Заголовки.getContentType

                match (currentValue, args.ТипСодержимого) with
                | (None, Some value) -> (Заголовки.ContentType, value) :: headers
                | _ -> headers

            async {
                args.CustomHeaders
                |> addContentLength
                |> addContentType
                |> List.iter (fun (key, value) ->
                        output.ЗаписатьЗаголовок key value
                        |> Async.RunSynchronously
                    )
            }

    ///Writes the output asynchronously
    let asyncWrite (requestId : Guid) (output : IOutput) response = 
        async {

            let statusCode = 
                match response.ТипОтвета with
                | StatusCode code -> code
                | Ресурс (code, _) -> code

            do!
                Writing.asyncWriteBody 
                <| output
                <| response

            do!
                Writing.asyncWriteHeaders
                <| output
                <| response

            do! output.SetStatusCode statusCode

            журналИнфо "[%A] Response rendered, status code is %A" requestId statusCode
        }    

    ///Gets the response to be rendered from the given state
    let asyncGetResponse (requestId : Guid) state = 
    
        let getMessageResponse (message : String) = 

            let bytes = 
                Encoding.UTF8.GetBytes (message)
                |> Array.toList

            {
                Ответ.Пустой
                with
                    ТипОтвета = Ресурс (StatusCodes.InternalServerError, bytes);
                    ТипСодержимого = Some MessageContentType;
            }

        async {
            return 
                match state with 
                | Running _ -> 

                    журналИнфо "[%A] Pipeline has failed to stop," requestId
                
                    getMessageResponse "Pipeline failed to stop"

                | Stopped type' ->
                    match type' with
                    | Exception e -> 
                    
                        журналИнфо "[%A] Pipeline has encountered an exception" requestId
                        
                        getMessageResponse e.Message

                    | Completed resp -> 
                    
                        журналИнфо "[%A] Pipeline has completed successfully" requestId
                        
                        resp
        }

    ///Runs the render phase asynchronously
    let asyncRun requestId context = 
        fun state ->
            async {

                журналИнфо "[%A] Beginning render phase" requestId

                let! response = asyncGetResponse requestId state
                do! asyncWrite requestId context response

            }

    ///Runs the render phase synchronously
    let run requestId context = 
        asyncRun requestId context 
        >> Async.RunSynchronously

