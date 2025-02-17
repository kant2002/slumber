﻿namespace Дрема

open System
open Дрема.Общее.Попытка
open Дрема.Execution
open Дрема.Framework
open Дрема.Framework.Helpers

///Contains functions and types for identifying the binding, content types and user of a request
module Discovery = 

    ///Represents the arguments used to execute the discovery phase
    type DiscoveryArgs = {
        Запрос : Запрос;
        Контейнер : Контейнер;
    }

    ///Represents the result of matching an HTTP request against a binding
    type MatchingResult = {
        Binding : Привязка;
        EndpointName : String;
        Parameters : (String * String) list;
    }
    with

        ///The empty matching result
        static member Empty = 
            {
                Binding = Привязка.Пустая;
                EndpointName = String.Empty;
                Parameters = [];
            }

    ///Contains functions for matching a request URI and verb to an operation
    module Matching = 
        
        open System.IO
        open Дрема.Общее.AsyncAttempt
        open Дрема.Framework.Ядро.Endpoints

        ///Normalises a URL for template matching, ensuring folder-level URLs end with a trailing slash.
        ///This is important for cases when the base URL and candidate URL are essentially equal - e.g.
        //http://localhost/api/ and http://localhost/api.
        let normaliseUrl (url : Uri) = 
            if (url.AbsolutePath.EndsWith "/") then
                url
            else
                Uri (
                    String.Format (
                        "{0}{1}/{2}",
                        url.GetLeftPart (UriPartial.Authority),
                        url.AbsolutePath,
                        url.Query
                    ),
                    UriKind.Absolute
                )

        ///Attempts to match a request URI to the given endpoint and returns the matching URI variables
        let private getTemplateVariables args (endpoint : ОконечнаяТочка) = 
        
            let url = normaliseUrl args.Запрос.Url.Raw
            let template = UriTemplate (endpoint.Шаблон, true)            
            let results = template.Match (args.Контейнер.БазовыйУрл, url)

            if results = null then
                None
            else
                results.BoundVariables
                |> NameValueCollection.toList
                |> Some

        ///Finds the endpoint bound to the request URI
        let asyncMatchEndpoint () = 
            fun args ->
                async { 

                    журналИнфо "[%A] Resolving endpoint for %A" args.Запрос.Id args.Запрос.Url.Путь

                    return
                        args.Контейнер.ОконечныеТочки
                        |> List.tryPick (fun endpoint ->
                                match (getTemplateVariables args endpoint) with
                                | Some parameters -> Some (endpoint, parameters)
                                | _ -> None
                            )    
                        |> успехИли StatusCodes.NotFound  
                }

        ///Find the binding for the request verb
        let asyncMatchBinding (endpoint, parameters) = 
            fun args -> 
                async {

                    журналИнфо "[%A] Resolving binding for %A" args.Запрос.Id args.Запрос.Verb

                    let createBindingInfo binding = 
                        match binding with
                        | Some binding' ->                             
                            {
                                MatchingResult.Empty 
                                with
                                    Binding = binding';
                                    EndpointName = endpoint.Название;
                                    Parameters = parameters;
                            }
                            |> Some
                        | _ -> None

                    return
                        endpoint
                        |> tryGetBinding args.Запрос.Verb
                        |> createBindingInfo
                        |> успехИли StatusCodes.MethodNotAllowed
                }

        ///Asynchronously gets the result of matching the current request to an endpoint
        let asyncGetMatchingResult =             
            asyncAttempt {

                let! endpoint, parameters = asyncMatchEndpoint ()
                let! result = asyncMatchBinding (endpoint, parameters)

                return result
            }

    ///Contains functions for applying securtiy to requests
    module Security = 

        ///Attempts to authenticate the current user, if applicable for the selected binding
        let asyncAuthenticateRequest (result : MatchingResult) (args : DiscoveryArgs) =
            async {
                
                журналИнфо "[%A] Authenticating request" args.Запрос.Id

                let isPublic = 
                    match (result.Binding.РежимБезопасности, args.Контейнер.Security.DefaultMode) with
                    | (Some Public, _) -> true
                    | (None, Public) -> true
                    | _ -> false                    

                return
                    if isPublic then

                        журналИнфо "[%A] Binding is public" args.Запрос.Id

                        Успех (None)
                    else
                        match args.Контейнер.Security.Авторизовать with
                        | None ->

                            журналПредупр "[%A] Binding is private but container defines no authentication function" args.Запрос.Id

                            Успех (None)

                        | Some auth ->
                            match (auth args.Запрос) with
                            | Разрешено userData ->

                                журналИнфо "[%A] Request was successfully authenticated" args.Запрос.Id
                            
                                Успех (userData)

                            | _ ->

                                журналИнфо "[%A] Authentication failed for request" args.Запрос.Id

                                Провал StatusCodes.Unauthorised
            }
            
    ///Contains functions for negotiating content types based on the Content-Type and Accept header
    module Negotiation = 

        open Дрема.Framework.Ядро.Containers

        ///The default content type to be used if the Content-Type of Accept headers are omitted
        let [<Literal>] DefaultMediaType = МедиаТипы.Text.Xml

        ///The 'any' content type
        let [<Literal>] AnyContentType = "*/*"

        ///Gets the reader to be used to deserialise any request body
        let asyncGetReader args = 
            async {

                журналИнфо "[%A] Negotiating request content type" args.Запрос.Id

                let requestedContentType = 
                    match (Заголовки.getContentType args.Запрос.Payload.Заголовки) with
                    | Some contentType -> contentType
                    | _ -> DefaultMediaType

                let targetContentType = 
                    args.Контейнер
                    |> applyForwarding requestedContentType

                if (targetContentType <> requestedContentType) then
                    журналИнфо "[%A] Request content type forwarding from %A to %A" args.Запрос.Id requestedContentType targetContentType

                let reader = 
                    args.Контейнер
                    |> получитьЧитателя targetContentType

                if (Option.isSome reader) then
                    журналИнфо "[%A] Selected request content type of %A" args.Запрос.Id targetContentType

                return
                    match reader with
                    | Some reader' -> Some (targetContentType, reader')
                    | _ -> None
            }

        ///Gets the writer to be used when serialising any response body
        let asyncGetWriter args =
            async {

                журналИнфо "[%A] Negotiating response content type" args.Запрос.Id

                let requestedContentType = 
                    match (Заголовки.getAccept args.Запрос.Payload.Заголовки) with
                    | Some contentType when (contentType <> AnyContentType) -> contentType
                    | _ -> DefaultMediaType

                let writer = 
                    requestedContentType
                    |> String.split ","
                    |> List.map String.trim
                    |> List.tryPick (fun contentType ->

                            let targetContentType = 
                                args.Контейнер
                                |> applyForwarding contentType

                            if (targetContentType <> contentType) then
                                журналИнфо "[%A] Response content type forwarding from %A to %A" args.Запрос.Id contentType targetContentType

                            match (получитьПисателя targetContentType args.Контейнер) with
                            | Some writer -> Some (targetContentType, writer)
                            | _ -> None
                        )

                match writer with
                | Some (contentType, _) ->
                    журналИнфо "[%A] Selected response content type of %A" args.Запрос.Id contentType
                | _ -> ()

                return  writer
            }

    ///Runs the discovery phase asynchronously
    let asyncRun args = 

        let getWriterInfo result =
            match result with
            | None -> None
            | Some (contentType, writer) -> 
                {
                    Writer = writer;
                    ContentType = contentType;
                }
                |> Some

        let getReaderInfo (contentType, reader) messageType = 
            match messageType with
            | None -> None
            | Some type' ->
                {   
                    Reader = reader;
                    ContentType = contentType;
                    MessageType = type';
                }
                |> Some

        let onAuthenticationSuccess (result : MatchingResult) userData = 
            async {

                let! reader = Negotiation.asyncGetReader args
                let! writer = Negotiation.asyncGetWriter args

                match (result.Binding.ТипСообщения, reader) with
                | (_, None) -> 
                    return stopWithStatus StatusCodes.ContentTypeNotSupported
                | (messageType, Some reader') ->                   
                    return
                        {
                            Request = args.Запрос;
                            Container = args.Контейнер;
                            Reader = (getReaderInfo reader' messageType);
                            Writer = (getWriterInfo writer);
                            Target = 
                                {
                                    EndpointName = result.EndpointName;
                                    Operation = result.Binding.Операция;
                                    Parameters = result.Parameters;
                                }
                            User = userData;
                        }
                        |> continue'
            }
            
        let onMatchingSuccess (result : MatchingResult) args =
            async {

                let! authResult = Security.asyncAuthenticateRequest result args

                match authResult with
                | Провал statusCode -> return stopWithStatus statusCode
                | Успех userData -> return! onAuthenticationSuccess result userData
            }

        async {

            журналИнфо "[%A] Beginning discovery phase for %A %A" args.Запрос.Id args.Запрос.Verb args.Запрос.Url.Путь
            
            let! matchingResult = Matching.asyncGetMatchingResult args            

            match matchingResult with
            | Провал statusCode -> return stopWithStatus statusCode
            | Успех info -> return! onMatchingSuccess info args
        }
        
    ///Runs the discovery phase synchronously
    let run = 
        asyncRun >> Async.RunSynchronously
        