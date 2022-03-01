namespace Дрема

open System
open Дрема.Common.Attempt
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
        Binding : Binding;
        EndpointName : String;
        Parameters : (String * String) list;
    }
    with

        ///The empty matching result
        static member Empty = 
            {
                Binding = Binding.Empty;
                EndpointName = String.Empty;
                Parameters = [];
            }

    ///Contains functions for matching a request URI and verb to an operation
    module Matching = 
        
        open System.IO
        open Дрема.Common.AsyncAttempt
        open Дрема.Framework.Core.Endpoints

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
        let private getTemplateVariables args (endpoint : Endpoint) = 
        
            let url = normaliseUrl args.Запрос.Url.Raw
            let template = UriTemplate (endpoint.Шаблон, true)            
            let results = template.Match (args.Контейнер.BaseUrl, url)

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

                    logInfo "[%A] Resolving endpoint for %A" args.Запрос.Id args.Запрос.Url.Path

                    return
                        args.Контейнер.Endpoints
                        |> List.tryPick (fun endpoint ->
                                match (getTemplateVariables args endpoint) with
                                | Some parameters -> Some (endpoint, parameters)
                                | _ -> None
                            )    
                        |> successOr StatusCodes.NotFound  
                }

        ///Find the binding for the request verb
        let asyncMatchBinding (endpoint, parameters) = 
            fun args -> 
                async {

                    logInfo "[%A] Resolving binding for %A" args.Запрос.Id args.Запрос.Verb

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
                        |> successOr StatusCodes.MethodNotAllowed
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
                
                logInfo "[%A] Authenticating request" args.Запрос.Id

                let isPublic = 
                    match (result.Binding.SecurityMode, args.Контейнер.Security.DefaultMode) with
                    | (Some Public, _) -> true
                    | (None, Public) -> true
                    | _ -> false                    

                return
                    if isPublic then

                        logInfo "[%A] Binding is public" args.Запрос.Id

                        Success (None)
                    else
                        match args.Контейнер.Security.Authenticate with
                        | None ->

                            logWarn "[%A] Binding is private but container defines no authentication function" args.Запрос.Id

                            Success (None)

                        | Some auth ->
                            match (auth args.Запрос) with
                            | Разрешено userData ->

                                logInfo "[%A] Request was successfully authenticated" args.Запрос.Id
                            
                                Success (userData)

                            | _ ->

                                logInfo "[%A] Authentication failed for request" args.Запрос.Id

                                Failure StatusCodes.Unauthorised
            }
            
    ///Contains functions for negotiating content types based on the Content-Type and Accept header
    module Negotiation = 

        open Дрема.Framework.Core.Containers

        ///The default content type to be used if the Content-Type of Accept headers are omitted
        let [<Literal>] DefaultMediaType = МедиаТипы.Text.Xml

        ///The 'any' content type
        let [<Literal>] AnyContentType = "*/*"

        ///Gets the reader to be used to deserialise any request body
        let asyncGetReader args = 
            async {

                logInfo "[%A] Negotiating request content type" args.Запрос.Id

                let requestedContentType = 
                    match (Заголовки.getContentType args.Запрос.Payload.Заголовки) with
                    | Some contentType -> contentType
                    | _ -> DefaultMediaType

                let targetContentType = 
                    args.Контейнер
                    |> applyForwarding requestedContentType

                if (targetContentType <> requestedContentType) then
                    logInfo "[%A] Request content type forwarding from %A to %A" args.Запрос.Id requestedContentType targetContentType

                let reader = 
                    args.Контейнер
                    |> getReader targetContentType

                if (Option.isSome reader) then
                    logInfo "[%A] Selected request content type of %A" args.Запрос.Id targetContentType

                return
                    match reader with
                    | Some reader' -> Some (targetContentType, reader')
                    | _ -> None
            }

        ///Gets the writer to be used when serialising any response body
        let asyncGetWriter args =
            async {

                logInfo "[%A] Negotiating response content type" args.Запрос.Id

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
                                logInfo "[%A] Response content type forwarding from %A to %A" args.Запрос.Id contentType targetContentType

                            match (getWriter targetContentType args.Контейнер) with
                            | Some writer -> Some (targetContentType, writer)
                            | _ -> None
                        )

                match writer with
                | Some (contentType, _) ->
                    logInfo "[%A] Selected response content type of %A" args.Запрос.Id contentType
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

                match (result.Binding.MessageType, reader) with
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
                                    Operation = result.Binding.Operation;
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
                | Failure statusCode -> return stopWithStatus statusCode
                | Success userData -> return! onAuthenticationSuccess result userData
            }

        async {

            logInfo "[%A] Beginning discovery phase for %A %A" args.Запрос.Id args.Запрос.Verb args.Запрос.Url.Path
            
            let! matchingResult = Matching.asyncGetMatchingResult args            

            match matchingResult with
            | Failure statusCode -> return stopWithStatus statusCode
            | Success info -> return! onMatchingSuccess info args
        }
        
    ///Runs the discovery phase synchronously
    let run = 
        asyncRun >> Async.RunSynchronously
        