﻿namespace Дрема

open System

///Contains common modules, types and functions used by Slumber
[<AutoOpen>]
module Common =

    ///Gets the ID of the executing thread
    let getThreadId () = 
        System.Threading.Thread.CurrentThread.ManagedThreadId

    ///Describes possible outcomes of an operation
    type Outcome<'TSuccess, 'TFailure> = 
        | Success of 'TSuccess
        | Failure of 'TFailure

    ///Converts an optional value to a Success or Failure value with the appropriate error
    let successOr error result = 
        match result with 
        | Some data -> Success data
        | _ -> Failure error   

    ///Record describing basic user properties
    type ДанныеПользователя = {
        Id : String;
        Properties : (String * String) list;
    }

    ///Contains modules for working with HTTP
    [<AutoOpen>]
    module Http = 

        open System.IO
        open System.Web
        open System.Collections.Specialized

        ///Contains constants for HTTP status codes
        [<RequireQualifiedAccess>]
        module StatusCodes = 

            ///http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html
        
            let [<Literal>] Continue = 100
            let [<Literal>] SwitchingProtocols = 101
            let [<Literal>] Ok = 200
            let [<Literal>] Created = 201
            let [<Literal>] Accepted = 202
            let [<Literal>] NonAuthoritativeInformation = 203
            let [<Literal>] NoContent = 204
            let [<Literal>] ResetContent = 205
            let [<Literal>] PartialContent = 206
            let [<Literal>] MultipleChoices = 300
            let [<Literal>] MovedPermanently = 301
            let [<Literal>] Found = 302
            let [<Literal>] SeeOther = 303
            let [<Literal>] NotModified = 304
            let [<Literal>] UseProxy = 305
            let [<Literal>] TemporaryRedirect = 307
            let [<Literal>] BadRequest = 400
            let [<Literal>] Unauthorised = 401
            let [<Literal>] PaymentRequired = 402
            let [<Literal>] Forbidden = 403
            let [<Literal>] NotFound = 404
            let [<Literal>] MethodNotAllowed = 405
            let [<Literal>] NotAcceptable = 406
            let [<Literal>] ProxyAuthenticationRequired = 407
            let [<Literal>] RequestTimeout = 408
            let [<Literal>] Conflict = 409
            let [<Literal>] Gone = 410
            let [<Literal>] LengthRequired = 411
            let [<Literal>] PreconditionFailed = 412
            let [<Literal>] RequestEntityTooLarge = 413
            let [<Literal>] RequestUriTooLong = 414
            let [<Literal>] ContentTypeNotSupported = 415
            let [<Literal>] RequestedRangeNotSatisfiable = 416
            let [<Literal>] ExpectationFailed = 417
            let [<Literal>] InternalServerError = 500            
            let [<Literal>] NotImplemented = 501            
            let [<Literal>] BadGateway = 502
            let [<Literal>] ServiceUnavailable = 503
            let [<Literal>] GatewayTimeout = 504
            let [<Literal>] HttpVersionNotSupported = 505

        ///Contains constans for HTTP verbs
        [<RequireQualifiedAccess>]
        module Verbs = 

            let [<Literal>] Get = "GET"
            let [<Literal>] Post = "POST"
            let [<Literal>] Put = "PUT"
            let [<Literal>] Delete = "DELETE"
            let [<Literal>] Options = "OPTIONS"
            let [<Literal>] Head = "HEAD"
            let [<Literal>] Patch = "PATCH"

        ///Contains constants for common media types
        module МедиаТипы = 

            [<RequireQualifiedAccess>]
            module Text = 
                let [<Literal>] Xml = "text/xml"
                let [<Literal>] Plain = "text/plain"
                let [<Literal>] Html = "text/html"

            [<RequireQualifiedAccess>]
            module Application = 
                let [<Literal>] Json = "application/json"
            
        ///Contains functions for working with HTTP headers                
        [<RequireQualifiedAccess>]
        module Заголовки = 

            let [<Literal>] ContentType = "Content-Type"
            let [<Literal>] Accept = "Accept"
            let [<Literal>] ContentLength = "Content-Length"

            ///Picks the value of a header with a given key from a key/value list
            let getValue key = 
                List.tryPick (fun (key', value) ->
                    if (String.same key key') then
                        Some value
                    else
                        None
                )

            ///Picks a non-empty header value from a key/value list
            let получитьНепустоеЗначение key headers = 
                match (getValue key headers) with
                | Some value when not (String.IsNullOrWhiteSpace value) -> Some value
                | _ -> None

            ///Gets the value of the Content-Type header from a key/value list
            let getContentType =
                получитьНепустоеЗначение ContentType

            ///Gets the value of the Accept header from a key/value list
            let getAccept = 
                получитьНепустоеЗначение Accept    

        ///Represents various forms of a request URL
        type Urls = {
            Raw : Uri;
            Path : String;
            Query : (String * String) list;
            BaseUrl : Uri;
        } 
        with

            ///The empty URL collection
            static member Empty = 

                let uri = Uri ("http://localhost/", UriKind.Absolute)

                {
                    Raw = uri;
                    Path = "/";
                    Query = [];
                    BaseUrl = uri;
                }

        ///Represents an HTTP request or response payload
        type Payload = {
            Заголовки : (String * String) list;
            Тело : Stream option;
        }
        with

            ///The empty HTTP payload
            static member Empty = 
                {
                    Заголовки = [];
                    Тело = None;
                }


        ///Represents a basic HTTP request
        type Запрос = {
            Id : Guid;
            Url : Urls;
            Verb : String;
            Payload : Payload;
        }
        with

            ///The empty HTTP request
            static member Empty = 
                {
                    Id = Guid.Empty;
                    Url = Urls.Empty;
                    Verb = String.Empty;
                    Payload = Payload.Empty;
                }

        ///Describes possible response types
        type ResponseType =
            | StatusCode of Int32
            | Resource of (Int32 * Byte list)

        ///Describes an HTTP response
        type Ответ = {
            ResponseType : ResponseType;
            ContentType : String option;
            CustomHeaders : (String * String) list;
        }
        with

            ///The empty response
            static member Empty =
                {
                    ResponseType = (StatusCode StatusCodes.NotImplemented);
                    ContentType = None;
                    CustomHeaders = [];
                }

        ///Describes a raw HTTP response
        type IOutput = 
            abstract member ЗаписатьТело : Byte list -> Async<unit>
            abstract member ЗаписатьЗаголовок : String -> String -> Async<unit>
            abstract member SetStatusCode : Int32 -> Async<unit>
            
        ///Wraps a raw HTTP response in the IOutput interface
        type HttpResponseOutput (raw : HttpResponseBase) = 

            static member Create raw = 
                HttpResponseOutput (raw) :> IOutput

            interface IOutput with

                member this.ЗаписатьТело bytes = 
                    async {
                        do! raw.OutputStream.AsyncWrite (List.toArray bytes)
                    }

                member this.ЗаписатьЗаголовок key value = 
                    async {
                        if (String.same Заголовки.ContentType key) then
                            raw.ContentType <- value
                        else
                            raw.Headers.[key] <- value
                    }

                member this.SetStatusCode statusCode = 
                    async {
                        raw.StatusCode <- statusCode
                    }

        ///Parses URL information from an HTTP request
        let parseUrls (raw : HttpRequestBase) = 

            let root = 
                Uri (raw.Url.GetLeftPart (UriPartial.Authority), UriKind.Absolute)

            let path = 
                match (raw.Url.AbsolutePath.Substring (raw.ApplicationPath.Length)) with
                | "" -> "/"
                | p -> p

            {
                Raw = raw.Url;                
                Path = path;
                Query = raw.QueryString |> NameValueCollection.toList;
                BaseUrl = Uri (root, raw.ApplicationPath);
            }

        ///Parses the payload of an HTTP request
        let parsePayload (raw : HttpRequestBase) = 

            let body = 
                if (raw.InputStream.Length > 0L) then
                    Some raw.InputStream
                else    
                    None

            {
                Заголовки = raw.Headers |> NameValueCollection.toList;
                Тело = body;
            }

        ///Parses an HTTP request
        let разобратьЗапрос (raw : HttpRequestBase) requestId = 
            {
                Id = requestId;
                Url = (parseUrls raw);
                Verb = raw.HttpMethod;
                Payload = (parsePayload raw);
            }

        ///Extension methods for HTTP objects
        [<AutoOpen>] 
        module ХттпРасширения = 

            ///HTTP request extension methods
            type HttpRequestBase with
                member this.Parse requestId = 
                    разобратьЗапрос this requestId

            ///HTTP response extension methods
            type HttpResponseBase with
                member this.AsOutput () = 
                    HttpResponseOutput.Create this

            ///HTTP context extension methods
            type HttpContextBase with
                member this.GetOutput () = this.Response.AsOutput ()
                member this.GetInput requestId = this.Request.Parse requestId

        ///Creates an absolute URL from a base URL and a relative URL
        let создатьАбсолютныйУри (базовыйУрл : Uri) (относительныйУрл : String) = 

            //NOTE That this function adds/removes slashes in the base and relative URLs. This is to 
            //maintain expected behaviour when working with extensionless URLs. By default, for example
            //http://localhost/app + api will result in http://localhost/api whereas the expected result is
            //likely http://localhost/app/api.

            let относительныйУрл' = 
                if (относительныйУрл.StartsWith "/") then
                    относительныйУрл.Substring 1
                else
                    относительныйУрл

            let базовыйУрл' = 
                if not (базовыйУрл.AbsoluteUri.EndsWith "/") then
                    Uri (базовыйУрл.AbsoluteUri + "/", UriKind.Absolute)
                else
                    базовыйУрл

            Uri (базовыйУрл', относительныйУрл')        

    ///Contains functions for working with operations
    [<AutoOpen>]
    module Операции = 

        let [<Literal>] DefaultUrl = "http://localhost"

        ///A function used to resolve dependencies
        type Resolver = Type -> obj option

        ///Represents metadata about an operation request
        type МетаданныеОперации = {
            ContainerUrl : Uri;
            EndpointName : String;
            Запрос : Запрос;
            Параметры : (String * String) list;
            Пользователь : ДанныеПользователя option;
            Resolver : Resolver option;
        }
        with

            ///Resolves a dependency
            member this.Resolve type' = 
                match this.Resolver with
                | Some resolver -> resolver type'
                | _ -> None

            ///Empty operation metadata
            static member Пустые = 
                {
                    ContainerUrl = Uri (DefaultUrl, UriKind.Absolute);
                    EndpointName = String.Empty;
                    Запрос = Запрос.Empty;
                    Параметры = [];
                    Пользователь = None;
                    Resolver = None;
                }

        ///Useful functions for working with metadata
        [<AutoOpen>]
        module Метаданные = 

            ///Union describing the result of trying to get some metadata
            type TryGetResult<'TFound> = 
                | Found of 'TFound
                | Missing
                | Malformed

            let private pickValue key =
                fun (key', value) ->
                    if (String.same key key') then
                        Some value
                    else
                        None

            ///Gets the parameters from metadata
            let getParameters meta = 
                meta.Параметры

            ///Gets the value of a parameter
            let getParameter key = 
                getParameters
                >> List.pick (pickValue key)

            ///Tries to get the value of a parameter
            let tryGetParameter key = 
                getParameters
                >> List.tryPick (pickValue key)

            ///Gets a parameter as the given type
            let getParameterAs<'TResult> key meta = 
                
                let value = 
                    getParameter key meta

                Convert.ChangeType (value, typeof<'TResult>) :?> 'TResult

            ///Tries to get a parameter as a given type
            let tryGetParameterAs<'TResult> key meta = 
                match (tryGetParameter key meta) with
                | None -> Missing
                | Some value ->
                    try
                        Found (Convert.ChangeType (value, typeof<'TResult>) :?> 'TResult)
                    with
                    | _ -> Malformed

            ///Gets a parameter using the given conversion
            let getParameterUsing key (conversion : String -> 'TResult) =                 
                getParameter key
                >> conversion

            ///Tries to gets a parameter using a given conversion
            let tryGetParameterUsing key (conversion : String -> 'TResult) meta = 
                match (tryGetParameter key meta) with
                | None -> Missing
                | Some value ->
                    try
                        value
                        |> conversion
                        |> Found
                    with
                    | _ -> Malformed

        ///Represents the context in which an operation is executed
        type OperationContext = {
            Metadata : МетаданныеОперации;
            Message : obj option;
        }      
        with

            ///The empty context
            static member Empty = 
                {
                    Metadata = МетаданныеОперации.Пустые;
                    Message = None;
                }  

        ///Record describing the result of an operation
        type OperationResult = {
            StatusCode : Int32 option;
            Resource : obj option;
            Headers : (String * String) list;
        }
        with

            ///The empty result
            static member Empty = 
                {
                    StatusCode = None;
                    Resource = None;
                    Headers = [];
                }

            ///Creates a result with only a status code
            static member StatusOnly (statusCode, headers) = 
                {
                    StatusCode = (Some statusCode);
                    Resource = None;
                    Headers = headers;
                }

            ///Creates a result with only a status code
            static member StatusOnly statusCode = 
                OperationResult.StatusOnly (statusCode, [])

            ///Creates a result with only a resource
            static member ResourceOnly (resource, headers) = 
                {
                    StatusCode = None;
                    Resource = (Some resource);
                    Headers = headers;
                }

            ///Creates a result with only a resource
            static member ResourceOnly resource = 
                OperationResult.ResourceOnly (resource, [])

            ///Creates a result with body a status code and a resource
            static member Both (statusCode, resource, headers) = 
                {
                    StatusCode = (Some statusCode);
                    Resource = (Some resource);
                    Headers = headers;
                }

            ///Creates a result with body a status code and a resource
            static member Both (statusCode, resource) = 
                OperationResult.Both (statusCode, resource, [])

        ///Type alias describing the signature for operations
        type Operation = OperationContext -> OperationResult

    ///Contains a monad similar to maybe which can be used to escape function chains on exceptions
    module Attempt = 

        ///Contains the main monad functions for attempt
        module Monad = 

            ///Binds two synchronous functions together, calling g if f succeeds. 
            let bind f g = 
                fun state ->
                    match (f state) with
                    | Success value -> g value state
                    | Failure data -> Failure data

            ///Lifts a value to the success state
            let return' value =             
                fun _ ->
                    Success value

        ///Workflow for the attempt monad
        type AttemptBuilder () = 
            
            member this.Bind (expr, rest) = 
                Monad.bind expr rest

            member this.Return expr = 
                Monad.return' expr

        //Syntactic sugar for the attempt workflow
        let attempt = 
            AttemptBuilder ()       

    ///Asynchronous version of the Attempt monad and workflow
    module AsyncAttempt = 

        ///Contains the monad functions for the aysnc attempt workflow
        module Monad = 

            open Attempt

            ///Binds two asynchrounous functions together, calling g if f succeeds
            let bind f g = 
                fun state ->
                    async {

                        let! result = f state

                        match result with
                        | Success value -> 
                            return! g value state
                        | Failure data -> 
                            return (Failure data)
                    }

            ///Lifts a value to an asynchronous success state
            let return' value = 
                fun _ -> 
                    async {
                        return (Success value)
                    }

        ///Workflow for the async attempt monad
        type AsyncAttemptBuilder () = 

            member this.Bind (expr, rest) = 
                Monad.bind expr rest

            member this.Return expr = 
                Monad.return' expr

        ///Gets the state of the workflow
        let getState () =
            fun state ->
                async {
                    return (Success state)
                }

        //Syntactic sugar for the asycn attempt workflow
        let asyncAttempt = 
            AsyncAttemptBuilder ()
    