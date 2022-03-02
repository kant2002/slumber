namespace Дрема.Tests

open FsUnit
open Xunit
open Xunit.Extensions
open System
open System.IO
open Дрема

module ``Discovery facts`` =

    open Discovery
    open Http
    open Framework
    open Setup.Bindings

    let должно = should
    let быть = be
    let равняться = equal

    let teapot (_ : obj option) = 
        РезультатОперации.ТолькоСтатус 418 //Teapot

    let endpoint = 
        {
            Название = "Dummy";
            Шаблон = "/people/{personCode}";
            Привязки = [ get teapot ]; 
        }

    let [<Literal>] ModuleName = "Discovery"

    [<AutoOpen>] 
    module Helpers = 
        
        let получитьАрг (relativeUrl : String) =     
        
            let читатель _ _ = 
                None

            let писатель _ = 
                []

            let relativeUrl' = 
                if (relativeUrl.StartsWith "/") then
                    relativeUrl.Substring (1)
                else
                    relativeUrl

            let baseUrl = 
                Uri ("http://localhost:8080/api/", UriKind.Absolute)

            {
                Запрос = 
                    {
                        Запрос.Пустой 
                        with
                            Url = 
                                {
                                    Urls.Пустые
                                    with
                                        Raw = Uri (baseUrl, relativeUrl');
                                        Путь = relativeUrl;
                                        BaseUrl = baseUrl;
                                };
                            Verb = "GET";
                            Payload = 
                                {
                                    Payload.Пустой
                                    with
                                        Тело = (Some (new MemoryStream () :> Stream));
                                }
                    } ;
                Контейнер =
                    {
                        Контейнер.Пустой
                        with
                            Endpoints = [ endpoint; ];
                            ВВ = 
                                {
                                    КонфигВВ.Пустой
                                    with
                                        Читатели = [ (МедиаТипы.Text.Xml, читатель); (МедиаТипы.Application.Json, читатель) ];
                                        Писатели = [ (МедиаТипы.Text.Xml, писатель); (МедиаТипы.Application.Json, писатель) ];
                                }
                            BaseUrl = baseUrl;
                    }
            }

        let добавитьЗаголовок name value (args : DiscoveryArgs) = 

            let payload'  = 
                { args.Запрос.Payload with Заголовки = (name, value) :: args.Запрос.Payload.Заголовки; }

            { args with Запрос = { args.Запрос with Payload = payload'; }; }

        let addForwardedType from to' args = 

            let container' =
                {
                    args.Контейнер
                    with
                        ВВ = 
                            {
                                args.Контейнер.ВВ
                                with
                                    ПеренаправляемыеТипы = [ (from, to'); ];
                            }
                }

            { args with Контейнер = container'; }
      
        let makePrivate auth args = 
            let security = { args.Контейнер.Security with DefaultMode = Private; Авторизовать = (Some auth); }
            in  { args with Контейнер = { args.Контейнер with Security = security; }; }

    module ``Matching facts`` = 

        open Discovery.Matching
        open Попытка

        let [<Literal>] ModuleName = "Discovery.Matching"

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``normaliseUrl function`` =

            let normaliseUrl' url = 
                Uri (url, UriKind.Absolute)
                |> normaliseUrl                

            [<Theory>]
            [<InlineData ("http://localhost/")>]
            [<InlineData ("http://localhost/api/")>]
            let ``URIs with a trailing slash are not changed`` url =
                normaliseUrl' url
                |> string
                |> должно равняться url

            [<Theory>]
            [<InlineData ("http://localhost", "http://localhost/")>]
            [<InlineData ("http://localhost?key=value", "http://localhost/?key=value")>]
            [<InlineData ("http://localhost/api", "http://localhost/api/")>]
            [<InlineData ("http://localhost/api?key=value", "http://localhost/api/?key=value")>]
            let ``A trailing slash is added after the path if not present`` url expected =
                normaliseUrl' url
                |> string
                |> должно равняться expected

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``asyncMatchEndpoint function`` = 

            let matchEndpoint = 
                asyncMatchEndpoint ()
                >> Async.RunSynchronously

            let [<Fact>] ``Unregistered URL returns HTTP 404`` () =

                let isNotFound outcome = 
                    match outcome with
                    | Провал statusCode -> statusCode = StatusCodes.NotFound
                    | _ -> false

                "/addresses"
                |> получитьАрг
                |> matchEndpoint
                |> isNotFound
                |> должно быть True

            let [<Fact>] ``Matching URL returns success`` () = 

                let isSuccess outcome = 
                    match outcome with
                    | Успех _ -> true
                    | _ -> false

                "/people/12345"
                |> получитьАрг
                |> matchEndpoint
                |> isSuccess
                |> должно быть True

            let [<Fact>] ``Trailing slashes are ignored`` () =

                let isSuccess outcome = 
                    match outcome with
                    | Успех _ -> true
                    | _ -> false

                "/people/12345/"
                |> получитьАрг
                |> matchEndpoint
                |> isSuccess
                |> должно быть True

            let [<Fact>] ``Successful match includes correct endpoint`` () =

                let getTemplate outcome = 
                    match outcome with
                    | Успех (endpoint, _) -> endpoint.Шаблон
                    | _ -> String.Empty

                "/people/12345"
                |> получитьАрг
                |> matchEndpoint
                |> getTemplate
                |> должно равняться "/people/{personCode}"

            let [<Fact>] ``Successful match includes matched URL parameters`` () =
                
                let getParameters outcome = 
                    match outcome with
                    | Успех (_, parameters) -> parameters
                    | _ -> []

                "/people/12345"
                |> получитьАрг
                |> matchEndpoint
                |> getParameters
                |> List.same [ ("PERSONCODE", "12345") ] //Note UriTemplate converts placeholders to upper case. Potentially annoying.
                |> должно быть True

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``asyncMatchBinding function`` =

            let matchBinding endpoint = 
                "/people/12345"
                |> получитьАрг
                |> asyncMatchBinding endpoint 
                |> Async.RunSynchronously

            let [<Fact>] ``No registered binding returns HTTP 405`` () = 

                let isNotSupported outcome = 
                    match outcome with
                    | Провал statusCode -> statusCode = StatusCodes.MethodNotAllowed
                    | _ -> false

                let endpoint' = 
                    { endpoint with Привязки = [ post teapot ] }

                (endpoint', [])
                |> matchBinding
                |> isNotSupported
                |> должно быть True
                
            let [<Fact>] ``Registered binding returns success`` () =

                let isSuccess outcome = 
                    match outcome with
                    | Успех _ -> true
                    | _ -> false
                
                (endpoint, [])
                |> matchBinding
                |> isSuccess
                |> должно быть True

            let [<Fact>] ``Success includes URL parameters`` () =
                
                let getParameters outcome = 
                    match outcome with
                    | Успех (result : MatchingResult) -> result.Parameters
                    | _ -> []

                let parameters = 
                    [ ("PERSONCODE", "12345"); ]

                (endpoint, parameters)
                |> matchBinding
                |> getParameters
                |> List.same parameters
                |> должно быть True

            let [<Fact>] ``Success includes operation`` () =
                
                let executeOp outcome = 
                    
                    let (context : КонтекстОперации) = 

                        let baseUrl = 
                            Uri ("http://localhost/api", UriKind.Absolute)

                        {
                            Метаданные = 
                                {
                                    МетаданныеОперации.Пустые
                                    with
                                        Запрос = 
                                            {
                                                Запрос.Пустой
                                                with
                                                    Url = 
                                                        {
                                                            Raw = Uri (baseUrl, "people/12345");
                                                            Путь = "/people/12345";
                                                            Запрос = [];
                                                            BaseUrl = baseUrl;
                                                        };                                                    
                                            };
                                };
                            Сообщение = None;
                        }

                    match outcome with
                    | Успех result -> Some (result.Binding.Операция context)
                    | _ -> None

                (endpoint, [])
                |> matchBinding
                |> executeOp
                |> should be (Some' ({ КодСтатуса = Some 418; Ресурс = None; Заголовки = []; }))

            let [<Fact>] ``Success includes message type`` () =

                let isStringMessage result = 
                    match result with
                    | Успех result -> result.Binding.ТипСообщения = (Some typedefof<obj>)
                    | _ -> false

                (endpoint, [])
                |> matchBinding
                |> isStringMessage
                |> должно быть True

            let [<Fact>] ``Success includes endpoint name`` () =

                let hasCorrectName result = 
                    match result with
                    | Успех (result : MatchingResult) -> result.EndpointName = endpoint.Название
                    | _ -> false

                (endpoint, [])
                |> matchBinding
                |> hasCorrectName
                |> должно быть True                

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``asyncGetMatchingResult function`` =

            let getMatchingResult = 
                asyncGetMatchingResult
                >> Async.RunSynchronously

            let [<Fact>] ``No registered URL returns HTTP 404`` () =
                
                let isNotFound outcome = 
                    match outcome with
                    | Провал statusCode -> statusCode = 404
                    | _ -> false

                "/addresses"
                |> получитьАрг
                |> getMatchingResult
                |> isNotFound
                |> должно быть True

            let [<Fact>] ``No registered operation returns HTTP 405`` () =
                
                let isNotSupported outcome = 
                    match outcome with
                    | Провал statusCode -> statusCode = 405
                    | _ -> false

                let changeVerb (args : DiscoveryArgs) = 
                    { args with Запрос = { args.Запрос with Verb = "POST"; }; }

                "/people/12345"
                |> получитьАрг
                |> changeVerb
                |> getMatchingResult
                |> isNotSupported
                |> должно быть True

            let [<Fact>] ``Registered URL and binding returns success`` () =

                let isSuccess outcome = 
                    match outcome with
                    | Успех _ -> true
                    | _ -> false

                "/people/12345"
                |> получитьАрг
                |> getMatchingResult
                |> isSuccess
                |> должно быть True

    module ``Security facts`` =

        open Security

        let [<Literal>] ModuleName = "Discovery.Security"
            
        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``asyncAuthenticateRequest function`` =
            
            open Matching

            [<AutoOpen>]
            module Helpers = 

                let authenticateRequest binding args = 
                    asyncAuthenticateRequest binding args
                    |> Async.RunSynchronously

                let setAuth f args = 
                    let security = { args.Контейнер.Security with Авторизовать = (Some f); }
                    in { args with Контейнер = { args.Контейнер with Security = security; }; }

                let setMode mode args = 
                    let security = { args.Контейнер.Security with DefaultMode = mode; } 
                    in { args with Контейнер = { args.Контейнер with Security = security; }; }

                let getResult mode = 
                    {
                        EndpointName = "";
                        Parameters = [];
                        Binding = 
                            {
                                Verb = "VERB";
                                РежимБезопасности = mode;
                                Операция = (fun _ -> РезультатОперации.Пустой);
                                ТипСообщения = None;
                            };
                    }

                let assertCalled isPublic defaultMode expected = 

                    let _called = ref false

                    let result = 
                        getResult isPublic

                    let auth _ = 
                        _called := true
                        Запрещено

                    получитьАрг "/12345"
                    |> setAuth auth
                    |> setMode defaultMode
                    |> authenticateRequest result
                    |> ignore

                    Assert.Equal (expected, _called.Value)

                let getUserData outcome = 
                    match outcome with
                    | Успех user -> user
                    | _ -> invalidOp "Unexpected outcome"

                let getStatusCode outcome = 
                    match outcome with
                    | Провал statusCode -> statusCode
                    | _ -> invalidOp "Unexpected outcome"

            let [<Fact>] ``Authentication function called for private bindings`` () =
                assertCalled
                <| (Some РежимБезопасности.Private)
                <| РежимБезопасности.Private
                <| true

            let [<Fact>] ``Authentication function not called for public bindings`` () =
                assertCalled
                <| (Some РежимБезопасности.Public)
                <| РежимБезопасности.Private
                <| false

            let [<Fact>] ``Authenticaiton function called for inherited bindings if mode is private`` () =
                assertCalled
                <| None
                <| РежимБезопасности.Private
                <| true

            let [<Fact>] ``Authentication function not called for inherited bindings if mode is public`` () =
                assertCalled
                <| None
                <| РежимБезопасности.Public
                <| false

            let [<Fact>] ``Correct user data is returned when authentication is successful`` () =
                
                let result = 
                    getResult (Some РежимБезопасности.Private)

                let auth _ =
                    Разрешено (Some { Id = "admin"; Свойства = []; })

                let isCorrect (data : ДанныеПользователя option) = 
                    match data with 
                    | Some user -> user.Id = "admin"
                    | _ -> false

                получитьАрг "/1235"
                |> setAuth auth
                |> authenticateRequest result
                |> getUserData
                |> isCorrect
                |> должно быть True

            let [<Fact>] ``Nothing is returned for private binding when no authentication function is set`` () =
                
                let result = getResult (Some РежимБезопасности.Private)

                получитьАрг "/12345"
                |> authenticateRequest result
                |> getUserData
                |> Option.isNone
                |> должно быть True

            let [<Fact>] ``Nothing is returned for public bindings`` () =
                
                let result = getResult (Some РежимБезопасности.Public)
                let auth _ = Разрешено (Some { Id = "admin"; Свойства = []; })

                получитьАрг "/1235"
                |> setAuth auth
                |> authenticateRequest result
                |> getUserData
                |> Option.isNone
                |> должно быть True

            let [<Fact>] ``Nothing is returned for inherited bindings when mode is public`` () =
                
                let result = getResult None
                let auth _ = Разрешено (Some { Id = "admin"; Свойства = []; })

                получитьАрг "/12345"
                |> setAuth auth
                |> setMode РежимБезопасности.Public
                |> authenticateRequest result
                |> getUserData
                |> Option.isNone
                |> должно быть True

            let [<Fact>] ``Nothing is returned for inherited bindings when mode is private but no authentication function is set`` () = 
                
                let result = getResult None

                получитьАрг "/12345"
                |> setMode РежимБезопасности.Private
                |> authenticateRequest result
                |> getUserData
                |> Option.isNone
                |> должно быть True

            let [<Fact>] ``HTTP 401 is returend when authentication is not successful`` () =
                
                let result = getResult (Some РежимБезопасности.Private)
                let auth _ = Запрещено

                получитьАрг "/12345"
                |> setAuth auth
                |> authenticateRequest result
                |> getStatusCode
                |> должно равняться StatusCodes.Unauthorised


    module ``Negotiation facts`` =

        open Discovery.Negotiation

        let [<Literal>] ModuleName = "Discovery.Negotiation"

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``asyncGetReader function`` =

            let получитьТипЗапроса = 

                let получитьТипКонтента результат = 
                    match результат with
                    | Some (типКонтента, _) -> Some типКонтента
                    | _ -> None

                asyncGetReader
                >> Async.RunSynchronously
                >> получитьТипКонтента

            let [<Fact>] ``Specified content type is selected if supported`` () =
                получитьАрг "/people/12345"
                |> добавитьЗаголовок "Content-Type" МедиаТипы.Text.Xml
                |> получитьТипЗапроса
                |> should be (Some' МедиаТипы.Text.Xml)

            let [<Fact>] ``Nothing is selected if content type is not supported`` () =
                получитьАрг "/people/12345"
                |> добавитьЗаголовок "Content-Type" "application/vendor"
                |> получитьТипЗапроса
                |> should be None'<String>

            let [<Fact>] ``Default content type is selected if none specified`` () =
                получитьАрг "/people/12345"
                |> получитьТипЗапроса
                |> should be (Some' DefaultMediaType)

            let [<Fact>] ``Target content type is used if specified content type is forwarded`` () =
                получитьАрг "/people/12345"
                |> добавитьЗаголовок "Content-Type" МедиаТипы.Text.Html
                |> addForwardedType МедиаТипы.Text.Html МедиаТипы.Text.Xml
                |> получитьТипЗапроса
                |> should be (Some' МедиаТипы.Text.Xml)

            let [<Fact>] ``Target content type is used in preference to specified content type even if supported`` () =
                получитьАрг "/people/12345"
                |> добавитьЗаголовок "Content-Type" МедиаТипы.Text.Xml
                |> addForwardedType МедиаТипы.Text.Xml МедиаТипы.Application.Json
                |> получитьТипЗапроса
                |> should be (Some' МедиаТипы.Application.Json)
                
        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``asyncGetWriter function`` =

            let получитьТипОтвета =

                let получитьТипКонтента result = 
                    match result with
                    | Some (contentType, _) -> Some contentType
                    | _ -> None

                asyncGetWriter
                >> Async.RunSynchronously
                >> получитьТипКонтента

            let [<Fact>] ``Specified content type is selected if supported`` () =
                получитьАрг "/people/12345"
                |> добавитьЗаголовок "Accept" МедиаТипы.Text.Xml
                |> получитьТипОтвета
                |> should be (Some' МедиаТипы.Text.Xml)

            let [<Fact>] ``First supported content type is selected if multiple types are specified`` () =
                получитьАрг "/people/12345"
                |> добавитьЗаголовок "Accept" "text/html, text/xml, application/json"
                |> получитьТипОтвета
                |> should be (Some' МедиаТипы.Text.Xml)

            let [<Fact>] ``Nothing is selected if content type is not supported`` () =
                получитьАрг "/people/12345"
                |> добавитьЗаголовок "Accept" МедиаТипы.Text.Html
                |> получитьТипОтвета
                |> should be None'<String>

            let [<Fact>] ``Default content type is selected if none specified`` () =
                получитьАрг "/people/12345"
                |> получитьТипОтвета
                |> should be (Some' DefaultMediaType)

            let [<Fact>] ``Default content type is selected if */* is specified`` () =
                получитьАрг "/people/12345"
                |> добавитьЗаголовок "Accept" "*/*"
                |> получитьТипОтвета
                |> should be (Some' DefaultMediaType)

            let [<Fact>] ``Target content type is used if specified content type is forwarded`` () =
                получитьАрг "/people/12345"
                |> добавитьЗаголовок "Accept" МедиаТипы.Text.Html
                |> addForwardedType МедиаТипы.Text.Html МедиаТипы.Text.Xml
                |> получитьТипОтвета
                |> should be (Some' МедиаТипы.Text.Xml)

            let [<Fact>] ``Target content type is used in preference to specified content type even if supported`` () =
                получитьАрг "/people/12345"
                |> добавитьЗаголовок "Accept" МедиаТипы.Text.Xml
                |> addForwardedType МедиаТипы.Text.Xml МедиаТипы.Application.Json
                |> получитьТипОтвета
                |> should be (Some' МедиаТипы.Application.Json)

            let [<Fact>] ``Content type forwarding is applied when multiple content types are specified`` () =
                получитьАрг "/people/12345"
                |> добавитьЗаголовок "Accept" "text/html, text/plain, application/json"
                |> addForwardedType МедиаТипы.Text.Html МедиаТипы.Text.Xml
                |> получитьТипОтвета
                |> should be (Some' МедиаТипы.Text.Xml)

    [<Trait (Traits.Names.Module, ModuleName)>]
    module ``run function`` = 

        open Framework
        open Discovery.Negotiation

        let [<Fact>] ``Unsupported content type returns HTTP 415`` () =

            let isNotSupported state = 
                match state with
                | Stopped stopType -> 
                    match stopType with
                    | Completed response ->
                        match response.ТипОтвета with
                        | StatusCode statusCode -> statusCode = StatusCodes.ContentTypeNotSupported
                        | _ -> false
                    | _ -> false
                | _ -> false

            получитьАрг "/people/12345"
            |> добавитьЗаголовок "Content-Type" МедиаТипы.Text.Html
            |> run
            |> isNotSupported
            |> должно быть True

        let [<Fact>] ``Bindings without messages return no reader information`` () =

            let hasNoReader state = 
                match state with
                | Running (args : Execution.ExecutionArgs) -> Option.isNone args.Reader
                | _ -> false

            let setBinding args = 

                let binding = 
                    get (fun () -> "Hello, World")

                let container = 
                    { args.Контейнер with Endpoints = [ { endpoint with Привязки = [ binding; ]; } ]; }

                { args with Контейнер = container; }

            получитьАрг "/people/12345"
            |> setBinding
            |> run
            |> hasNoReader
            |> должно быть True

        let [<Fact>] ``Bindings with messages return correct reader information`` () =
            
            let isReaderCorrect state = 
                match state with
                | Running (args : Execution.ExecutionArgs) -> 
                    match args.Reader with
                    | Some reader -> 
                        reader.ContentType = DefaultMediaType
                            && reader.MessageType = typedefof<obj>
                    | _ -> false
                | _ -> false

            получитьАрг "/people/12345"
            |> run
            |> isReaderCorrect
            |> должно быть True

        let [<Fact>] ``Correct writer information is returned`` () =
            
            let isWriterCorrect state = 
                match state with
                | Running (args : Execution.ExecutionArgs) ->
                    match args.Writer with
                    | Some writer -> writer.ContentType = DefaultMediaType
                    | _ -> false
                | _ -> false

            получитьАрг "/people/12345"
            |> run
            |> isWriterCorrect
            |> должно быть True

        let [<Fact>] ``Unauthorised request returns HTTP 401`` () =

            let isUnauthorised' state = 
                match state with
                | Stopped stopType ->
                    match stopType with
                    | Completed response ->
                        match response.ТипОтвета with
                        | StatusCode statusCode -> statusCode = StatusCodes.Unauthorised
                        | _ -> false
                    | _ -> false
                | _ -> false

            получитьАрг "/people/12345"
            |> makePrivate (fun _ -> Запрещено)
            |> run
            |> isUnauthorised'
            |> должно быть True

        let [<Fact>] ``User details are returned if provided by authentication function`` () =
            
            let hasUserDetails state = 
                match state with
                | Running (args : Execution.ExecutionArgs) -> 
                    match args.User with
                    | Some data -> data.Id = "user.name"
                    | _ -> false
                | _ -> false

            получитьАрг "/people/12345"
            |> makePrivate (fun _ -> Разрешено (Some { Id = "user.name"; Свойства = []; }))
            |> run
            |> hasUserDetails
            |> должно быть True

