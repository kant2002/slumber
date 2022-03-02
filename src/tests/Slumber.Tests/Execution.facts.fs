namespace Дрема.Tests

open System
open System.Text
open System.IO
open FsUnit
open Xunit
open Дрема

module ``Execution facts`` =
    
    open Execution
    open Http

    let [<Literal>] ModuleName = "Execution"

    [<AutoOpen>]
    module Helpers = 

        let getRequest body = 
            
            let stream = 
                if (String.IsNullOrWhiteSpace body) then
                    None
                else
                    
                    let bytes = 
                        body |> Encoding.UTF8.GetBytes

                    Some (new MemoryStream (bytes) :> Stream)

            let baseUrl = 
                Uri ("http://localhost:8080/api", UriKind.Absolute)

            {
                Запрос.Пустой
                with
                    Url = 
                        {
                            Urls.Пустые
                            with
                                Raw = Uri (baseUrl, "people");
                                Путь = "/people";
                                BaseUrl = baseUrl;
                        };
                    Verb = "POST";
                    Payload = 
                        {
                            Payload.Пустой
                            with
                                Тело = stream;
                        }
            }

        let getReader (value : String) = 
            fun _ _ ->
                value
                |> box
                |> Some

        let getArgs (hasBody, hasReader) = 

            let body = 
                if hasBody then
                    "Hello, World"
                else
                    String.Empty

            let reader = 
                if hasReader then
                    Some {
                        ContentType = МедиаТипы.Text.Xml;
                        Reader = (getReader body);
                        MessageType = typedefof<String>;
                    }
                else
                    None

            let writer = 
                {
                    ContentType = МедиаТипы.Text.Xml;
                    Writer = (fun _ -> []);
                }

            {
                ExecutionArgs.Empty
                with
                    Request = (getRequest body);
                    Reader = reader;
                    Writer = Some writer;
                    Target =
                        {
                            TargetInfo.Empty
                            with
                                Operation = fun _ -> РезультатОперации.Оба (200, "Hello, World", [ ("key", "value"); ])
                        };
            }

        let getArgs' () = 
            getArgs (true, true)

        let setOperation op (args : ExecutionArgs) = 

            let target = 
                { args.Target with Operation = op; }

            { args with Target = target; }

        let setWriter writer (args : ExecutionArgs) = 
            { args with Writer = writer; }
    
    [<Trait (Traits.Names.Module, ModuleName)>]
    module ``asyncGetMessage function`` = 

        let getMessage =    
            asyncGetMessage ()
            >> Async.RunSynchronously

        let [<Fact>] ``No body returns nothing`` () =

            //NOTE This should never happen when executed as a part of the default pipeline but is possible from the arguments
                
            let isNothing result = 
                match result with
                | Успех None -> true
                | _ -> false

            getArgs (false, true)
            |> getMessage
            |> isNothing
            |> should be True

        let [<Fact>] ``No reader returns nothing`` () =
                
            let isNothing result = 
                match result with
                | Успех None -> true
                | _ -> false
                    
            getArgs (true, false)
            |> getMessage
            |> isNothing
            |> should be True

        let [<Fact>] ``The message is returned correctly`` () =
                
            let isCorrect result = 
                match result with
                | Успех (Some value) -> ((string value) = "Hello, World")
                | _ -> false

            getArgs (true, true)
            |> getMessage
            |> isCorrect
            |> should be True

        let [<Fact>] ``Exception from reader returns HTTP 400`` () =
                
            let isBadRequest result = 
                match result with
                | Провал statusCode -> statusCode = StatusCodes.BadRequest
                | _ -> false

            let setFailingReader args = 
                    
                let reader = 
                    match args.Reader with
                    | Some reader' -> 
                        Some {
                            reader'
                            with
                                Reader = (fun _ _ -> raise (InvalidOperationException ()));
                        }
                    | _ -> None

                { args with Reader = reader; }

            getArgs (true, true)
            |> setFailingReader
            |> getMessage
            |> isBadRequest
            |> should be True

    [<Trait (Traits.Names.Module, ModuleName)>]
    module ``asyncGetContext function`` = 

        let getContext args = 
            args
            |> asyncGetContext ()
            |> Async.RunSynchronously

        let [<Fact>] ``Request ID is set correctly on OperationMetadata`` () =

            let requestId = Guid.NewGuid ()
            
            let isCorrect result = 
                match result with
                | Успех context -> context.Метаданные.Запрос.Id = requestId
                | _ -> false

            let setRequestId (args : ExecutionArgs) = 
                { args with Request = { args.Request with Id = requestId; } } 

            getArgs' ()
            |> setRequestId
            |> getContext
            |> isCorrect
            |> should be True

        let [<Fact>] ``User is set correctly on OperationMetadata when provided`` () =

            let setUser (args : ExecutionArgs) = 
                { args with User = (Some { Id = "user.name"; Свойства = []; }); }

            let isCorrect result = 
                match result with
                | Успех context -> 
                    match context.Метаданные.Пользователь with
                    | Some data -> data.Id = "user.name"
                    | _ -> false
                | _ -> false

            getArgs' ()
            |> setUser
            |> getContext
            |> isCorrect
            |> should be True

        let [<Fact>] ``No user is set on OperationMetadata when not provided`` () =
            
            let isCorrect result = 
                match result with
                | Успех context -> Option.isNone context.Метаданные.Пользователь
                | _ -> false

            getArgs' ()
            |> getContext
            |> isCorrect
            |> should be True            

        let [<Fact>] ``Resolver is set correctly on OperationMetadata when provided`` () = 

            //NOTE This test really isn't nice, but Object.Reference(resolver, resolver') returns
            //false. Must be a better way.
            
            let resolver type' = 
                if (type' = typeof<int>) then
                    Some (box 42)
                else
                    None
            
            let setResolver args = 
                { args with Container = { args.Container with Resolver = (Some resolver); }; }                

            let isCorrect result = 
                match result with
                | Успех context -> 
                    match context.Метаданные.Resolver with
                    | Some resolver' -> 
                        match (resolver' typeof<int>) with
                        | Some value -> value = (box 42)
                        | _ -> false
                    | _ -> false
                | _ -> false

            getArgs' ()
            |> setResolver
            |> getContext
            |> isCorrect
            |> should be True

        let [<Fact>] ``No resolver is et on OperationMetadata when not provided`` () = 
            
            let isCorrect result = 
                match result with
                | Успех context -> Option.isNone context.Метаданные.Resolver
                | _ -> false

            getArgs' ()
            |> getContext
            |> isCorrect
            |> should be True

    [<Trait (Traits.Names.Module, ModuleName)>]
    module ``asyncInvokeOperation function`` = 
            
        let invokeOperation context args = 
            asyncInvokeOperation context args
            |> Async.RunSynchronously

        let createContext () = 

            let baseUrl = 
                Uri ("http://localhost:8080/api", UriKind.Absolute)

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

        let [<Fact>] ``Successful returns correct value`` () =

            let isCorrectValue result = 
                match result with
                | Успех data ->
                    data.КодСтатуса = (Some 200)
                        && data.Ресурс = (Some (box "Hello, World"))
                | _ -> false

            getArgs' ()
            |> invokeOperation (createContext ())
            |> isCorrectValue
            |> should be True

        let [<Fact>] ``Exception returns HTTP 500`` () =
            
            let isServerError result = 
                match result with
                | Провал statusCode -> statusCode = 500
                | _ -> false

            getArgs' ()
            |> setOperation (fun _ -> raise (InvalidOperationException ()))
            |> invokeOperation (createContext ())
            |> isServerError
            |> should be True

    [<Trait (Traits.Names.Module, ModuleName)>]
    module ``asyncGetResponse function`` =

        open System.Text

        let getResponse result args = 
            asyncGetResponse result args
            |> Async.RunSynchronously

        let [<Fact>] ``Empty result returns default status code and no resource`` () =

            let isDefaultStatusCode result = 
                match result with
                | Успех (StatusCode statusCode, _) -> statusCode = DefaultStatusCode
                | _ -> false

            getArgs' ()
            |> getResponse РезультатОперации.Пустой
            |> isDefaultStatusCode
            |> should be True

        let [<Fact>] ``Status code with no resource returns specified status code`` () =

            let isStatusCode result = 
                match result with
                | Успех (StatusCode 418, _) -> true
                | _ -> false

            getArgs' ()
            |> getResponse (РезультатОперации.ТолькоСтатус 418)
            |> isStatusCode
            |> should be True

        let [<Fact>] ``Default status code used when none specified`` () =

            let isDefaultStatusCode result = 
                match result with
                | Успех (Ресурс (DefaultStatusCode, _), _) -> true
                | _ -> false

            getArgs' ()
            |> getResponse ({ КодСтатуса = None; Ресурс = (Some (box  "Hello, World")); Заголовки = []; })
            |> isDefaultStatusCode
            |> should be True

        let [<Fact>] ``Resource with no writer returns HTTP 406`` () =

            let isNotAcceptable result = 
                match result with
                | Провал statusCode -> statusCode = StatusCodes.NotAcceptable
                | _ -> false

            getArgs' ()
            |> setWriter None
            |> getResponse (РезультатОперации.ТолькоРесурс "Hello, World")
            |> isNotAcceptable
            |> should be True

        let [<Fact>] ``Writer exception returns HTTP 406`` () =

            let isNotAcceptable result = 
                match result with
                | Провал statusCode -> statusCode = StatusCodes.NotAcceptable
                | _ -> false

            let writer = 
                {
                    ContentType = МедиаТипы.Text.Plain;
                    Writer = (fun _ -> raise (InvalidOperationException ()));
                }

            getArgs' ()
            |> setWriter (Some writer)
            |> getResponse (РезультатОперации.ТолькоРесурс "Hello, World")
            |> isNotAcceptable
            |> should be True

        let [<Fact>] ``Writer output is returned correctly`` () =

            let bytes = 
                Encoding.UTF8.GetBytes "Hello, World"
                |> Array.toList

            let writer =
                {
                    ContentType = МедиаТипы.Text.Plain;
                    Writer = (fun _ -> bytes);
                }

            let isCorrectValue result = 
                match result with
                | Успех (Ресурс (_, bytes'), _) -> bytes' = bytes
                | _ -> false

            getArgs' ()
            |> setWriter (Some writer)
            |> getResponse (РезультатОперации.ТолькоРесурс "Hello, World")
            |> isCorrectValue
            |> should be True

        let [<Fact>] ``Headers are returned with resource`` () =

            let headers = 
                [ ("key", "value"); ]
            
            let headersAreCorrect result = 
                match result with
                | Успех (_, headers') -> headers' = headers
                | _ -> false

            getArgs' ()
            |> getResponse (РезультатОперации.ТолькоРесурс ("Hello, World", headers))
            |> headersAreCorrect
            |> should be True

        let [<Fact>] ``Headers are returned with status code`` () =

            let headers =
                [ ("key", "value"); ]
            
            let headersAreCorrect result = 
                match result with
                | Успех (_, headers') -> headers' = headers
                | _ -> false

            getArgs' ()
            |> getResponse (РезультатОперации.ТолькоСтатус (200, headers))
            |> headersAreCorrect
            |> should be True

    [<Trait (Traits.Names.Module, ModuleName)>]
    module ``run function`` = 

        open Framework

        let [<Fact>] ``Stops due to completion`` () =
            
            let isStopped result = 
                match result with
                | Stopped (Completed _) -> true
                | _ -> false

            getArgs' () 
            |> run
            |> isStopped
            |> should be True

        let [<Fact>] ``Returns the correct response type`` () =
            
            let isCorrectResponse result = 
                match result with
                | Stopped (Completed resp) ->
                    match resp.ТипОтвета with
                    | Ресурс (200, []) -> true
                    | _ -> false
                | _ -> false

            getArgs' ()
            |> run
            |> isCorrectResponse
            |> should be True

        let [<Fact>] ``Returns the correct headers`` () =
            
            let areHeadersCorrect result = 
                match result with
                | Stopped (Completed resp) -> resp.CustomHeaders = [ ("key", "value"); ]
                | _ -> false

            getArgs' ()
            |> run
            |> areHeadersCorrect
            |> should be True

        let [<Fact>] ``Returns the content type of the writer if present`` () =
            
            let isContentTypeCorrect result = 
                match result with
                | Stopped (Completed resp) ->
                    match resp.ТипСодержимого with
                    | Some contentType -> contentType = МедиаТипы.Text.Xml
                    | _ -> false
                | _ -> false

            getArgs' ()
            |> run
            |> isContentTypeCorrect
            |> should be True

        let [<Fact>] ``Returns no content type if no writer present`` () =
            
            let isContentTypeCorrect result = 
                match result with
                | Stopped (Completed resp) -> Option.isNone resp.ТипСодержимого
                | _ -> false

            getArgs' ()
            |> setOperation (fun _ -> РезультатОперации.Пустой)
            |> setWriter None
            |> run
            |> isContentTypeCorrect
            |> should be True


