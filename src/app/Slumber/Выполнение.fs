namespace Дрема

open System
open HandyFS.Option
open Дрема.Общее.AsyncAttempt
open Дрема.Framework
open Дрема.Framework.Helpers
open Дрема.Render

///Contains functions used to invoke operations
module Execution =

    ///The default status code used for successful execution
    let [<Literal>] DefaultStatusCode = StatusCodes.Ok  

    ///Represents the selected message deserialiser and associated information
    type ReaderInfo = {
        Reader : ВВСообщений.Читатель;
        ContentType : String;
        MessageType : Type;
    }

    ///Represents the selected message serialiser and associated information
    type WriterInfo = {
        Writer : ВВСообщений.Писатель;
        ContentType : String;
    }

    ///Represents the selected operation and associated information
    type TargetInfo = {
        EndpointName : String;
        Operation : Операция;
        Parameters : (String * String) list;
    }
    with

        ///Empty target information
        static member Empty = 
            {
                EndpointName = String.Empty;
                Operation = (fun _ -> РезультатОперации.Пустой);
                Parameters = [];
            }

    ///Represents the arguments used to run the execution phase
    type ExecutionArgs = {
        Request : Запрос;
        Container : Контейнер;
        Reader : ReaderInfo option;
        Writer : WriterInfo option;
        Target : TargetInfo;
        User : ДанныеПользователя option;
    }
    with

        ///Empty execution args
        static member Empty = 
            {
                Request = Запрос.Пустой;
                Container = Контейнер.Пустой;
                Reader = None;
                Writer = None;
                Target = TargetInfo.Empty;
                User = None;
            }

    ///Reads a message from the input stream
    let asyncGetMessage () =
        fun args ->
            async {

                try 
                    let message = 
                        match (args.Request.Payload.Тело, args.Reader) with
                        | (Some _, None) ->

                            журналПредупр "[%A] Request message received but no reader selected" args.Request.Id

                            None

                        | (None, _) | (_, None) -> None
                        | (Some stream, Some reader) ->

                            журналИнфо "[%A] Deserialising request message" args.Request.Id

                            reader.Reader
                            <| stream
                            <| reader.MessageType

                    return Успех message
                with
                | e -> 

                    журналИсключение e "[%A] Exception encountered reading request body: %A" args.Request.Id e.Message

                    ///TODO Standard error message for this error?

                    return Провал StatusCodes.BadRequest                
            }

    ///Gets the operation context for a set of arguments
    let asyncGetContext () = 
        asyncAttempt {

            let! message = asyncGetMessage ()
            let! args = getState ()

            let context = 
                {
                    Метаданные = 
                        {
                            ContainerUrl = args.Container.BaseUrl;
                            EndpointName = args.Target.EndpointName;
                            Запрос = args.Request;
                            Параметры = args.Target.Parameters;
                            Пользователь = args.User;
                            Resolver = args.Container.Resolver;
                        };
                    Сообщение = message;
                } 
                              
            return context
        }

    ///Invokes the operation and returns the result
    let asyncInvokeOperation (context : КонтекстОперации) =
        fun (args : ExecutionArgs) ->
            async {

                журналИнфо "[%A] Invoking operation" args.Request.Id

                try 
                    return (Успех (args.Target.Operation context))
                with
                | e ->

                    журналИсключение e "[%A] An exception was encountered invoking the operation: %A" args.Request.Id e.Message

                    //TODO Error message to display?

                    return (Провал StatusCodes.InternalServerError)
            }

    ///Serialises the resource returned by the operation, if any
    let asyncGetResponse (result : РезультатОперации) =
        fun args -> 
            async {

                журналИнфо "[%A] Constructing operation response" args.Request.Id

                let statusCode =
                    result.КодСтатуса
                    |> someOr DefaultStatusCode

                try 
                    return 
                        match (result.Ресурс, args.Writer) with 
                        | (Some _, None) -> 

                            журналИнфо "[%A] Response resource returned but no writer selected" args.Request.Id

                            Провал StatusCodes.NotAcceptable

                        | (None, _) -> 

                            журналИнфо "[%A] No response resource returned, status code is %A" args.Request.Id statusCode

                            Успех (StatusCode statusCode, result.Заголовки)

                        | (Some resource, Some writer) ->

                            журналИнфо "[%A] Response resource returned, status code is %A" args.Request.Id statusCode

                            let bytes = 
                                writer.Writer resource

                            Успех (Ресурс (statusCode, bytes), result.Заголовки)
                with
                | e ->

                    
                    журналИсключение e "[%A] Exception encountered writing response body: %A" args.Request.Id e.Message

                    ///TODO Error message to display

                    return Провал StatusCodes.NotAcceptable
            }

    ///Asynchronously runs the execution phase
    let asyncRun (args : ExecutionArgs) =

        let (-->) = AsyncAttempt.Monad.bind
        let start = AsyncAttempt.Monad.return' ()

        async {

            журналИнфо "[%A] Beginning operation execution phase" args.Request.Id

            let! result =             
                args 
                |> start
                --> asyncGetContext
                --> asyncInvokeOperation
                --> asyncGetResponse

            let responseType, headers = 
                match result with
                | Успех (responseType, headers) -> (responseType, headers)
                | Провал statusCode -> (StatusCode statusCode, [])

            let contentType = 
                match args.Writer with
                | Some writer -> Some writer.ContentType
                | _ -> None

            let response = 
                { 
                    ТипОтвета = responseType;
                    ТипСодержимого = contentType;
                    CustomHeaders = headers;
                }

            return (stopWithResponse response)
        }

    ///Runs the execution phase
    let run<'TState> : ExecutionArgs -> State<'TState> = 
        asyncRun
        >> Async.RunSynchronously

