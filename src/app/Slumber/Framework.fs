namespace Дрема

open System

///Contains functions and types used to construct and execute request pipelines
module Framework =

    ///Contains functions and types for serialising and deserialising request and response bodies
    module ВВСообщений = 

        open System.IO

        ///Type alias for the signature of deserialisers
        type Читатель = Stream -> Type -> obj option

        ///Type alias for the signature of serialisers
        type Писатель = obj -> byte list


    ///Contains core types and functions used by Slumber
    [<AutoOpen>]
    module Ядро = 

        let [<Literal>] DefaultUrl = "http://localhost/"

        ///Union describing possible security modes
        type РежимБезопасности =
            | Public
            | Private
        
        ///Represents a verb bound operation binding
        type Привязка = {
            Verb : String;
            ТипСообщения : Type option;
            Операция : Операция;
            РежимБезопасности : РежимБезопасности option;
        }          
        with

            ///The empty binding
            static member Пустая =
                {
                    Verb = String.Empty;
                    ТипСообщения = None;
                    Операция = (fun _ -> РезультатОперации.Пустой);
                    РежимБезопасности = None;
                }

        ///Represents a URL bound endpoint exposing zero or more operations
        type ОконечнаяТочка = {
            Название : String;
            Шаблон : String;
            Привязки : Привязка list;
        }
        with

            ///The empty endpoint
            static member Empty =
                {
                    Название = String.Empty;
                    Шаблон = String.Empty;
                    Привязки = [];
                }    

        ///Represents supported content types 
        type КонфигВВ = {
            Читатели : (String * ВВСообщений.Читатель) list;
            Писатели : (String * ВВСообщений.Писатель) list;
            ПеренаправляемыеТипы : (String * String) list;
        }
        with

            ///The empty content types selection
            static member Пустой = 
                {
                    Читатели = [];
                    Писатели = [];
                    ПеренаправляемыеТипы = [];
                }        

        ///Union describing possible results of user authentication
        type РезультатАвторизации = 
            | Разрешено of (ДанныеПользователя option)
            | Запрещено        

        ///Represents the security configuration of a container
        type КонфигБезопасности = {
            DefaultMode : РежимБезопасности;
            Авторизовать : (Запрос -> РезультатАвторизации) option;
        }
        with

            ///Default security configuration
            static member Default = 
                {
                    DefaultMode = Public;
                    Авторизовать = None;
                }

        ///Represents a collection of endpoints and associated configuration data
        type Контейнер = {
            ОконечныеТочки : ОконечнаяТочка list;
            ВВ : КонфигВВ;
            БазовыйУрл : Uri;
            Security : КонфигБезопасности;
            Resolver : Resolver option;
        }
        with

            ///The empty container
            static member Пустой = 
                {
                    ОконечныеТочки = [];
                    ВВ = КонфигВВ.Пустой;
                    БазовыйУрл = Uri (DefaultUrl, UriKind.Absolute);
                    Security = КонфигБезопасности.Default;
                    Resolver = None;
                }

        ///Union describing possible configuration modes
        type РежимКонфигурации = 
            | Implicit
            | Explicit of Контейнер
            | Mixed of (Контейнер -> Контейнер)

        ///Defines a class which can describe a container for a given base URL
        type ИОписаниеКонтейнера = 
            abstract member Описать : Uri -> Контейнер

        ///Contains functions for working with containers
        module Containers = 

            ///Gets a container's endpoint collection
            let getEndpoints container =
                container.ОконечныеТочки

            ///Gets a container's base URL
            let getBaseUrl container = 
                container.БазовыйУрл

            ///Gets the container's IO configuration
            let getIO container = 
                container.ВВ

            ///Gets the readers configured for a container
            let getReaders container = 
                container.ВВ.Читатели

            let private getMessageIO contentType = 
                List.tryPick (fun (contentType', io) ->
                        if (String.same contentType contentType') then
                            Some io
                        else
                            None
                    )

            ///Gets the reader for the given content type
            let получитьЧитателя contentType container = 
                container.ВВ.Читатели
                |> getMessageIO contentType

            ///Gets the writers configured for a container
            let getWriters container = 
                container.ВВ.Писатели

            ///Gets the writer for the given content type
            let получитьПисателя contentType container = 
                container.ВВ.Писатели
                |> getMessageIO contentType

            ///True if a type is forwarded
            let isForwarded contentType container = 
                container.ВВ.ПеренаправляемыеТипы
                |> List.exists (fst >> String.same contentType)

            ///Applies forwarding to a content type, returning the fowrarded type if one is configured
            let applyForwarding contentType container = 

                let forwardedType = 
                    container.ВВ.ПеренаправляемыеТипы
                    |> List.tryPick (fun (from, to') ->
                            if (String.same from contentType) then
                                Some to'
                            else
                                None
                        )

                match forwardedType with
                | None -> contentType
                | Some contentType -> contentType

            ///Gets the security config of a container
            let getSecurityConfig container = 
                container.Security

            ///Gets an endpoint from a container by name
            let tryGetEndpointByName name container = 
                container.ОконечныеТочки
                |> List.tryPick (fun endpoint ->
                        if (String.same endpoint.Название name) then
                            Some endpoint
                        else
                            None
                    )

        ///Functions for working with endpoints
        module Endpoints =

            ///Gets the name of a given endpoint
            let getName endpoint = 
                endpoint.Название

            ///Gets an endpoint's template
            let getTemplate endpoint =
                endpoint.Шаблон

            ///Gets an endpoint's binding collection
            let getBindings endpoint =
                endpoint.Привязки

            ///Tries to get a binding by verb
            let tryGetBinding verb endpoint =
                endpoint.Привязки
                |> List.tryFind (fun binding ->
                        String.same verb binding.Verb
                    )

    ///Contains functions and types for running request pipelines
    [<AutoOpen>]
    module Конвейер = 

        ///Describes reasons for the pipeline to stop
        type ТипОстановки = 
            | Исключение of Exception
            | Завершено of Ответ

        ///Describes possible pipeline states
        type Состояние<'ТСостояние> = 
            | Запущен of 'ТСостояние
            | Остановлен of ТипОстановки   

        ///Gets the state resulting from the execution of a function
        let private getNextState f arg = 
            async {
                try 
                    return! (f arg)
                with
                | e -> 
                    return (Остановлен (Исключение e))
            }

        ///Binds two pipeline phases together
        let (-->) f g = 
            fun arg ->   
                async {              

                    let! state = 
                        getNextState f arg

                    match (state) with
                    | Запущен arg' -> return! (g arg')
                    | Остановлен type' -> return (Остановлен type')
                }

        ///Binds the final pipeline phase to the rest of the pipeline
        let (--|) pipeline end' = 
            fun arg ->
                async {
            
                    let! state = 
                        getNextState pipeline arg

                    return! (end' state)
                }

        ///Lifts an argument to the running state
        let start arg =
            async {
                return (Запущен arg)
            }

    ///Contains pipeline utility functions
    module Helpers =

        ///Stops execution of the pipeline with the given response
        let stopWithResponse response = 
            response
            |> Завершено
            |> Остановлен

        ///Stops execution of the pipline with the given status code
        let stopWithStatus statusCode = 
            {
                ТипОтвета = (StatusCode statusCode);
                ТипСодержимого = None;
                CustomHeaders = [];
            }
            |> Завершено
            |> Остановлен

        ///Stops the execution of the pipeline with an error
        let остановитьСОшибкой e = 
            Исключение e
            |> Остановлен

        ///Continues execution of the pipeline with the given arguments
        let continue' arg = 
            Запущен arg

    ///Contains functions for loading implicit configuration
    [<RequireQualifiedAccess>]
    module НеявнаяКонфигурация = 

        open System.IO
        open System.Reflection
        open System.Collections.Concurrent
        open System.Web.Compilation
        open HandyFS.Types

        ///Instantiates and queries the first container description that can be found in the /bin/ folder 
        let private найти базовыйУрл = 

            ///TODO Tidy up / abstract this function

            BuildManager.GetReferencedAssemblies () 
            |> ignore

            let попробоватьПолучитьТипы (сборка : Assembly) = 
                try
                    сборка.GetTypes ()
                with
                | :? ReflectionTypeLoadException -> Array.empty

            let попробоватьСоздатьОписание (тип' : Type) = 
                try
                    Some ((Activator.CreateInstance тип') :?> ИОписаниеКонтейнера)
                with
                | _ -> None

            let типы = 
                AppDomain.CurrentDomain.GetAssemblies ()
                |> Array.Parallel.collect попробоватьПолучитьТипы
                |> Array.filter (implements typeof<ИОписаниеКонтейнера>)

            if (Array.isEmpty типы) then
                invalidOp "Implicit configuration requires a type that implements IContainerDescription but none could be found."

            else
                match (Array.tryPick попробоватьСоздатьОписание типы) with
                | Some описание -> описание.Описать базовыйУрл
                | _ -> invalidOp "No type implementing IContainerDescription could be instantiated." //TODO More appropriate exception type?
    
        ///Instantiates and queries the first container description that can be found in the /bin/ folder and caches the result
        let получить = 

            let конфиги = ConcurrentDictionary<Uri, Контейнер> ()

            fun (базовыйУрл : Uri) -> 
                конфиги.GetOrAdd (
                    базовыйУрл, 
                    найти
                )

