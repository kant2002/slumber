﻿namespace Дрема

open System

///Contains functions and types used to construct and execute request pipelines
module Framework =

    ///Contains functions and types for serialising and deserialising request and response bodies
    module MessageIO = 

        open System.IO

        ///Type alias for the signature of deserialisers
        type Reader = Stream -> Type -> obj option

        ///Type alias for the signature of serialisers
        type Writer = obj -> byte list


    ///Contains core types and functions used by Slumber
    [<AutoOpen>]
    module Core = 

        let [<Literal>] DefaultUrl = "http://localhost/"

        ///Union describing possible security modes
        type SecurityMode =
            | Public
            | Private
        
        ///Represents a verb bound operation binding
        type Binding = {
            Verb : String;
            MessageType : Type option;
            Operation : Operation;
            SecurityMode : SecurityMode option;
        }          
        with

            ///The empty binding
            static member Empty =
                {
                    Verb = String.Empty;
                    MessageType = None;
                    Operation = (fun _ -> OperationResult.Empty);
                    SecurityMode = None;
                }

        ///Represents a URL bound endpoint exposing zero or more operations
        type Endpoint = {
            Название : String;
            Шаблон : String;
            Bindings : Binding list;
        }
        with

            ///The empty endpoint
            static member Empty =
                {
                    Название = String.Empty;
                    Шаблон = String.Empty;
                    Bindings = [];
                }    

        ///Represents supported content types 
        type IOConfig = {
            Readers : (String * MessageIO.Reader) list;
            Writers : (String * MessageIO.Writer) list;
            ПеренаправляемыеТипы : (String * String) list;
        }
        with

            ///The empty content types selection
            static member Empty = 
                {
                    Readers = [];
                    Writers = [];
                    ПеренаправляемыеТипы = [];
                }        

        ///Union describing possible results of user authentication
        type РезультатАвторизации = 
            | Разрешено of (ДанныеПользователя option)
            | Запрещено        

        ///Represents the security configuration of a container
        type SecurityConfig = {
            DefaultMode : SecurityMode;
            Authenticate : (Запрос -> РезультатАвторизации) option;
        }
        with

            ///Default security configuration
            static member Default = 
                {
                    DefaultMode = Public;
                    Authenticate = None;
                }

        ///Represents a collection of endpoints and associated configuration data
        type Контейнер = {
            Endpoints : Endpoint list;
            IO : IOConfig;
            BaseUrl : Uri;
            Security : SecurityConfig;
            Resolver : Resolver option;
        }
        with

            ///The empty container
            static member Empty = 
                {
                    Endpoints = [];
                    IO = IOConfig.Empty;
                    BaseUrl = Uri (DefaultUrl, UriKind.Absolute);
                    Security = SecurityConfig.Default;
                    Resolver = None;
                }

        ///Union describing possible configuration modes
        type ConfigurationMode = 
            | Implicit
            | Explicit of Контейнер
            | Mixed of (Контейнер -> Контейнер)

        ///Defines a class which can describe a container for a given base URL
        type IContainerDescription = 
            abstract member Describe : Uri -> Контейнер

        ///Contains functions for working with containers
        module Containers = 

            ///Gets a container's endpoint collection
            let getEndpoints container =
                container.Endpoints

            ///Gets a container's base URL
            let getBaseUrl container = 
                container.BaseUrl

            ///Gets the container's IO configuration
            let getIO container = 
                container.IO

            ///Gets the readers configured for a container
            let getReaders container = 
                container.IO.Readers

            let private getMessageIO contentType = 
                List.tryPick (fun (contentType', io) ->
                        if (String.same contentType contentType') then
                            Some io
                        else
                            None
                    )

            ///Gets the reader for the given content type
            let getReader contentType container = 
                container.IO.Readers
                |> getMessageIO contentType

            ///Gets the writers configured for a container
            let getWriters container = 
                container.IO.Writers

            ///Gets the writer for the given content type
            let getWriter contentType container = 
                container.IO.Writers
                |> getMessageIO contentType

            ///True if a type is forwarded
            let isForwarded contentType container = 
                container.IO.ПеренаправляемыеТипы
                |> List.exists (fst >> String.same contentType)

            ///Applies forwarding to a content type, returning the fowrarded type if one is configured
            let applyForwarding contentType container = 

                let forwardedType = 
                    container.IO.ПеренаправляемыеТипы
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
                container.Endpoints
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
                endpoint.Bindings

            ///Tries to get a binding by verb
            let tryGetBinding verb endpoint =
                endpoint.Bindings
                |> List.tryFind (fun binding ->
                        String.same verb binding.Verb
                    )

    ///Contains functions and types for running request pipelines
    [<AutoOpen>]
    module Pipeline = 

        ///Describes reasons for the pipeline to stop
        type StopType = 
            | Exception of Exception
            | Completed of Ответ

        ///Describes possible pipeline states
        type State<'TState> = 
            | Running of 'TState
            | Stopped of StopType   

        ///Gets the state resulting from the execution of a function
        let private getNextState f arg = 
            async {
                try 
                    return! (f arg)
                with
                | e -> 
                    return (Stopped (Exception e))
            }

        ///Binds two pipeline phases together
        let (-->) f g = 
            fun arg ->   
                async {              

                    let! state = 
                        getNextState f arg

                    match (state) with
                    | Running arg' -> return! (g arg')
                    | Stopped type' -> return (Stopped type')
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
                return (Running arg)
            }

    ///Contains pipeline utility functions
    module Helpers =

        ///Stops execution of the pipeline with the given response
        let stopWithResponse response = 
            response
            |> Completed
            |> Stopped

        ///Stops execution of the pipline with the given status code
        let stopWithStatus statusCode = 
            {
                ResponseType = (StatusCode statusCode);
                ContentType = None;
                CustomHeaders = [];
            }
            |> Completed
            |> Stopped

        ///Stops the execution of the pipeline with an error
        let stopWithError e = 
            Exception e
            |> Stopped

        ///Continues execution of the pipeline with the given arguments
        let continue' arg = 
            Running arg

    ///Contains functions for loading implicit configuration
    [<RequireQualifiedAccess>]
    module ImplicitConfiguration = 

        open System.IO
        open System.Reflection
        open System.Collections.Concurrent
        open System.Web.Compilation
        open HandyFS.Types

        ///Instantiates and queries the first container description that can be found in the /bin/ folder 
        let private find baseUrl = 

            ///TODO Tidy up / abstract this function

            BuildManager.GetReferencedAssemblies () 
            |> ignore

            let tryGetTypes (assembly : Assembly) = 
                try
                    assembly.GetTypes ()
                with
                | :? ReflectionTypeLoadException -> Array.empty

            let tryCreateDescription (type' : Type) = 
                try
                    Some ((Activator.CreateInstance type') :?> IContainerDescription)
                with
                | _ -> None

            let types = 
                AppDomain.CurrentDomain.GetAssemblies ()
                |> Array.Parallel.collect tryGetTypes
                |> Array.filter (implements typeof<IContainerDescription>)

            if (Array.isEmpty types) then
                invalidOp "Implicit configuration requires a type that implements IContainerDescription but none could be found."

            else
                match (Array.tryPick tryCreateDescription types) with
                | Some description -> description.Describe baseUrl
                | _ -> invalidOp "No type implementing IContainerDescription could be instantiated." //TODO More appropriate exception type?
    
        ///Instantiates and queries the first container description that can be found in the /bin/ folder and caches the result
        let get = 

            let configs = ConcurrentDictionary<Uri, Контейнер> ()

            fun (baseUrl : Uri) -> 
                configs.GetOrAdd (
                    baseUrl, 
                    find
                )

