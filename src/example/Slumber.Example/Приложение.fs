namespace Дрема.Пример

open System
open System.Web
open System.IO
open log4net
open log4net.Config
open Дрема.Протоколирование

type App () = 
    inherit HttpApplication ()

    let [<Literal>] LogName = "Slumber.log"

    static let mutable _initialised = false

    ///Sets up log4net logging
    let configureLogging () = 

        XmlConfigurator.Configure (
            new FileInfo ( 
                Path.Combine (
                    AppDomain.CurrentDomain.BaseDirectory,
                    "log4net.config"
                )
            )
        )
        |> ignore

        let log = 
            LogManager.GetLogger LogName

        установитьПисательЖурнала (fun entry ->
            match entry with
            | Отладка msg -> 
                if log.IsDebugEnabled then
                    log.Debug (msg)
            | Информация msg ->
                if log.IsInfoEnabled then
                    log.Info (msg)
            | Предупреждение msg ->
                if log.IsWarnEnabled then
                    log.Warn (msg)
            | Ошибка (msg, err) ->
                if log.IsErrorEnabled then
                    match err with 
                    | Some ex ->  log.Error (msg, ex)
                    | _ -> log.Error (msg)
        )

    member this.Application_Start (_ : obj, _ : EventArgs) =
        if (not _initialised) then

            _initialised <- true

            configureLogging ()