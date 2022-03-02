namespace Дрема.Пример

open System
open System.Web
open System.IO
open log4net
open log4net.Config
open Дрема.Протоколирование

type Приложение () = 
    inherit HttpApplication ()

    let [<Literal>] ИмяЛога = "Slumber.log"

    static let mutable _инициализировано = false

    ///Sets up log4net logging
    let настроитьЖурналирование () = 

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
            LogManager.GetLogger ИмяЛога

        установитьПисательЖурнала (fun запись ->
            match запись with
            | Отладка сообщ -> 
                if log.IsDebugEnabled then
                    log.Debug (сообщ)
            | Информация сообщ ->
                if log.IsInfoEnabled then
                    log.Info (сообщ)
            | Предупреждение сообщ ->
                if log.IsWarnEnabled then
                    log.Warn (сообщ)
            | Ошибка (сообщ, ошиб) ->
                if log.IsErrorEnabled then
                    match ошиб with 
                    | Some искл ->  log.Error (сообщ, искл)
                    | _ -> log.Error (сообщ)
        )

    member this.Application_Start (_ : obj, _ : EventArgs) =
        if (not _инициализировано) then

            _инициализировано <- true

            настроитьЖурналирование ()