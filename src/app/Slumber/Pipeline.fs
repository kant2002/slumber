namespace Дрема

open System
open System.Web
open Дрема.Framework

///Contains functions for running the default Slumber pipeline
module Pipeline = 

    ///Runs the standard Slumber pipeline asynchronously
    let asyncRun mode (input : Запрос) (output : IOutput) = 

        журналИнфо "[%A] Pipeline begins for request to %A" input.Id input.Url.Raw.AbsoluteUri

        let execute = 
            start
            --> Bootstrap.asyncRun mode
            --> Discovery.asyncRun
            --> Execution.asyncRun
            --| Render.asyncRun input.Id output
    
        execute input

    ///Runs the standard Slumber pipeline synchronously
    let run mode input output = 
        asyncRun mode input output
        |> Async.RunSynchronously