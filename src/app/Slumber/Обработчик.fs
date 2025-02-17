﻿namespace Дрема

open System
open System.Web
open Дрема.Framework

///Default implementation of an HTTP handler for Slumber
type SlumberHandler () =
    interface IHttpHandler with

        member this.ProcessRequest (context : HttpContext) = 

            let id = Guid.NewGuid ()
            let wrapped = HttpContextWrapper (context)
            let input = wrapped.GetInput id
            let output = wrapped.GetOutput ()

            Конвейер.run
            <| Implicit
            <| input
            <| output

        member this.IsReusable = 
            false
            

