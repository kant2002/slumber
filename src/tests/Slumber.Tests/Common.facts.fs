﻿namespace Дрема.Tests

open FsUnit
open Xunit
open Xunit.Extensions
open System
open System.IO
open System.Web
open System.Text
open Дрема

module ``Common facts`` =

    module ``Http facts`` = 

        open Http

        let [<Literal>] ModuleName = "Common.Http"

        [<AutoOpen>]
        module Helpers = 

            let getRequestTo (url : String) (app : String) (body : string) = 
                {
                    new HttpRequestBase () with

                        member this.Url = Uri (url, UriKind.Absolute)
                        member this.ApplicationPath = app
                        member this.HttpMethod = "POST"
                    
                        member this.QueryString = 
                            [
                                ("page", "5");
                            ]
                            |> List.toNvc

                        member this.Headers = 
                            [
                                ("Content-Type", MediaTypes.Text.Xml);
                                ("Accept", MediaTypes.Application.Json);
                            ]
                            |> List.toNvc

                        member this.InputStream = 

                            let bytes = 
                                if (String.IsNullOrWhiteSpace body) then
                                    Array.empty<Byte>
                                else
                                    Encoding.UTF8.GetBytes body

                            new MemoryStream (bytes) :> Stream


                }

            let getRequest (body : string) = 
                getRequestTo "http://localhost:8080/api/people" "/api" body

            let request = 
                getRequest "Hello, World"

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``parseUrls function`` =

            let [<Fact>] ``Raw URL is copied across`` () =
            
                let urls = 
                    parseUrls request

                urls.Raw |> should equal request.Url

            let [<Fact>] ``Path is set correctly`` () =

                let urls = 
                    parseUrls request

                urls.Path |> should equal "/people"

            let [<Fact>] ``Forward slash is set for root path`` () =

                let urls = 
                    getRequestTo "http://localhost:8080/api" "/api" "Hello, World"
                    |> parseUrls

                urls.Path |> should equal "/"

            let [<Fact>] ``Query is set correctly`` () =

                let urls = 
                    parseUrls request

                urls.Query |> List.same [ ("page", "5"); ] |> should be True

            let [<Fact>] ``BaseUrl is set correctly`` () =
                
                let urls = 
                    parseUrls request

                urls.BaseUrl |> should equal (Uri ("http://localhost:8080/api", UriKind.Absolute))

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``parsePayload function`` = 

            let [<Fact>] ``Headers are set correctly`` () =

                let payload = 
                    parsePayload request

                payload.Заголовки |> List.same [ ("Content-Type", MediaTypes.Text.Xml); ("Accept", MediaTypes.Application.Json); ] |> should be True

            let [<Fact>] ``Body input stream is copied when not empty`` () =

                let payload = 
                    parsePayload request

                use reader = 
                    new StreamReader (Option.get payload.Тело)

                let body = 
                    reader.ReadToEnd ()

                body |> should equal "Hello, World"

            let [<Fact>] ``Body input stream is not copied when empty`` () =

                let payload = 
                    getRequest String.Empty
                    |> parsePayload

                payload.Тело |> should be None'<Stream>                    

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``parseRequest function`` = 

            let [<Fact>] ``Verb is set correctly`` () =

                let request' = 
                    разобратьЗапрос request Guid.Empty

                request'.Verb |> should equal "POST"

            let [<Fact>] ``Request ID is set correctly`` () =

                let id = Guid.NewGuid ()
                let request' = разобратьЗапрос request id

                request'.Id |> should equal id

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``createAbsoluteUri function`` = 

            [<Theory>]
            [<InlineData ("http://localhost", "api", "http://localhost/api")>]
            [<InlineData ("http://localhost", "/api", "http://localhost/api")>]
            [<InlineData ("http://localhost/", "api", "http://localhost/api")>]
            [<InlineData ("http://localhost/", "/api", "http://localhost/api")>]
            [<InlineData ("http://localhost/app", "api", "http://localhost/app/api")>]
            [<InlineData ("http://localhost/app", "/api", "http://localhost/app/api")>]
            [<InlineData ("http://localhost/app/", "api", "http://localhost/app/api")>]
            [<InlineData ("http://localhost/app/", "/api", "http://localhost/app/api")>]
            let ``Returns correct URL`` baseUrl relativeUrl expectedUrl = 

                let baseUrl' = Uri (baseUrl, UriKind.Absolute)
                let actualUrl = создатьАбсолютныйУри baseUrl' relativeUrl

                actualUrl.AbsoluteUri |> should equal expectedUrl

        module ``Headers facts`` =

            let [<Literal>] ModuleName = "Common.Http.Headers"

            let headers =
                [
                    ("Content-Type", MediaTypes.Text.Xml);
                    ("Authorization", String.Empty);
                ]

            [<Trait (Traits.Names.Module, ModuleName)>]
            module ``getValue function`` = 

                let [<Fact>] ``Correct value is returned when header is present`` () =
                    headers
                    |> Заголовки.getValue "Content-Type"
                    |> should be (Some' MediaTypes.Text.Xml)

                let [<Fact>] ``None is returned when header is not present`` () = 
                    headers
                    |> Заголовки.getValue "Accept"
                    |> should be None'<String>

            [<Trait (Traits.Names.Module, ModuleName)>]
            module ``getNonEmptyValue function`` =

                let [<Fact>] ``Correct value is returned when the header is present and non-empty`` () =
                    headers
                    |> Заголовки.получитьНепустоеЗначение "Content-Type"
                    |> should be (Some' MediaTypes.Text.Xml)

                let [<Fact>] ``None is returned when the header is present and empty`` () =
                    headers
                    |> Заголовки.получитьНепустоеЗначение "Authorization"
                    |> should be None'<String>

                let [<Fact>] ``None is returned when the header is not present`` () =
                    headers
                    |> Заголовки.получитьНепустоеЗначение "Accept"
                    |> should be None'<String>

        module ``HttpResponseOutput facts`` = 

            open System.Collections.Specialized
            open System.Web

            [<AutoOpen>]
            module Helpers =

                type FakeResponse () = 
                    inherit HttpResponseBase ()

                    let stream = new MemoryStream () :> Stream
                    let headers = NameValueCollection ()

                    override this.OutputStream = stream
                    override this.Headers = headers
                    override val ContentType = String.Empty with get, set

                let getOutput () =
                    
                    let raw = FakeResponse ()
                    let wrapped = raw.AsOutput ()

                    (raw, wrapped)

                let getOutputLength (raw : FakeResponse) =
                    raw.OutputStream.Length

                let outputContains message (raw : FakeResponse) = 

                    raw.OutputStream.Position <- 0L

                    use reader = new StreamReader (raw.OutputStream)

                    reader.ReadToEnd () = message

            [<Trait (Traits.Names.Module, ModuleName)>]
            module ``ЗаписатьТело function`` = 

                let [<Fact>] ``Body is written correctly to stream`` () =
                    
                    let raw, wrapped = getOutput ()
                    let message = "Hello, World"
                    let bytes = message |> Encoding.UTF8.GetBytes |> Array.toList

                    wrapped.ЗаписатьТело (bytes)
                    |> Async.RunSynchronously

                    raw |> outputContains message |> should be True                    

            [<Trait (Traits.Names.Module, ModuleName)>]
            module ``ЗаписатьЗаголовок function`` = 

                let ``Normal headers are set correctly`` ()  =
                    
                    let raw, wrapped = getOutput ()

                    wrapped.ЗаписатьЗаголовок "Custom" "Value"
                    |> Async.RunSynchronously

                    raw.Headers.AllKeys |> Array.exists ((=) "Custom") |> should be True
                    raw.Headers.["Custom"] |> should equal "Value"

                let ``Content-Type header is set correctly`` () = 
                    
                    let raw, wrapped = getOutput ()

                    wrapped.ЗаписатьЗаголовок Заголовки.ContentType "text/xml"
                    |> Async.RunSynchronously

                    raw.Headers.AllKeys.Length |> should equal 0
                    raw.ContentType |> should equal "text/xml"