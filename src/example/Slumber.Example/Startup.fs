namespace Дрема.Пример

open System
open System.Text.RegularExpressions
open System.Runtime.Serialization

module Startup = 

    open Дрема    
    open Дрема.Framework
    open Дрема.Общее.Http

    [<AutoOpen>]
    module Модель = 

        [<DataContract (Name = "service", Namespace = "")>] 
        type Сервис = {
            [<field: DataMember (Name = "name")>] Название : String;
            [<field: DataMember (Name = "url")>] Url : String;
        }

        [<DataContract (Name = "service-catalog", Namespace = "")>]
        type КаталогСервисов = {
            [<field: DataMember (Name = "self")>] Self : String;
            [<field: DataMember (Name = "services")>] Сервисы : Сервис seq;
        }
    
    let получитьКаталог (мета : МетаданныеОперации) = 

        let базовыйУрл = мета.ContainerUrl
        let контейнер = НеявнаяКонфигурация.получить базовыйУрл

        let сервисы = 
            контейнер.ОконечныеТочки
            |> List.filter (fun оконечнаяТочка -> not (Regex.IsMatch (оконечнаяТочка.Шаблон, "{.+?}"))) //Display only top level, non-parameterised endpoints
            |> List.map (fun оконечнаяТочка ->

                    let урл = 
                        match оконечнаяТочка.Шаблон with
                        | "/" -> string базовыйУрл
                        | _ -> string (создатьАбсолютныйУри базовыйУрл оконечнаяТочка.Шаблон)

                    {
                        Название = оконечнаяТочка.Название;
                        Url = урл;
                    }
                )

        {
            Self = базовыйУрл.AbsoluteUri;
            Сервисы = сервисы;
        }

