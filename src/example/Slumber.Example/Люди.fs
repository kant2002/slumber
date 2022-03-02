namespace Дрема.Пример

open System
open System.Runtime.Serialization
open System.IO
open System.Collections.Generic
open Дрема
open Дрема.Общее.Операции.Метаданные

module People = 

    let private getUrl baseUrl (relativeUrl : String) = 
        Uri (baseUrl, relativeUrl)
        |> string

    [<AutoOpen>]
    module Модель = 

        [<DataContract (Name = "person-summary", Namespace = "")>]
        type PersonSummary = {
            [<field: DataMember (Name = "id")>] Id : Int32;
            [<field: DataMember (Name = "full-name")>] FullName : String;
            [<field: DataMember (Name = "age")>] Возраст : Int32;
            [<field: DataMember (Name = "url")>] Url : String;
            [<field: DataMember (Name = "created-by")>] CreatedBy : String
        }
        with

            static member BasedOn baseUrl (person : Repository.Человек) = 
                {
                    Id = person.Id;
                    FullName = person.FullName;
                    Возраст = person.Возраст;
                    Url = (getUrl baseUrl (sprintf "/people/%d" person.Id));
                    CreatedBy = person.CreatedBy;
                }

        [<DataContract (Name = "person-catalog", Namespace = "")>]
        type КаталогЛюдей = {
            [<field: DataMember (Name = "self")>] Self : String;
            [<field: DataMember (Name = "people")>] Люди : PersonSummary seq;
        }

        [<DataContract (Name = "person", Namespace = "")>]
        type PersonMessage = {
            [<field: DataMember (Name = "full-name", Order = 0)>] FullName : String;
            [<field: DataMember (Name = "age", Order = 1, IsRequired = false)>] Возраст : Int32;
        }

        [<DataContract (Name = "person-created", Namespace = "")>]
        type PersonCreated = {
            [<field: DataMember (Name = "id")>] Id : Int32;
            [<field: DataMember (Name = "url")>] Url : String;
        }

    let getPeople (репозиторий : Repository.IRepository) =
        fun (search : String option) (meta : МетаданныеОперации) ->

            let people = 
                репозиторий.All ()
                |> Seq.map (PersonSummary.BasedOn meta.ContainerUrl)

            {
                Self = (getUrl meta.ContainerUrl "people");
                Люди = people;
            }

    let getPerson (репозиторий : Repository.IRepository) = 
        fun (id : Int32) (мета : МетаданныеОперации) ->
            try 
                match (репозиторий.Find id) with
                | Some person -> РезультатОперации.ТолькоРесурс (PersonSummary.BasedOn мета.ContainerUrl person)
                | _ -> РезультатОперации.ТолькоСтатус 404        
            with
            | :? FormatException -> РезультатОперации.ТолькоСтатус 400

    let addPerson (репозиторий : Repository.IRepository) = 
        fun (сообщение : PersonMessage) (мета : МетаданныеОперации) ->
        
            let имяПользователя = 
                match мета.Пользователь with
                | Some пользователь -> пользователь.Id
                | _ -> String.Empty

            let person = 
                {
                    Repository.Человек.Empty
                    with
                        FullName = сообщение.FullName;
                        Возраст = сообщение.Возраст;
                        CreatedBy = имяПользователя;
                }
                |> репозиторий.Save

            let url =  getUrl мета.ContainerUrl (sprintf "/people/%d" person.Id)

            РезультатОперации.ТолькоРесурс { Id = person.Id; Url = url; }

    let deletePerson (репозиторий : Repository.IRepository) =
        fun id ->
            репозиторий.Delete id

    let updatePerson (репозиторий : Repository.IRepository) = 
        fun id (update : PersonMessage) ->
            match (репозиторий.Find id) with
            | Some person ->

                {
                    person
                    with
                        FullName = update.FullName;
                        Возраст = update.Возраст
                }
                |> репозиторий.Save
                |> ignore

                РезультатОперации.ТолькоСтатус 200

            | _ -> РезультатОперации.ТолькоСтатус 404