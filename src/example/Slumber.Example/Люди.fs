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
    module Model = 

        [<DataContract (Name = "person-summary", Namespace = "")>]
        type PersonSummary = {
            [<field: DataMember (Name = "id")>] Id : Int32;
            [<field: DataMember (Name = "full-name")>] FullName : String;
            [<field: DataMember (Name = "age")>] Age : Int32;
            [<field: DataMember (Name = "url")>] Url : String;
            [<field: DataMember (Name = "created-by")>] CreatedBy : String
        }
        with

            static member BasedOn baseUrl (person : Repository.Person) = 
                {
                    Id = person.Id;
                    FullName = person.FullName;
                    Age = person.Age;
                    Url = (getUrl baseUrl (sprintf "/people/%d" person.Id));
                    CreatedBy = person.CreatedBy;
                }

        [<DataContract (Name = "person-catalog", Namespace = "")>]
        type PersonCatalog = {
            [<field: DataMember (Name = "self")>] Self : String;
            [<field: DataMember (Name = "people")>] People : PersonSummary seq;
        }

        [<DataContract (Name = "person", Namespace = "")>]
        type PersonMessage = {
            [<field: DataMember (Name = "full-name", Order = 0)>] FullName : String;
            [<field: DataMember (Name = "age", Order = 1, IsRequired = false)>] Age : Int32;
        }

        [<DataContract (Name = "person-created", Namespace = "")>]
        type PersonCreated = {
            [<field: DataMember (Name = "id")>] Id : Int32;
            [<field: DataMember (Name = "url")>] Url : String;
        }

    let getPeople (repository : Repository.IRepository) =
        fun (search : String option) (meta : МетаданныеОперации) ->

            let people = 
                repository.All ()
                |> Seq.map (PersonSummary.BasedOn meta.ContainerUrl)

            {
                Self = (getUrl meta.ContainerUrl "people");
                People = people;
            }

    let getPerson (repository : Repository.IRepository) = 
        fun (id : Int32) (meta : МетаданныеОперации) ->
            try 
                match (repository.Find id) with
                | Some person -> РезультатОперации.ТолькоРесурс (PersonSummary.BasedOn meta.ContainerUrl person)
                | _ -> РезультатОперации.ТолькоСтатус 404        
            with
            | :? FormatException -> РезультатОперации.ТолькоСтатус 400

    let addPerson (repository : Repository.IRepository) = 
        fun (message : PersonMessage) (meta : МетаданныеОперации) ->
        
            let userName = 
                match meta.Пользователь with
                | Some user -> user.Id
                | _ -> String.Empty

            let person = 
                {
                    Repository.Person.Empty
                    with
                        FullName = message.FullName;
                        Age = message.Age;
                        CreatedBy = userName;
                }
                |> repository.Save

            let url =  getUrl meta.ContainerUrl (sprintf "/people/%d" person.Id)

            РезультатОперации.ТолькоРесурс { Id = person.Id; Url = url; }

    let deletePerson (repository : Repository.IRepository) =
        fun id ->
            repository.Delete id

    let updatePerson (repository : Repository.IRepository) = 
        fun id (update : PersonMessage) ->
            match (repository.Find id) with
            | Some person ->

                {
                    person
                    with
                        FullName = update.FullName;
                        Age = update.Age
                }
                |> repository.Save
                |> ignore

                РезультатОперации.ТолькоСтатус 200

            | _ -> РезультатОперации.ТолькоСтатус 404