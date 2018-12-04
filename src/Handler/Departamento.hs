{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Departamento where

import Import
import Text.Lucius
import Text.Julius
import Database.Persist.Sql

formDepartamento :: FormInput Handler(Int,Text,Text)
formDepartamento = pure (,)
    <*> ireq intField "id"
    <*> ireq textField "nome"
    <*> ireq textField "local"

getDepartamentosR :: Handler Html   
getDepartamentosR = do 
    departamentos <- runDB $ selectList [] [Asc DepartamentoId]
    defaultLayout $ do 
        $(whamletFile "templates/departamentos.hamlet")
        
postSalvarDepartamentoR :: Handler Html
postSalvarDepartamentoR = do 
    ((res,_),_) <- runFormPost formDepartamento
    case res of 
        FormSuccess departamento -> do 
            deptoid <- runDB $ insert departamento
            redirect DepartamentosR
        _ -> redirect MenuR

postApagarDepartamentoR :: DepartamentoId -> Handler Html
postApagarDepartamentoR deptoid = do 
    runDB $ delete deptoid
    redirect DepartamentosR