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

formDepartamento :: FormInput Handler Departamento
formDepartamento = pure Departamento
    <*> ireq textField "nome"
    <*> ireq textField "local"

getDepartamentosR :: Handler Html   
getDepartamentosR = do 
    departamentos <- runDB $ selectList [] [Asc DepartamentoId]
    defaultLayout $ do 
        $(whamletFile "templates/departamentos.hamlet")
        toWidget[julius|
            [].slice.call(document.getElementsByClassName("modal-trigger")).forEach(function(x){
                x.addEventListener("click",function(){
                    document.forms["frmEdicao"].setAttribute("action", this.dataset.edicao);
                    document.forms["frmEdicao"].elements["nome"].setAttribute("value", this.dataset.deptonome);
                    document.forms["frmEdicao"].elements["local"].setAttribute("value", this.dataset.deptolocal);
                });
            });
        |]
        toWidget[julius|
            document.addEventListener('DOMContentLoaded', function() {
                var elems = document.querySelectorAll('.modal');
                var instances = M.Modal.init(elems,"");
            });
        |]
        
        
postSalvarDepartamentoR :: Handler Html
postSalvarDepartamentoR = do 
    res <- runInputPostResult formDepartamento
    case res of 
        FormSuccess departamento -> do 
            deptoid <- runDB $ insert departamento
            redirect DepartamentosR
        _ -> redirect MenuR

postApagarDepartamentoR :: DepartamentoId -> Handler Html
postApagarDepartamentoR deptoid = do 
    runDB $ delete deptoid
    redirect DepartamentosR

postEditarDepartamentoR :: DepartamentoId -> Handler Html
postEditarDepartamentoR deptoid = do
    res <- runInputPostResult formDepartamento
    case res of 
        FormSuccess departamento -> do 
            deptoid <- runDB $ replace deptoid departamento
            redirect DepartamentosR
        _ -> redirect MenuR
