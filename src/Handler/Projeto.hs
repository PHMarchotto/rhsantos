{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Projeto where

import Import
import Text.Lucius
import Text.Julius
import Database.Persist.Sql

formProjeto :: FormInput Handler Projeto
formProjeto = pure Projeto
    <*> ireq textField "descricao"
    <*> ireq dayField "dtInicio"
    <*> ireq dayField "dtFim"
    <*> ireq (selectField (optionsPersistKey [] [] departamentoNome)) "departamento"

getProjetosR :: Handler Html
getProjetosR = do 
    projetos <- runDB $ selectList [] [Asc ProjetoId]
    departamentos <- runDB $ selectList [] [Asc DepartamentoId]
    projetoDepartamento <- mapM (\f -> runDB $ get404 $ projetoCdDepto $ entityVal $ f) projetos
    let (psdp) = zip projetos projetoDepartamento
    defaultLayout $ do 
        $(whamletFile "templates/projetos.hamlet")
        
        toWidget[julius|
            [].slice.call(document.getElementsByClassName("modal-trigger")).forEach(function(x){
                x.addEventListener("click",function(){
                    document.forms["frmEdicao"].setAttribute("action", this.dataset.edicao);
                    document.forms["frmEdicao"].elements["descricao"].setAttribute("value", this.dataset.projdescricao);
                    document.forms["frmEdicao"].elements["dtInicio"].setAttribute("value", this.dataset.projdtinicio);
                    document.forms["frmEdicao"].elements["dtFim"].setAttribute("value", this.dataset.projdtfim);
                });
            });
        |]
        
        toWidget[julius|
            document.addEventListener('DOMContentLoaded', function() {
                var elems = document.querySelectorAll('.modal');
                var instances = M.Modal.init(elems,"");
            });
        |]
        
        toWidget[julius|
            document.addEventListener('DOMContentLoaded', function() {
                var elems = document.querySelectorAll('select');
                var instances = M.FormSelect.init(elems,"");
            });
        |]
        
postSalvarProjetoR :: Handler Html
postSalvarProjetoR = do 
    res <- runInputPostResult formProjeto
    case res of 
        FormSuccess projeto -> do 
            projid <- runDB $ insert projeto
            redirect ProjetosR
        FormFailure erro -> do
            setMessage [shamlet|
                        <h1>
                            #{show erro}
                    |]
            redirect ProjetosR

postApagarProjetoR :: ProjetoId -> Handler Html
postApagarProjetoR projid = do 
    runDB $ delete projid
    redirect ProjetosR

postEditarProjetoR :: ProjetoId -> Handler Html
postEditarProjetoR projid = do
    res <- runInputPostResult formProjeto
    case res of 
        FormSuccess projeto -> do 
            projid <- runDB $ replace projid projeto
            redirect ProjetosR
        FormFailure erro -> do
            setMessage [shamlet|
                        <h1>
                            #{show erro}
                    |]
            redirect ProjetosR