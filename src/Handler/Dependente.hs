{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Dependente where

import Import
import Text.Lucius
import Text.Julius
import Database.Persist.Sql

formDependente :: FormInput Handler Dependente
formDependente = pure Dependente
    <*> ireq textField "nome"
    <*> ireq textField "rg"
    <*> ireq dayField "dtNasc"
    <*> ireq (selectField (optionsPersistKey [] [] funcionarioNome)) "funcionario"

getDependentesR :: Handler Html   
getDependentesR = do 
    dependentes <- runDB $ selectList [] [Asc DependenteId]
    funcionarios <- runDB $ selectList [] [Asc FuncionarioId]
    dependenteFuncionario <- mapM (\f -> runDB $ get404 $ dependenteCdFunc $ entityVal $ f) dependentes
    let (dpfs) = zip dependentes dependenteFuncionario
    defaultLayout $ do 
        $(whamletFile "templates/dependentes.hamlet")
        
        toWidget[julius|
            [].slice.call(document.getElementsByClassName("modal-trigger")).forEach(function(x){
                x.addEventListener("click",function(){
                    document.forms["frmEdicao"].setAttribute("action", this.dataset.edicao);
                    document.forms["frmEdicao"].elements["nome"].setAttribute("value", this.dataset.depenome);
                    document.forms["frmEdicao"].elements["rg"].setAttribute("value", this.dataset.deperg);
                    document.forms["frmEdicao"].elements["dtNasc"].setAttribute("value", this.dataset.depedtnasc);
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
        
postSalvarDependenteR :: Handler Html
postSalvarDependenteR = do 
    res <- runInputPostResult formDependente
    case res of 
        FormSuccess dependente -> do 
            depeid <- runDB $ insert dependente
            redirect DependentesR
        FormFailure erro -> do
            setMessage [shamlet|
                        <h1>
                            #{show erro}
                    |]
            redirect DependentesR

postApagarDependenteR :: DependenteId -> Handler Html
postApagarDependenteR depeid = do 
    runDB $ delete depeid
    redirect DependentesR

postEditarDependenteR :: DependenteId -> Handler Html
postEditarDependenteR depeid = do
    res <- runInputPostResult formDependente
    case res of 
        FormSuccess dependente -> do 
            depeid <- runDB $ replace depeid dependente
            redirect DependentesR
        FormFailure erro -> do
            setMessage [shamlet|
                        <h1>
                            #{show erro}
                    |]
            redirect DependentesR