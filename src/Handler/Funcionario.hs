{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Funcionario where

import Import
import Text.Lucius
import Text.Julius
import Database.Persist.Sql

formFuncionario :: FormInput Handler Funcionario
formFuncionario = pure Funcionario
    <*> ireq textField "nome"
    <*> ireq textField "cpf"
    <*> ireq dayField "dtNasc"
    <*> ireq textField "cargo"
    <*> ireq doubleField "salario"
    <*> ireq (selectField (optionsPersistKey [] [] departamentoNome)) "departamento"

getFuncionariosR :: Handler Html   
getFuncionariosR = do 
    funcionarios <- runDB $ selectList [] [Asc FuncionarioId]
    departamentos <- runDB $ selectList [] [Asc DepartamentoId]
    funcionarioDepartamento <- mapM (\f -> runDB $ get404 $ funcionarioCdDepto $ entityVal $ f) funcionarios
    let (fsds) = zip funcionarios funcionarioDepartamento
    defaultLayout $ do 
        $(whamletFile "templates/funcionarios.hamlet")
        toWidget[julius|
            [].slice.call(document.getElementsByClassName("modal-trigger")).forEach(function(x){
                x.addEventListener("click",function(){
                    document.forms["frmEdicao"].setAttribute("action", this.dataset.edicao);
                    document.forms["frmEdicao"].elements["nome"].setAttribute("value", this.dataset.funcnome);
                    document.forms["frmEdicao"].elements["cpf"].setAttribute("value", this.dataset.funccpf);
                    document.forms["frmEdicao"].elements["dtNasc"].setAttribute("value", this.dataset.funcdtnasc);
                    document.forms["frmEdicao"].elements["cargo"].setAttribute("value", this.dataset.funccargo);
                    document.forms["frmEdicao"].elements["salario"].setAttribute("value", this.dataset.funcsalario);
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
        
postSalvarFuncionarioR :: Handler Html
postSalvarFuncionarioR = do 
    res <- runInputPostResult formFuncionario
    case res of 
        FormSuccess funcionario -> do 
            funcid <- runDB $ insert funcionario
            redirect FuncionariosR
        FormFailure x -> do
            setMessage [shamlet|
                        <h1>
                            #{show x} Erro!
                    |]
            redirect FuncionariosR

postApagarFuncionarioR :: FuncionarioId -> Handler Html
postApagarFuncionarioR funcid = do 
    runDB $ delete funcid
    redirect FuncionariosR

postEditarFuncionarioR :: FuncionarioId -> Handler Html
postEditarFuncionarioR funcid = do
    res <- runInputPostResult formFuncionario
    case res of 
        FormSuccess funcionario -> do 
            funcid <- runDB $ replace funcid funcionario
            redirect FuncionariosR
        _ -> redirect MenuR