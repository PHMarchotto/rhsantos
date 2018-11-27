{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Funcionario where

import Import

postFuncionarioR :: Handler TypedContent
postFuncionarioR = do 
    func <- requireJsonBody :: Handler Funcionario
    funcid <- runDB $ insert func
    sendStatusJSON created201 (object ["resp" .= funcid])

getListaFuncR :: Handler TypedContent
getListaFuncR  = do 
    funcs <- runDB $ selectList [] [Asc FuncionarioNome]
    sendStatusJSON ok200 (object ["resp" .= funcs])