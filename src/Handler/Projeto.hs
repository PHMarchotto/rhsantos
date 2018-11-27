{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Projeto where

import Import

postProjetoR :: Handler TypedContent
postProjetoR = do 
    proj <- requireJsonBody :: Handler Projeto
    projid <- runDB $ insert proj
    sendStatusJSON created201 (object ["resp" .= projid])

getListaProjR :: Handler TypedContent
getListaprojR  = do 
    projs <- runDB $ selectList [] [Asc FuncioNome]
    sendStatusJSON ok200 (object ["resp" .= projs])
    
atualizaProjR