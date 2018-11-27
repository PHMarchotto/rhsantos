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
postDepartamentoR :: Handler TypedContent
postDepartamentoR = do 
    dep <- requireJsonBody :: Handler Departamento
    depid <- runDB $ insert dep
    sendStatusJSON created201 (object ["resp" .= depid])