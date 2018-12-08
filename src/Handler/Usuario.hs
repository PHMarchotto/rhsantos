{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Usuario where

import Import
import Text.Lucius
import Text.Julius
import Database.Persist.Sql

formUsuario :: FormInput Handler Usuario
formUsuario = pure Usuario
    <*> ireq textField "nome"
    <*> ireq emailField "email"
    <*> ireq passwordField "senha"
    
    
getUsuariosR :: Handler Html   
getUsuariosR = do 
    usuarios <- runDB $ selectList [] [Asc UsuarioId]
    defaultLayout $ do 
        $(whamletFile "templates/usuario.hamlet")


postSalvarUsuarioR :: Handler Html
postSalvarUsuarioR = do 
    res <- runInputPostResult formUsuario
    case res of 
        FormSuccess usuario -> do 
            usuid <- runDB $ insert usuario
            redirect UsuariosR
        FormFailure erro -> do 
            setMessage [shamlet|
                        <h1>
                            #{show erro}
                    |]
            redirect UsuariosR