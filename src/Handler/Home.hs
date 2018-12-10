{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Home where

import Import
import Text.Lucius
import Text.Julius
import Database.Persist.Sql
import Settings.StaticFiles

formLogin :: FormInput Handler (Text, Text)
formLogin = pure (,)
    <*> ireq emailField "login" 
    <*> ireq passwordField "senha" 
    
getHomeR :: Handler Html
getHomeR = do
    msg <- getMessage
    sess <- lookupSession "_USR"
    defaultLayout $ do 
        $(whamletFile "templates/home.hamlet")
    
postLoginR :: Handler Html
postLoginR = do
    (login, senha) <- runInputPost formLogin
    usr <- runDB $ selectFirst [UsuarioEmail ==. login, UsuarioSenha ==. senha] []
    case usr of 
        Just (Entity usrid usuario) -> do 
            setSession "_USR" (pack (show usuario))
            redirect MenuR
        Nothing -> do 
            setMessage [shamlet|
                <h1>
                    Usuario não encontrado
            |]
            redirect HomeR     
            
            
