{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Destaque where

import Import
import Text.Lucius
import Text.Julius
import Database.Persist.Sql

getDestaqueR :: Handler Html
getDestaqueR = 
    defaultLayout $ do 
        addStylesheet $ StaticR css_estilo_css
        $(whamletFile "templates/destaque.hamlet")