{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Menu where

import Import
import Text.Lucius
import Text.Julius
import Database.Persist.Sql

getMenuR :: Handler Html
getMenuR = do
    defaultLayout $ do 
        $(whamletFile "templates/menu.hamlet")