{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.PaginaInicial where

import Import
import Text.Lucius
import Text.Julius
import Database.Persist.Sql

getPaginaInicialR :: Handler Html
getPaginaInicialR = do 
    dependente <- runDB $ selectList [] [Asc DependenteNome]
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "/templates/index.hamlet")


/*        addStylesheet $ StaticR css_bootstrap_css
 para chamar o