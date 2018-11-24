{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Aluno where

{-import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

postAlunoR :: Handler TypedContent
postAlunoR = do
    aluno <- requireJsonBody :: Handler Aluno
    alunoid <- runDB $ insert aluno
    sendStatusJSON created201 (object ["resp" .= alunoid])
    
getListAlunoR :: Handler TypedContent
getListAlunoR = do
    alunos <- runDB $ selectList [] [Asc AlunoNome]
    sendStatusJSON ok200 (object ["resp" .= alunos])
    
getPerfilR :: AlunoId -> Handler TypedContent
getPerfilR alunoid = do
    aluno <- runDB $ get404 alunoid
    sendStatusJSON ok200 (object ["resp" .= aluno])

deleteApagarR :: AlunoId -> Handler TypedContent
deleteApagarR alunoid = do 
    _ <- runDB $ get404 alunoid
    runDB $ delete alunoid
    sendStatusJSON noContent204 (object [])
    
putAlterarR :: AlunoId -> Handler TypedContent
putAlterarR alunoid = do 
    _ <- runDB $ get404 alunoid
    novoAluno <- requireJsonBody :: Handler Aluno
    runDB $ replace alunoid novoAluno
    sendStatusJSON noContent204 (object [])
    
patchAltNomeR :: AlunoId -> Text -> Handler TypedContent
patchAltNomeR alunoid nome = do 
    _ <- runDB $ get404 alunoid
    runDB $ update alunoid [AlunoNome =. nome]
    sendStatusJSON noContent204 (object [])-}