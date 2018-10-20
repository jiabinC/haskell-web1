{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module UrlParams where

import Servant ( QueryParam
    , Capture
    , PlainText
    , Get
    , Proxy(..)
    , type (:>)      -- Syntax for importing type operator
    , type (:<|>)
    , (:<|>)(..)
    )
import Network.Wai.Handler.Warps
import Servant.Server(Handler,Server,Application,serve)
import Control.Monad.IO.Class(liftIO)


handlerName :: Maybe String -> Handler String
handlerName name = case name of 
    Just name -> return name 
    Nothing -> return "without name"

handlerRequiredName :: String -> Handler String 
handlerRequiredName name = return name

type MyAPI = "name" :> QueryParam "input" String :> Get '[PlainText] String 
           :<|> "name" :> Capture "input" String :> Get '[PlainText] String 


myAPI :: Proxy MyAPI
myAPI = Proxy 

server :: Server MyAPI
server = handlerName :<|> handlerRequiredName

app :: Application
app = serve myAPI server

mainFn = run 4000 app
