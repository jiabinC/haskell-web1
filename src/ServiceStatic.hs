{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module ServiceStatic where

import Servant ( QueryParam
    , PlainText
    , BasicAuth
    , BasicAuthCheck(..)
    , BasicAuthData(..)
    , BasicAuthResult(..)
    , MimeRender(..)
    , Get
    , Context((:.), EmptyContext)
    , Proxy(..)
    , type (:>)      -- Syntax for importing type operator
    , type (:<|>)
    , (:<|>)(..)
    )
import Servant.Server 
import Network.Wai.Handler.Warp 
import Control.Monad.IO.Class (liftIO)

type MyAPI = "static" :> Raw

myAPI :: Proxy MyAPI
myapi = Proxy

server :: Server MyAPI
server = serveDirectoryWebApp "/home/abin/Desktop/"

app :: Application 
app = serve myapi server

mainFn :: IO ()
mainFn = run 4100 app