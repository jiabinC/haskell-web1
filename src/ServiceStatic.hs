{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module ServiceStatic where

import Servant ( QueryParam
    , PlainText
    , BasicAuth
    , serveDirectoryWebApp
    , BasicAuthCheck(..)
    , BasicAuthData(..)
    , BasicAuthResult(..)
    , MimeRender(..)
    , Get
    , Raw
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
myAPI = Proxy

server :: Server MyAPI
server = serveDirectoryWebApp "/home/abin/Desktop/"

app :: Application 
app = serve myAPI server

mainFn :: IO ()
mainFn = run 4100 app