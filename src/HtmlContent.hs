{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HtmlContent where

import Servant ( QueryParam
               , PlainText
               , Get
               , Proxy(..)
               , type (:>)      -- Syntax for importing type operator
               , type (:<|>)
               , (:<|>)(..)
               , Accept(..)
               , MimeRender(..)
               )
import Servant.Server (Handler, Server, Application, serve)
import Network.Wai.Handler.Warp (run)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy.Char8 as C


data HTML

instance Accept HTML where
    contentType _ = "text/html"
instance MimeRender HTML String where
    mimeRender _ val = C.pack val
instance MimeRender HTML Int where
    mimeRender _ val = C.pack $ show val

type HTMLApi = "name" :> Get '[HTML] String
                :<|> "age" :> Get '[HTML] Int

htmlApi :: Proxy HTMLApi
htmlApi = Proxy

handlerName :: Handler String
handlerName = return "abin"

handlerAge :: Handler Int
handlerAge = return 20

server :: Server HTMLApi
server = handlerName :<|> handlerAge

app :: Application
app = serve htmlApi server

mainFn :: IO ()
mainFn = run 4002 app
