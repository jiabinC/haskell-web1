{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module BasicAuth where

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
import Servant.Server (Handler, Server, Application, serveWithContext)
import Network.Wai.Handler.Warp (run)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy.Char8 as C

-- instance MimeRender PlainText Int where
--     mimeRender _ val = C.pack $ show val

data User = User

handlerName :: User -> Handler String
handlerName user = return "abin"

handlerAge :: Handler String
handlerAge = return "20"



type AuthApi = BasicAuth "example auth" User :> "person" :>"name" :> Get '[PlainText] String
             :<|> "person" :> "age" :> Get '[PlainText] String

server :: Server AuthApi
server = handlerName :<|> handlerAge

authApi :: Proxy AuthApi
authApi = Proxy

myAuthCheck :: BasicAuthData -> IO (BasicAuthResult User)
myAuthCheck (BasicAuthData u p) = return $ if u == "abin" && p == "123" then Authorized User else BadPassword

app :: Application
app = serveWithContext (Proxy :: Proxy AuthApi) ctx server
  where
    ctx = (BasicAuthCheck myAuthCheck) :. EmptyContext

mainFn4 :: IO ()
mainFn4 = run 4007 app