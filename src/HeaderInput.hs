{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module HeaderInput where
import Servant ( Header
               , PlainText
               , Get
               , Proxy(..)
               , type (:>)      -- Syntax for importing type operator
               , type (:<|>)
               , (:<|>)(..)
               )
import Servant.Server (Handler, Server, Application, serve)
import Network.Wai.Handler.Warp (run)
import Control.Monad.IO.Class (liftIO)

type HeadApi = "name" :> Header "header" String :> Get '[PlainText] String

headApi :: Proxy HeadApi
headApi = Proxy

handlerName :: Maybe String -> Handler String
handlerName head = case head of
    Just head -> return head
    Nothing -> return "no header"

server :: Server HeadApi
server = handlerName

app :: Application
app = serve headApi server

mainFn3 :: IO ()
mainFn3 = run 4003 app
