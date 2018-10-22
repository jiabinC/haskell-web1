{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module JsonOutput where

import Servant ( QueryParam
    , PlainText
    , Get
    , JSON
    , Proxy(..)
    , type (:>)      -- Syntax for importing type operator
    , type (:<|>)
    , (:<|>)(..)
    )

import Servant.Server (Handler, Server, Application, serve)
import Network.Wai.Handler.Warp (run)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, ToJSON(..), (.=))

data Payload = Payload String String

instance ToJSON Payload where 
  toJSON (Payload itemOne itemTwo) = object ["itemOne" .= toJSON itemOne, "itemTwo" .= toJSON itemTwo]



handlerPayload :: Handler Payload
handlerPayload = return $ Payload "itemOne" "itemTwo"

type JSONType = GET '[JSON] Payload
server :: Server JSONType
server = handlerPayload

jsonType :: Proxy JSONType
jsonType = proxy

app :: Application
app = serve jsonType server

mainFn1 :: IO ()
mainFn1 = run 4001 app
