{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Loggia.APIType where

import           Data.Proxy
import           Data.Text
import           Loggia.Types
import           Servant.API

type LoggiaAPI
  =    "album" :> Capture "album-id" Text :> Get '[JSON] [SPage]
  :<|> "app"   :> "img"   :> Raw
  :<|> "app"   :> Raw

loggiaAPI :: Proxy LoggiaAPI
loggiaAPI = Proxy


