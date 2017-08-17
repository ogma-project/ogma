{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Web.Ogma.Api where

import           Servant.API

import           Web.Ogma.Data


type PaginatedGet x y = QueryParam "offset" :> QueryParam "count" :> Get x [y]

type OgmaAPI =
       "events" :> PaginatedGet '[JSON] Event
  :<|> "events" :> "where" :> QueryParam "where" Surface :> PaginatedGet '[JSON] Event
  :<|> "events" :> "when" :> QueryParam "when" TimeInterval :> PaginatedGet '[JSON] Event
  :<|> "events" :> "where" :> QueryParam "where" Surface :> "when" :> QueryParam "when" TimeInterval :> PaginatedGet '[JSON] Event
