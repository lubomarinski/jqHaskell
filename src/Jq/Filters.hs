module Jq.Filters where

data Filter = Identity

instance Show Filter where
  show (Identity) = "."

instance Eq Filter where
  Identity == Identity = True
  _ == _ = undefined

data Config = ConfigC {filters :: Filter}


filterIdentitySC :: Filter
filterIdentitySC = Identity

filterStringIndexingSC :: String -> Filter
filterStringIndexingSC = undefined

filterPipeSC :: Filter -> Filter -> Filter
filterPipeSC = undefined

filterCommaSC :: Filter -> Filter -> Filter
filterCommaSC = undefined
