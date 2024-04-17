module Jq.Filters where

import Jq.Json

data Filter = 
  Identity |
  Parenthesis Filter |
  GenIndex Filter Filter Bool |
  ArrayRange Filter Filter Filter Bool |
  FLiteral JSON |
  FComma Filter Filter |
  FPipe Filter Filter |
  FRecDesc Filter |
  FArray Filter |
  FObject [(Filter, Filter)]


instance Show Filter where
  show (Identity) = "."
  show (Parenthesis f) = "(" ++ show f ++ ")"
  -- TODO

instance Eq Filter where
  Identity == Identity = True
  (Parenthesis x) == (Parenthesis y) = x == y
  (GenIndex xf xi xb) == (GenIndex yf yi yb) = (xf == yf) && (xi == yi) && (xb == yb)
  (ArrayRange xf xn xm xb) == (ArrayRange yf yn ym yb) = (xf == yf) && (xn == yn) && (xm == ym) && (xb == yb)
  (FLiteral xj) == (FLiteral yj) = xj == yj
  (FComma xb xa) == (FComma yb ya) = (xb == yb) && (xa == ya)
  (FPipe xb xa) == (FPipe yb ya) = (xb == yb) && (xa == ya)
  (FRecDesc x) == (FRecDesc y) = x == y
  _ == _ = False

data Config = ConfigC {filters :: Filter}


filterIdentitySC :: Filter
filterIdentitySC = Identity

filterStringIndexingSC :: String -> Filter
filterStringIndexingSC s = undefined

filterPipeSC :: Filter -> Filter -> Filter
filterPipeSC = undefined

filterCommaSC :: Filter -> Filter -> Filter
filterCommaSC = undefined
