module Jq.Filters where

import Jq.Json

data Filter = 
  FIdentity |
  FParenthesis Filter |
  FGenIndex Filter Filter Bool |
  FArrayRange Filter Filter Filter Bool |
  FLiteral JSON |
  FComma Filter Filter |
  FPipe Filter Filter |
  FRecDesc Filter |
  FArray Filter |
  FObject [(Filter, Filter)] |
  FTryCatch Filter Filter


instance Show Filter where
  show (FIdentity) = "."
  show (FParenthesis f) = "(" ++ show f ++ ")"
  -- TODO

instance Eq Filter where
  FIdentity == FIdentity = True
  (FParenthesis x) == (FParenthesis y) = x == y
  (FGenIndex xf xi xb) == (FGenIndex yf yi yb) = (xf == yf) && (xi == yi) && (xb == yb)
  (FArrayRange xf xn xm xb) == (FArrayRange yf yn ym yb) = (xf == yf) && (xn == yn) && (xm == ym) && (xb == yb)
  (FLiteral xj) == (FLiteral yj) = xj == yj
  (FComma xb xa) == (FComma yb ya) = (xb == yb) && (xa == ya)
  (FPipe xb xa) == (FPipe yb ya) = (xb == yb) && (xa == ya)
  (FRecDesc x) == (FRecDesc y) = x == y
  (FArray x) == (FArray y) = x == y
  (FObject x) == (FObject y) = x == y
  (FPipe xt xc) == (FPipe yt yc) = (xt == yt) && (xc == yc)
  _ == _ = False

data Config = ConfigC {filters :: Filter}


filterIdentitySC :: Filter
filterIdentitySC = FIdentity

filterStringIndexingSC :: String -> Filter
filterStringIndexingSC s = undefined

filterPipeSC :: Filter -> Filter -> Filter
filterPipeSC = undefined

filterCommaSC :: Filter -> Filter -> Filter
filterCommaSC = undefined
