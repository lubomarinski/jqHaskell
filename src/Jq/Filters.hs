module Jq.Filters where

data Filter = 
  Identity |
  Parenthesis Filter |
  ObjectIndex Filter String Bool |
  ArrayIndex Filter Int Bool |
  ArrayRange Filter Int Int Bool


instance Show Filter where
  show (Identity) = "."
  show (Parenthesis f) = "(" ++ show f ++ ")"
  -- TODO

instance Eq Filter where
  Identity == Identity = True
  (Parenthesis x) == (Parenthesis y) = x == y
  (ObjectIndex xf xs xb) == (ObjectIndex yf ys yb) = (xf == yf) && (xs == ys) && (xb == yb)
  (ArrayIndex xf xn xb) == (ArrayIndex yf yn yb) = (xf == yf) && (xn == yn) && (xb == yb)
  (ArrayRange xf xn xm xb) == (ArrayRange yf yn ym yb) = (xf == yf) && (xn == yn) && (xm == ym) && (xb == yb)
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
