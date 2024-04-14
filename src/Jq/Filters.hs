module Jq.Filters where

data Filter = 
  Identity |
  Parenthesis Filter |
  ObjectIndex Filter String Bool |
  ArrayIndex Filter Int |
  ArrayRange Filter Int Int 


instance Show Filter where
  show (Identity) = "."
  show (Parenthesis f) = "(" ++ show f ++ ")"
  -- TODO

instance Eq Filter where
  Identity == Identity = True
  (Parenthesis x) == (Parenthesis y) = x == y
  (ObjectIndex xf xs xb) == (ObjectIndex yf ys yb) = (xf == yf) && (xs == ys) && (xb == yb)
  (ArrayIndex xf xn) == (ArrayIndex yf yn) = (xf == yf) && (xn == yn)
  (ArrayRange xf xn xm) == (ArrayRange yf yn ym) = (xf == yf) && (xn == yn) && (xm == ym)
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
