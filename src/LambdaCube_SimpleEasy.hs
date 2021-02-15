module LambdaCube_SimpleEasy where

-- Based on: http://augustss.blogspot.com/2007/10/simpler-easier-in-recent-paper-simply.html
import Data.List (delete, union)
{- HLINT ignore "Eta reduce" -}

-- File mnemonics:
--   env = typing environment
--   vid = variable identifier in Bind or Var
--   br = binder variant (Lambda or Pi)
--   xyzTyp = type of xyz
--   body = body of Lambda or Pi abstraction
--   fn = expression in a function position
--   arg = expression in an argument position

-- Variable identifiers. We 'export' only Eq, Show, freshVid
newtype Vid = Vid String deriving (Eq, Show)

-- Generates a fresh symbol given a list of forbidden symbols.
freshVid :: [Vid] -> Vid -> Vid
freshVid forbidden vid | vid `notElem` forbidden = vid
freshVid forbidden (Vid str) = freshVid forbidden $ Vid (str ++ "'")

-- Syntax tree of lambda-pi expressions
data Expr
  = Var Vid
  | App Expr Expr
  | Bind Binder Vid Expr Expr -- both Lambda and Pi expressions
  | Star
  | Box
  deriving (Show)

data Binder = Lambda | Pi deriving (Eq, Show)

-- Standard alpha equality.
alphaEq :: Expr -> Expr -> Bool
alphaEq (Var vid) (Var vid') = vid == vid'
alphaEq (App fn arg) (App fn' arg') = alphaEq fn fn' && alphaEq arg arg'
alphaEq (Bind br vid vidTyp body) (Bind br' vid' vidTyp' body') =
  br == br' && alphaEq vidTyp vidTyp' && alphaEq body (subst vid' (Var vid) body')
alphaEq Star Star = True
alphaEq Box Box = True
alphaEq _ _ = False

-- List of free variables.
freeVars :: Expr -> [Vid]
freeVars (Var vid) = [vid]
freeVars (App fn arg) = freeVars fn `union` freeVars arg
freeVars (Bind _ vid vidTyp body) = freeVars vidTyp `union` (vid `delete` freeVars body)
freeVars Star = []
freeVars Box = []

-- Capture avoiding substitution.
subst :: Vid -> Expr -> Expr -> Expr
subst subVid subExpr = aux where
  subExprFV = freeVars subExpr
  aux (Var vid) = if vid == subVid then subExpr else Var vid
  aux (App fn arg) = App (aux fn) (aux arg)
  aux (Bind br vid vidTyp body)
    | vid == subVid = Bind br vid (aux vidTyp) body
    | vid `elem` subExprFV =
      -- Bind br vid ... would capture vid in our substitution, need to rename to newVid
      let fv = subExprFV ++ freeVars body
          newVid = freshVid fv vid
          newBody = subst vid (Var newVid) body
          newExpr = Bind br newVid vidTyp newBody
        in aux newExpr -- will call into the 'otherwise' below
    | otherwise = Bind br vid (aux vidTyp) (aux body)
  aux Star = Star
  aux Box = Box

-- Strong normalization, computes normal form. Guaranteed to terminate for typeCheck-ed terms.
norm :: Expr -> Expr
norm expr = spine expr [] where
  spine (App fn arg) args = spine fn (arg : args)
  spine (Bind br vid vidTyp body) [] = Bind br vid (norm vidTyp) (norm body)
  spine (Bind Lambda vid _ body) (arg : args) = spine (subst vid arg body) args
  -- spine (Bind Pi _ _ _) (_:_) = error "should not happen for well typed terms"
  spine fn args = foldl App fn (map norm args)

-- Given an typing environment and an expression, returns Left type error or Right type.
typeCheck :: [(Vid, Expr)] -> Expr -> Either String Expr
typeCheck [] (Var vid) = Left $ "Cannot find variable: " ++ show vid ++ "\n"
typeCheck ((envVid, envTyp) : _) (Var vid) | envVid == vid = Right envTyp
typeCheck (_ : env) (Var vid) = typeCheck env (Var vid)
typeCheck env (App fn arg) = do
  fnTyp <- norm <$> typeCheck env fn
  argTyp <- norm <$> typeCheck env arg
  case fnTyp of
    -- The core rule of App, type of the variable must be equal to the type of the argument.
    Bind Pi vid vidTyp retTyp | alphaEq argTyp (norm vidTyp) -> Right $ subst vid arg retTyp
    _ -> Left $ "Function type: " ++ show fnTyp ++ "\nArgument type: " ++ show argTyp ++ "\n"
typeCheck env (Bind br vid vidTyp body) = do
  vidTypTyp <- norm <$> typeCheck env vidTyp
  -- Invariant: all types in environemnt are well typed so that we can normalize them.
  bodyTyp <- norm <$> typeCheck ((vid, vidTyp) : env) body
  case br of
    Lambda -> do
      -- The type of (\(vid:vidTyp) -> body) is ((vid:vidTyp) -> bodyTyp)
      let retTyp = Bind Pi vid vidTyp bodyTyp
      _ <- typeCheck env retTyp
      Right retTyp
    Pi -> case (vidTypTyp, bodyTyp) of
      -- These determin in which corner of the lambda cube are we in.
      (Star, Star) -> Right Star
      (Box, Star) -> Right Star
      (Star, Box) -> Right Box
      (Box, Box) -> Right Box
      _ -> Left $ "Bad Pi abstraction: " ++ show (Bind br vid vidTyp body) ++ "\n"
typeCheck _ Star = Right Box
typeCheck _ Box = Left "CoC has only one universe 'Star' avaliable to the user.\n"