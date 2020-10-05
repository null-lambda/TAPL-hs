module Parser
  ( term
  , cmd
  , program
  )
where

import           Control.Monad
import           Control.Monad.Trans.State
import           Debug.Trace
import           Data.Char
import           Data.Function
import           Data.Void
import           Data.Bifunctor
import           Data.Functor
import           Data.Functor.Identity
import           Data.Maybe
import           Text.Megaparsec         hiding ( State )
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import           Text.Megaparsec.Debug

import           Syntax

type Parser a = StateT Context (ParsecT Void String Identity) a

reservedKeywords =
  [ "if"
  , "then"
  , "else"
  , "true"
  , "false"
  , "lambda"
  , "unit"
  , "let"
  , "in"
  , "pred"
  , "succ"
  , "iszero"
  , "Bool"
  , "Nat"
  , "Unit"
  , "timesfloat"
  , "fix"
  , "as"
  , "case"
  , "of"
  ]
isReservedName = (`elem` reservedKeywords)

spaces :: Parser ()
lexeme :: Parser a -> Parser a
symbol :: String -> Parser String
reserved :: String -> Parser ()
identifier :: Parser String
varName :: Parser String
typeName :: Parser String
spaces = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")
lexeme = L.lexeme spaces
symbol = L.symbol spaces
reserved s = lexeme $ try $ do
  string s
  notFollowedBy letterChar <?> "end of" ++ show s
notReserved p = lexeme $ try $ do
  s <- p
  if isReservedName s
    then fail $ "reserved word " ++ show s
    else return s
identifier = notReserved $ (:) <$> letterChar <*> many alphaNumChar
varName = notReserved $ (:) <$> lowerChar <*> many alphaNumChar
typeName = notReserved $ (:) <$> upperChar <*> many alphaNumChar
natural = lexeme L.decimal
float = lexeme L.float
stringLiteral = char '"' *> manyTill L.charLiteral (char '"')
parens = between (symbol "(") (symbol ")")
angles = between (symbol "<") (symbol ">")
curly = between (symbol "{") (symbol "}")
underscore = symbol "_"
colon = symbol ":"
semicolon = symbol ";"
comma = symbol ","
dot = symbol "."
equals = symbol "="


-- left recursion solver, which removes infinite loop in parsing
{- 
  e := e dot P | base 
  ---------------------------  
  e := base e' 
  e' := dot P e' | empty
-}
leftRecursion :: (a -> Parser a) -> Parser a -> Parser a
leftRecursion append base = do
  x <- base
  rest x where
  rest x =
    do
        r <- append x
        rest r
      <|> return x

rightRecursion :: (Parser a -> Parser a) -> Parser a -> Parser a
rightRecursion prepend base = e where e = try (prepend e) <|> base

infixN :: Parser (a -> a -> a) -> Parser a -> Parser a
infixN op p = do
  x <- p
  f <- op
  y <- p
  return $ x `f` y

infixL :: Parser (a -> a -> a) -> Parser a -> Parser a
infixL op p = leftRecursion append p where
  append x = do
    f <- op
    y <- p
    return $ x `f` y


-- efficient version of rightRecursion (no 'try' function backtracking)
infixR :: Parser (a -> a -> a) -> Parser a -> Parser a
infixR op p = e where
  e = do
    x <- p
    append x e <|> return x
  append x e = do
    f <- op
    y <- e
    return $ x `f` y

-- type 

tyBase = choice $ map
  (\(name, t) -> reserved name >> return t)
  [ ("Unit"  , TyUnit)
  , ("Bool"  , TyBool)
  , ("Nat"   , TyNat)
  , ("String", TyString)
  , ("Float" , TyFloat)
  ]
tyUnitAbb = symbol "()" >> return TyUnit
tyVar = (<?> "type variable") $ do
  x   <- typeName
  ctx <- get
  i   <- case nameToIndex ctx x of
    Just i  -> return i
    Nothing -> fail $ "unbound type variable name " ++ show x
  let n = length ctx
  return (TyVar i n)
tyArrow = infixR (symbol "->" >> return TyArrow)

recordLabel = choice [identifier, show <$> natural1] where
  natural1 = do
    n <- natural
    if n >= 1 then return n else fail "record index must start with 1"
variantLabel = identifier
tyRecord baseType = label "record type" $ do
  rs <- curly $ flip sepBy1 comma $ do
    l  <- optional $ try (recordLabel <* symbol ":")
    t1 <- baseType
    return (l, t1)
  let defaultLabel = map show [1 ..]
  let rs' = zipWith (\i (ml, t1) -> (fromMaybe i ml, t1)) defaultLabel rs
  return $ TyRecord rs'
tyVariant baseType = label "variant type" $ do
  rs <- angles $ flip sepBy1 comma $ do
    l  <- variantLabel <* symbol ":"
    t1 <- baseType
    return (l, t1)
  return $ TyVariant rs

tyAtom =
  choice [tyUnitAbb, parens ty, tyRecord ty, tyVariant ty, tyBase, tyVar]
ty = tyArrow tyAtom <?> "type"

-- term 

tmVar = (<?> "variable") $ try $ do
  x   <- varName
  ctx <- get
  i   <- case nameToIndex ctx x of
    Just i  -> return i
    Nothing -> fail $ "unbound variable name " ++ show x
  let n = length ctx
  return (TmVar i n)
tmAbs baseTerm = do
  choice [reserved "lambda", void $ symbol "\\"]
  ctx <- get
  vs  <- (<?> "parameter list") $ some $ do
    x1  <- varName
    ty1 <- colon >> ty
    modify ((x1, NameBind) :)
    return (x1, ty1)
  t1 <- dot >> baseTerm
  put ctx
  return $ foldr (uncurry TmAbs) t1 vs
tmApp = infixL (return TmApp)

tmLet baseTerm = do
  x   <- reserved "let" >> varName
  ctx <- get
  t1  <- equals >> baseTerm
  modify ((x, NameBind) :)
  t2 <- reserved "in" >> baseTerm
  put ctx
  return $ TmLet x t1 t2

tmRecord baseTerm = label "record" $ do
  rs <- curly $ flip sepBy1 comma $ do
    l  <- optional $ try (recordLabel <* equals)
    t1 <- baseTerm
    return (l, t1)
  let defaultLabel = map show [1 ..]
  let rs' = zipWith (\i (ml, t1) -> (fromMaybe i ml, t1)) defaultLabel rs
  return $ TmRecord rs'
tmProj = leftRecursion $ \t1 -> label "projection" $ do
  symbol "."
  TmProj t1 <$> recordLabel
tmTag baseTerm = label " tag" $ do
  symbol "<"
  l <- variantLabel
  symbol "="
  t1 <- baseTerm
  symbol ">"
  ty1 <- reserved "as" >> ty
  return $ TmTag l t1 ty1
tmCase baseTerm = label "case" $ do
  reserved "case"
  t <- baseTerm
  reserved "of"
  cases <- flip sepBy1 (symbol "|") $ label "case tag" $ do
    symbol "<"
    ln <- variantLabel
    symbol "="
    xn <- varName
    symbol ">"
    ctx <- get
    modify ((xn, NameBind) :)
    symbol "=>"
    tn <- baseTerm
    put ctx
    return (ln, (xn, tn))
  return $ TmCase t cases


tmIf baseTerm =
  TmIf
    <$> (reserved "if" >> baseTerm)
    <*> (reserved "then" >> baseTerm)
    <*> (reserved "else" >> baseTerm)
tmNat = f <$> natural where
  f 0 = TmZero
  f n = TmSucc $ f (n - 1)
tmFloat = TmFloat <$> try float
tmString = TmString <$> try stringLiteral
tmLiteral = (<?> "literal") $ choice $ r ++ [tmString, tmFloat, tmNat] where
  r = map (\(name, t) -> reserved name >> return t)
          [("unit", TmUnit), ("true", TmTrue), ("false", TmFalse)]
tmUnaryFunc baseTerm = choice $ map
  (\(name, f) -> reserved name >> f <$> baseTerm)
  [("succ", TmSucc), ("pred", TmPred), ("iszero", TmIsZero), ("fix", TmFix)]
tmBinaryFunc baseTerm = choice $ map
  (\(name, f) -> reserved name >> f <$> baseTerm <*> baseTerm)
  [("timesfloat", TmTimesFloat)]
tmUnitAbb = symbol "()" >> return TmUnit

expAtom = choice
  [tmUnitAbb, parens term, tmRecord term, tmTag term, tmVar, tmLiteral, tmVar]
expAsc = choice [expAtom]
expPath = choice [tmProj expAsc, expAsc]
expApp = choice [tmUnaryFunc expPath, tmBinaryFunc expPath, tmApp expPath]
exp10 = choice [tmAbs term, tmIf term, tmLet term, tmCase term, expApp]
exp0 = exp10
term = exp0 <?> "term"

cmdEval = CmdEval <$> term

tmBind = varBind <|> tmAbbBind where
  varBind   = colon >> VarBind <$> ty
  tmAbbBind = equals >> TmAbbBind <$> term <*> optional (colon >> ty)
tyBind = tyAbbBind <|> tyVarBind where
  tyVarBind = return TyVarBind
  tyAbbBind = equals >> TyAbbBind <$> ty
cmdBind = do
  x <- identifier
  b <- if isLower $ head x then tmBind else tyBind
  modify ((x, b) :)
  return $ CmdBind x b

cmd :: Parser Command
cmd = cmdEval <|> cmdBind

program :: Parser [Command]
program = spaces *> endBy1 cmd semicolon <* eof

