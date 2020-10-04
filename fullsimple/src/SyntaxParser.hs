module SyntaxParser
  ( term, cmd
  , program
  )
where

import           Control.Monad
import           Control.Monad.Trans.State
import           Debug.Trace
import           Data.Char
import           Data.Void
import           Data.Functor
import           Data.Functor.Identity
import           Text.Megaparsec         hiding ( State )
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

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
  ]
isReservedName = (`elem` reservedKeywords)

spaces :: Parser ()
lexeme :: Parser a -> Parser a
symbol :: String -> Parser String
reserved :: String -> Parser String
identifier :: Parser String
varName :: Parser String
typeName :: Parser String
spaces = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")
lexeme = L.lexeme spaces
symbol = L.symbol spaces
reserved s = lexeme $ try (string s)
notReserved p = lexeme $ try $ do
  s <- p
  if isReservedName s then fail $ "reserved word " ++ show s else return s
identifier = notReserved $ (:) <$> letterChar <*> many alphaNumChar
varName = notReserved $ (:) <$> lowerChar <*> many alphaNumChar
typeName = notReserved $ (:) <$> upperChar <*> many alphaNumChar
natural = lexeme L.decimal
parens = between (symbol "(") (symbol ")")
angles = between (symbol "<") (symbol ">")
colon = symbol ":"
semicolon = symbol ";"
comma = symbol ","
dot = symbol "."
equals = symbol "="

infixN :: Parser (a -> a -> a) -> Parser a -> Parser a
infixN op p = do
  x <- p
  f <- op
  y <- p
  return $ x `f` y

infixL :: Parser (a -> a -> a) -> Parser a -> Parser a
infixL op p = do
  x <- p
  rest x where
  rest x =
    do
        f <- op
        y <- p
        rest $ x `f` y
      <|> return x

infixR :: Parser (a -> a -> a) -> Parser a -> Parser a
infixR op p = scan
 where
  scan = do
    x <- p
    rest x
  rest x =
    do
        f <- op
        y <- scan
        return $ x `f` y
      <|> return x

-- type 

tyBase = choice $ map
  (\(name, t) -> reserved name >> return t)
  [("Unit", TyUnit), ("()", TyUnit), ("Bool", TyBool), ("Nat", TyNat)]
tyVar = do
  x   <- typeName
  ctx <- get
  i   <- case nameToIndex ctx x of
    Just i  -> return i
    Nothing -> fail $ "unbound type variable name " ++ show x
  let n = length ctx
  return (TyVar i n)
tyArrow = infixR (symbol "->" >> return TyArrow)

tyA = choice [tyBase, tyVar, parens ty]
ty = tyArrow tyA <?> "type"

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
  ctx <- get
  reserved "lambda" <|> symbol "\\"
  vs <- (<?> "parameter list") $ some $ do
    x1  <- varName
    ty1 <- colon >> ty
    modify ((x1, NameBind) :)
    return (x1, ty1)
  t1 <- dot >> baseTerm
  put ctx
  return $ foldr (uncurry TmAbs) t1 vs
tmApp = infixL (return TmApp)

tmLet baseTerm = do
  ctx <- get
  x   <- reserved "let" >> varName
  t1  <- symbol "=" >> baseTerm
  modify ((x, NameBind) :)
  t2 <- reserved "in" >> baseTerm
  put ctx
  return $ TmLet x t1 t2

tmLiteral = (<?> "atom (literal)") $ choice $ map
  (\(name, t) -> reserved name >> return t)
  [("unit", TmUnit), ("()", TmUnit), ("true", TmTrue), ("false", TmFalse)]
tmIf baseTerm =
  TmIf
    <$> (reserved "if" >> baseTerm)
    <*> (reserved "then" >> baseTerm)
    <*> (reserved "else" >> baseTerm)
tmNat = f <$> natural where
  f 0 = TmZero
  f n = TmSucc $ f (n - 1)
tmUnaryFunc baseTerm = choice $ map
  (\(name, f) -> reserved name >> f <$> baseTerm)
  [("succ", TmSucc), ("pred", TmPred), ("iszero", TmIsZero)]

expA = choice [tmLiteral, tmVar, tmNat, parens term] -- tuple, list, sequence, ...
expF = choice [tmUnaryFunc expA, tmApp expA] -- function application 
exp10 = choice [tmAbs term, tmIf term, tmLet term, expF]
exp0 = exp10
term = exp0 <?> "term"

eval = CmdEval <$> term

tmBind = varBind <|> tmAbbBind where
  varBind   = colon >> VarBind <$> ty
  tmAbbBind = equals >> TmAbbBind <$> term <*> optional (colon >> ty)
tyBind = tyAbbBind <|> tyVarBind where
  tyVarBind = return TyVarBind
  tyAbbBind = equals >> TyAbbBind <$> ty
bind = do
  x <- identifier
  b <- if isLower $ head x then tmBind else tyBind
  modify ((x, b) :)
  return $ CmdBind x b


cmd :: Parser Command
cmd = try eval <|> bind 

program :: Parser [Command]
program = spaces *> endBy1 cmd semicolon

