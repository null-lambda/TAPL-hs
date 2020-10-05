module Parser
  ( term
  , program
  )
where

import           Control.Monad
import           Control.Monad.Trans.State
import           Debug.Trace
import           Data.Void
import           Data.Functor
import           Data.Functor.Identity
import           Text.Megaparsec         hiding ( State )
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

import           Syntax

type Parser a = StateT Context (ParsecT Void String Identity) a

reservedKeywords = ["if", "then", "else", "true", "false", "lambda"]
isReservedName = (`elem` reservedKeywords)

spaces = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")
lexeme = L.lexeme spaces :: Parser a -> Parser a
symbol = L.symbol spaces :: String -> Parser String
reserved s = lexeme $ try (string s)
identifier = lexeme $ try $ do
  s <- lexeme ((:) <$> letterChar <*> many alphaNumChar)
  if isReservedName s then fail $ "reserved word " ++ show s else return s
natural = lexeme L.decimal
parens = between (symbol "(") (symbol ")")
angles = between (symbol "<") (symbol ">")
colon = symbol ":"
semicolon = symbol ";"
comma = symbol ","
dot = symbol "."

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

tyBool = symbol "Bool" >> return TyBool
tyArrow = infixR (symbol "->" >> return TyArrow)

tyA = choice [tyBool, parens ty]
ty = tyArrow tyA <?> "type"
tyBind = colon >> ty <?> "type binding"

tmTrue = TmTrue <$ reserved "true"
tmFalse = TmFalse <$ reserved "false"
tmIf baseTerm =
  TmIf
    <$> (reserved "if" >> baseTerm)
    <*> (reserved "then" >> baseTerm)
    <*> (reserved "else" >> baseTerm)
tmVar = (<?> "variable") $ try $ do
  ctx <- get
  x   <- identifier
  i   <- case nameToIndex ctx x of
    Just i  -> return i
    Nothing -> fail $ "unbound variable name " ++ show x
  let n = length ctx
  return (TmVar i n)
tmAbs baseTerm = do
  ctx <- get
  reserved "lambda" <|> symbol "\\"
  vs <- (<?> "parameter list") $ some $ do
    x1  <- identifier
    ty1 <- tyBind
    modify ((x1, NameBind) :)
    return (x1, ty1)
  t1 <- dot >> baseTerm
  put ctx
  return $ foldr (uncurry TmAbs) t1 vs
tmApp = infixL (return TmApp)

literal = choice [tmTrue, tmFalse] <?> "literal"
expA = choice [literal, tmVar, parens term] -- tuple, list, sequence, ...
exp10 = choice [tmAbs term, tmIf term, tmApp expA]
exp0 = exp10
term = exp0 <?> "term"

eval = CmdEval <$> term
bind = do
  x   <- identifier
  ty1 <- tyBind
  modify ((x, VarBind ty1) :)
  return (CmdBind x ty1) <?> "signature"

cmd :: Parser Command
cmd = try eval <|> bind

program :: Parser [Command]
program = spaces *> endBy1 cmd semicolon

