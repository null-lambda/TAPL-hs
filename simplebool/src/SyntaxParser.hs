{-# LANGUAGE TupleSections #-}

module SyntaxParser
  ( term
  , program
  )
where

import           Control.Monad                  ( void )
import           Data.Functor
import           Data.Functor.Identity
import           Text.Parsec             hiding ( spaces
                                                , Parser
                                                )
import qualified Text.Parsec.Token             as T
import           Syntax

type Parser a = ParsecT String Context Identity a

languageDef = T.LanguageDef
  { T.commentStart    = "/*"
  , T.commentEnd      = "*/"
  , T.commentLine     = ""
  , T.nestedComments  = False
  , T.identStart      = letter
  , T.identLetter     = alphaNum
  , T.opStart         = letter
  , T.opLetter        = alphaNum
  , T.reservedNames   = ["if", "then", "else", "true", "false", "lambda"]
  , T.reservedOpNames = []
  , T.caseSensitive   = True
  }


lexer = T.makeTokenParser languageDef
reserved = T.reserved lexer
identifier = T.identifier lexer
dot = T.dot lexer
colon = T.colon lexer
spaces = T.whiteSpace lexer
semi = T.semi lexer
parens = T.parens lexer
symbol = T.symbol lexer
natural = T.natural lexer

tyBool = symbol "Bool" >> return TyBool
tyArrow = chainr1 (tyBool <|> parens ty) (symbol "->" >> return TyArrow)
ty = tyArrow

tmTrue = TmTrue <$ reserved "true"
tmFalse = TmFalse <$ reserved "false"
tmIf =
  TmIf
    <$> (reserved "if" >> term)
    <*> (reserved "then" >> term)
    <*> (reserved "else" >> term)
tmVar = do
  ctx <- getState
  im  <- nameToIndex ctx <$> identifier
  i   <- case im of
    Just x  -> return x
    Nothing -> fail "unbound variable name"
  let n = length ctx
  return $ TmVar i n
tmAbs = do
  ctx <- getState
  reserved "lambda" <|> void (symbol "\\")
  vs <- many1 $ do
    x1 <- identifier
    colon
    ty1 <- ty
    modifyState ((x1, NameBind) :)
    return (x1, ty1)
  t1 <- dot >> term
  setState ctx
  return $ foldr (uncurry TmAbs) t1 vs
tmApp = chainl1
  (tmTrue <|> tmFalse <|> tmIf <|> tmAbs <|> tmVar <|> parens term)
  (return TmApp)
term = tmApp

eval = CmdEval <$> term
bind = do
  x <- identifier
  symbol ":"
  ty1 <- ty
  modifyState ((x, VarBind ty1) :)
  return $ CmdBind x ty1

cmd :: Parser Command
cmd = try eval <|> bind

program :: Parser [Command]
program = spaces *> endBy1 cmd semi

