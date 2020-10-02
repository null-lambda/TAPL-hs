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

languageDef = T.LanguageDef { T.commentStart    = "/*"
                            , T.commentEnd      = "*/"
                            , T.commentLine     = ""
                            , T.nestedComments  = False
                            , T.identStart      = letter
                            , T.identLetter     = alphaNum
                            , T.opStart         = letter
                            , T.opLetter        = alphaNum
                            , T.reservedNames   = ["lambda"]
                            , T.reservedOpNames = []
                            , T.caseSensitive   = True
                            }


lexer = T.makeTokenParser languageDef
reserved = T.reserved lexer
identifier = T.identifier lexer
dot = T.dot lexer
spaces = T.whiteSpace lexer
semi = T.semi lexer
parens = T.parens lexer
symbol = T.symbol lexer
natural = T.natural lexer

tmVar = do
  ctx <- getState
  im  <- nameToIndex ctx <$> identifier
  i   <- case im of
    Just x  -> return x
    Nothing -> fail "Unbound variable name"
  let n = length ctx
  return $ TmVar i n
tmAbs = do
  ctx <- getState
  reserved "lambda" <|> void (symbol "\\")
  vs <- many1 identifier
  dot
  setState (reverse vs ++ ctx)
  t1 <- term
  setState ctx
  return $ foldr TmAbs t1 vs
{-
tmAbs = do
  ctx <- getState
  reserved "lambda" <|> void (symbol "\\")
  v <- identifier
  dot
  setState (v : ctx)
  t1 <- term
  setState ctx
  return $ TmAbs v t1
-}
tmApp = chainl1 (tmAbs <|> tmVar <|> parens term) (return TmApp)
term = tmApp

eval = Eval <$> term

binder = (<* symbol "/")
bind = do
  ctx <- getState
  s   <- identifier
  setState (s : ctx)
  binder . return $ Bind s

cmd = try eval <|> bind

program :: Parser [Command]
program = spaces *> endBy1 cmd semi

