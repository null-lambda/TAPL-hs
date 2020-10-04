module SyntaxParser
  ( term
  , program
  )
where

import           Control.Monad                  ( void )
import           Data.Functor
import           Syntax
import           Text.ParserCombinators.Parsec
import           Text.Parsec.Language           ( emptyDef )
import qualified Text.ParserCombinators.Parsec.Token
                                               as P

languageDef = emptyDef
  { P.reservedNames  = [ "true"
                       , "false"
                       , "if"
                       , "then"
                       , "else"
                       , "succ"
                       , "pred"
                       , "iszero"
                       ]
  , P.commentStart   = "/*"
  , P.commentEnd     = "*/"
  , P.commentLine    = ""
  , P.nestedComments = False
  }

lexer = P.makeTokenParser languageDef
reserved = P.reserved lexer
whiteSpace = P.whiteSpace lexer
semi = P.semi lexer
parens = P.parens lexer
symbol = P.symbol lexer
natural = P.natural lexer

tmTrue = TmTrue <$ reserved "true"

tmFalse = TmFalse <$ reserved "false"

tmNat =
  let naturalToTerm 0 = TmZero
      naturalToTerm n = TmSucc $ naturalToTerm (n - 1)
  in  naturalToTerm <$> natural

tmIf =
  TmIf
    <$> (reserved "if" >> term)
    <*> (reserved "then" >> term)
    <*> (reserved "else" >> term)

tmSucc = TmSucc <$> (reserved "succ" >> term)

tmPred = TmPred <$> (reserved "pred" >> term)

tmIsZero = TmPred <$> (reserved "iszero" >> term)

term :: Parser Term
term =
  tmTrue
    <|> tmFalse
    <|> tmNat
    <|> tmIf
    <|> tmSucc
    <|> tmPred
    <|> tmIsZero
    <|> parens term

program :: Parser [Term]
program = whiteSpace *> endBy1 term semi

