module Parser
    ( parse
    )
  where

import Ast
import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as P
import Control.Monad

expr :: Parser Expr
expr =  try (builtin2 "+" Plus)
    <|> try (builtin2 "-" Minus)
    <|> try (builtin2 "*" Times)
    <|> try (builtin2 "<" Less)
    <|> try (builtin2 ">" Greater)
    <|> try (builtin2 "<=" LessEq)
    <|> try (builtin2 ">=" GreaterEq)
    <|> try (builtin2 "=" Eq)
    <|> try (builtin2 "/=" NotEq)
    <|> try (builtin2 "&&" And)
    <|> try (builtin2 "||" Or)
    <|> try ifExpr
    <|> try (builtin1 "print" Print)
    <|> try letBinding
    <|> value (IntLit . read) (lexeme $ many1 digit)
    <|> call
    <|> reference
  where
    value f p = liftM f $ try $ parens p <|> p

    letBinding = parens $ do
      _ <- symbol "let"
      bindings <- parens $ many binding
      exprs <- many expr
      return $ Let bindings exprs

    ifExpr = parens $ do
      _ <- symbol "if"
      cond <- expr
      thenB <- expr
      elseB <- expr
      return $ If cond thenB elseB

    binding = parens $ do
      i <- identifier
      e <- expr
      return (i, e)

    reference = liftM Ref identifier

    call = parens $ do
      i <- identifier
      args <- many expr
      return $ Call i args

    builtin1 s c = parens $ do
      _ <- symbol s
      a <- expr
      return $ c a

    builtin2 s c = parens $ do
      _ <- symbol s
      a <- expr
      b <- expr
      return $ c a b

parse :: String -> Either ParseError Program
parse = P.parse program ""

program :: Parser Program
program = many fun <* eof

fun :: Parser Function
fun = parens $ do
  _ <- symbol "defun"
  name <- identifier
  args <- arguments
  body <- many expr
  return $ Function name args body

identifier :: Parser Identifier
identifier = lexeme . many1 . oneOf $ allowedChars
  where allowedChars = concat [ "+-*/-"
                              , ['a'..'z']
                              , ['A'..'Z']
                              ]

arguments :: Parser [Identifier]
arguments = parens $ many $ lexeme identifier

-----------

parens :: Parser a -> Parser a
parens p = symbol "(" *> p <* symbol ")"

lexeme :: Parser a -> Parser a
lexeme parser = parser <* spaces

symbol :: String -> Parser String
symbol = lexeme . string
