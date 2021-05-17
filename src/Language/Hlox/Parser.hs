{-# LANGUAGE OverloadedStrings #-}

module Language.Hlox.Parser where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Language.Hlox.Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

type ParserError = ParseErrorBundle Text Void

parseProgramLine :: Parser Statement
parseProgramLine = parseDeclaration

parseDeclaration :: Parser Statement
parseDeclaration = parseVarDeclStmt <|> parseStatement

parseStatement :: Parser Statement
parseStatement = parsePrintStatement <|> parseExprStatement

parseVarDeclStmt :: Parser Statement
parseVarDeclStmt = do
  symbol "var"
  ident <- parseIdentifier
  value <- optional (symbol "=" >> parseExpression)
  symbol ";"
  return $ Declaration ident value

parsePrintStatement :: Parser Statement
parsePrintStatement = Print <$> (symbol "print" *> parseExpression <* symbol ";")

parseExprStatement :: Parser Statement
parseExprStatement = Expression <$> (parseExpression <* symbol ";")

parseExpression :: Parser Expression
parseExpression = parseEquality

parseEquality :: Parser Expression
parseEquality = do
  left <- parseCompare
  rest <- many $ do
    op <-
      parseIdentMany
        [ ("==", Eq)
        , ("!=", NotEq)
        ]
    right <- parseCompare
    return (op, right)
  return $ foldl (\left (op, right) -> Binary left op right) left rest

parseCompare :: Parser Expression
parseCompare = do
  left <- parseTerm
  rest <- many $ do
    op <-
      parseIdentMany
        [ ("<=", LessEq)
        , (">=", GreaterEq)
        , ("<", Less)
        , (">", Greater)
        ]
    right <- parseTerm
    return (op, right)
  return $ foldl (\left (op, right) -> Binary left op right) left rest

parseTerm :: Parser Expression
parseTerm = do
  left <- parseFactor
  rest <- many $ do
    op <- parseIdentMany [("+", Plus), ("-", Minus)]
    right <- parseFactor
    return (op, right)
  return $ foldl (\left (op, right) -> Binary left op right) left rest

parseFactor :: Parser Expression
parseFactor = do
  left <- parseUnary
  rest <- many $ do
    op <- parseIdentMany [("/", Divide), ("*", Multiply)]
    right <- parseUnary
    return (op, right)
  return $ foldl (\left (op, right) -> Binary left op right) left rest

parseUnary :: Parser Expression
parseUnary =
  ( do
      op <- parseIdentMany [("!", Not), ("-", Negate)]
      Unary op <$> parseUnary
  )
    <|> parsePrimary

parsePrimary :: Parser Expression
parsePrimary = parseLiteral <|> parseGrouping <|> parseVariable

parseVariable :: Parser Expression
parseVariable =
  Variable <$> parseIdentifier

parseIdentifier :: Parser Text
parseIdentifier = lexeme $ do
  first <- letterChar
  rest <- many (letterChar <|> digitChar)
  return $ T.pack $ first : rest

parseLiteral :: Parser Expression
parseLiteral = Literal <$> parseValue

parseGrouping :: Parser Expression
parseGrouping = do
  symbol "("
  expr <- parseExpression
  symbol ")"
  return $ Grouping expr

parseValue :: Parser Value
parseValue =
  parseNumber
    <|> parseString
    <|> parseNil
    <|> parseBool

parseBool :: Parser Value
parseBool =
  Bool
    <$> ( (symbol "true" >> return True)
            <|> (symbol "false" >> return False)
        )

parseNil :: Parser Value
parseNil = Nil <$ symbol "nil"

parseNumber :: Parser Value
parseNumber =
  label "number literal" $
    lexeme $
      Number . read
        <$> ( some digitChar
                <> option "" ((T.unpack <$> string ".") <> some digitChar)
            )

parseString :: Parser Value
parseString =
  label "string literal" $
    lexeme $
      do
        char '"'
        x <- many $ (char '\\' >> stringContentBackslash) <|> satisfy (/= '\"')
        char '"'
        return $ String $ T.pack x

stringContentBackslash :: Parser Char
stringContentBackslash =
  (char '\"' >> return '\"')
    <|> (char 'n' >> return '\n')
    <|> (char 'r' >> return '\r')
    <|> (char 't' >> return '\t')
    <|> (char '\\' >> return '\\')

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = lexeme . string

sc :: Parser ()
sc =
  L.space
    space1 -- (2)
    (L.skipLineComment "//") -- (3)
    (L.skipBlockComment "/*" "*/") -- (4)

parseIdentMany :: [(Text, a)] -> Parser a
parseIdentMany val = choice $ uncurry parseIdent <$> val

parseIdent :: Text -> a -> Parser a
parseIdent s o = symbol s >> return o
