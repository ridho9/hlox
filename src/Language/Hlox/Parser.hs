{-# LANGUAGE OverloadedStrings #-}

module Language.Hlox.Parser where

import Data.Foldable (foldl')
import Data.Functor
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Language.Hlox.Syntax
import Language.Hlox.Value
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

type ParserError = ParseErrorBundle Text Void

parseProgram :: Parser [Statement]
parseProgram = many parseProgramLine

parseProgramLine :: Parser Statement
parseProgramLine = parseDeclaration

parseDeclaration :: Parser Statement
parseDeclaration = parseVarDeclStmt <|> parseStatement

parseVarDeclStmt :: Parser Statement
parseVarDeclStmt = parseVarDeclStmtBody <* symbol ";"

parseVarDeclStmtBody = do
  ident <- symbol "var" *> parseIdentifier
  value <- optional (symbol "=" >> parseExpression)
  return $ Declaration ident value

parseStatement :: Parser Statement
parseStatement =
  parseIfStatement
    <|> parsePrintStatement
    <|> parseWhileStatement
    <|> ((symbol "break" *> symbol ";") $> Break)
    <|> parseForStatement
    <|> parseExprStatement
    <|> parseBlockStatement

parseForStatement :: Parser Statement
parseForStatement = do
  symbol "for" *> symbol "("
  initializer <- optional (parseVarDeclStmtBody <|> (Expression <$> parseExpression)) <* symbol ";"
  condition <- (parseExpression <|> return (Literal $ Bool True)) <* symbol ";"
  increment <- (parseExpression <|> return (Literal $ Bool True)) <* symbol ")"
  body <- parseStatement

  return $
    Block
      [ fromMaybe (Expression $ Literal Nil) initializer
      , While condition $ Block [body, Expression increment]
      ]

parseWhileStatement :: Parser Statement
parseWhileStatement = do
  cond <- symbol "while" *> symbol "(" *> parseExpression <* symbol ")"
  While cond <$> parseStatement

parseIfStatement :: Parser Statement
parseIfStatement = do
  cond <- symbol "if" *> symbol "(" *> parseExpression <* symbol ")"
  ifTrue <- parseStatement
  ifFalse <- optional (symbol "else" *> parseStatement)
  return $ If cond ifTrue ifFalse

parsePrintStatement :: Parser Statement
parsePrintStatement = Print <$> (symbol "print" *> parseExpression <* symbol ";")

parseExprStatement :: Parser Statement
parseExprStatement = (Expression <$> parseExpression) <* symbol ";"

parseBlockStatement :: Parser Statement
parseBlockStatement = Block <$> (symbol "{" *> many parseDeclaration <* symbol "}")

parseExpression :: Parser Expression
parseExpression = parseAssignment

parseAssignment :: Parser Expression
parseAssignment =
  try
    ( do
        ident <- parseIdentifier
        symbol "="
        Assignment ident <$> parseAssignment
    )
    <|> parseLogicOr

parseLogicOr :: Parser Expression
parseLogicOr = do
  left <- parseLogicAnd
  rest <- many $ do
    op <-
      parseIdentMany
        [("or", Or)]
    right <- parseLogicAnd
    return (op, right)
  return $ foldl' (\left (op, right) -> Logical left op right) left rest

parseLogicAnd :: Parser Expression
parseLogicAnd = do
  left <- parseEquality
  rest <- many $ do
    op <-
      parseIdentMany
        [("and", And)]
    right <- parseEquality
    return (op, right)
  return $ foldl' (\left (op, right) -> Logical left op right) left rest

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
  return $ foldl' (\left (op, right) -> Binary left op right) left rest

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
  return $ foldl' (\left (op, right) -> Binary left op right) left rest

parseTerm :: Parser Expression
parseTerm = do
  left <- parseFactor
  rest <- many $ do
    op <- parseIdentMany [("+", Plus), ("-", Minus)]
    right <- parseFactor
    return (op, right)
  return $ foldl' (\left (op, right) -> Binary left op right) left rest

parseFactor :: Parser Expression
parseFactor = do
  left <- parseUnary
  rest <- many $ do
    op <- parseIdentMany [("/", Divide), ("*", Multiply)]
    right <- parseUnary
    return (op, right)
  return $ foldl' (\left (op, right) -> Binary left op right) left rest

parseUnary :: Parser Expression
parseUnary =
  ( do
      op <- parseIdentMany [("!", Not), ("-", Negate)] <?> "unary operator"
      Unary op <$> parseUnary
  )
    <|> parseCall

parseCall :: Parser Expression
parseCall = do
  prim <- parsePrimary
  calls <- many parseCallArgs
  case calls of
    [] -> return prim
    exprs -> return $ foldl' Call prim calls

parseCallArgs :: Parser [Expression]
parseCallArgs = do
  args <- symbol "(" *> sepEndBy parseExpression (symbol ",") <* symbol ")"
  if length args < 256 then return args else fail "can't have call with more than 255 arguments"

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
    <?> "boolean"

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
