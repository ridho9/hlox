{-# LANGUAGE OverloadedStrings #-}

module Language.Hlox.Parser where

import Control.Monad.Combinators.Expr
import Data.Foldable (foldl')
import Data.Functor
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Language.Hlox.Syntax
import Language.Hlox.Value
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

parseProgram :: Parser [Statement ()]
parseProgram = many parseProgramLine

parseProgramLine :: Parser (Statement ())
parseProgramLine = parseDeclaration

parseDeclaration :: Parser (Statement ())
parseDeclaration = parseVarDeclStmt <|> parseStatement

parseVarDeclStmt :: Parser (Statement ())
parseVarDeclStmt = parseVarDeclStmtBody <* symbol ";"

parseVarDeclStmtBody :: Parser (Statement ())
parseVarDeclStmtBody = do
  ident <- symbol "var" *> parseIdentifier
  value <- optional (symbol "=" >> parseExpression)
  return $ Declaration () ident value

parseStatement :: Parser (Statement ())
parseStatement =
  parseIfStatement
    <|> parsePrintStatement
    <|> parseWhileStatement
    <|> ((symbol "break" *> symbol ";") $> Break ())
    <|> parseForStatement
    <|> parseExprStatement
    <|> parseBlockStatement

parseForStatement :: Parser (Statement ())
parseForStatement = do
  symbol "for" *> symbol "("
  initializer <- optional (parseVarDeclStmtBody <|> (Expression () <$> parseExpression)) <* symbol ";"
  condition <- (parseExpression <|> return (Literal () $ Bool True)) <* symbol ";"
  increment <- (parseExpression <|> return (Literal () $ Bool True)) <* symbol ")"
  body <- parseStatement

  return $
    Block
      ()
      [ fromMaybe (Expression () $ Literal () Nil) initializer
      , While () condition $ Block () [body, Expression () increment]
      ]

parseWhileStatement :: Parser (Statement ())
parseWhileStatement = do
  cond <- symbol "while" *> symbol "(" *> parseExpression <* symbol ")"
  While () cond <$> parseStatement

parseIfStatement :: Parser (Statement ())
parseIfStatement = do
  cond <- symbol "if" *> symbol "(" *> parseExpression <* symbol ")"
  ifTrue <- parseStatement
  ifFalse <- optional (symbol "else" *> parseStatement)
  return $ If () cond ifTrue ifFalse

parsePrintStatement :: Parser (Statement ())
parsePrintStatement = Print () <$> (symbol "print" *> parseExpression <* symbol ";")

parseExprStatement :: Parser (Statement ())
parseExprStatement = (Expression () <$> parseExpression) <* symbol ";"

parseBlockStatement :: Parser (Statement ())
parseBlockStatement = Block () <$> (symbol "{" *> many parseDeclaration <* symbol "}")

parseExpression :: Parser (Expression ())
parseExpression = parseAssignment

parseAssignment :: Parser (Expression ())
parseAssignment =
  try
    ( do
        ident <- parseIdentifier
        symbol "="
        Assignment () ident <$> parseAssignment
    )
    <|> parseMathExpression

parseMathExpression :: Parser (Expression ())
parseMathExpression = makeExprParser parseCall operatorTable <?> "expression"

parseCall :: Parser (Expression ())
parseCall = do
  prim <- parsePrimary
  calls <- many parseCallArgs
  case calls of
    [] -> return prim
    exprs -> return $ foldl' (Call ()) prim calls

parseCallArgs :: Parser [Expression ()]
parseCallArgs = do
  args <- symbol "(" *> sepEndBy parseExpression (symbol ",") <* symbol ")"
  if length args < 256 then return args else fail "can't have call with more than 255 arguments"

parsePrimary :: Parser (Expression ())
parsePrimary = parseLiteral <|> parseGrouping <|> parseVariable

parseVariable :: Parser (Expression ())
parseVariable =
  Variable () <$> parseIdentifier

parseIdentifier :: Parser Text
parseIdentifier = lexeme $ do
  first <- letterChar
  rest <- many (letterChar <|> digitChar)
  return $ T.pack $ first : rest

parseLiteral :: Parser (Expression ())
parseLiteral = Literal () <$> parseValue

parseGrouping :: Parser (Expression ())
parseGrouping = do
  symbol "("
  expr <- parseExpression
  symbol ")"
  return $ Grouping () expr

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

operatorTable :: [[Operator Parser (Expression ())]]
operatorTable =
  [ [prefix "-" (Unary () Negate), prefix "!" (Unary () Not)]
  , [binary "/" (Binary () Divide), binary "*" (Binary () Multiply)]
  , [binary "+" (Binary () Plus), binary "-" (Binary () Minus)]
  , [binary "<=" (Binary () LessEq), binary ">=" (Binary () GreaterEq), binary "<" (Binary () Less), binary ">" (Binary () Greater)]
  , [binary "==" (Binary () Eq), binary "!=" (Binary () LessEq)]
  , [binary "and" (Logical () And)]
  , [binary "or" (Logical () Or)]
  ]

binary name f = InfixL (symbol name $> f)

prefix name f = Prefix (symbol name $> f)