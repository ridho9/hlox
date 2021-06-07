{-# LANGUAGE OverloadedStrings #-}

module Language.Hlox.Parser where

import Control.Monad.Combinators.Expr
import Data.Foldable (foldl')
import Data.Functor
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Language.Hlox.Annotation
import Language.Hlox.Syntax
import Language.Hlox.Value
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

parseProgram :: Parser [Statement Annotation]
parseProgram = many parseProgramLine

parseProgramLine :: Parser (Statement Annotation)
parseProgramLine = parseDeclaration

parseDeclaration :: Parser (Statement Annotation)
parseDeclaration = parseVarDeclStmt <|> parseStatement

parseVarDeclStmt :: Parser (Statement Annotation)
parseVarDeclStmt = parseVarDeclStmtBody <* symbol ";"

parseVarDeclStmtBody :: Parser (Statement Annotation)
parseVarDeclStmtBody = do
  pos <- annotatePos
  ident <- symbol "var" *> parseIdentifier
  value <- optional (symbol "=" >> parseExpression)
  return $ Declaration pos ident value

parseStatement :: Parser (Statement Annotation)
parseStatement =
  parseIfStatement
    <|> parsePrintStatement
    <|> parseWhileStatement
    <|> (annotatePos >>= \pos -> (symbol "break" *> symbol ";") $> Break pos)
    <|> parseForStatement
    <|> parseExprStatement
    <|> parseBlockStatement

parseForStatement :: Parser (Statement Annotation)
parseForStatement = do
  forPos <- annotatePos
  symbol "for" *> symbol "("
  initPos <- annotatePos
  initializer <- optional (parseVarDeclStmtBody <|> (Expression initPos <$> parseExpression)) <* symbol ";"
  condPos <- annotatePos
  condition <- (parseExpression <|> return (Literal condPos (Bool True))) <* symbol ";"
  incPos <- annotatePos
  increment <- (parseExpression <|> return (Literal incPos (Bool True))) <* symbol ")"
  body <- parseStatement

  return $
    Block
      forPos
      [ fromMaybe (Expression initPos $ Literal initPos Nil) initializer
      , While forPos condition $ Block forPos [body, Expression incPos increment]
      ]

parseWhileStatement :: Parser (Statement Annotation)
parseWhileStatement = do
  pos <- annotatePos
  cond <- symbol "while" *> symbol "(" *> parseExpression <* symbol ")"
  While pos cond <$> parseStatement

parseIfStatement :: Parser (Statement Annotation)
parseIfStatement = do
  pos <- annotatePos
  cond <- symbol "if" *> symbol "(" *> parseExpression <* symbol ")"
  ifTrue <- parseStatement
  ifFalse <- optional (symbol "else" *> parseStatement)
  return $ If pos cond ifTrue ifFalse

parsePrintStatement :: Parser (Statement Annotation)
parsePrintStatement = Print <$> annotatePos <*> (symbol "print" *> parseExpression <* symbol ";")

parseExprStatement :: Parser (Statement Annotation)
parseExprStatement = (Expression <$> annotatePos <*> parseExpression) <* symbol ";"

parseBlockStatement :: Parser (Statement Annotation)
parseBlockStatement = Block <$> annotatePos <*> (symbol "{" *> many parseDeclaration <* symbol "}")

parseExpression :: Parser (Expression Annotation)
parseExpression = parseAssignment

parseAssignment :: Parser (Expression Annotation)
parseAssignment =
  try
    ( do
        pos <- annotatePos
        ident <- parseIdentifier
        symbol "="
        Assignment pos ident <$> parseAssignment
    )
    <|> parseMathExpression

parseMathExpression :: Parser (Expression Annotation)
parseMathExpression = makeExprParser parseCall operatorTable <?> "expression"

parseCall :: Parser (Expression Annotation)
parseCall = do
  pos <- annotatePos
  prim <- parsePrimary
  calls <- many parseCallArgs
  case calls of
    [] -> return prim
    exprs -> return $ foldl' (Call pos) prim calls

parseCallArgs :: Parser [Expression Annotation]
parseCallArgs = do
  args <- symbol "(" *> sepEndBy parseExpression (symbol ",") <* symbol ")"
  if length args < 256 then return args else fail "can't have call with more than 255 arguments"

parsePrimary :: Parser (Expression Annotation)
parsePrimary = parseLiteral <|> parseGrouping <|> parseVariable

parseVariable :: Parser (Expression Annotation)
parseVariable =
  Variable <$> annotatePos <*> parseIdentifier

parseIdentifier :: Parser Text
parseIdentifier = lexeme $ do
  first <- letterChar
  rest <- many (letterChar <|> digitChar)
  return $ T.pack $ first : rest

parseGrouping :: Parser (Expression Annotation)
parseGrouping = do
  pos <- annotatePos
  symbol "("
  expr <- parseExpression
  symbol ")"
  return $ Grouping pos expr

parseLiteral :: Parser (Expression Annotation)
parseLiteral = Literal <$> annotatePos <*> parseValue

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

operatorTable :: [[Operator Parser (Expression Annotation)]]
operatorTable =
  [ [prefix "-" (`Unary` Negate), prefix "!" (`Unary` Not)]
  , [binary "/" (`Binary` Divide), binary "*" (`Binary` Multiply)]
  , [binary "+" (`Binary` Plus), binary "-" (`Binary` Minus)]
  , [binary "<=" (`Binary` LessEq), binary ">=" (`Binary` GreaterEq), binary "<" (`Binary` Less), binary ">" (`Binary` Greater)]
  , [binary "==" (`Binary` Eq), binary "!=" (`Binary` LessEq)]
  , [binary "and" (`Logical` And)]
  , [binary "or" (`Logical` Or)]
  ]

binary name f = InfixL $ do
  pos <- annotatePos
  symbol name
  return $ f pos

prefix name f = Prefix $ do
  pos <- annotatePos
  symbol name
  return $ f pos

annotatePos :: Parser Annotation
annotatePos = Annotation <$> getSourcePos