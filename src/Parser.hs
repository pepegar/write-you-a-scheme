module Parser (
        readExpr,
        showVal,
        LispVal(Atom, List, DottedList, Number, String, Bool),
        ThrowsError,
        LispError(
            NumArgs,
            TypeMismatch,
            Parser,
            BadSpecialForm,
            NotFunction,
            UnboundVar,
            Default),
        throwError,
        extractValue,
        trapError
) where

import Control.Monad
import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

instance Show LispVal where show = showVal

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where show = showError

instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either LispError


showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right a) = a

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"


parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '"'
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val
