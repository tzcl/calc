import Control.Monad.State
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token

data Expr
  = Const Double
  | Name String
  | Neg Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Mod Expr Expr
  | Add Expr Expr
  | Sub Expr Expr
  deriving (Show)

data Statement
  = Print Expr
  | Assign String
  deriving (Show)

lexer :: TokenParser ()
lexer =
  makeTokenParser
    ( javaStyle
        { opStart = oneOf "+-*/%",
          opLetter = oneOf "+-*/%"
        }
    )

parseNumber :: Parser Expr
parseNumber = do
  val <- naturalOrFloat lexer
  case val of
    Left i -> return $ Const $ fromIntegral i
    Right n -> return $ Const n

parseName :: Parser Expr
parseName = do
  name <- identifier lexer
  return $ Name name

parseExpr :: Parser Expr
parseExpr =
  buildExpressionParser
    [ [ Prefix (reservedOp lexer "-" >> return Neg),
        Prefix (reservedOp lexer "+" >> return id)
      ],
      [ Infix (reservedOp lexer "*" >> return Mul) AssocLeft,
        Infix (reservedOp lexer "/" >> return Div) AssocLeft,
        Infix (reservedOp lexer "%" >> return Mod) AssocLeft
      ],
      [ Infix (reservedOp lexer "+" >> return Add) AssocLeft,
        Infix (reservedOp lexer "-" >> return Sub) AssocLeft
      ]
    ]
    parseTerm

parseTerm :: Parser Expr
parseTerm = parens lexer parseExpr <|> parseNumber <|> parseName

parsePrint :: Parser Statement
parsePrint = do
  reserved lexer "print"
  Print <$> parseExpr

parseInput :: Parser Statement
parseInput = do
  whiteSpace lexer
  s <- parsePrint
  eof
  return s

-- Interpreter
type Calculator a = StateT (M.Map String Expr) IO a

interpretExpr :: Expr -> Calculator Double
interpretExpr (Const n) = return n
interpretExpr (Name n) = do
  varmap <- get
  case M.lookup n varmap of
    Nothing -> fail $ "Uknown identifier: " ++ n
    Just e -> interpretExpr e
interpretExpr (Neg expr) = do
  val <- interpretExpr expr
  return $ negate val
interpretExpr (Mul e1 e2) = do
  v1 <- interpretExpr e1
  v2 <- interpretExpr e2
  return $ v1 * v2
interpretExpr (Div e1 e2) = do
  v1 <- interpretExpr e1
  v2 <- interpretExpr e2
  return $ v1 / v2
interpretExpr (Mod e1 e2) = do
  v1 <- interpretExpr e1
  v2 <- interpretExpr e2
  let n1 = floor v1
      n2 = floor v2
      m = n1 `mod` n2
  return $ fromInteger m
interpretExpr (Add e1 e2) = do
  v1 <- interpretExpr e1
  v2 <- interpretExpr e2
  return $ v1 + v2
interpretExpr (Sub e1 e2) = do
  v1 <- interpretExpr e1
  v2 <- interpretExpr e2
  return $ v1 - v2

interpretStatement :: Statement -> Calculator ()
interpretStatement (Print expr) = do
  n <- interpretExpr expr
  liftIO $ print n

defaultVars :: M.Map String Expr
defaultVars =
  M.fromList
    [ ("pi", Const 3.14)
    ]

calculate :: String -> IO ()
calculate s =
  case ret of
    Left e -> putStrLn $ "error: " ++ show e
    Right n -> evalStateT (interpretStatement n) defaultVars
  where
    ret = parse parseInput "calculator" s

main :: IO ()
main = getContents >>= mapM_ calculate . lines
