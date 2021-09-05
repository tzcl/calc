import Control.Monad.Error
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
  | Function String Expr
  | Conditional Condition Expr Expr
  deriving (Show)

data Condition
  = And Condition Condition
  | Or Condition Condition
  | Not Condition
  | Equal Expr Expr
  | LessThan Expr Expr
  | LessThanEqual Expr Expr
  deriving (Show)

data FunctionBody = FunctionBody String Expr
  deriving (Show)

data Statement
  = Print Expr
  | Assignment String Expr
  | Definition String FunctionBody
  deriving (Show)

lexer :: TokenParser ()
lexer =
  makeTokenParser
    ( javaStyle
        { opStart = oneOf "+-*/%|&=!<>",
          opLetter = oneOf "+-*/%|&=!<>",
          reservedNames = ["let", "def", "print", "if", "then", "else"]
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

parseFunction :: Parser Expr
parseFunction = do
  ident <- identifier lexer
  expr <- parens lexer parseExpr
  return $ Function ident expr

parseConditional :: Parser Expr
parseConditional = do
  reserved lexer "if"
  c <- parseCondition
  reserved lexer "then"
  e1 <- parseExpr
  reserved lexer "else"
  Conditional c e1 <$> parseExpr

parseCondition :: Parser Condition
parseCondition =
  buildExpressionParser
    [ [Prefix (reservedOp lexer "~" >> return Not)],
      [ Infix (reservedOp lexer "&&" >> return And) AssocLeft,
        Infix (reservedOp lexer "||" >> return Or) AssocLeft
      ]
    ]
    parseConditionalTerm

parseConditionalTerm :: Parser Condition
parseConditionalTerm =
  parens lexer parseCondition <|> parseComparison

parseComparison :: Parser Condition
parseComparison = do
  e1 <- parseExpr
  f <-
    (reserved lexer "==" >> return (Equal e1))
      <|> (reserved lexer "<" >> return (LessThan e1))
      <|> (reserved lexer "<=" >> return (LessThanEqual e1))
      <|> (reserved lexer ">" >> return (Not . LessThanEqual e1))
      <|> (reserved lexer ">=" >> return (Not . LessThan e1))
      <|> (reserved lexer "!=" >> return (Not . Equal e1))
  f <$> parseExpr

parseTerm :: Parser Expr
parseTerm = parens lexer parseExpr <|> parseNumber <|> try parseFunction <|> try parseConditional <|> parseName

parsePrint :: Parser Statement
parsePrint = do
  reserved lexer "print"
  Print <$> parseExpr

parseAssignment :: Parser Statement
parseAssignment = do
  reserved lexer "let"
  ident <- identifier lexer
  reservedOp lexer "="
  Assignment ident <$> parseExpr

parseDefinition :: Parser Statement
parseDefinition = do
  reserved lexer "def"
  ident <- identifier lexer
  argname <- parens lexer $ identifier lexer
  reservedOp lexer "="
  Definition ident . FunctionBody argname <$> parseExpr

parseInput :: Parser Statement
parseInput = do
  whiteSpace lexer
  s <- parsePrint <|> parseAssignment <|> parseDefinition
  eof
  return s

-- Interpreter
type StoredVal = Either Double FunctionBody

type Calculator = StateT (M.Map String StoredVal) IO

type SafeCalculator a = ErrorT String Calculator a

interpretCondition :: Condition -> SafeCalculator Bool
interpretCondition (Not c) = interpretCondition c >>= return . not
interpretCondition (And c1 c2) = do
  b1 <- interpretCondition c1
  b2 <- interpretCondition c2
  return (b1 && b2)
interpretCondition (Or c1 c2) = do
  b1 <- interpretCondition c1
  b2 <- interpretCondition c2
  return (b1 || b2)
interpretCondition (Equal e1 e2) = do
  v1 <- interpretExpr e1
  v2 <- interpretExpr e2
  return (v1 == v2)
interpretCondition (LessThan e1 e2) = do
  v1 <- interpretExpr e1
  v2 <- interpretExpr e2
  return (v1 < v2)
interpretCondition (LessThanEqual e1 e2) = do
  v1 <- interpretExpr e1
  v2 <- interpretExpr e2
  return (v1 <= v2)

interpretExpr :: Expr -> SafeCalculator Double
interpretExpr (Const n) = return n
interpretExpr (Name n) = do
  varmap <- get
  case M.lookup n varmap of
    Nothing -> fail $ "Unknown identifier: " ++ n
    Just (Left n) -> return n
    Just (Right _) -> fail ("You must call function " ++ n)
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
interpretExpr (Function fn e) = do
  context <- get
  case M.lookup fn context of
    Nothing -> fail ("Unknown function: " ++ fn)
    Just (Left _) -> fail ("Cannot call constant: " ++ fn)
    Just (Right (FunctionBody argname expr)) -> do
      n <- interpretExpr e
      modify (M.insert argname (Left n))
      r <- interpretExpr expr
      put context
      return r
interpretExpr (Conditional cond e1 e2) = do
  b <- interpretCondition cond
  if b then interpretExpr e1 else interpretExpr e2

interpretStatement :: Statement -> SafeCalculator ()
interpretStatement (Print expr) = do
  n <- interpretExpr expr
  liftIO $ print n
interpretStatement (Assignment ident expr) = do
  n <- interpretExpr expr
  modify (M.insert ident (Left n))
interpretStatement (Definition fn body) =
  modify (M.insert fn (Right body))

defaultVars :: M.Map String StoredVal
defaultVars =
  M.fromList
    [ ("pi", Left 3.14)
    ]

calculate :: String -> Calculator ()
calculate s =
  case ret of
    Left e -> liftIO $ putStrLn $ "error: " ++ show e
    Right n -> do
      res <- runErrorT $ interpretStatement n
      case res of
        Left e' -> liftIO $ putStrLn $ "run error: " ++ e'
        Right _ -> return ()
  where
    ret = parse parseInput "calculator" s

calculator :: Calculator ()
calculator =
  liftIO getContents >>= mapM_ calculate . lines

main :: IO ()
main = evalStateT calculator defaultVars
