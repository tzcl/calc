import Text.Parsec
import Text.Parsec.String

parseNumber :: Parser Int
parseNumber = do
  neg <- (char '-' >> return "-") <|> return ""
  n' <- many1 $ oneOf "0123456789"
  return $ read (neg ++ n')

parseAddition :: Parser Int
parseAddition = do
  n1 <- parseNumber
  char '+'
  n2 <- parseNumber
  return (n1 + n2)

calculation :: Parser Int
calculation = do
  try parseAddition <|> parseNumber

calculate :: String -> String
calculate s =
  case ret of
    Left e -> "error: " ++ show e
    Right n -> "answer: " ++ show n
  where
    ret = parse calculation "" s

main :: IO ()
main = interact (unlines . map calculate . lines)
