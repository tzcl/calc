import Text.Parsec
import Text.Parsec.String

parseNumber :: Parser Int
parseNumber = do
  neg <- try $ char '-'
  n' <- many1 $ oneOf "0123456789"
  return $ read (neg : n')

calculation :: Parser Int
calculation = do
  parseNumber

calculate :: String -> String
calculate s =
  case ret of
    Left e -> "error: " ++ show e
    Right n -> "answer: " ++ show n
  where
    ret = parse calculation "" s

main :: IO ()
main = interact (unlines . map calculate . lines)
