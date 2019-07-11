module CheckSyntax where

data Result
  = OK
  | Error ErrorMessage
  deriving (Show, Eq)

data ErrorMessage
  = InvalidParenthesis
  deriving (Eq, Show)

isInvalid :: String -> Bool
isInvalid = not . null . checkInvalids

checkInvalids :: String -> [Result]
checkInvalids source = filter (/= OK) $ do
  checkInvalid <- checkingInvalids
  return $ checkInvalid source

checkingInvalids :: [String -> Result]
checkingInvalids =
  [ checkParenthesis
  ]

checkParenthesis :: String -> Result
checkParenthesis source =
  if null (deleteParentheses (filter (`elem` "[]") source))
     then OK
     else Error InvalidParenthesis

hasParenthesis :: String -> Bool
hasParenthesis [_]          = False
hasParenthesis []           = False
hasParenthesis ('[':']':xs) = True
hasParenthesis (x:xs)       = hasParenthesis xs

deleteParenthesis :: String -> String
deleteParenthesis []           = []
deleteParenthesis ('[':']':xs) = xs
deleteParenthesis (x:xs)       = x : deleteParenthesis xs

deleteParentheses :: String -> String
deleteParentheses source
  | hasParenthesis source = deleteParentheses $ deleteParenthesis source
  | otherwise = source
