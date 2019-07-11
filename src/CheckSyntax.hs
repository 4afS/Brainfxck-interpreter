module CheckSyntax where

data Result
  = OK
  | Error ErrorMessage
  deriving Eq

data ErrorMessage
  = InvalidParenthesis
  deriving (Eq, Show)

isInvalid :: String -> Bool
isInvalid source  = all (== OK)$ do
  checkInvalid <- checkInvalids
  return $ checkInvalid source

checkInvalids :: [String -> Result]
checkInvalids =
  [ checkParenthesis
  ]

checkParenthesis :: String -> Result
checkParenthesis source =
  if null (deleteParentheses source)
     then OK
     else Error InvalidParenthesis

hasParenthesis :: String -> Bool
hasParenthesis [_]          = False
hasParenthesis []          = False
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
