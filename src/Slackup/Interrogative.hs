module Slackup.Interrogative where

import Control.Monad

type Question = String
type Answer = String

data Interrogative = Interrogative Question Answer deriving (Show, Eq)

ask :: Question -> Interrogative
ask q = Interrogative q "N/A"

interview = map ask

asked :: Interrogative -> Question
asked (Interrogative q a) = q

answer :: Interrogative -> Answer -> Interrogative
answer (Interrogative q _) a = Interrogative q a

answered :: Interrogative -> Answer
answered (Interrogative q a) = a

showInterrogative :: Interrogative -> String
showInterrogative i = unlines [(asked i), (answered i)]

askQuestion :: Interrogative -> IO Interrogative
askQuestion q = do
  putStrLn . asked $ q
  a <- getLine
  return $ answer q a

getInterview :: FilePath -> IO [Interrogative]
getInterview filepath = do
  questions <- fmap lines . readFile $ filepath
  answers <- mapM askQuestion (interview questions)
  return $ answers

putInterview :: FilePath -> [Interrogative] -> IO ()
putInterview filepath answers = do
  let results = map showInterrogative answers
  {- result <- foldl (++) "Results: " results -}
  let result = unlines results
  out <- writeFile filepath result
  return $ out
