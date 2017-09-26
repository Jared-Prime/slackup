module Main where

import Slackup.Interrogative

main :: IO [()]
main = do
  participants <- readParticipants "app/participants.dat"
  out <- mapM performInterview participants
  return $ out

readParticipants :: FilePath -> IO [String]
readParticipants filepath = do
  participants <- fmap lines . readFile $ filepath
  return $ participants

performInterview :: String -> IO ()
performInterview subject = do
  answers <- getInterview "app/questions.dat"
  out <- putInterview ("app/answers." ++ subject) answers
  return $ out