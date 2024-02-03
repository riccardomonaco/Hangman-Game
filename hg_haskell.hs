{- Logical and Functional Programming Project a.y. 2023-2024
   Lecturer: Prof. Marco Bernardo
   Students: 
            Tommaso Remedi  - 300535 
            Riccardo Monaco - 300537 -}

-- Haskell program to play the hangman game 

import System.Random
import System.Console.ANSI
import Data.List (nub)
import System.IO

-- List of secret words to guess
secretWords :: [String]
secretWords = ["haskell", "programmazione", "funzionale", "linguaggio", "computazione"]

main :: IO ()
main = env_setup

-- Function to set up the game environment, picking the word to guess
env_setup :: IO ()
env_setup = do
    clearConsole
    putStrLn "Welcome to the Hangman Game!"
    putStrLn $ "Initial state: \n" ++ drawHangman 0
    secretWord <- selectSecretWord
    playGame secretWord [] 6

-- Function to select a random secret word
selectSecretWord :: IO String
selectSecretWord = do
    index <- randomRIO (0, length secretWords - 1)
    return $ secretWords !! index

-- Function to play the game
playGame :: String -> String -> Int -> IO ()
playGame secretWord currentAttempt remainingAttempts
    | guessedWord secretWord currentAttempt = handleWin secretWord
    | remainingAttempts == 0 = handleLoss secretWord
    | otherwise = handleInProgress secretWord currentAttempt remainingAttempts

-- Function to handle the loss scenario
handleLoss :: String -> IO ()
handleLoss secretWord = do
    clearConsole
    putStr (drawHangman 0)
    putStrLn "You've Lost! Your hangman has been hanged."
    putStrLn $ "The secret word was: " ++ secretWord

-- Function to handle the win scenario
handleWin :: String -> IO ()
handleWin secretWord = do
    clearConsole
    showGameState secretWord
    putStrLn $ "You've guessed it! The secret word was: " ++ secretWord

-- Function to handle the in-progress scenario
handleInProgress :: String -> String -> Int -> IO ()
handleInProgress secretWord currentAttempt remainingAttempts = do
    clearConsole
    putStrLn $ "Current word: " ++ displayWord secretWord currentAttempt
    putStr (drawHangman remainingAttempts)
    putStrLn $ "Remaining attempts: " ++ show remainingAttempts
    letter <- readFirstChar
    executeAttempt secretWord currentAttempt letter remainingAttempts

-- Function to handle reading the first character
readFirstChar :: IO Char
readFirstChar = do
    putStr "Guess a letter: "
    hFlush stdout
    char <- getChar
    _ <- getLine  -- Clear the input buffer
    if char `elem` ['a'..'z']
        then return char
        else do
            putStrLn "Please enter a valid lowercase letter."
            readFirstChar

-- Function to update the current attempt based on the guessed letter
executeAttempt :: String -> String -> Char -> Int -> IO ()
executeAttempt secretWord currentAttempt letter remainingAttempts
    | letter `elem` secretWord = playGame secretWord (nub $ currentAttempt ++ [letter]) remainingAttempts
    | otherwise = playGame secretWord currentAttempt (remainingAttempts - 1)

-- Function to show the current game state
showGameState :: String -> IO ()
showGameState secretWord = putStrLn $ "Current word: " ++ secretWord

-- Function to check if the word has been guessed
guessedWord :: String -> String -> Bool
guessedWord secretWord currentAttempt =
    all (`elem` currentAttempt) secretWord

-- Function to display the word with guessed letters or underscores
displayWord :: String -> String -> String
displayWord secretWord currentAttempt =
    [if c `elem` currentAttempt || c == ' ' then c else '_' | c <- secretWord]

-- Function to draw the hangman based on the remaining attempts
drawHangman :: Int -> String
drawHangman remainingAttempts =
    unlines $
        [ "  +---+"
        , "  |   |"
        ] ++
        drawCase remainingAttempts ++
        ["========="]
  where
    drawCase 6 = ["      |", "      |", "      |"]
    drawCase 5 = ["  O   |", "      |", "      |"]
    drawCase 4 = ["  O   |", "  |   |", "      |"]
    drawCase 3 = ["  O   |", " /|   |", "      |"]
    drawCase 2 = ["  O   |", " /|\\  |", "      |"]
    drawCase 1 = ["  O   |", " /|\\  |", " /    |"]
    drawCase 0 = ["  O   |", " /|\\  |", " / \\  |"]
    drawCase _ = []

-- Function to clear the console
clearConsole :: IO ()
clearConsole = clearScreen >> setCursorPosition 0 0
