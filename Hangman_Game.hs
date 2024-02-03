{- Logical and Functional Programming Project a.y. 2023-2024
   Lecturer: Prof. Marco Bernardo
   Students: 
            Tommaso Remedi  - 300535 
            Riccardo Monaco - 300537 -}

{- Haskell program to play the hangman game. -} 

import System.Random -- needed to randomly extract a number as the index of the word to guess
import Data.List -- needed to use nub, which removes duplicate items from a list
import System.Console.ANSI -- needed to clean the console
import System.IO -- needed to acquire the user input 

main :: IO ()
main = envSetup 6

{- Words to guess list. -}

wordsToGuess :: [String]
wordsToGuess = ["haskell", "programmazione", "funzionale", "linguaggio", "computazione"]

{- The function envSetup sets the game environment, picking the word to guess. -}

envSetup :: IO ()
envSetup = do
    clearScreen
    putStrLn "Welcome to the Hangman Game!"
    putStrLn $ "Initial state: \n" ++ drawHangman 0
    wordToGuess <- selectWord
    play wordToGuess [] remainingAttempts

{- The function selectWord randomly selects a word from the given list: -}

selectWord :: IO String
selectWord = do
    index <- randomRIO (0, length wordsToGuess - 1)
    return $ wordsToGuess !! index

{- The function play manages the game, it updates the guessed letters and the attempts:
   - The first parameter stands for the word to guess;
   - The second parameter stands for the letters already guessed;
   - The third parameter stands for the remaining attempts. -}

play :: String -> String -> Int -> IO ()
play wordToGuess guessedLetters remainingAttempts
    | checkGuessed wordToGuess guessedLetters = handleWin wordToGuess
    | remainingAttempts == 0 = handleLoss wordToGuess
    | otherwise = handleInProgress wordToGuess guessedLetters remainingAttempts

{- The function handleLoss handles the case when the user runs out of attempts:
   - The first parameter stands for the word to guess. -}

handleLoss :: String -> IO ()
handleLoss wordToGuess = do
    clearScreen
    putStr (drawHangman 0)
    putStrLn "You've Lost! Your hangman has been hanged."
    putStrLn $ "The secret word was: " ++ wordToGuess

{- The function handleWin handles the case when the user wins:
   - The first parameter stands for the word to guess. -}

handleWin :: String -> IO ()
handleWin wordToGuess = do
    clearScreen
    putStrLn $ "You've guessed it! The secret word was: " ++ wordToGuess

{- The function handleInProgress handles the in-progress scenario:
   - The first parameter stands for the word to guess;
   - The second parameter stands for the letters already guessed;
   - The third parameter stands for the remaining attempts. -}

handleInProgress :: String -> String -> Int -> IO ()
handleInProgress wordToGuess guessedLetters remainingAttempts = do
    clearScreen
    putStrLn $ "Current word: " ++ renderWord wordToGuess guessedLetters
    putStr (drawHangman remainingAttempts)
    putStrLn $ "Remaining attempts: " ++ show remainingAttempts
    insertedLetter <- readFirstChar
    updateAttempt wordToGuess guessedLetters insertedLetter remainingAttempts

{- The function readFirstChar reads and checks the validity of the input:
   - The first parameter stands for the first inserted char. -}

readFirstChar :: IO Char
readFirstChar = do
    putStr "Guess a letter: "
    hFlush stdout
    char <- getChar
    _ <- getLine
    if char `elem` ['a'..'z']
        then return char
        else do
            putStrLn "Please enter a valid lowercase letter."
            readFirstChar

{- The function updateAttempt updates the guessed letters list with eventually a new one:
   - The first parameter stands for the word to guess;
   - The second parameter stands for the letters already guessed;
   - The third parameter stands for the letter the user has inserted;
   - The fourth parameter stands for the remaining attempts. -}

updateAttempt :: String -> String -> Char -> Int -> IO ()
updateAttempt wordToGuess guessedLetters insertedLetter remainingAttempts
    | insertedLetter `elem` wordToGuess = play wordToGuess (nub $ guessedLetters ++ [insertedLetter]) remainingAttempts
    | otherwise = play wordToGuess guessedLetters (remainingAttempts - 1)

{- The function checkGuessed returns true if all the letters have been guessed:
   - The first parameter stands for the word to guess;
   - The second parameter stands for the letters already guessed. -}

checkGuessed :: String -> String -> Bool
checkGuessed wordToGuess guessedLetters =
    all (`elem` guessedLetters) wordToGuess

{- The function renderWord prints a letter if it is found in the word,
   an underscore if not:
   - The first parameter stands for the letters of the word to guess;
   - The second parameter stands for the letters already guessed. -}

renderWord :: String -> String -> String
renderWord wordToGuess guessedLetters =
    [if c `elem` guessedLetters || c == ' ' then c else '_' | c <- wordToGuess]

{- The function drawHangman prints the characters to draw the countours of the hangman:
   - The first parameter stands for the remaining attempts. -}

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

