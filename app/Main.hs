module Main (
    main,
) where

import Prelube

import System.Environment (getArgs)
import Text.Read (readMaybe)

import Solutions.Day1 qualified as D1


main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Nothing -> error "Could not parse input."
        Just input -> do
            txtInp <- toTxt <$> readFile (inputFile input)
            case pickSolver input of
                Nothing -> error $ "Bad combination of day/part: " <>  show input
                Just solver -> putTxtLn $ solver txtInp

data Input = Input
    { inputDay :: Int
    , inputPart :: Int
    , inputFile :: String
    }
    deriving (Eq, Show)

parseArgs :: [String] -> Maybe Input
parseArgs = \case
    [dayStr, partStr, fpStr] -> do
        day <- readMaybe dayStr
        part <- readMaybe partStr
        pure (Input day part fpStr)
    _ -> Nothing

pickSolver :: Input -> Maybe (Text -> Text)
pickSolver = \case 
    Input 1 1 _ -> Just (D1.solve1)
    Input 1 2 _ -> Just (D1.solve2)

    _ -> Nothing
