module Main (
    main,
) where

import Prelube

import System.Environment (getArgs)
import Text.Read (readMaybe)

import Solutions.Day01 qualified as D1
import Solutions.Day02 qualified as D2
import Solutions.Day03 qualified as D3
import Solutions.Day05 qualified as D5

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Nothing -> error "Could not parse input."
        Just input -> do
            txtInp <- toTxt <$> readFile (inputFile input)
            case pickSolver input of
                Nothing -> error $ "Bad combination of day/part: " <> show input
                Just solver -> putTxtLn . showtSR . solver $ txtInp

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

pickSolver :: Input -> Maybe (Text -> SolverResult)
pickSolver = \case
    Input 1 1 _ -> Just (D1.solve1)
    Input 1 2 _ -> Just (D1.solve2)
    Input 2 1 _ -> Just (D2.solve1)
    Input 2 2 _ -> Just (D2.solve2)
    Input 3 1 _ -> Just (D3.solve1)
    Input 3 2 _ -> Just (D3.solve2)
    Input 5 1 _ -> Just (D5.solve1)
    Input 5 2 _ -> Just (D5.solve2)
    _ -> Nothing
