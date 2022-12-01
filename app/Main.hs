module Main (
    main,
) where

import Prelube

import Text.Read (readMaybe)

import Solutions.Day1 qualified as D1


main :: IO ()
main = do
    inp <- getLine  -- TODO: replace with some option parsing library
    case parseInput inp of
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

parseInput :: String -> Maybe Input
parseInput s = do
    [dayStr, partStr, fpStr] <- pure $ words s
    day <- readMaybe dayStr
    part <- readMaybe partStr
    pure (Input day part fpStr)

pickSolver :: Input -> Maybe (Text -> Text)
pickSolver = \case 
    Input 1 1 _ -> Just (D1.solve1)
    Input 1 2 _ -> Just (D1.solve2)

    _ -> Nothing
