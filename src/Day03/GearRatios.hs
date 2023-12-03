-- Day 3: Gear Ratios

module Day03.GearRatios (solve) where

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  putStrLn contents
