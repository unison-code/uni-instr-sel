{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards #-}

import Language.InstructionSelection.CPModel.Json

main :: IO ()
main =
  do json <- readFile "test.json"
     putStrLn $ show $ fromJson json
