data Point = Point {x :: Int, y :: Int} deriving (Show)

greet :: String -> IO ()
greet name = putStrLn $ "Hi, " ++ name ++ "!"

square :: Int -> Int
square n = n * n

loop :: Int -> IO ()
loop count
  | count < 3 = do
      putStrLn $ "While loop: " ++ show count
      loop (count + 1)
  | otherwise = return ()

main :: IO ()
main = do
  let name = "Alice"
      x = 5
      y = 3.14
      active = True

  greet name
  putStrLn $ "Square of " ++ show x ++ " is " ++ show (square x)

  let p = Point 3 4
  putStrLn $ show p

  let nums = [1, 2, 3]
  mapM_ (\n -> putStr $ show n ++ " ") nums
  putStrLn ""

  let ages = [("Alice", 30), ("Bob", 25)]
  case lookup "Bob" ages of
    Just age -> putStrLn $ "Bob is " ++ show age ++ " years old."
    Nothing -> return ()

  loop 0
