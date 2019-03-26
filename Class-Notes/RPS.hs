{-
Richard Burns

Two player Rock, Paper, Scissors game.

based on Thompson, Ch. 8
-}

module RPS where


data Move = Rock | Paper | Scissors
    deriving (Show, Eq)

-- to record the series of moves by each player
type Tournament = ([Move], [Move])



-- playing interactively
play :: IO ()
play = playInteractive ([], [])
-- helper function
playInteractive :: Tournament -> IO ()
playInteractive (p1History, p2History) =
    do
        putStr "Player 1: "
        ch1 <- getChar
        putStr "\nPlayer 2: "
        ch2 <- getChar
        if not (ch1 `elem` "rpsRPS" && ch2 `elem` "rpsRPS")
            then showResults (p1History, p2History)   -- end game
            else do let p1Move = convertMove ch1
                    let p2Move = convertMove ch2
                    putStrLn ("\nP1 played: " ++ show p1Move ++ " P2 played: " ++ show p2Move)
                    let currentOutcome = outcome p1Move p2Move
                    case currentOutcome of
                      0 -> putStrLn "Tie that round"
                      1 -> putStrLn "P1 won that round"
                      -1 -> putStrLn "P2 won that round"
                    playInteractive (p1Move:p1History, p2Move:p2History)   -- game loop


convertMove :: Char -> Move
convertMove ch
  = case ch of
    'r' -> Rock
    'R' -> Rock
    'p' -> Paper
    'P' -> Paper
    's' -> Scissors
    'S' -> Scissors


showResults :: Tournament -> IO ()
showResults (mine, yours)
    | sum > 0   = putStrLn ("\nP1 won overall: " ++ show sum)
    | sum < 0   = putStrLn ("\nP2 won overall: " ++ show sum)
    | otherwise = putStrLn ("\nTie game overall!")
    where
        sum = tournamentOutcome (mine, yours)

-- Outcome of a play
--   +1 for first player wins
--   -1 for second player wins
--    0 for a draw
outcome :: Move -> Move -> Integer
outcome Rock Rock = 0
outcome Rock Paper = -1
outcome Rock Scissors = 1
outcome Paper Rock = 1
outcome Paper Paper = 0
outcome Paper Scissors = -1
outcome Scissors Rock = -1
outcome Scissors Paper = 1
outcome Scissors Scissors = 0



tournamentOutcome :: Tournament -> Integer
tournamentOutcome (moves1,moves2)
  = sum [ outcome move1 move2 | (move1,move2) <- zip moves1 moves2 ]
