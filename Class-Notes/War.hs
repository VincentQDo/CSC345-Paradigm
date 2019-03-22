module War where

data Suit = Hearts | Clubs | Diamonds | Spades
    deriving (Eq, Ord, Show)
--Eq will allow the use of ==, Ord will allow the use of < and >, Show will show the datatype as strings
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Eq, Ord, Show)

type Card = (Rank, Suit)
type Deck = [Card]
type Hand = [Card]

--------------------
-- not used
playerOneWinner :: (Hand, Hand) -> Bool
playerOneWinner (_ , [])  = True
playerOneWinner (_ , _)   = False

playerTwoWinner :: (Hand, Hand) -> Bool
playerTwoWinner ([], _)   = True
playerTwoWinner (_ , _)   = False
--------------------

makeDeck :: Deck
-- This will create a deck of cards using list comprehension with 52 cards
makeDeck = [(r,s) | r <- [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace],
                    s <- [Hearts, Clubs, Diamonds, Spades]]

dealCards :: Deck -> (Hand,Hand) -> (Hand,Hand)
-- will return the cards each player have in their hand if the deck is empty
dealCards [] (p1,p2) = (p1,p2)
-- x:y:t means x and y are the first 2 itmes in the list and t is the tail
dealCards (x:y:t) (p1,p2) = dealCards t (x:p1, y:p2)
-- will give error if the list only have 2 items
dealCards _ _ = error "unexpected"

-- need shuffling -- why is this a problem?. This is a problem for functional programming because each input can have only one output


-- how to loop? how to stop game? throw an exception for now
play :: (Hand,Hand) -> (Hand,Hand)
play (p1, p2)
-- when player win the program will crash, if player 1 is empty then throw exception that the other player has won
    | null p1  = error "player 2 wins"
    | null p2  = error "player 1 wins"
    --the first card in player 1 and player 2 will be the one that we will throw down
    | p == 1   = play ( tail p1 ++ [topcardp1, topcardp2] , tail p2 )
    | p == 2   = play ( tail p1, tail p2 ++ [topcardp1, topcardp2] )
    -- | p == 0   = error "war!"   -- tie, "War scenario"
    | p == 0 && r == 1 = play ( tail p1 ++ [topcardp1, topcardp2] , tail p2 )
    | p == 0 && r == 2 = play ( tail p1, tail p2 ++ [topcardp1, topcardp2] )
    | otherwise = error "unexpected"
    where
        topcardp1 = head p1
        topcardp2 = head p2
        p = betterCard topcardp1 topcardp2
        r = betterRank topcardp1 topcardp2


betterCard :: Card -> Card -> Int
--given player 1 and player 2 cards we will return the better card
betterCard (r1,_) (r2,_)
    | r1 > r2   = 1   -- first card better
    | r2 > r1   = 2   -- second card better
    | otherwise = 0   -- tie


betterRank :: Card -> Card -> Int
betterRank c1 c2
    | getSuit c1 > getSuit c2   = 1   -- first card better
    | getSuit c2 > getSuit c1   = 2   -- second card better
    | otherwise                 = error ("unexpected" ++ show c1 ++ " " ++ show c2)

getSuit :: Card -> Suit
getSuit c = snd c

--first gonna deal cards begin with empty hand, what ever dealCards function return will be the input for play
emptyHand = []
run = play (dealCards makeDeck (emptyHand, emptyHand))
