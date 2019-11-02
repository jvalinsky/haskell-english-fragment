module Main where

import EF3semShort
import EF2synShort
import Model

sent1 :: Sent
sent1 = Sent (The1 (Pl (Plur  Witch))) (VP3 Be Magical)

sent2 :: Sent
sent2 =  Sent (A1 Person) (VP3 Be Magical)

sent3 :: Sent
sent3 = Sent (Empty Alice) (VP3 Be Foolish)

sent4 :: Sent
sent4 = Sent (The1 (Pl (Plur Giant))) (VP3 Be Rusty)

sent5 :: Sent
sent5 = Sent (The1  (Sng Warrior)) (VP3 Be Numerous)

sent6 :: Sent
sent6 = Sent (The1  (Pl (Plur Warrior))) (VP3 Be Numerous)

sent7 :: Sent
sent7 = Sent (The1  (Sng Witch)) (VP3 Be Magical)

sent8 :: Sent
sent8 = Sent (The1  (Sng Sword)) (VP3 Be Rusty)

sent9 :: Sent
sent9 = Sent (The1  (Pl (Plur Witch))) (VP3 Be Magical)

sent10 :: Sent
sent10 = Sent (The1  (Sng Coven)) (VP3 Be Magical)

sent11 :: Sent
sent11 = Sent (A3 (RSCN1 Witch That (VP0 Run))) (VP0 Laugh)

sent12 :: Sent
sent12 = (Sent (The4 Gold_) (VP3 Be Old))

sent13 :: Sent
sent13 = (Sent (The2 Gold (Sng Ring)) (VP3 Be Old))

main :: IO ()
main = do
    putStr "Here are some example sentences and their evaluations: \n"
    putStr "Example for Plural Entity: The witches are magical.\n"
    putStr ("Encoded form: " ++ (show sent1) ++ "\n")
    putStr ("Result: " ++ (show (intSent sent1)) ++ "\n")

    putStr "Example for Singular Entity: A person is magical.\n"
    putStr ("Encoded form: " ++ (show sent2) ++ "\n")
    putStr ("Result: " ++ (show (intSent sent2)) ++ "\n")

    putStr "Example for Proper noun: Alice is foolish.\n"
    putStr ("Encoded form: " ++ (show sent3) ++ "\n")
    putStr ("Result: " ++ (show (intSent sent3)) ++ "\n")

    putStr "Negative Example for Plural Entity: The Giants are Rusty.\n"
    putStr ("Encoded form: " ++ (show sent4) ++ "\n")
    putStr ("Result: " ++ (show (intSent sent4)) ++ "\n")

    putStr "Example for Collective Predicate: The warrior is numerous. \n"
    putStr ("Encoded form: " ++ (show sent5) ++ "\n")
    putStr ("Result: " ++ (show (intSent sent5)) ++ "\n")

    putStr "Example for Collective Predicate: The warriors are numerous. \n"
    putStr ("Encoded form: " ++ (show sent6) ++ "\n")
    putStr ("Result: " ++ (show (intSent sent6)) ++ "\n")

    putStr "Example for 'The': The witch is magical. \n"
    putStr ("Encoded form: " ++ (show sent7) ++ "\n")
    putStr ("Result: " ++ (show (intSent sent7)) ++ "\n")

    putStr "Example for 'The': The sword is rusty. \n"
    putStr ("Encoded form: " ++ (show sent8) ++ "\n")
    putStr ("Result: " ++ (show (intSent sent8)) ++ "\n")

    putStr "Example for 'The': The witches are magical. \n"
    putStr ("Encoded form: " ++ (show sent9) ++ "\n")
    putStr ("Result: " ++ (show (intSent sent9)) ++ "\n")

    putStr "Example for distributive: The Coven is magical. \n"
    putStr ("Encoded form: " ++ (show sent10) ++ "\n")
    putStr ("Result: " ++ (show (intSent sent10)) ++ "\n")
    
    putStr "Example for RCN: A witch that runs laughs. \n"
    putStr ("Encoded form: " ++ (show sent11) ++ "\n")
    putStr ("Result: " ++ (show (intSent sent11)) ++ "\n")

    putStr "Example for Mass: The Gold is old. \n"
    putStr ("Encoded form: " ++ (show sent12) ++ "\n")
    putStr ("Result: " ++ (show (intSent sent12)) ++ "\n")

    putStr "Example for Mass: The Gold Ring is old. \n"
    putStr ("Encoded form: " ++ (show sent13) ++ "\n")
    putStr ("Result: " ++ (show (intSent sent13)) ++ "\n")