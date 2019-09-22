module EF3synShort where

    import Data.List
    
    data Verb = Sing | Laugh | Run | Fight | Push | Play | See deriving Show
    data ProperNoun = Alice | Bob | Charlie | Steve | Bill | Superman deriving Show
    data Noun = Proper ProperNoun | Cat | Wizard | Postman | Teacher deriving Show
    



    

{-

Nouns have grammatical number, in English something is either singular or plural

The giants laughed.
The wizards gave the stone to the giants.
The boys pushed the piano.
The boys pushed the pianos.
The boy pushed the piano.
The dwarves defeated the giants with the swords.
The boys admired the princess.


The water broke through the dam.
The bottle was full of water.
The bottle was full of sand.
The bottle was full of crickets.
The ground was covered in sand.
The ground was covered in crickets.


Rats live on no evil star.
    
    
    
    
    
    
    -}
