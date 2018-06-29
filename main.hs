module Main where

set   = "RNBKQBNR\nPPPPPPPP\n........\n........\n........\n........\npppppppp\nrnbkqbnr\n"
errorText = "X  X  X  X  X  X  X  X\ninvalid move try again\nX  X  X  X  X  X  X  X\n"
top = "  1 2 3 4 5 6 7 8 \n"

validate :: String -> Bool
validate xs = all (flip elem "12345678") xs && length xs == 4

move :: String -> String -> String
move board [a,b,c,d] = [(swap i (getIndex a b) (getIndex c d)) |i<-[0..(length board -1)]] where
    getIndex a b = ((8-(read [b]))*9) + ((read [a])-1)
    swap i from to | from == i    = '.'
                   | to   == i    = board !! from
                   | otherwise    = board !! i

render :: String -> String
render xs = top ++ (unlines.rLine.reverse.lines $ xs) ++ top where
    rLine = traverse (\x y -> (show x++y!!(x-1)++show x) >>= (:" ")) [8,7..1]

play = do
    putStrLn $ render set
    let next new old = do
        newMove <- getLine
        case (newMove,(validate newMove)) of
            ("undo",_)  -> putStrLn ("\n" ++ (render old)) >> next old old
            ("reset",_) -> play
            (_,True)    -> putStrLn ("\n" ++ (render (move new newMove))) >> next (move new newMove) new
            (_,False)   -> putStrLn ("\n" ++ errorText) >> next new old                                            
    next set set
