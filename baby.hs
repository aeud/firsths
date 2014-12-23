doubleMe x = x + x
doubleSmallNumber x = (if x < 100 then 2*x else x)
factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)

fn = ceiling . negate . tan . cos . max 50  