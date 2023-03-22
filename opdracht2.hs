module Opdracht2 where

-- import Data.Char for Opdracht 2.4
import Data.Char

-- Opdracht 2.1a
euclid :: Integer -> Integer -> Integer
euclid x y =
    if y `rem` x == 0 then x else euclid (y `rem` x) x

-- Opdracht 2.1b
egcd :: Integer -> Integer -> (Integer,Integer,Integer)
egcd 0 b = (b, 0 ,1)
egcd a b =
    let (g, s, t) = egcd (b `mod` a) a
    in (g, t - (b `div` a) * s, s)

egcdAlwaysPositive :: Integer -> Integer -> (Integer,Integer,Integer)
egcdAlwaysPositive a b = 
    let (g, s, t) = egcd a b
    in if s < 0 || t < 0 then (g, s+a*b, t+a*b) else (g, s, t)

-- Opdraht 2.2
m :: Integer -> Integer -> Integer
m p q = p * q

eulersTotient :: Integer -> Integer -> Integer
eulersTotient p q = (p - 1) * (q - 1)

e :: Integer -> Integer -> Integer
e p q = [e|e<-[1..eulersTotient p q -1], euclid e (eulersTotient p q) == 1] !! 1
-- the `!! 1` suffix means that the second from the list is returned

-- `In order for the congruence relation a ≡ b (mod n) to hold, n must divide a - b`
congruency :: Integer -> Integer -> Integer -> Bool
congruency a b n = rem (a - b) n == 0

d :: Integer -> Integer -> Integer
d e eulersTotient = [d|d<-[1..100000], congruency (d*e) 1 eulersTotient] !! 1
-- the `!! 1` suffix means that the second from the list is returned

-- Opdracht 2.3a
rsaencrypt :: (Integer, Integer) -> Integer -> Integer
rsaencrypt (e, m) x = (x ^ e) `mod` m

-- Opdracht 2.3b
rsadecrypt :: (Integer, Integer) -> Integer -> Integer
rsadecrypt (d, m) x = (x ^ d) `mod` m

-- Opdracht 2.4
encryptchar :: (Integer, Integer) -> Char -> Integer
encryptchar (e, m) x = rsaencrypt (e, m) (toInteger ( ord x))

decryptchar :: (Integer, Integer) -> Integer -> Char
decryptchar (d, m) x = chr (fromInteger (rsadecrypt (d, m) x))

-- Opdracht 2.5

{--

Generate public and private keys.

pA = 19
qA = 23

pB = 17
qB = 103

mA = m pA qA
eA = e pA qA
dA = d eA (eulersTotient pA qA)

mB = m pB qB
eB = e pB qB
dB = d eB (eulersTotient pB qB)

message = 23

    ghci> rsaencrypt (dB, mB) (rsaencrypt (eA, mA) message)
    1134
    ghci> dA
    713
    ghci> mA
    437
    ghci> dB
    2285
    ghci> mB
    1751
    ghci> rsadecrypt (dA, mA) (rsadecrypt (eB, mB) 1134)
    ???

    The encrypted message `1134` cannot be decrypted without the private keys.
    The only things known are: public keys and modulo`s.
--}

-- Opdracht 2.6

{--

We take the example given in Opdracht 2.5, but add a `man in the middle`: Charlie.
Again, we generate public and private keys for Alice and Bob.

pA = 19
qA = 23

pB = 17
qB = 103

mA = m pA qA
eA = e pA qA
dA = d eA (eulersTotient pA qA)

mB = m pB qB
eB = e pB qB
dB = d eB (eulersTotient pB qB)

Now Charlie generates 2 pairs; one for Alice and one for Bob.

pCa = 7
qCa = 89

pCb = 73
qCb = 53

mCa = m pCa qCa
eCa = e pCa qCa
dCa = d eCa (eulersTotient pCa qCa)

mCb = m pCb qCb
eCb = e pCb qCb
dCb = d eCb (eulersTotient pCb qCb)

We have the same message.

message = 23

Alice thinks she encrypts the message with Bobs public key, but she uses Charlie`s `Bob` public key.

    ghci> rsaencrypt (dCb, mCb) (rsaencrypt (eA, mA) message)
    3047

The message is send to Charlie, while Alice thinks the message is directly send to Bob.
Charlie decrypts the message.

    ghci> rsadecrypt (dA, mA) (rsadecrypt (eCb, mCb) 3047)
    23

Charlie can do this because Alice believes he is Bob and he used his public key.
Charlie reads the message and encrypts it for Bob.

    ghci> rsaencrypt (dB, mB) (rsaencrypt (eCa, mCa) message)
    228

Bob receives the encrypted message and believes it to be encrypted by Alice.
He decrypts the message to read it.

    ghci> rsadecrypt (dCa, mCa) (rsadecrypt (eB, mB) 228)
    23

The original message was 23, so the message is not corrupted in this case. However, Charlie could send a different message to Bob than it recieved from Alice.

--}
