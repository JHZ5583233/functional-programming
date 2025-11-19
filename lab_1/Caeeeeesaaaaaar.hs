import Data.Char (ord, chr)

shiftCharacter :: Int -> Char -> Char
shiftCharacter k c
    | a > 90 = chr (a - 26)
    | a < 65 = chr (a + 26)
    | otherwise = chr a
    where
        a = ord c - mod k 26

cipherEncode :: Int -> String -> String
cipherEncode k ms = cipherEncodeHelp k ms 1

cipherEncodeHelp :: Int -> String -> Int -> String
cipherEncodeHelp k ms n
    | null ms = []
    | h == ' ' = ' ' : cipherEncodeHelp k (tail ms) n
    | otherwise = shiftCharacter (k * n) h : cipherEncodeHelp k (tail ms) (n + 1)
    where
        h = head ms

cipherDecode :: Int -> String -> String
cipherDecode k ms = cipherDecodeHelp k ms 1

cipherDecodeHelp :: Int -> String -> Int -> String
cipherDecodeHelp k ms n
    | null ms = []
    | h == ' ' = ' ': cipherDecodeHelp k (tail ms) n
    | otherwise = shiftCharacter (- (k * n)) h : cipherDecodeHelp k (tail ms) (n + 1)
    where
        h = head ms