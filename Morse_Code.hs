-- Author: Alec Knutsen
-- DATE: April 16, 2016

import Data.List
import Data.List.Split
import Data.Char


-- Converts a String from Morse Code to a corresponding character --
-- Note each character is either followed by one space (seperate letters within the same word)
-- or seven spaces (seperate words)
convertChar :: String -> String
convertChar my_string
     |my_string == ".- " = "A"
     |my_string == "-... " = "B"
     |my_string == "-.-. " = "C"
     |my_string == "-.. " = "D"
     |my_string == ". " = "E"
     |my_string == "..-. " = "F"
     |my_string == "--. " = "G"
     |my_string == ".... " = "H"
     |my_string == ".. " = "I"
     |my_string == ".--- " = "J"
     |my_string == "-.- " = "K"
     |my_string == ".-.. " = "L"
     |my_string == "-- " = "M"
     |my_string == "-. " = "N"
     |my_string == "--- " = "O"
     |my_string == ".--. " = "P"
     |my_string == "--.- " = "Q"
     |my_string == ".-. " = "R"
     |my_string == "... " = "S"
     |my_string == "- " = "T"
     |my_string == "..- " = "U"
     |my_string == "...- " = "V"
     |my_string == ".-- " = "W"
     |my_string == "-..- " = "X"
     |my_string == "-.-- " = "Y"
     |my_string == "--.. " = "Z"
     |my_string == ".---- " = "1"
     |my_string == "..--- " = "2"
     |my_string == "...-- " = "3"
     |my_string == "....- " = "4"
     |my_string == "..... " = "5"
     |my_string == "-.... " = "6"
     |my_string == "--... " = "7"
     |my_string == "---.. " = "8"
     |my_string == "----. " = "9"
     |my_string == "----- " = "0"
     |my_string == ".-       " = "A "
     |my_string == "-...       " = "B "
     |my_string == "-.-.       " = "C "
     |my_string == "-..       " = "D "
     |my_string == ".       " = "E "
     |my_string == "..-.       " = "F "
     |my_string == "--.       " = "G "
     |my_string == "....       " = "H "
     |my_string == "..       " = "I "
     |my_string == ".---       " = "J "
     |my_string == "-.-       " = "K "
     |my_string == ".-..       " = "L "
     |my_string == "--       " = "M "
     |my_string == "-.       " = "N " 
     |my_string == "---       " = "O "
     |my_string == ".--.       " = "P "
     |my_string == "--.-       " = "Q "
     |my_string == ".-.       " = "R "
     |my_string == "...       " = "S "
     |my_string == "-       " = "T "
     |my_string == "..-       " = "U "
     |my_string == "...-       " = "V "
     |my_string == ".--       " = "W "
     |my_string == "-..-       " = "X " 
     |my_string == "-.--       " = "Y "
     |my_string == "--..       " = "Z "
     |my_string == ".----       " = "1 "
     |my_string == "..---       " = "2 "
     |my_string == "...--       " = "3 "
     |my_string == "....-       " = "4 "
     |my_string == ".....       " = "5 "
     |my_string == "-....       " = "6 "
     |my_string == "--...       " = "7 "
     |my_string == "---..       " = "8 "
     |my_string == "----.       " = "9 "
     |my_string == "-----       " = "0 "
     |otherwise = "Error"
     
-- Associates each charactter A-Z,1-9,0, ' ' with a corresponding number 0-36
charToNum :: Char -> Int 
charToNum string_to_convert
    |string_to_convert == 'A' = 0
    |string_to_convert == 'B' = 1
    |string_to_convert == 'C' = 2
    |string_to_convert == 'D' = 3
    |string_to_convert == 'E' = 4
    |string_to_convert == 'F' = 5
    |string_to_convert == 'G' = 6
    |string_to_convert == 'H' = 7
    |string_to_convert == 'I' = 8
    |string_to_convert == 'J' = 9
    |string_to_convert == 'K' = 10
    |string_to_convert == 'L' = 11
    |string_to_convert == 'M' = 12
    |string_to_convert == 'N' = 13
    |string_to_convert == 'O' = 14
    |string_to_convert == 'P' = 15
    |string_to_convert == 'Q' = 16
    |string_to_convert == 'R' = 17
    |string_to_convert == 'S' = 18
    |string_to_convert == 'T' = 19
    |string_to_convert == 'U' = 20
    |string_to_convert == 'V' = 21
    |string_to_convert == 'W' = 22
    |string_to_convert == 'X' = 23
    |string_to_convert == 'Y' = 24
    |string_to_convert == 'Z' = 25
    |string_to_convert == '1' = 26
    |string_to_convert == '2' = 27
    |string_to_convert == '3' = 28
    |string_to_convert == '4' = 29
    |string_to_convert == '5' = 30
    |string_to_convert == '6' = 31
    |string_to_convert == '7' = 32
    |string_to_convert == '8' = 33
    |string_to_convert == '9' = 34
    |string_to_convert == '0' = 35
    |string_to_convert == ' ' = 36
    |otherwise = 100
    
    
-- Associates each charactter 0-36  with a corresponding character A-Z,1-9,0, ' '
numToChar :: Int -> Char
numToChar int_to_convert
    |int_to_convert == 0 = 'A'
    |int_to_convert == 1 = 'B'
    |int_to_convert == 2 = 'C'
    |int_to_convert == 3 = 'D'
    |int_to_convert == 4 = 'E'
    |int_to_convert == 5 = 'F'
    |int_to_convert == 6 = 'G'
    |int_to_convert == 7 = 'H'
    |int_to_convert == 8 = 'I'
    |int_to_convert == 9 = 'J'
    |int_to_convert == 10 = 'K'
    |int_to_convert == 11 = 'L'
    |int_to_convert == 12 = 'M'
    |int_to_convert == 13 = 'N'
    |int_to_convert == 14 = 'O'
    |int_to_convert == 15 = 'P'
    |int_to_convert == 16 = 'Q'
    |int_to_convert ==  17 = 'R'
    |int_to_convert == 18 = 'S'
    |int_to_convert == 19 = 'T'
    |int_to_convert == 20 = 'U'
    |int_to_convert == 21 = 'V'
    |int_to_convert == 22 = 'W'
    |int_to_convert == 23 = 'X'
    |int_to_convert == 24 = 'Y'
    |int_to_convert == 25 = 'Z'
    |int_to_convert == 26 = '1'
    |int_to_convert == 27  = '2'
    |int_to_convert == 28 = '3'
    |int_to_convert == 29 = '4'
    |int_to_convert == 30 = '5'
    |int_to_convert == 31 = '6'
    |int_to_convert == 32 = '7'
    |int_to_convert == 33 = '8'
    |int_to_convert == 34 = '9'
    |int_to_convert == 35 = '0'
    |int_to_convert == 36 = ' '
    |otherwise = 'p'
    
--Takes a string of dots, dashes, and spaces and converts it to a message according to the Morse Code Guidelines    
helperMorse :: String -> Int -> String
helperMorse my_string1 index1 
                           --If we have reached the last character in the morse code string
                           |index1 >= length my_string1 = convertChar(take index1 my_string1 ++ " ")
                           --If we have a valid character and no spaces remain, convert the morse code to a string and recurse on a new array
                           |convertChar(take index1 my_string1) /= "Error" && my_string1!!index1 /= ' ' = convertChar(take index1 my_string1) ++ helperMorse (drop index1 my_string1) 0
                           --If we do not have a valid character or we have an empty space, go to the next index
                           |convertChar(take index1 my_string1) == "Error" || my_string1!!index1 == ' '   = helperMorse my_string1 (index1+1)
                           
-- Converts a String to a list of integers according to charToNum
messageToNum :: String -> Int -> [Int]
messageToNum message index2
                           --Reach the end--
                           |index2 >= length message =[]
                           --Recurse if we have not reached the end prepending the integer--
                           |index2 < length message = charToNum(message!!index2):messageToNum message (index2+1) 
                           
-- Shifts each element in an integer array by a certain shift excluding integers that correspond to empty characters
shiftNums:: Int -> Int -> [Int] -> [Int]
shiftNums shift_num index_start int_array
            --Reached the end of the integer array--
            |index_start >= length int_array = []
            --Empty space, do not change the integer value--
            |int_array!!index_start == 36 = int_array!!index_start: shiftNums shift_num (index_start+1) int_array
            --If the new shift produces an integer that is greater than the maximum allowed value, use mod and prepend
            |(int_array!!index_start +shift_num) >= 36 = ((int_array!!index_start+shift_num) `mod` 36): shiftNums shift_num (index_start+1) int_array
            --If the new shift is in a valid range and not an empty character, shift by the shift and prepend
            |int_array!!index_start /= 36 && (int_array!!index_start +shift_num) < 36 = (int_array!!index_start+shift_num): shiftNums shift_num (index_start+1) int_array           
            

--Convert an integer array to a String accroding to numToChar
intToMessage:: [Int] -> Int -> String
intToMessage int_arr index 
                           --End of integer array
                           |index >= length int_arr =[]
                           --Prepend the converted integer to the array 
                           |index < length int_arr = numToChar(int_arr!!index):intToMessage int_arr (index+1) 
                           

--Function to get the shift number from the Morse Code in a String 
getShift :: String -> String
getShift [] = ""
getShift (x:xs) = if x /= 'M' then x:getShift xs else ""
                           
--Function to convert the shift number from the previous function into an Int
stringtoInt :: String -> Int
stringtoInt xs = read (getShift xs)

--Function to get the String after the M character
getString:: String -> String
getString (x:xs) = if x == 'M' then xs else getString xs


--Uses helperMorse to decode a Morse Code Message into alphanumeric characters  
fromMorseCode :: String -> String 
fromMorseCode my_array = helperMorse my_array 0
                        
                
--Uses messageToNum, shiftNums, and intToMessage to preform a right Ceasar shift by the key value
decodeCipher :: String -> Int -> String 
decodeCipher str key =  (intToMessage (shiftNums key 0 (messageToNum str 0)) 0)

-- Takes in morsecode and converts it to a readable String 
decodeText :: String -> String
decodeText message = decodeCipher (getString(fromMorseCode message)) (stringtoInt(fromMorseCode message))


-- Example Input for Morse Code: 
inputStringA = "..--- -- .- --. -. ..-. -.-. .--. -.-. -... -.- -.-. --.- --.- ----. . -.-. ..- --. .-. ..-. .--- -.-. .-. .-. -.-. .--. --.- ----. .-.. -... .-.. ... -.- ----- -.-. .--. -..- ---.. -.-- --.. .---- ..--- ...-- ....- ..... -...."

main = do
    putStrLn $ "Input: " ++ inputStringA
    putStrLn $ "Before Cesar Shift: " ++ (fromMorseCode inputStringA)
    putStrLn $ "Ciphered: " ++ (decodeText inputStringA)