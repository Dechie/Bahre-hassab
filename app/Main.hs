module Main where

main :: IO ()
main = putStrLn "The Ethiopian Calendar calculations"


ameteAlem year = year + 5500
modulo year = year `mod` 4 

findEvangelist :: (Integral a) => a -> String
findEvangelist enteredYear 
    | result <= 0 = "yohannes" 
    | result <= 1 = "matewos" 
    | result <= 2 = "markos" 
    | result <= 3 = "lukas" 
    where result = modulo . ameteAlem $ enteredYear  
 
 
meteneRabit year =  ameteAlem year `div` 4
tinteKemer year = (meteneRabit year + ameteAlem year ) `mod` 7

findStartDay :: (Integral a) => a -> String
findStartDay enteredYear 
    | result <= 0 = "monday" 
    | result <= 1 = "tuesday" 
    | result <= 2 = "wednesday" 
    | result <= 3 = "thursday" 
    | result <= 4 = "friday" 
    | result <= 5 = "saturday" 
    | result <= 6 = "sunday" 
    where result = tinteKemer $ enteredYear  


medeb year = ameteAlem year `mod` 19

wenber year = medeb year - 1 

abektie year = (wenber year * 11) `mod` 30 

metqi year = (wenber year * 19) `mod` 30

-- beale metqi has 2 values, which month it is and what day it is
bealeMetqi :: (Integral a) => a -> String 
bealeMetqi value
  | value >= 15 = "Meskerem"
  | value < 15 = "Tikimt" 

-- implementing beale-metqi to tewsak
-- 1. helper function: day string to day number value
-- the following calculations depend on the day of bealeMetqi. 
-- but there is no straitfoward way to calculate that.
-- for now we will assume that day is given and proceed with calculations
tewsak value 
  | value <= "sunday" = 7
  | value <= "monday" = 6
  | value <= "tuesday" = 5
  | value <= "wednesday" = 4
  | value <= "thursday" = 3
  | value <= "friday" = 2
  | value <= "saturday" = 8

mebaja_hamer year metqi_day = metqi year + tewsak metqi_day

-- returns the number of the day of the year.
-- (120-150 if in tirr, 150-180 if yekatit)
nineveh_day mebaja_hamer bealeMetqi 
  | bealeMetqi >= 15 = (120 + mebaja_hamer)
  | otherwise = (150 + mebaja_hamer)

-- calculaitng holidays
-- pivot point is the day of nineveh
-- again this sends the number of the day of the year.

findTsomeDay pivot cue 
    | cue == "abiy tsome" = pivot + 14 
    | cue == "debre zeyit" = pivot + 41 
    | cue == "hossana" = pivot + 62 
    | cue == "siklet" = pivot + 67 
    | cue == "tinsaye" = pivot + 69 
    | cue == "rikbe kahnat" = pivot + 93 
    | cue == "erget" = pivot + 108 
    | cue == "peraklitos" = pivot + 118 
    | cue == "tsome hawaryat" = pivot + 119 
    | cue == "tsome dihnet" = pivot + 121 
    
-- Note: there is connection whatsoever between these functions
-- in the sense that you can plug absolutely any value to them and t
-- they will give you an answer and the answer is simply correct.
-- just not correct in your context, your data.
-- e.g. you can give it wrong metqi or nineveh and it will give you 
-- some "valid" result, but the result is only wrong in the 
-- context of the year "2016". it has supposed "correct" values
-- but haskell doesn't care about that, it just gives you the answer to 
-- the stupid value you gave it. and maybe lazy evaluation is tied to this too idk
-- there is no flow of data from one function to the next function
-- the flow of data exists only in your head.
-- or if you create some kind of memory with the client app the consumes
-- this app's data (if ever this is to have an API)
