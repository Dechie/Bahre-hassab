module Main where

main :: IO ()
main = putStrLn "Ethiopian Calendar calculations"


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
abiy_tsome pivot = pivot + 14

debre_zeit pivot = pivot + 41

hossana pivot = pivot + 62

siklet pivot = pivot + 67

tinsaye pivot = pivot + 69

rikbe_kahnat pivot = pivot + 93

erget pivot = pivot + 108

peraklitos pivot = pivot + 118

tsome_hawaryat pivot = pivot + 119

tsome_dihnet pivot = pivot + 121

