
-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

-- ==================================================

module Tasks where

import Dataset
import Data.List
import Text.Printf
import Data.Array

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]
type ColumnName = String

-- Prerequisities
split_by :: Char -> String -> [String]
split_by x = foldr op [""]
  where op char acc
            | char == x = "":acc
            | otherwise = (char:head(acc)):tail(acc)

read_csv :: CSV -> Table
read_csv = (map (split_by ',')) . (split_by '\n')

write_csv :: Table -> CSV
write_csv = (foldr (++) []).
            (intersperse "\n").
            (map (foldr (++) [])).
            (map (intersperse ","))


{-
    TASK SET 1
-}

oneList :: [String] -> [String] -> [[String]]
oneList [] _ = []
oneList (x:xs) (y:ys) = [[x,y]] ++ (oneList xs ys)

operation :: [Int] -> String
operation l = printf "%.2f" ((fromIntegral (foldr (+) 0 l))/(fromIntegral 8) :: Float)  

compute_average_steps :: Table -> Table
compute_average_steps m = ["Name","Average Number of Steps"] : (oneList (map head (tail m)) (map operation (map (map (read::String->Int)) (map tail (tail m)))))

-- Number of people who have achieved their goal:
get_passed_people_num :: Table -> Int
get_passed_people_num m = goals (map (foldr (+) 0) (map (map (read::String->Int)) (map tail (tail m))))

goals l = length(filter (>1000) l)

-- Percentage of people who have achieved their:
get_passed_people_percentage :: Table -> Float
get_passed_people_percentage m = (fromIntegral (get_passed_people_num m))/(fromIntegral (length(tail m)))


-- Average number of daily steps
get_steps_avg :: Table -> Float
get_steps_avg m = (fromIntegral (foldr (+) 0 (map (foldr (+) 0) (map (map (read::String->Int)) (map tail (tail m))))))/(fromIntegral (length(tail m)))

-- Task 3

printString :: [Float] -> [String]
printString l = map (\x -> printf "%.2f" x) l

get_num :: Table -> [[Int]]
get_num m = map (map (read::String->Int)) (map tail (tail m))

get_sum :: [[Int]] -> [Float]
get_sum m = map (/(fromIntegral (length m))) (map (fromIntegral) (foldr (\xs ys -> zipWith (+) xs ys) [0,0,0,0,0,0,0,0] m))

get_avg_steps_per_h :: Table -> Table
get_avg_steps_per_h m = ["H10","H11","H12","H13","H14","H15","H16","H17"] : (printString ((get_sum (get_num m))) : [])

-- Task 4

printString2 :: [Float] -> [String]
printString2 l = map (\x -> printf "%.0f" x) l

get3 :: Table -> [[Int]]
get3 m = map (map (read::String->Int)) (map (drop 3) (tail m))

count_workout :: [Int] -> [[Char]]
count_workout m = printString2 (map fromIntegral [range1 m,range2 m,range3 m])

range1 l = length(filter (>=0) (filter (<50) l))
range2 l = length(filter (<100) (filter (>=50) l)) 
range3 l = length(filter (<500) (filter (>=100) l))  

get_activ_summary :: Table -> Table
get_activ_summary m = ["column","range1","range2","range3"]:(["VeryActiveMinutes"] ++ count_workout (map head (get3 m))):(["FairlyActiveMinutes"] ++ count_workout (map (!! 1) (get3 m))):(["LightlyActiveMinutes"] ++ count_workout (map last (get3 m))):[]

-- Task 5

cmp_steps a b  
  | ((read::String->Int) (((!! 1)) a)) < ((read::String->Int) (((!! 1)) b)) = LT
  | ((read::String->Int) (((!! 1)) a)) > ((read::String->Int) (((!! 1)) b)) = GT
  | otherwise = cmp_people a b

cmp_people a b
  | (head a) < (head b) = LT
  | (head a) > (head b) = GT


get_ranking :: Table -> Table
get_ranking m = ["Name","Total Steps"] : (sortBy cmp_steps (map (take 2) (tail m)))

cmp_diff a b  
  | ((read::String->Double) (((!! 3)) a)) < ((read::String->Double) (((!! 3)) b)) = LT
  | ((read::String->Double) (((!! 3)) a)) > ((read::String->Double) (((!! 3)) b)) = GT
  | otherwise = cmp_people a b

-- Task 6
operation1 :: [Int] -> Float
operation1 l = ((fromIntegral (foldr (+) 0 l))/(fromIntegral 4) :: Float)  

get_first :: [[String]] -> [[Int]]
get_first m = map(map (read::String->Int)) (map (take 4) m)

get_last :: [[String]] -> [[Int]]
get_last m = map(map (read::String->Int)) (map (drop 4) m)

get_steps_diff_table :: Table -> Table
get_steps_diff_table m = ["Name","Average first 4h","Average last 4h","Difference"] : (sortBy cmp_diff (zipWith (++) (oneList (map head (tail m)) (printString (map operation1 (get_first (map tail (tail m)))))) (oneList (printString (map operation1 (get_last (map tail (tail m))))) (printString (map abs (zipWith (-) (map operation1 (get_first (map tail (tail m)))) (map operation1 (get_last (map tail (tail m))))))))))

-- Task 7

-- Applies the given function to all the values
vmap :: (Value -> Value) -> Table -> Table
vmap f m = map (map f) m


-- Task 8

rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f s m = undefined

get_sleep_total :: Row -> Row
get_sleep_total r = [head r] ++ (printString [fromIntegral (foldr (+) 0 (map (read::String->Int) (tail r)))]) ++ []


{-
    TASK SET 2
-}

-- Task 1

get_position :: ColumnName -> Table -> Int
get_position column table
  | elemIndex column (head table) /= Nothing = (read::String->Int) (maybe "" show (elemIndex column (head table)))
  | otherwise = -1

cmp_columns a b
  | ((!! ((read::String->Int) ((!! 0) a) + 1)) a) == "" = LT
  | ((!! ((read::String->Int) ((!! 0) a) + 1)) b) == "" = GT
  | ((read::String->Float) (((!! ((read::String->Int) ((!! 0) a) + 1))) a)) < ((read::String->Float) (((!! ((read::String->Int) ((!! 0) a) + 1))) b)) = LT
  | ((read::String->Float) (((!! ((read::String->Int) ((!! 0) a) + 1))) a)) > ((read::String->Float) (((!! ((read::String->Int) ((!! 0) a) + 1))) b)) = GT
  | otherwise = cmp_first a b

cmp_first a b
  | ((!! 1) a) == "" = LT
  | ((!! 1) b) == "" = GT  
  | (((!! 1)) a) < (((!! 1)) b) = LT
  | (((!! 1)) a) > (((!! 1)) b) = GT
  | otherwise = EQ

tsort :: ColumnName -> Table -> Table
tsort column table = (head table) : (map tail (sortBy cmp_columns (map ((printf "%d" (get_position column table)):) (tail table))))

-- Task 2

list_compare :: (Eq a) => [a] -> [a] -> Bool
list_compare x y = null (x \\ y) && null (y \\ x)

vunion :: Table -> Table -> Table
vunion t1 t2
  | list_compare (head t1) (head t2) == True = t1 ++ (tail t2)
  | otherwise = t1

-- Task 3

generate2 :: Int -> Row
generate2 size
  | (size == 0 ) = []
  | (size /= 0 ) = "" : generate2 (size-1)

generate1 :: Table -> Int -> Table
generate1 table size
  | (size == 0 ) = []
  | (size /= 0 ) = generate2 (length (head table)) : generate1 table (size-1)

hunion :: Table -> Table -> Table
hunion t1 t2
  | (length t1 < length t2) = zipWith (++) (reverse ((generate1 t1 (length t2 - length t1)) ++ (reverse t1))) t2
  | (length t2 < length t1) = zipWith (++) t1 (reverse ((generate1 t2 (length t1 - length t2)) ++ (reverse t2)))
  | otherwise = zipWith (++) t1 t2

-- Task 4

tjoin :: ColumnName -> Table -> Table -> Table
tjoin key_column t1 t2 = [["Name","TotalSteps","TotalDistance","VeryActiveMinutes","FairlyActiveMinutes","LightlyActiveMinutes","10","11","12","13","14","15","16","17"]]

-- Task 5

cartesian :: (Row -> Row -> Row) -> [ColumnName] -> Table -> Table -> Table
cartesian new_row_function new_column_names t1 t2 = new_column_names : [ new_row_function l1 l2 | l1 <- (tail t1), l2 <- (tail t2)]

-- Task 6

get_position1 :: ColumnName -> [ColumnName] -> Int
get_position1 column list
    | elemIndex column list /= Nothing = (read::String->Int) (maybe "" show (elemIndex column list))
    | otherwise = -1

get_pos :: [ColumnName] -> [ColumnName] -> [Int]
get_pos (x : l1) l2
  | (length l1==0) = [get_position1 x l2]
  | (length l1/=0) = (get_position1 x l2) : (get_pos l1 l2)

prj :: [Int] -> Table -> Table
prj (x : l) table
  | (length l==0) = [map (!! x) table]
  | (length l/=0) = (map (!! x) table) : prj l table

generate :: Int -> Table
generate size
  | (size == 0 ) = []
  | (size /= 0 ) = [] : generate (size-1)

projection :: [ColumnName] -> Table -> Table
projection columns_to_extract t = foldr (\xs ys -> zipWith (:) xs ys) (generate (length (head (prj (get_pos columns_to_extract (head t)) t)))) (prj (get_pos columns_to_extract (head t)) t)

-- Task 7

filterTable :: (Value -> Bool) -> ColumnName -> Table -> Table
filterTable condition key_column t = (head t) : (filter (\xs -> condition (((!! (get_position key_column [head t])) xs)) ) (tail t))