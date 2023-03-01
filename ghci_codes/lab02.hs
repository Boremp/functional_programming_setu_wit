--- Naming Values worksheet

------------------------------------------------
-- pi is a real number which is approximately
-- three and 1,416 ten thousandths. Define a Double
-- with this approximate value.

pie:: Double             
pie = 3.14159

--------------------------------------------------
-- The formula for the area of a circle is 
-- pie times r squared, where r is the radius of 
-- the circle. Define a Double which is the area 
-- in square centimeters of a circle with diameter 
-- 10 centimeters.

areaCircleDiameter10:: Double
areaCircleDiameter10 = pie * 5^2

----------------------------------------------
-- Define an Integer that is the number of 
-- seconds in a week.

secondsIn1Week:: Integer
secondsIn1Week = 60^2 * 24 * 7

---------------------------------------------
-- Define a List of Integers with the elements
-- 1 to 10 in increasing order

oneToTen:: [Integer]
oneToTen = [1..10]

---------------------------------------------
-- Define a string that is your first name

yourFirstName:: String
yourFirstName = "Pedro"

---------------------------------------------
-- Define an Integer that is your age

yourAge:: Integer     
yourAge = 20

---------------------------------------------
-- Define a Bool truthvalue that is 
-- (yourAge is greater than 19) or (yourFirstName is "Tim")

compareName:: Bool
compareName = yourFirstName == "Tim"

---------------------------------------------
-- Define an Double that is the average of 
-- 3.0, 7.42, and 24.8

average:: Double
average = (sum[3.0,7.42,24.8]) / 3.0

---------------------------------------------
-- Define an Integer that is the remainder
-- when 14563 is divided by 22. Hint use the 
-- "mod" operator. Try it out to see how it works

remainder::Integer 
remainder = 14563 `rem` 22

---------------------------------------------
-- Define a tuple that has your first name 
-- and your age.

tuple:: (String,Integer)
tuple = (yourFirstName, yourAge)

---------------------------------------------
-- Compute the difference between your
-- approximation of pie, and the fraction
-- 22 divided by 7.

difference:: Double
difference = pie - 22/7

-- #################################################################
-- #################################################################
-- #################################################################

-- add a type declaration
-- to each of the named expressions
i1:: Integer  -- I have done the first one for you
i1 = 45

i2:: String
i2 = "123"

i3:: Bool
i3 = 45 <= i1

i4:: Char
i4 = 'c'

i5::[String]
i5 = ["abc","ok"]

i6:: String
i6 = head i5

i7:: String --or [Char]
i7 = tail "abc"  -- Recall a string is a shorthand for a list of Char

i8:: (Bool,Double)
i8 = (True,4.5)

i9:: [Integer]
i9 = [i1,34]

-------------------------------------------------
-- For each named expression replace "undefined"
-- with an expression with the same type as the declaration

j1:: (String,Integer)
j1 = (yourFirstName,yourAge)

j2:: [Integer]
j2 = oneToTen

j3:: Char
j3 = i4


j4:: Double
j4 = pie


j5:: (Integer,String,Integer,Char)
j5 = (yourAge,i2,i1,j3)

j6:: ([Char],(Bool,String))
j6 = (i2,(i3,i2))

j7:: [[Bool]]
j7 = [[i3,i3],[i3,i3]]

j8:: [(String,Bool)]
j8 = [(yourFirstName,i3),("Sofia",True)]

-- #################################################################
-- #################################################################
-- #################################################################
-- #################################################################
-- EXERCISE TYPES AND CLASSES

-- 1 What are the types of the following values?
p1::[Char]
p1 = ['a','b','c']

p2::(Char,Char,Char)
p2 = ('a','b','c')

p3::[(Bool,Char)]
p3 = [(False, '0'), (True, '1')]

p4::([Char],[Char])
p4 = (['1', '0'], ['0', '1'])

p5:: [[a] -> [a]]
p5 = [tail, init, reverse] 

-- 2 Write down definitions that have the following types.
bools :: [Bool]
bools = [True,False]

nums :: [[Int]]
nums = [[1,2,3],[4,5,6]]

add :: Int -> Int -> Int-> Int
add x y z = x + y + z

copy :: a -> (a,a)
copy a = (a,a)

apply :: (a -> b) -> a -> b
apply  f x = f x 

-- 3 What are the types of the following functions?
second:: [a] -> a
second xs = head (tail xs)

swap:: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair:: a -> b -> (a,b)
pair x y = (x,y)

double:: Num a => a -> a
double x = x*2

pallindrome :: Eq a => [a] -> Bool
pallindrome xs = reverse xs == xs

twice:: (a -> a) -> a -> a
twice f x = f (f x)

-- FINISHED