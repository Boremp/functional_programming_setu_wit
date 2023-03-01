-- SECTION B - Precendences
-- Please fill in your name and student number
-- Student Name : Pedro Borem
-- Student Number : 20103988

-- This section is worth  30 Marks



--  In this section replace each "X2 = undefined" with "X2 = fully-parenthesiszed-expression"



-- such that X1 and X2 evaluate to the same value.

-- You may want to consult the table of precedences (see Lab-03)

-- you should use ghci to check is your answer correct.

--- For example I have done "a2" below



a1 = 3 + 5 * 5

a2 = (3 + (5 * 5))





------------------------------

b1:: Bool

b1 = 10 == 2 + 4 - 2

b2 = (10 == ((2 + 4) - 2))



------------------------------

c1 = length [1,2,3] + 8 `div` 6

c2 = ((length [1,2,3]) + (8 `div` 6))



-----------------------------

d1 = 5 ^ length [3,3,4]

d2 =  (5 ^ (length [3,3,4]))



-----------------------------

e1 = head [2,3,4]  ^ 2*3

e2 = (((head [2,3,4])  ^ 2)*3)



-----------------------------

f1 = 3 == 4  &&  2 > 5 `div` 3

f2 = ((3 == 4)  &&  (2 > (5 `div` 3)))



------------------------------

g1 = 5 < 10  && 3 < 4 `div`3

g2 = ((5 < 10)  && (3 < (4 `div`3)))



h1 = 34*2 > 4 && odd 4

h2 = (((34*2) > 4) && (odd 4))

allComplete = a1 == a1 && b1 == b2 && c1 == c2 && d1 == d2 && e1 == e2 && f1 == f2 && g1 == g2 && h1 == h2
-- This should be called whan all the x2 functions are written (changed from 'undefined'). If it returns true, then all are working.



-- SECTION C - TYPES
-- Please fill in your name and student number
-- Student Name : Pedro Borem
-- Student Number : 20103988
--This section is worth  30 Marks

-- For each named expression replace "undefined"

-- with an expression with the same type as the declaration

-- Note where the answer is a list, you will not get marks for giving an empty list as an answer, 

-- even though it is syntactically correct. (Nice idea, though!)



j1:: (String,Integer)

j1 = ("Pedro",20)

-- possible answer here would be 

-- j1 = ("Me", 1)



j2:: [Bool]

j2 =  [True,False,True,False]



j3:: (String, [Integer])

j3 =  ("Pedro", [0..9])





j4:: [[Char]]

j4 = [['a'..'z'],['A'..'Z'],['0'..'9']]





j5:: (Integer,[Bool],Char)

j5 =  (20,j2,'A')



j6:: (Integer,(String,Integer, String))

j6 = (20103988,("Pedro",20,"Borem"))



j7:: [(Integer, String)]

j7 =  [(1,"Pedro"),(2,"Aline"),(3,"Leticia")]



j8:: [[(Integer,Bool)]]

j8 = [[(1,True),(2,False)],[(2022,True),(2023,False)]]