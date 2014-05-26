import Test.HUnit
import Parsing


t1 = TestCase (assertEqual "for parse '(f b)'," [(Application (Variable "f",Variable "b"),"")] (parse term "(f b)"))


t2 = TestCase (assertEqual "for parse '(\\x . (f x))'" [(Lambda ("x",Application (Variable "f",Variable "x")),"")] (parse term "(\\x . (f x))"))


t3 = TestCase (assertEqual "for parse '(\\x . (\\y . (y (f x))))'" [(Lambda ("x",Lambda ("y",Application (Variable "y",Application (Variable "f",Variable "x")))),"")] (parse term "(\\x . (\\y . (y (f x))))"))

t4 = TestCase (assertEqual "for parse '(f (b x))'" [(Application (Variable "f",Application (Variable "b",Variable "x")),"")] (parse term "(f (b x))"))

t5 = TestCase (assertEqual "for parse '(f (b 5))'" [(Application (Variable "f",Application (Variable "b",Constant 5)),"")] (parse term "(f (b 5))"))


main :: IO Counts
main =
	runTestTT $ TestList[t1, t2, t3, t4, t5]


