import Test.HUnit
import Parsing
import Interpreter

t1 = TestCase (assertEqual "for parse '(f b)'," [(Application (Variable "f",Variable "b"),"")] (parse term "(f b)"))


t2 = TestCase (assertEqual "for parse '(\\x . (f x))'" [(Lambda ("x",Application (Variable "f",Variable "x")),"")] (parse term "(\\x . (f x))"))


t3 = TestCase (assertEqual "for parse '(\\x . (\\y . (y (f x))))'" [(Lambda ("x",Lambda ("y",Application (Variable "y",Application (Variable "f",Variable "x")))),"")] (parse term "(\\x . (\\y . (y (f x))))"))

t4 = TestCase (assertEqual "for parse '(f (b x))'" [(Application (Variable "f",Application (Variable "b",Variable "x")),"")] (parse term "(f (b x))"))

t5 = TestCase (assertEqual "for parse '(f (b (Num 5)))'" [(Application (Variable "f",Application (Variable "b",Constant (Num 5))),"")] (parse term "(f (b 5))"))

t6 = TestCase (assertEqual "for parse '(f (b 5) f)'" [] (parse term "(f (b (Num 5)) f)"))

t7 = TestCase (assertEqual "for parse '((a b) c)'" [(Application (Application (Variable "a",Variable "b"),Variable "c"),"")] (parse term "((a b) c)"))



t8 = TestCase (assertEqual "for freeVariables 'Application (Variable 'y', Lambda ('x', Constant Succ))'" ["y"]  (freeVariables (Application (Variable "y", Lambda ("x", Constant (Num 5))))))

t9 = TestCase (assertEqual "for substitution" (Application (Constant (Num 5), Lambda ("x",Constant (Num 5))))  (substitution (Application (Variable "y", Lambda ("x",Constant (Num 5)))) "y" (Constant (Num 5))))

t10 = TestCase (assertEqual "for betaConversion 1" (Constant (Num 5)) (betaConversion (Application (Lambda ("x", Variable "x"), Constant (Num 5)))))

t11 = TestCase (assertEqual "for etaConversion 1" (Constant Succ) (etaConversion (Lambda("x", Application(Constant Succ, Variable "x")))))

t12 = TestCase (assertEqual "for betaConversion 2" (Application (Lambda ("w", Variable "w"), Lambda ("w", Variable "w"))) (betaConversion (Application (Lambda ("z", Application (Variable "z", Variable "z")), Lambda ("w", Variable "w")))))


t13 = TestCase (assertEqual "for betaConversion 3" (Variable "y") (betaConversion (Application (Lambda ("x", Variable "y"), Variable "z"))))


t14 = 
	let
		t1 = (Application ((Lambda ("x", Variable "y")), (Application (Lambda ("z", Application (Variable "z", Variable       "z")), Lambda ("w", Variable "w")))))
		goal = (Variable "y")
		in TestCase (assertEqual "for betaConversion 4" goal (betaConversion t1))

t15 =
	let
		term1 = (Lambda("x", Application(Constant Succ, Variable "x")))
		conv1 = etaConversion term1
		goal1 = Constant Succ
	in TestCase $ assertEqual "for deltaConversion 1" goal1 conv1

t16 =
	let
		term2 = Application(Constant Succ, Constant (Num 5))
		conv2 = deltaConversion term2
		goal2 = Constant (Num 6)
	in TestCase $ assertEqual "for deltaConversion 2" goal2 conv2


t17 = 
	let
		term3 = Application (Application (Constant Add, Constant (Num 5)), Constant (Num 3))
		conv3 = deltaConversion term3
		goal3 = Constant (Num 8)
	in TestCase $ assertEqual "for deltaConversion 3" goal3 conv3

main :: IO Counts
main =
	runTestTT $ TestList[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14]


