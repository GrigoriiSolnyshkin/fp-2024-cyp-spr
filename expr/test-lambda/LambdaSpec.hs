import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( (@?=), testCase, assertBool, assertFailure )

import Lambda.Data
import Lambda.Parser
import Data.Either (isLeft)

bigOmega = "x" &-> "x" &*= "x"
complex = ("x" &*< ("x" &*< (("x" &*= "x") &*> "x"))) &*> "x"
complexStr = "x (x (x x x)) x"

printerData = [ ("x" &*= "y", "x y")
              , ("x" &*= "y" &*> "z", "x y z")
              , ("x" &*< ("y" &*= "z"), "x (y z)")
              , ("x" &*= "y" &* ("x" &*= "z"), "x y (x z)")
              , (Primitive "xx", "xx")
              , (bigOmega, "\\x.x x")
              , ("x" &-> "y" &-> "x" &*= "y", "\\x y.x y")
              , ("x" &-> "x" &*< ("x" &*= "x"), "\\x.x (x x)")
              , ("x" &-> "y" &-> "z" &-> "x" &*= "y" &*> "z", "\\x y z.x y z")
              , (bigOmega &* bigOmega, "(\\x.x x) (\\x.x x)")
              , ("y" &*< bigOmega &*> "z", "y (\\x.x x) z")
              , ("alpha" &*< (bigOmega &* bigOmega), "alpha ((\\x.x x) (\\x.x x))")
              , (complex, complexStr)
              , (bigOmega &* complex, "(\\x.x x) (" ++ complexStr ++ ")")
              ]

testPrinter :: TestTree
testPrinter = testGroup "Printer" $ map (\(lambda, expected) -> testCase ("show " ++ expected) $ show lambda @?= expected) printerData


testParser :: TestTree
testParser = testGroup "Parser" [reversedPrinterOk, parseSameOk, parserNotOk]
  where
    reversedPrinterOkCases = map (\(x, y) -> (y, x)) printerData

    reversedPrinterOk = testGroup "reversedPrinterOk" $ map (\(string, lambda) -> testCase ("parse " ++ string) $ parseTerm string @?= Right lambda) reversedPrinterOkCases

    parseSameOkCases = [ ["x y", "x  y", " x y ", "(x y)", "((x ) ( y) ) ", "(x (y))"]
                       , ["x y z", "(x y) z", "((x) (y)) ((z))"]
                       , ["x", " x", "  x", "  x  ", "( x) "]
                       , ["xx yy", "xx  yy", "(xx) yy"]
                       , ["\\x.x x", " \\x. x x", "\\ x .x  (x)"]
                       , ["\\x.\\y.x y", "\\x y.x y", "\\x.(\\y. x y)"]
                       , ["(\\x.x x) (\\x.x x)", "(\\x.x x)( \\x.x x ) "]
                       ]

    parseSameOk = testGroup "parseSameOk" $ concatMap (\stringLst -> zipWith (\string1 string2-> testCase ("parsing same " ++ string1 ++ " and " ++ string2) $ parseTerm string1 @?= parseTerm string2) stringLst (tail stringLst)) parseSameOkCases

    parserNotOkCases = [ "", " ", "  ", "-", "8", "8u", "xy + xy", "\\x", "+", "((x)", "(x))", "(\\x)", "(\\x.x x)(\\x.)", "\\x y z.", ".", "\\.", "\\x..x", "\\.x x"]

    parserNotOk = testGroup "parserNotOk" $ map (\string -> testCase ("not parse " ++ string) $ assertBool "" $ isLeft $ parseTerm string) parserNotOkCases


main :: IO ()
main =
  defaultMain $ testGroup "Lambdas" [ testPrinter, testParser ]

