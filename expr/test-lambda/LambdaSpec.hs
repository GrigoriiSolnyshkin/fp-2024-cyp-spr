import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( (@?=), testCase, assertBool, assertFailure )

import Lambda.Data

testPrinter :: TestTree
testPrinter = testGroup "Printer" $ map (\(lambda, expected) -> testCase ("show " ++ expected) $ show lambda @?= expected) printerData
  where
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

    bigOmega = "x" &-> "x" &*= "x"
    complex = ("x" &*< ("x" &*< (("x" &*= "x") &*> "x"))) &*> "x"
    complexStr = "x (x (x x x)) x"

main :: IO ()
main = 
  defaultMain $ testGroup "Lambdas" [ testPrinter ]

