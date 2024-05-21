{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( (@?=), testCase, assertBool, assertFailure )

import qualified Data.Set as S
import Description.Data --(forceMarkFinal, graceUnmarkFinal, forceAddARule, forceSetInitial, AutomataState (AutomataState), AutomataRule (AutomataRule), AutomataDesc(..), newADesc, graceAddAState, graceRemoveAState)
import Description.Parser (parseDescription)


main :: IO ()
main =
  defaultMain tests

tests = testGroup "Automata" [parsePrintGroup, invariants, nonInvariants]
    where
        fsi s = forceSetInitial (AutomataState s)
        fmf s = forceMarkFinal (AutomataState s)
        far i c f = forceAddARule $ AutomataRule (AutomataState i) c (AutomataState f)
        gar i c f = snd . graceAddARule (AutomataRule (AutomataState i) c (AutomataState f))
        frs s = forceRemoveAState (AutomataState s)
        gas s = graceAddAState (AutomataState s)
        grs s = snd . graceRemoveAState (AutomataState s)
        guf s = graceUnmarkFinal (AutomataState s)
        gsi s = snd . graceSetInitial (AutomataState s)
        gmf s = snd . graceMarkFinal (AutomataState s)

        constructFromEmpty = constructFrom newADesc
        constructFrom = foldl (\dsc action -> action dsc)

        parsePrintGroup = testGroup "Parse . Print = Id" $ map (\acts -> let desc = constructFromEmpty acts in testCase (getAName desc) $ parseDescription (show desc) @?= Right desc) cases
        cases = [ [\d -> d {getAName = "name", alphabet = S.fromList "01"}, fsi "init", fmf "final", far "init" '0' "final"]
                , [\d -> d {getAName = "other", alphabet = S.fromList "abc"}, fsi "init2", fmf "init2", far "init2" 'a' "init2", far "init2" 'b' "init2", far "init2" 'c' "init2"]
                , [\d -> d {getAName = "third", alphabet = S.fromList ['a' .. 'z']}] ++ map (\c -> fmf (c:"")) ['a' .. 'y'] ++ concatMap (\c -> map (\otherC -> far (c:"") otherC (otherC:"")) ['a' .. 'z']) ['a' .. 'z'] ++ [fsi "a"]
                ]

        changeCases = [ ([gsi "jjj"], "gsi jjj")
                      , ([gsi "final", gsi "init"], "gsi final, gsi init")
                      , ([gsi "init"], "gsi init")
                      , ([grs "init"], "grs init")
                      , ([grs "final"], "grs final")
                      , ([frs "final", gas "final", gmf "final", gar "init" '0' "final"], "frs final, gas final, gmf final, gar init 0 final")
                      , ([frs "final", fmf "final", gar "init" '0' "final"], "frs final, fmf final, gar init 0 final")
                      , ([frs "final", far "init" '0' "final", gmf "final"], "frs final, far init 0 final, final")
                      , ([fsi "other", gar "init" '1' "other", frs "other", gsi "init"], "fsi other, gar init 1 other, frs other, gsi init")
                      , ([guf "init"], "guf init")
                      , ([gmf "final", guf "init"], "gmf final, guf init")
                      , ([guf "final", gmf "final"], "guf final, gmf final")
                      , ([fmf "final"], "fmf final")
                      , ([far "init" '0' "init", far "init" '0' "final"], "far init 0 init, far init 0 final")
                      , ([gar "init" '0' "init"], "gar init 0 init")
                      ]

        changeCasesFail = [ ([fsi "jjj"], "fsi jjj")
                          , ([far "init" '0' "init"], "far init 0 init")
                          , ([gsi "final"], "gsi final")
                          , ([gas "other"], "gas other")
                          , ([frs "final"], "frs final")
                          , ([fsi "other"], "fsi other")
                          , ([fmf "other"], "fmf other")
                          , ([gar "final" '0' "init"], "gar final 0 init")
                          , ([gar "init" '1' "final"], "gar init 1 final")
                          , ([guf "final"], "guf final")
                          , ([gmf "init"], "gmf init")
                          ]

        invariants = testGroup "Invariant changes" $ map (\(arg, str) -> testCase str $ initDesc @?= constructFrom initDesc arg) changeCases
          where initDesc = constructFromEmpty $ head cases

        nonInvariants = testGroup "Non-invariant changes" $ map (\(arg, str) -> let otherDesc = constructFrom initDesc arg in testCase str $ assertBool (show initDesc ++ "vs\n" ++ show otherDesc) $ initDesc /= otherDesc) changeCasesFail
          where initDesc = constructFromEmpty $ head cases
        


