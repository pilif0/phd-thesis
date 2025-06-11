{-# LANGUAGE TypeFamilies #-}

module Main
  ( main
  ) where

import           ProcessComposition.Diagram.BoxAndWire (drawPrimitiveBasic,
                                                        drawProcess,
                                                        labelInterface)
import           ProcessComposition.Diagram.SyntaxTree (processDiag,
                                                        resTermDiag,
                                                        resourceDiag)
import qualified ProcessComposition.Isabelle.Arith     as Arith
import           ProcessComposition.Isabelle.Process
import qualified ProcessComposition.Isabelle.Rat       as Rat
import           ProcessComposition.Isabelle.Resource
import qualified ProcessComposition.Isabelle.ResTerm   as RT
import           ProcessComposition.Isabelle.Sum_Type  (Sum (..))

import ProcessComposition.Isabelle.Factorio.CraftWagon     (makeWagon)
import ProcessComposition.Isabelle.Factorio.FourGears      (fourGears)
import ProcessComposition.Isabelle.Factorio.Item
import ProcessComposition.Isabelle.Factorio.Machine
import ProcessComposition.Isabelle.Factorio.Manufacturing  (Manu_meta, perform)
import ProcessComposition.Isabelle.Factorio.TestingPrelims (assembling_machine_1,
                                                            craftWagon,
                                                            iron_plate)

import qualified ProcessComposition.Isabelle.Marking.Abstract as MAbs
import qualified ProcessComposition.Isabelle.Marking.Refined  as MRef

import qualified ThreeSocks as TS

import Diagrams.Backend.SVG (SVG, renderSVG)
import Diagrams.Prelude     (Diagram, strutX, (===), (|||))
import Diagrams.TwoD.Align  (alignT)
import Diagrams.TwoD.Size   (dims2D, height, width)

import qualified Data.Bifunctor     (bimap, second)
import           System.Environment (getArgs)

-- Set up faulty machine figures (faked, because it depicts extended theory)
operate =
  Primitive (parallel [res "Cutter", res "Plate"])
            (nonD (parallel [res "Cutter", nonD (res "Cut Shape") (res "Scrap")]) (res "Cutter Blocked"))
            "Operate" ()
fix =
  Primitive (res "Cutter Blocked")
            (parallel [res "Cutter", nonD (res "Cut Shape") (res "Scrap")])
            "Fix" ()
unblock =
  Primitive (res "Cutter Blocked")
            (nonD (parallel [res "Cutter", nonD (res "Cut Shape") (res "Scrap")]) (res "Cutter Blocked"))
            "Unblock" ()
faultyMachFigures =
  [ ( "operate_and_fix_left"
    , Seq operate
          ( Opt (Identity (parallel [res "Cutter", nonD (res "Cut Shape") (res "Scrap")]))
                fix)
    )
  , ( "operate_and_fix_right"
    , OptDistrOut (res "Cutter") (res "Cut Shape") (res "Scrap")
    )
  , ( "operate_unblock_fix"
    , Seq operate
          ( Opt ( Identity (parallel [res "Cutter", nonD (res "Cut Shape") (res "Scrap")]))
                ( Seq unblock
                      ( Opt (Identity (parallel [res "Cutter", nonD (res "Cut Shape") (res "Scrap")]))
                            fix)))
    )
  ]

-- Set up Factorio case study figures
factorioFigures =
  [ ("makeWagon", makeWagon "iron-smelter-in" "iron-smelter-out" "steel-smelter-in" "steel-smelter-out" "gears-in" "gears-out" "wagons-in" "wagons-out")
  , ("fourGears", fourGears "iron-smelter-in" "iron-smelter-out" "gears-in" "gears-out")
  , ("perform", map_process id id (const "Craft Cargo Wagon") id performProc)
  , ("merge", mergeProc)
  , ("split", splitProc)
  , ("move", moveProc)
  , ("unit-counit", Par unitProc counitProc)
  , ("move-split-merge", Par moveProc (Par splitProc mergeProc))
  ]
    where liftFlowAction = map_process Inl id id Inl
          isaFrac n d = Rat.divide_rat (Rat.of_int (Arith.Int_of_integer n)) (Rat.of_int (Arith.Int_of_integer d))
          mergeProc = liftFlowAction (merge iron_plate "Location" (isaFrac 2 1) (isaFrac 3 1))
          splitProc = liftFlowAction (split iron_plate "Location" (isaFrac 2 1) (isaFrac 3 1))
          moveProc = liftFlowAction (move iron_plate (isaFrac 3 1) "Source" "Destination")
          unitProc = liftFlowAction (unit iron_plate "Location")
          counitProc = liftFlowAction (counit iron_plate "Location")
          performProc = perform craftWagon (Arith.nat_of_integer 2) "Source" "Destination"

-- Set up Factorio resource atom labelling
showNat :: Arith.Nat -> String
showNat = show . Arith.integer_of_nat

showRat :: Rat.Rat -> String
showRat (Rat.Frct (n, d)) =
  if d == Arith.one_int
    then show (Arith.integer_of_int n)
    else show (Arith.integer_of_int n) ++ "/" ++ show (Arith.integer_of_int d)

labelFactorioRes :: Sum Flow Mach_block -> String
labelFactorioRes (Inl x) =
  itemLabel (flowItem x) ++ "[" ++ showRat (flowRate x) ++ "] at " ++ flowLoc x
labelFactorioRes (Inr x) =
  machineLabel (mblockMach x) ++ "<" ++ show (Arith.integer_of_nat . mblockCount $ x) ++ "> at " ++ mblockIn x ++ " \x2192 " ++ mblockOut x

-- Set up marking case study figures
markingAbstractFigures =
  [ ("collectSubs", MAbs.collectSubs)
  , ("markAll", MAbs.markAll)
  , ("submitMarks", MAbs.submitMarks)
  , ("releaseMarks", MAbs.releaseMarks)
  , ("marking", MAbs.markingProcess)
  ]

students = ["Connor", "Alex", "Hamish", "Becky"]

markingRefinedFigures =
  [ ("marking_1", marking_1)
  , ("collectStudent", MRef.collectStudent id "Connor")
  , ("marking_2", marking_2)
  , ("markStudent", MRef.markStudent id "Alex")
  , ("marking_3", marking_3)
  , ("submitMark", MRef.submitMark id "Hamish")
  , ("marking_4", marking_4)
  ]
    where marking_1 = process_refineRes (MRef.refinement students) id MAbs.markingProcess
          marking_2 = MRef.collectionSplit students id marking_1
          marking_3 = MRef.markSplit students id marking_2
          marking_4 = MRef.submitSplit students id marking_3


-- Set up marking atom labelling
labelAbsLRes :: MAbs.Lres -> String
labelAbsLRes MAbs.Students       = "Students"
labelAbsLRes MAbs.Submissions    = "Submissions"
labelAbsLRes MAbs.Marks          = "Marks"
labelAbsLRes MAbs.MarksSubmitted = "Marks submitted"
labelAbsLRes MAbs.MarksReleased  = "Marks released"

labelAbsCRes :: MAbs.Cres -> String
labelAbsCRes MAbs.Instructions             = "Instructions"

labelRefLRes :: MRef.Lres String -> String
labelRefLRes (MRef.Student x)       = x
labelRefLRes (MRef.Submission x)    = "Submission of " ++ x
labelRefLRes (MRef.Mark x)          = "Mark of " ++ x
labelRefLRes (MRef.MarkSubmitted x) = "Mark of " ++ x ++ " submitted"
labelRefLRes MRef.MarksReleased     = "Marks released"

-- Set up three-socks case study figures
threeSocksFigures =
  [ ("conformant", TS.conformantPlan)
  , ("contingent", TS.contingentPlan)
  ]

-- Set up three-socks atom labelling
labelThreeSocks :: TS.LRes -> String
labelThreeSocks TS.Hidden = "Hidden"
labelThreeSocks TS.Black  = "Black"
labelThreeSocks TS.White  = "White"

-- Set up other process figures
processDemos =
  [ ("interchange", Seq (Par (Primitive (res "A") (res "B") "R" ()) (Primitive (res "I") (res "J") "S" ())) (Par (Primitive (res "B") (res "C") "T" ()) (Primitive (res "J") (res "K") "U" ())))
  , ("canDone", Seq (Primitive (res "I") (parallel [res "A", res "B", res "C"]) "P" ()) (Par (Identity (res "A")) (Par (Primitive (res "B") (res "O") "Q" ()) (Identity (res "C")))))
  , ("primitive", Primitive (parallel [res "A", res "B", res "C"]) (parallel [res "X", res "Y"]) "Process" ())
  , ("opt_distr", Opt (Primitive (res "Left") (nonD (res "Heads") (res "Tails")) "P" ()) (Primitive (res "Right") (nonD (res "Heads") (res "Tails")) "Q" ()))
  , ("seq_done", Seq (Par (Primitive (res "I") (parallel [res "A"]) "P" ()) (Identity (parallel [res "B", res "C"]))) (Primitive (parallel [res "A", res "B", res "C"]) (res "O") "Q" ()))
  , ("pay_drink_change", Seq (Seq (Primitive (parallel [res "Machine (0)", res "Cash (10)"]) (res "Machine (10)") "Add to credit" ()) (Primitive (res "Machine (10)") (parallel [res "Machine (5)", res "Drink"]) "Get drink" ())) (Par (Primitive (res "Machine (5)") (parallel [res "Machine (0)", res "Cash (5)"]) "Get change" ()) (Identity (res "Drink"))))
  , ("diagPrimitive", Primitive (parallel [res "A", res "B"]) (res "C") "Primitive" ())
  , ("diagSeq", Seq (Primitive (res "A") (parallel [res "B", res "C"]) "P" ()) (Primitive (parallel [res "B", res "C"]) (res "D") "Q" ()))
  , ("diagPar", Par (Primitive (res "A") (parallel [res "B", res "C"]) "P" ()) (Primitive (parallel [res "B", res "C"]) (res "D") "Q" ()))
  , ("diagOpt", Opt (Primitive (parallel [res "A", res "B"]) (parallel [res "X", res "Y"]) "P" ()) (Primitive (res "C") (parallel [res "X", res "Y"]) "Q" ()))
  , ("diagRepresent", Represent (Primitive (parallel [res "A", res "B"]) (res "C") "P" ()))
  , ("diagIdentity", Swap (parallel [res "A", nonD (res "X") (res "Y")]) empty)
  , ("diagSwap", Swap (parallel [res "A", res "B"]) (res "C"))
  , ("diagInjectL", InjectL (res "X") (res "Y"))
  , ("diagInjectR", InjectR (res "X") (res "Y"))
  , ("diagOptDistrIn", OptDistrIn (res "A") (res "X") (res "Y"))
  , ("diagOptDistrOut", OptDistrOut (res "A") (res "X") (res "Y"))
  , ("diagDuplicate", Duplicate "data")
  , ("diagErase", Erase "data")
  , ("diagApply", Apply (res "I") (res "O"))
  , ("diagRepeat", Repeat (res "I") (res "O"))
  , ("diagClose", Close (res "I") (res "O"))
  , ("diagOnce", Once (res "I") (res "O"))
  , ("diagForget", Forget (parallel [res "A", res "B"]))
  ]

-- Box-and-wire figures
demosBox =
  map (Data.Bifunctor.second (map_process labelFactorioRes (const "()") id (const ()))) factorioFigures ++
  map (Data.Bifunctor.bimap ("before_" ++ ) (map_process labelAbsLRes labelAbsCRes id id)) markingAbstractFigures ++
  map (Data.Bifunctor.bimap ("after_" ++ ) (map_process labelRefLRes labelAbsCRes id id)) markingRefinedFigures ++
  map (Data.Bifunctor.bimap ("threeSocks_" ++ ) (map_process labelThreeSocks (const "()") id id)) threeSocksFigures ++
  faultyMachFigures ++
  processDemos

-- Process syntax figures
demosPSyn =
  [ ("interchange-sp", Seq (Par (Primitive (res "A") (res "B") "R" ()) (Primitive (res "I") (res "J") "S" ())) (Par (Primitive (res "B") (res "C") "T" ()) (Primitive (res "J") (res "K") "U" ())))
  , ("interchange-ps", Par (Seq (Primitive (res "A") (res "B") "R" ()) (Primitive (res "B") (res "C") "T" ())) (Seq (Primitive (res "I") (res "J") "S" ()) (Primitive (res "J") (res "K") "U" ())))
  ]

-- Resource term syntax figures
demosRTSyn =
  [ ("res_eq_0", RT.Res "A")
  , ("res_eq_1", RT.Parallel [RT.Res "A"])
  , ("res_eq_2", RT.Parallel [RT.Empty, RT.Res "A"])
  , ("res_eq_3", RT.Parallel [RT.Empty, RT.Parallel [RT.Res "A"]])
  , ("res_eq_4", RT.Parallel [RT.Empty, RT.Parallel [RT.Res "A"], RT.Parallel []])
  ]

-- Resource syntax figures
demosRSyn =
  []

-- Other diagram figures (not just process or resource)
specialCanDo :: Diagram SVG
specialCanDo = p ||| strutX 1 ||| q
  where
    p = labelInterface . drawProcess drawPrimitiveBasic $ Primitive (res "I") (parallel [res "A", res "B", res "C"]) "P" ()
    q = labelInterface . drawProcess drawPrimitiveBasic $ Primitive (res "B") (res "O") "Q" ()

specialSeqCorrect :: Diagram SVG
specialSeqCorrect = p ||| strutX 1 ||| q
  where
    p = labelInterface . drawProcess drawPrimitiveBasic $ Primitive (res "I") (parallel [res "A", res "B", res "C"]) "P" ()
    q = labelInterface . drawProcess drawPrimitiveBasic $ Primitive (parallel [res "A", res "B", res "C"]) (res "O") "Q" ()

specialSeqBad :: Diagram SVG
specialSeqBad = p ||| strutX 1 ||| q
  where
    p = labelInterface . drawProcess drawPrimitiveBasic $ Primitive (res "I") (parallel [res "A"]) "P" ()
    q = labelInterface . drawProcess drawPrimitiveBasic $ Primitive (parallel [res "A", res "B", res "C"]) (res "O") "Q" ()

specialSeqFix :: Diagram SVG
specialSeqFix = p ||| strutX 1 ||| q
  where
    p = alignT . labelInterface . drawProcess drawPrimitiveBasic $ Par (Primitive (res "I") (parallel [res "A"]) "P" ()) (Identity (parallel [res "B", res "C"]))
    q = alignT . labelInterface . drawProcess drawPrimitiveBasic $ Primitive (parallel [res "A", res "B", res "C"]) (res "O") "Q" ()

demosSpecial =
  [ ("canDo", specialCanDo)
  , ("seq_correct", specialSeqCorrect)
  , ("seq_bad", specialSeqBad)
  , ("seq_fix", specialSeqFix)
  ]

-- Helpers for parsing the destination argument and rendering a diagram as SVG
parseDest :: IO String
parseDest = head <$> getArgs

renderSize d = dims2D (f * width d) (f * height d)
  where f = 25

renderDiag :: String -> Diagram SVG -> IO ()
renderDiag p d = renderSVG p (renderSize d) d

-- Describe how each box-and-wire demo is drawn, given a base path and
-- file-process pair
renderBoxDemo :: String -> (String, Process String String String ()) -> IO ()
renderBoxDemo p (f, x) =
  (renderDiag fullpath
  . labelInterface
  . drawProcess drawPrimitiveBasic
  $ x)
  >> putStrLn ("Rendered box-and-wire: " ++ fullpath)
    where
      fullpath = p ++ "/" ++ f ++ ".svg"

-- Describe how each process syntax demo is drawn, given a base path and
-- file-process pair
renderPSynDemo :: String -> (String, Process String String String ()) -> IO ()
renderPSynDemo p (f, x) =
  (renderDiag fullpath
  . processDiag
  $ x)
  >> putStrLn ("Rendered process syntax: " ++ fullpath)
    where
      fullpath = p ++ "/" ++ f ++ ".svg"

-- Describe how each resource term syntax demo is drawn, given a base path and
-- file-term pair
renderRTSynDemo :: String -> (String, RT.Res_term String String) -> IO ()
renderRTSynDemo p (f, x) =
  (renderDiag fullpath
  . resTermDiag
  $ x)
  >> putStrLn ("Rendered resource term syntax: " ++ fullpath)
    where
      fullpath = p ++ "/" ++ f ++ ".svg"

-- Describe how each resource syntax demo is drawn, given a base path and
-- file-resource pair
renderRSynDemo :: String -> (String, Resource String String) -> IO ()
renderRSynDemo p (f, x) =
  (renderDiag fullpath
  . resourceDiag
  $ x)
  >> putStrLn ("Rendered resource syntax: " ++ fullpath)
    where
      fullpath = p ++ "/" ++ f ++ ".svg"

-- Describe how each other demo is drawn, given a base path and
-- file-diagram pair
renderSpecialDemo :: String -> (String, Diagram SVG) -> IO ()
renderSpecialDemo p (f, x) =
  renderDiag fullpath x
  >> putStrLn ("Rendered other: " ++ fullpath)
    where
      fullpath = p ++ "/" ++ f ++ ".svg"

-- When run, get the base path and render each demo into it
main :: IO ()
main = do
  p <- parseDest
  foldr (\x y -> renderBoxDemo p x >> y) (return ()) demosBox
  foldr (\x y -> renderPSynDemo p x >> y) (return ()) demosPSyn
  foldr (\x y -> renderRTSynDemo p x >> y) (return ()) demosRTSyn
  foldr (\x y -> renderRSynDemo p x >> y) (return ()) demosRSyn
  foldr (\x y -> renderSpecialDemo p x >> y) (return ()) demosSpecial
