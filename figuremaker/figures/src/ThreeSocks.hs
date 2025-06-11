module ThreeSocks
  ( conformantPlan
  , contingentPlan
  , LRes(..)
  ) where

import ProcessComposition.Isabelle.Process
import ProcessComposition.Isabelle.Resource

(#) :: Resource a b -> Resource a b -> Resource a b
(#) = resource_par

data LRes = Hidden | Black | White
  deriving (Eq, Read, Show)

hidden = res Hidden
black = res Black
white = res White

pick :: Process LRes () String ()
pick = Primitive hidden (nonD black white) "Pick sock" ()

contingentPlan :: Process LRes () String ()
contingentPlan = seq_process_list
  [ Par (Identity (hidden # hidden)) pick
  , OptDistrIn (hidden # hidden) black white
  , Opt
      (seq_process_list
        [ Swap (hidden # hidden) black
        , Par (Identity (black # hidden)) pick
        , OptDistrIn (black # hidden) black white
        , Opt
            (seq_process_list
              [ Par (Identity black) (Swap hidden black)
              , Par (Identity (black # black)) (Forget hidden)
              , InjectL (black # black # anything) (white # white # anything)
              ])
            (seq_process_list
              [ Par (Identity black) (Swap hidden white)
              , Par (Identity (black # white)) pick
              , OptDistrIn (black # white) black white
              , Opt
                  (seq_process_list
                    [ Par (Identity black) (Swap white black)
                    , Par (Identity (black # black)) (Forget white)
                    , InjectL (black # black # anything) (white # white # anything)
                    ])
                  (seq_process_list
                    [ Swap black (white # white)
                    , Par (Identity (white # white)) (Forget black)
                    , InjectR (black # black # anything) (white # white # anything)
                    ])
              ])
        ])
      (seq_process_list
        [ Swap (hidden # hidden) white
        , Par (Identity (white # hidden)) pick
        , OptDistrIn (white # hidden) black white
        , Opt
            (seq_process_list
              [ Par (Identity white) (Swap hidden black)
              , Par (Identity (white # black)) pick
              , OptDistrIn (white # black) black white
              , Opt
                  (seq_process_list
                    [ Swap white (black # black)
                    , Par (Identity (black # black)) (Forget white)
                    , InjectL (black # black # anything) (white # white # anything)
                    ])
                  (seq_process_list
                    [ Par (Identity white) (Swap black white)
                    , Par (Identity (white # white)) (Forget black)
                    , InjectR (black # black # anything) (white # white # anything)
                    ])
              ])
            (seq_process_list
              [ Par (Identity white) (Swap hidden white)
              , Par (Identity (white # white)) (Forget hidden)
              , InjectR (black # black # anything) (white # white # anything)
              ])
        ])
  ]

conformantPlan :: Process LRes () String ()
conformantPlan = seq_process_list
  [ par_process_list [pick, pick, pick]
  , OptDistrIn (nonD black white # nonD black white) black white
  , Opt
      (seq_process_list
        [ Swap (nonD black white # nonD black white) black
        , OptDistrIn (black # nonD black white) black white
        , Opt
            (seq_process_list
              [ Par (Identity black) (Swap (nonD black white) black)
              , OptDistrIn (black # black) black white
              , Opt (innerNoSwap True black) (innerNoSwap True white)
              ])
            (seq_process_list
              [ Par (Identity black) (Swap (nonD black white) white)
              , OptDistrIn (black # white) black white
              , Opt (innerSmallSwap True) (innerBigSwap False)
              ])
        ])
      (seq_process_list
        [ Swap (nonD black white # nonD black white) white
        , OptDistrIn (white # nonD black white) black white
        , Opt
            (seq_process_list
              [ Par (Identity white) (Swap (nonD black white) black)
              , OptDistrIn (white # black) black white
              , Opt (innerBigSwap True) (innerSmallSwap False)
              ])
            (seq_process_list
              [ Par (Identity white) (Swap (nonD black white) white)
              , OptDistrIn (white # white) black white
              , Opt (innerNoSwap False black) (innerNoSwap False white)
              ])
        ])
  ]
  where fInj isLeft = if isLeft then InjectL else InjectR
        picked isLeft  = if isLeft then black else white
        unpicked isLeft  = if isLeft then white else black
        innerNoSwap isLeft r =
          let s = picked isLeft; f = fInj isLeft
          in seq_process_list
              [ Par (Identity (s # s)) (Forget r)
              , f (black # black # anything) (white # white # anything)
              ]
        innerSmallSwap isLeft =
          let s = picked isLeft; r = unpicked isLeft
          in Seq (Par (Identity s) (Swap r s)) (innerNoSwap isLeft r)
        innerBigSwap isLeft =
          let s = picked isLeft; r = unpicked isLeft
          in Seq (Swap r (s # s)) (innerNoSwap isLeft r)
