{-# OPTIONS_GHC -w #-}
module Parser where 

import Lexer
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

data HappyAbsSyn t4 t5
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,256) ([52768,260,4,0,0,61440,57959,7,0,0,0,13192,65,0,0,64,50176,8345,14464,1043,0,0,52478,252,512,63488,61751,3,0,52768,260,39364,32800,4920,4100,33383,57856,4172,40000,521,13192,65,9841,8,0,0,0,0,0,192,0,24,8,0,16384,0,3072,1024,52768,260,32768,6,0,0,256,0,832,0,0,0,0,10111,8318,1230,1,64,14464,1043,0,0,16384,32771,32771,1,512,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","Exp","Type","num","'+'","\"&&\"","\"==\"","true","false","if","then","else","var","'\\\\'","':'","\"->\"","'('","')'","Bool","Number","'-'","'*'","'!'","\"||\"","\"<<\"","\">>\"","%eof"]
        bit_start = st * 29
        bit_end = (st + 1) * 29
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..28]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (6) = happyShift action_2
action_0 (10) = happyShift action_4
action_0 (11) = happyShift action_5
action_0 (12) = happyShift action_6
action_0 (15) = happyShift action_7
action_0 (16) = happyShift action_8
action_0 (19) = happyShift action_9
action_0 (25) = happyShift action_10
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (6) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (6) = happyShift action_2
action_3 (7) = happyShift action_16
action_3 (8) = happyShift action_17
action_3 (9) = happyShift action_18
action_3 (10) = happyShift action_4
action_3 (11) = happyShift action_5
action_3 (12) = happyShift action_6
action_3 (15) = happyShift action_7
action_3 (16) = happyShift action_8
action_3 (19) = happyShift action_9
action_3 (23) = happyShift action_19
action_3 (24) = happyShift action_20
action_3 (25) = happyShift action_10
action_3 (26) = happyShift action_21
action_3 (27) = happyShift action_22
action_3 (28) = happyShift action_23
action_3 (29) = happyAccept
action_3 (4) = happyGoto action_15
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_4

action_5 _ = happyReduce_3

action_6 (6) = happyShift action_2
action_6 (10) = happyShift action_4
action_6 (11) = happyShift action_5
action_6 (12) = happyShift action_6
action_6 (15) = happyShift action_7
action_6 (16) = happyShift action_8
action_6 (19) = happyShift action_9
action_6 (25) = happyShift action_10
action_6 (4) = happyGoto action_14
action_6 _ = happyFail (happyExpListPerState 6)

action_7 _ = happyReduce_2

action_8 (15) = happyShift action_13
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (6) = happyShift action_2
action_9 (10) = happyShift action_4
action_9 (11) = happyShift action_5
action_9 (12) = happyShift action_6
action_9 (15) = happyShift action_7
action_9 (16) = happyShift action_8
action_9 (19) = happyShift action_9
action_9 (25) = happyShift action_10
action_9 (4) = happyGoto action_12
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (6) = happyShift action_2
action_10 (10) = happyShift action_4
action_10 (11) = happyShift action_5
action_10 (12) = happyShift action_6
action_10 (15) = happyShift action_7
action_10 (16) = happyShift action_8
action_10 (19) = happyShift action_9
action_10 (25) = happyShift action_10
action_10 (4) = happyGoto action_11
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (6) = happyShift action_2
action_11 (7) = happyShift action_16
action_11 (8) = happyShift action_17
action_11 (9) = happyShift action_18
action_11 (10) = happyShift action_4
action_11 (11) = happyShift action_5
action_11 (12) = happyShift action_6
action_11 (15) = happyShift action_7
action_11 (16) = happyShift action_8
action_11 (19) = happyShift action_9
action_11 (23) = happyShift action_19
action_11 (24) = happyShift action_20
action_11 (25) = happyShift action_10
action_11 (26) = happyShift action_21
action_11 (27) = happyShift action_22
action_11 (28) = happyShift action_23
action_11 (4) = happyGoto action_15
action_11 _ = happyReduce_8

action_12 (6) = happyShift action_2
action_12 (7) = happyShift action_16
action_12 (8) = happyShift action_17
action_12 (9) = happyShift action_18
action_12 (10) = happyShift action_4
action_12 (11) = happyShift action_5
action_12 (12) = happyShift action_6
action_12 (15) = happyShift action_7
action_12 (16) = happyShift action_8
action_12 (19) = happyShift action_9
action_12 (20) = happyShift action_34
action_12 (23) = happyShift action_19
action_12 (24) = happyShift action_20
action_12 (25) = happyShift action_10
action_12 (26) = happyShift action_21
action_12 (27) = happyShift action_22
action_12 (28) = happyShift action_23
action_12 (4) = happyGoto action_15
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (17) = happyShift action_33
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (6) = happyShift action_2
action_14 (7) = happyShift action_16
action_14 (8) = happyShift action_17
action_14 (9) = happyShift action_18
action_14 (10) = happyShift action_4
action_14 (11) = happyShift action_5
action_14 (12) = happyShift action_6
action_14 (13) = happyShift action_32
action_14 (15) = happyShift action_7
action_14 (16) = happyShift action_8
action_14 (19) = happyShift action_9
action_14 (23) = happyShift action_19
action_14 (24) = happyShift action_20
action_14 (25) = happyShift action_10
action_14 (26) = happyShift action_21
action_14 (27) = happyShift action_22
action_14 (28) = happyShift action_23
action_14 (4) = happyGoto action_15
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (6) = happyShift action_2
action_15 (7) = happyShift action_16
action_15 (8) = happyShift action_17
action_15 (9) = happyShift action_18
action_15 (10) = happyShift action_4
action_15 (11) = happyShift action_5
action_15 (12) = happyShift action_6
action_15 (15) = happyShift action_7
action_15 (16) = happyShift action_8
action_15 (19) = happyShift action_9
action_15 (23) = happyShift action_19
action_15 (24) = happyShift action_20
action_15 (25) = happyShift action_10
action_15 (26) = happyShift action_21
action_15 (27) = happyShift action_22
action_15 (28) = happyShift action_23
action_15 (4) = happyGoto action_15
action_15 _ = happyReduce_15

action_16 (6) = happyShift action_2
action_16 (10) = happyShift action_4
action_16 (11) = happyShift action_5
action_16 (12) = happyShift action_6
action_16 (15) = happyShift action_7
action_16 (16) = happyShift action_8
action_16 (19) = happyShift action_9
action_16 (25) = happyShift action_10
action_16 (4) = happyGoto action_31
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (6) = happyShift action_2
action_17 (10) = happyShift action_4
action_17 (11) = happyShift action_5
action_17 (12) = happyShift action_6
action_17 (15) = happyShift action_7
action_17 (16) = happyShift action_8
action_17 (19) = happyShift action_9
action_17 (25) = happyShift action_10
action_17 (4) = happyGoto action_30
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (6) = happyShift action_2
action_18 (10) = happyShift action_4
action_18 (11) = happyShift action_5
action_18 (12) = happyShift action_6
action_18 (15) = happyShift action_7
action_18 (16) = happyShift action_8
action_18 (19) = happyShift action_9
action_18 (25) = happyShift action_10
action_18 (4) = happyGoto action_29
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (6) = happyShift action_2
action_19 (10) = happyShift action_4
action_19 (11) = happyShift action_5
action_19 (12) = happyShift action_6
action_19 (15) = happyShift action_7
action_19 (16) = happyShift action_8
action_19 (19) = happyShift action_9
action_19 (25) = happyShift action_10
action_19 (4) = happyGoto action_28
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (6) = happyShift action_2
action_20 (10) = happyShift action_4
action_20 (11) = happyShift action_5
action_20 (12) = happyShift action_6
action_20 (15) = happyShift action_7
action_20 (16) = happyShift action_8
action_20 (19) = happyShift action_9
action_20 (25) = happyShift action_10
action_20 (4) = happyGoto action_27
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (6) = happyShift action_2
action_21 (10) = happyShift action_4
action_21 (11) = happyShift action_5
action_21 (12) = happyShift action_6
action_21 (15) = happyShift action_7
action_21 (16) = happyShift action_8
action_21 (19) = happyShift action_9
action_21 (25) = happyShift action_10
action_21 (4) = happyGoto action_26
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (6) = happyShift action_2
action_22 (10) = happyShift action_4
action_22 (11) = happyShift action_5
action_22 (12) = happyShift action_6
action_22 (15) = happyShift action_7
action_22 (16) = happyShift action_8
action_22 (19) = happyShift action_9
action_22 (25) = happyShift action_10
action_22 (4) = happyGoto action_25
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (6) = happyShift action_2
action_23 (10) = happyShift action_4
action_23 (11) = happyShift action_5
action_23 (12) = happyShift action_6
action_23 (15) = happyShift action_7
action_23 (16) = happyShift action_8
action_23 (19) = happyShift action_9
action_23 (25) = happyShift action_10
action_23 (4) = happyGoto action_24
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (6) = happyShift action_2
action_24 (7) = happyShift action_16
action_24 (8) = happyShift action_17
action_24 (9) = happyShift action_18
action_24 (10) = happyShift action_4
action_24 (11) = happyShift action_5
action_24 (12) = happyShift action_6
action_24 (15) = happyShift action_7
action_24 (16) = happyShift action_8
action_24 (19) = happyShift action_9
action_24 (23) = happyShift action_19
action_24 (24) = happyShift action_20
action_24 (25) = happyShift action_10
action_24 (26) = happyShift action_21
action_24 (27) = happyShift action_22
action_24 (28) = happyShift action_23
action_24 (4) = happyGoto action_15
action_24 _ = happyReduce_10

action_25 (6) = happyShift action_2
action_25 (7) = happyShift action_16
action_25 (8) = happyShift action_17
action_25 (9) = happyShift action_18
action_25 (10) = happyShift action_4
action_25 (11) = happyShift action_5
action_25 (12) = happyShift action_6
action_25 (15) = happyShift action_7
action_25 (16) = happyShift action_8
action_25 (19) = happyShift action_9
action_25 (23) = happyShift action_19
action_25 (24) = happyShift action_20
action_25 (25) = happyShift action_10
action_25 (26) = happyShift action_21
action_25 (27) = happyShift action_22
action_25 (28) = happyShift action_23
action_25 (4) = happyGoto action_15
action_25 _ = happyReduce_9

action_26 (6) = happyShift action_2
action_26 (7) = happyShift action_16
action_26 (8) = happyShift action_17
action_26 (9) = happyShift action_18
action_26 (10) = happyShift action_4
action_26 (11) = happyShift action_5
action_26 (12) = happyShift action_6
action_26 (15) = happyShift action_7
action_26 (16) = happyShift action_8
action_26 (19) = happyShift action_9
action_26 (23) = happyShift action_19
action_26 (24) = happyShift action_20
action_26 (25) = happyShift action_10
action_26 (26) = happyShift action_21
action_26 (27) = happyShift action_22
action_26 (28) = happyShift action_23
action_26 (4) = happyGoto action_15
action_26 _ = happyReduce_12

action_27 (6) = happyShift action_2
action_27 (8) = happyShift action_17
action_27 (9) = happyShift action_18
action_27 (10) = happyShift action_4
action_27 (11) = happyShift action_5
action_27 (15) = happyShift action_7
action_27 (16) = happyShift action_8
action_27 (19) = happyShift action_9
action_27 (25) = happyShift action_10
action_27 (26) = happyShift action_21
action_27 (27) = happyShift action_22
action_27 (28) = happyShift action_23
action_27 (4) = happyGoto action_15
action_27 _ = happyReduce_7

action_28 (6) = happyShift action_2
action_28 (8) = happyShift action_17
action_28 (9) = happyShift action_18
action_28 (10) = happyShift action_4
action_28 (11) = happyShift action_5
action_28 (15) = happyShift action_7
action_28 (16) = happyShift action_8
action_28 (19) = happyShift action_9
action_28 (24) = happyShift action_20
action_28 (25) = happyShift action_10
action_28 (26) = happyShift action_21
action_28 (27) = happyShift action_22
action_28 (28) = happyShift action_23
action_28 (4) = happyGoto action_15
action_28 _ = happyReduce_6

action_29 (6) = happyShift action_2
action_29 (10) = happyShift action_4
action_29 (11) = happyShift action_5
action_29 (15) = happyShift action_7
action_29 (16) = happyShift action_8
action_29 (19) = happyShift action_9
action_29 (25) = happyShift action_10
action_29 (26) = happyShift action_21
action_29 (27) = happyShift action_22
action_29 (28) = happyShift action_23
action_29 (4) = happyGoto action_15
action_29 _ = happyReduce_17

action_30 (6) = happyShift action_2
action_30 (9) = happyShift action_18
action_30 (10) = happyShift action_4
action_30 (11) = happyShift action_5
action_30 (15) = happyShift action_7
action_30 (16) = happyShift action_8
action_30 (19) = happyShift action_9
action_30 (25) = happyShift action_10
action_30 (26) = happyShift action_21
action_30 (27) = happyShift action_22
action_30 (28) = happyShift action_23
action_30 (4) = happyGoto action_15
action_30 _ = happyReduce_11

action_31 (6) = happyShift action_2
action_31 (8) = happyShift action_17
action_31 (9) = happyShift action_18
action_31 (10) = happyShift action_4
action_31 (11) = happyShift action_5
action_31 (15) = happyShift action_7
action_31 (16) = happyShift action_8
action_31 (19) = happyShift action_9
action_31 (24) = happyShift action_20
action_31 (25) = happyShift action_10
action_31 (26) = happyShift action_21
action_31 (27) = happyShift action_22
action_31 (28) = happyShift action_23
action_31 (4) = happyGoto action_15
action_31 _ = happyReduce_5

action_32 (6) = happyShift action_2
action_32 (10) = happyShift action_4
action_32 (11) = happyShift action_5
action_32 (12) = happyShift action_6
action_32 (15) = happyShift action_7
action_32 (16) = happyShift action_8
action_32 (19) = happyShift action_9
action_32 (25) = happyShift action_10
action_32 (4) = happyGoto action_39
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (19) = happyShift action_36
action_33 (21) = happyShift action_37
action_33 (22) = happyShift action_38
action_33 (5) = happyGoto action_35
action_33 _ = happyFail (happyExpListPerState 33)

action_34 _ = happyReduce_16

action_35 (18) = happyShift action_42
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (19) = happyShift action_36
action_36 (21) = happyShift action_37
action_36 (22) = happyShift action_38
action_36 (5) = happyGoto action_41
action_36 _ = happyFail (happyExpListPerState 36)

action_37 _ = happyReduce_18

action_38 _ = happyReduce_19

action_39 (6) = happyShift action_2
action_39 (7) = happyShift action_16
action_39 (8) = happyShift action_17
action_39 (9) = happyShift action_18
action_39 (10) = happyShift action_4
action_39 (11) = happyShift action_5
action_39 (12) = happyShift action_6
action_39 (14) = happyShift action_40
action_39 (15) = happyShift action_7
action_39 (16) = happyShift action_8
action_39 (19) = happyShift action_9
action_39 (23) = happyShift action_19
action_39 (24) = happyShift action_20
action_39 (25) = happyShift action_10
action_39 (26) = happyShift action_21
action_39 (27) = happyShift action_22
action_39 (28) = happyShift action_23
action_39 (4) = happyGoto action_15
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (6) = happyShift action_2
action_40 (10) = happyShift action_4
action_40 (11) = happyShift action_5
action_40 (12) = happyShift action_6
action_40 (15) = happyShift action_7
action_40 (16) = happyShift action_8
action_40 (19) = happyShift action_9
action_40 (25) = happyShift action_10
action_40 (4) = happyGoto action_45
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (18) = happyShift action_44
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (6) = happyShift action_2
action_42 (10) = happyShift action_4
action_42 (11) = happyShift action_5
action_42 (12) = happyShift action_6
action_42 (15) = happyShift action_7
action_42 (16) = happyShift action_8
action_42 (19) = happyShift action_9
action_42 (25) = happyShift action_10
action_42 (4) = happyGoto action_43
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (6) = happyShift action_2
action_43 (7) = happyShift action_16
action_43 (8) = happyShift action_17
action_43 (9) = happyShift action_18
action_43 (10) = happyShift action_4
action_43 (11) = happyShift action_5
action_43 (12) = happyShift action_6
action_43 (15) = happyShift action_7
action_43 (16) = happyShift action_8
action_43 (19) = happyShift action_9
action_43 (23) = happyShift action_19
action_43 (24) = happyShift action_20
action_43 (25) = happyShift action_10
action_43 (26) = happyShift action_21
action_43 (27) = happyShift action_22
action_43 (28) = happyShift action_23
action_43 (4) = happyGoto action_15
action_43 _ = happyReduce_14

action_44 (19) = happyShift action_36
action_44 (21) = happyShift action_37
action_44 (22) = happyShift action_38
action_44 (5) = happyGoto action_46
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (6) = happyShift action_2
action_45 (7) = happyShift action_16
action_45 (8) = happyShift action_17
action_45 (9) = happyShift action_18
action_45 (10) = happyShift action_4
action_45 (11) = happyShift action_5
action_45 (12) = happyFail []
action_45 (15) = happyShift action_7
action_45 (16) = happyShift action_8
action_45 (19) = happyShift action_9
action_45 (23) = happyShift action_19
action_45 (24) = happyShift action_20
action_45 (25) = happyShift action_10
action_45 (26) = happyShift action_21
action_45 (27) = happyShift action_22
action_45 (28) = happyShift action_23
action_45 (4) = happyGoto action_15
action_45 _ = happyReduce_13

action_46 (20) = happyShift action_47
action_46 _ = happyFail (happyExpListPerState 46)

action_47 _ = happyReduce_20

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (TokenNum happy_var_1))
	 =  HappyAbsSyn4
		 (Num happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn4
		 (Var happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 _
	 =  HappyAbsSyn4
		 (BFalse
	)

happyReduce_4 = happySpecReduce_1  4 happyReduction_4
happyReduction_4 _
	 =  HappyAbsSyn4
		 (BTrue
	)

happyReduce_5 = happySpecReduce_3  4 happyReduction_5
happyReduction_5 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Add happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  4 happyReduction_6
happyReduction_6 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Sub happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  4 happyReduction_7
happyReduction_7 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Mult happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  4 happyReduction_8
happyReduction_8 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Not happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  4 happyReduction_9
happyReduction_9 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Less happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  4 happyReduction_10
happyReduction_10 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Greater happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  4 happyReduction_11
happyReduction_11 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (And happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  4 happyReduction_12
happyReduction_12 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Or happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happyReduce 6 4 happyReduction_13
happyReduction_13 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (If happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 6 4 happyReduction_14
happyReduction_14 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Lam happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_2  4 happyReduction_15
happyReduction_15 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (App happy_var_1 happy_var_2
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  4 happyReduction_16
happyReduction_16 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Paren happy_var_2
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  4 happyReduction_17
happyReduction_17 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Eq happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  5 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn5
		 (TBool
	)

happyReduce_19 = happySpecReduce_1  5 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn5
		 (TNum
	)

happyReduce_20 = happyReduce 5 5 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (TFun happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 29 29 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenNum happy_dollar_dollar -> cont 6;
	TokenAdd -> cont 7;
	TokenAnd -> cont 8;
	TokenEq -> cont 9;
	TokenTrue -> cont 10;
	TokenFalse -> cont 11;
	TokenIf -> cont 12;
	TokenThen -> cont 13;
	TokenElse -> cont 14;
	TokenVar happy_dollar_dollar -> cont 15;
	TokenLam -> cont 16;
	TokenColon -> cont 17;
	TokenArrow -> cont 18;
	TokenLParen -> cont 19;
	TokenRParen -> cont 20;
	TokenBoolean -> cont 21;
	TokenNumber -> cont 22;
	TokenSub -> cont 23;
	TokenMult -> cont 24;
	TokenNot -> cont 25;
	TokenOr -> cont 26;
	TokenLess -> cont 27;
	TokenGreater -> cont 28;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 29 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
parser tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a 
parseError _ = error "Syntax error!"
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
