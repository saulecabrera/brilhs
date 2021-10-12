{-# LANGUAGE OverloadedStrings #-}
module BrilSpec where

import Test.Hspec
import Bril (formBlocks)
import Program (Program(..))
import Fn (Fn(..), void)
import Instr (Instr(..))
import Data.Text
import Block (Block(..), indexed)

spec = 
  describe "formBlocks" $ do
    it "returns no blocks when the program contains no functions" $ do
      let p = Program []
      formBlocks p `shouldBe`  []

    it "places labels as the first instruction of a block" $ do
      let actualBlocks = formBlocks $ Program [main' [Label "start", Label "end"]]
      let expectedBlocks = [ Block "start" []
                           , Block "end" []
                           ]

      actualBlocks `shouldBe` expectedBlocks
    
    it "returns a single block if there are no terminator insructions" $ do
      let actualBlocks = formBlocks $ Program [main' [Print ["1"], Print ["2"]]]
      Prelude.length actualBlocks `shouldBe`  1

    it "divides blocks by terminator instructions" $ do
      let actualBlocks = formBlocks $ Program [main' [ Print ["1"]
                                                     , Jmp "label"
                                                     , Label "label"
                                                     , Print ["2"]
                                                     ]
                                            ]
      let expectedBlocks = [ Block "block_0" [Print ["1"], Jmp "label"]
                           , Block "label" [Print ["2"]]
                           ]

      actualBlocks `shouldBe` expectedBlocks 

main' = void "main" []

main = hspec spec
