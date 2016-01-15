module Tests where
import Test.HUnit
import FuzzyLib

tests = test [ "test-triangle-move1" ~: (triangle (5,6,7)) 5.3 ~=? ((triangle (-5,-4,-3)) . flip (-) 10) 5.3,
               "test-triangle-move2" ~: (triangle (5,6,7)) 6.3 ~=? ((triangle (-5,-4,-3)) . flip (-) 10) 6.3,
               "test-triangle-move3" ~: (triangle (5,8,9.5)) 6 ~=? ((triangle (-5,-2,-0.5)) . flip (-) 10) 6,

               "test-trapezoid-move1" ~: (trapezoid (5,8,9.5, 10)) 6 ~=? ((trapezoid (-5,-2,-0.5, 0)) . flip (-) 10) 6,
               "test-trapezoid-move2" ~: (trapezoid (5,8,9.5, 10)) 9 ~=? ((trapezoid (-5,-2,-0.5, 0)) . flip (-) 10) 9,
               "test-trapezoid-move3" ~: (trapezoid (5,8,9.5, 10)) 9.7 ~=? ((trapezoid (-5,-2,-0.5, 0)) . flip (-) 10) 9.7,

               "test-sramp-move1" ~: (sramp (5,8)) 9 ~=? ((sramp (-5,-2)) . flip (-) 10) 9,
               "test-sramp-move2" ~: (sramp (5,8)) 7 ~=? ((sramp (-5,-2)) . flip (-) 10) 7,

               "test-zramp-move1" ~: (zramp (5,8)) 4 ~=? ((zramp (-5,-2)) . flip (-) 10) 4,
               "test-zramp-move2" ~: (zramp (5,8)) 7 ~=? ((zramp (-5,-2)) . flip (-) 10) 7,

               "test-trapezoid1" ~: (trapezoid (5,8,9.5, 10)) 4 ~=? 0,
               "test-trapezoid21" ~: (trapezoid (5,8,9.5, 10)) 6 > 0 ~? "",
               "test-trapezoid22" ~: (trapezoid (5,8,9.5, 10)) 6 < 1 ~? "",
               "test-trapezoid3" ~: (trapezoid (5,8,9.5, 10)) 9 ~=? 1,
               
               "test-sramp-zramp" ~: (sramp (5,8)) 7 ~=? (1 - ((zramp (5,8))  7))
             ]
