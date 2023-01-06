module Tests where
import IC.TestSuite
import Data.List
import Types
import SC
import Examples

-- map topLevelFunctions [e4,e12,e20] == [1,2,0]
topLevelFunctionsTestCases
  = [ ([e4,e12,e20]) ==> [1,2,0]
    ]

-- unionAll ([] :: [[Int]]) == []
-- unionAll [[3]] == [3]
-- sort (unionAll [[2, 5], [4, 2], [5, 1]]) == [1,2,4,5]
unionAllTestCases1
  = [ ([] :: [[Int]]) ==> [],
      [[3]] ==> [3]
    ]
unionAllTestCases2
  = [
      unionAll ([[2, 5], [4, 2], [5, 1]]) ==> [1,2,4,5]
    ]

-- sort (freeVars e6) == ["x"]
-- sort (freeVars e7) == ["z"]
-- sort (freeVars e8) == ["y","z"]
-- sort (freeVars e9) ==["x"]
-- sort (freeVars e10) == ["y","z"]
-- sort (freeVars e11) == ["x","z"]
freeVarsTestCases
  = [ (freeVars e6) ==> ["x"],
      (freeVars e7) ==> ["z"],
      (freeVars e8) ==> ["y","z"],
      (freeVars e9) ==> ["x"],
      (freeVars e10) ==> ["y","z"],
      (freeVars e11) ==> ["x","z"]
    ]

-- sort (buildFVMap e1) == [("f",[]),("g",["x"])]
-- sort (buildFVMap e2) == [("succ",[])]
-- sort (buildFVMap e9) == [("g",["x"])]
-- sort (buildFVMap e10) == [("g",["x"])]
-- sort (buildFVMap e11) == [("g",["x"])]
-- sort (buildFVMap e12) == [("g",[]),("h",[])]
-- sort (buildFVMap e16) == [("f1",[]),("g",[]),("h",[])]
-- sort (buildFVMap e19) == [("f",["y","z"]),("g",["y","z"])]
buildFVMapTestCases
  = [ (buildFVMap e1) ==> [("f",[]),("g",["x"])],
      (buildFVMap e2) ==> [("succ",[])],
      (buildFVMap e9) ==> [("g",["x"])],
      (buildFVMap e10) ==> [("g",["x"])],
      (buildFVMap e11) ==> [("g",["x"])],
      (buildFVMap e12) ==> [("g",[]),("h",[])],
      (buildFVMap e16) ==> [("f1",[]),("g",[]),("h",[])],
      (buildFVMap e19) ==> [("f",["y","z"]),("g",["y","z"])]
    ]

-- modifyFunctions (buildFVMap e1) e1 == e1''
-- modifyFunctions (buildFVMap e20) e20 == e20
-- modifyFunctions (buildFVMap e21) e21 == e21
modifyFunctionsTestCases
  = [ ((buildFVMap e1),e1) ==> e1'',
      ((buildFVMap e20),e20) ==> e20,
      ((buildFVMap e21),e21) ==> e21
    ]

allTestCases
  = [
      TestCase  "topLevelFunctions" (map topLevelFunctions)
                topLevelFunctionsTestCases
      ,
      TestCase  "unionAll1" (unionAll)
                unionAllTestCases1
      ,
      TestCase  "unionAll2" (sort)
                unionAllTestCases2
      ,
      TestCase  "freeVars" (sort)
                freeVarsTestCases
      ,
      TestCase  "buildFVMap" (sort)
                buildFVMapTestCases
      ,
      TestCase  "modifyFunctions" (uncurry modifyFunctions)
                modifyFunctionsTestCases
    ]

runTests = mapM_ goTest allTestCases

main = runTests
