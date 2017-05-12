#r @"..\packages\Expecto.4.2.1\lib\net40\Expecto.dll"
open Expecto

#load "Library.fs"
open Puzzle

[<Tests>]
let tests =
  testList "Puzzle" [
    testList "Check" [
      testCase "toDo" <| fun _ ->
        /// The fixed number that's next in line.
        let next = 2 
        Expect.isTrue (Check.toDo next 0)
            "The toDo function should return true for 0 values (= empty cells)."
        Expect.isFalse (Check.toDo next -1)
            "The toDo function should return false for -1 values (= already passed cells)."
        Expect.isTrue (Check.toDo next 2)
            "The toDo function should return true when encountering a number that's greater than or equal to next."
        Expect.isFalse (Check.toDo next 1)
            "The toDo function should return false when encountering a number that's smaller than next."

      testCase "passed" <| fun _ ->
        /// The fixed number that's next in line.
        let next = 2 
        Expect.isTrue (Check.passed next -1)
            "The passed function should return true for -1 values (= already passed cells)."
        Expect.isFalse (Check.passed next 0)
            "The passed function should return false for 0 values (= empty cells)."
        Expect.isTrue (Check.passed next 1)
            "The passed function should return true when encountering a number that's smaller than next."
        Expect.isFalse (Check.passed next 2)
            "The passed function should return false when encountering a number that's greater than or equal to next."

      testCase "nCells" <| fun _ ->
        /// Applied path can no longer become NP-Complete.
        let grid =
          [| [| 4; 0;-1; 2; 5|]
             [| 0; 0;-1; 1; 0|] // <-        
             [| 0; 0;-1; 3;-1|]         
             [| 0; 0; 0; 0; 0|]         
             [| 0; 0; 0; 0; 0|] |]
        /// The fixed number that's next in line.
        let next = 4 
        Expect.equal (Check.nCellsToDo grid next) 18
            "The nCellsToDo function should count all cells that are still to do."
        // Moving up
        Expect.equal (Check.nCellsWithinReach grid next (1,4)) 2 // -> 5 -> 0 
            "The nCellsWithinReach function should count all cells that are still within reach."
        // Moving down
        Expect.equal (Check.nCellsWithinReach grid next (3,4)) 16 // 10 + 6
            "The nCellsWithinReach function should count all cells that are still within reach."
        // Logic implemented in Solve.isInvalidGrid
        Expect.isTrue (Check.nCellsWithinReach grid next (1,4) < Check.nCellsToDo grid next)
            "Predicate should return true when path can no longer becomen NP-Complete."
        Expect.isTrue (Check.nCellsWithinReach grid next (3,4) < Check.nCellsToDo grid next)
            "Predicate should return true when path can no longer becomen NP-Complete."

      testCase "move" <| fun _ ->
        /// We are currently at number 1.
        let grid =
          [| [| 4; 0; 0; 2; 5|]
             [| 0; 0; 0; 1; 0|]        
             [| 0; 0; 0; 3; 0|]         
             [| 0; 0; 0; 0; 0|]         
             [| 0; 0; 0; 0; 0|] |]
        /// Grid dimension and number of fixed points.
        let dim, nFix = 5, 5
        /// Indexers of number 1.
        let i, j = 1,3
        /// The fixed number that's next in line.
        let next = 2 
        Expect.isNone (Check.move grid dim nFix (i+1) j next)
            "The move function should return None when a number is encountered that's not next."
        Expect.isSome (Check.move grid dim nFix (i-1) j next)
            "The move function should return Some(next) when the next number is encountered."
        Expect.isSome (Check.move grid dim nFix i (j-1) next)
            "The move function should return Some(0) when an empty cell is encounterd."
        // IndexOutOfRange
        Expect.isNone (Check.move grid dim nFix -1 j next)
            "The move function should return None when index is out of the bounds of the array."
        Expect.isNone (Check.move grid dim nFix i dim next)
            "The move function should return None when index is out of the bounds of the array."

      testCase "isInvalidGrid" <| fun _ ->
        /// Only final move, consuming 5, is left.
        let grid =
          [| [| 4;-1;-1; 2; 5|]
             [|-1;-1;-1; 1;-1|] // <- current position
             [|-1;-1;-1; 3;-1|]         
             [|-1;-1;-1;-1;-1|]         
             [|-1;-1;-1;-1;-1|] |]
        /// Indexers of the remaining 5.
        let i, j = 0,4
        /// The fixed number that's next in line.
        let next = 5 
        // Moving up
        Expect.isTrue (Check.nCellsWithinReach grid next (i,j) < Check.nCellsToDo grid next)
            "Predicate should return true because there're no adjacent cells left to do (0 < 1)."
        Expect.isFalse (Check.isInvalidGrid grid next (Some(5)) i j)
            "The isInvalidGrid function should also take the final cell into account."

      testCase "nCellBounds" <| fun _ ->
        /// We are currently at number 3.
        let grid =
          [| [| 4; 0;-1; 2; 5|]
             [| 0; 0;-1; 1; 0|]      
             [| 0; 0;-1; 3; 0|]         
             [| 0; 0; 0; 0; 0|]         
             [| 0; 0; 0; 0; 0|] |]
        /// The fixed number that's next in line.
        let next = 4
        Expect.equal (Check.nCellBounds grid next 4 2) 3
            "The nCellBounds function should count the end of the grid as boundaries."
        Expect.equal (Check.nCellBounds grid next 3 3) 2
            "The nCellBounds function should count already passed cells as boundaries."
        Expect.equal (Check.nCellBounds grid next 1 1) 3
            "The nCellBounds function should not count to do cells as boundaries."
    ]

    /// Implements recursive functions that are not tail recursive,
    /// utilized in Generate.newPuzzleOfScore.
    testList "Analyse" [
      testCase "contains" <| fun _ ->
        /// [| [|1; 0|] ;
        ///    [|2; 0|] |]
        let path = 
           Node ((1, 0, S),End,
             Node ((1, 1, R),End,End,End,
                 Node ((1, 2, D),End,End,
                     Node ((2, 3, L),End,End,End,End),End)),End,
             Node ((2, 1, D),End,End,End,End))
        Expect.isTrue (Analyse.contains 3 path)
            "The contains function should return true when a path reaches given depth."
        Expect.isFalse (Analyse.contains 4 path)
            "The contains function should return false when a path doesn't reach given depth."

      testCase "collect" <| fun _ ->
        /// [| [|1; 0|] ;
        ///    [|2; 0|] |]
        let path = 
           Node ((1, 0, S),End,
              Node ((1, 1, R),End,End,End,
                 Node ((1, 2, D),End,End,
                     Node ((2, 3, L),End,End,End,End),End)),End,
              Node ((2, 1, D),End,End,End,End))
        /// NP-Complete
        let moveList = [ S ; R ; D ; L ]
        Expect.equal (Analyse.collect 3 path) moveList
            "The collect function should return the moves of the longest path, when given the depth of the tree."
        Expect.equal (Analyse.collect 4 path) []
            "The collect function should return an empty list when the given the depth exceeds the depth of the tree."

      testCase "score" <| fun _ ->
        /// [| [|1; 0|] ;
        ///    [|2; 0|] |]
        let path = 
           Node ((1, 0, S),End,
             Node ((1, 1, R),End,End,End,
               Node ((1, 2, D),End,End,
                 Node ((2, 3, L),End,End,End,End),End)),End,
             Node ((2, 1, D),End,End,End,End))
        /// Depth of the tree.
        let depth = 3
        /// [ S ; R ; D ; L ] -> 2 + 2 + 2
        Expect.equal (Analyse.score depth path) 6
            "The score function should accumulate 2 with each change of movement."
        /// [| [|1; 0; 0; 2|] |] 
        let path = 
          Node ((1, 0, S),End, 
            Node ((1, 1, R),End, 
              Node ((1, 2, R),End,
                Node ((2, 3, R),End,End,End,End),End,End),End, End),End, 
            Node ((2, 1, D),End,End,End,End))
        /// Depth of the tree.
        let depth = 3
        /// [ S ; R ; R ; R ] -> 2 + 1 + 1
        Expect.equal (Analyse.score depth path) 4
            "The score function should accumulate 1 when the direction doesn't change."
    ]
  ]

runTests defaultConfig tests