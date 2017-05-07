let test = 
  [| [|4; 0; 0; 2; 5|]
     [|0; 0; 0; 1; 0|]         
     [|0; 0; 0; 3; 0|]         
     [|0; 0; 0; 0; 0|]         
     [|0; 0; 0; 0; 0|] |] |> array2D

#load "Library.fs"

let iOri = 1
let jOri = 3
let nFix = 5
let dim  = 5

// Solve
open Puzzle.Solve
let internal path = walkGrid iOri jOri test nFix
getDepth path
getCount nFix dim path

// Generate
open Puzzle.Generate
let puzzle = newPuzzle 4 4

//let fail = newPuzzle 5 12

//let dim  = 6
//let nFix = 8
//let pop  = rndGen dim
//let grid = ref <| Array2D.create dim dim 0
//let iOri , jOri = ref 0 , ref 0
//nxtGrid grid iOri jOri dim nFix pop
//let internal path = ref <| walkGrid !iOri !jOri !grid nFix
//
//while getCount nFix dim !path <> 1 do
//    nxtGrid grid iOri jOri dim nFix pop
//    path := walkGrid !iOri !jOri !grid nFix
//
//getCount nFix dim !path
//!grid
//
//[[0; 0; 0; 0; 0; 5]
// [0; 7; 0; 0; 6; 0]
// [0; 4; 0; 0; 0; 0]
// [0; 0; 0; 1; 0; 0]
// [0; 8; 0; 3; 0; 0]
// [0; 0; 0; 0; 0; 2]]
//
//[[8; 0; 0; 0; 7; 0]
// [0; 5; 0; 0; 0; 0]
// [0; 0; 0; 0; 0; 2]
// [0; 4; 6; 0; 3; 0]
// [0; 0; 0; 0; 0; 0]
// [1; 0; 0; 0; 0; 0]]

//let dim  = 6
//let nFix = 9
//let pop  = rndGen dim
//let grid = ref <| Array2D.create dim dim 0
//let iOri , jOri = ref 0 , ref 0
//nxtGrid grid iOri jOri dim nFix pop
//let internal path = ref <| walkGrid !iOri !jOri !grid nFix
//
//while getCount nFix dim !path <> 1 do
//    nxtGrid grid iOri jOri dim nFix pop
//    path := walkGrid !iOri !jOri !grid nFix
//
//getCount nFix dim !path
//!grid
//
//[[0; 0; 0; 0; 0; 0]
// [4; 3; 2; 7; 0; 0]
// [0; 0; 0; 0; 0; 0]
// [5; 0; 9; 6; 0; 0]
// [0; 0; 8; 0; 1; 0]
// [0; 0; 0; 0; 0; 0]]
//
//[[0; 0; 0; 0; 0; 2]
// [0; 0; 9; 3; 0; 0]
// [0; 0; 0; 8; 0; 0]
// [7; 0; 0; 1; 0; 0]
// [0; 0; 0; 0; 0; 0]
// [6; 0; 0; 4; 0; 5]]

//let dim  = 6
//let nFix = 10 // Expensive -> >10s
//let pop  = rndGen dim
//let grid = ref <| Array2D.create dim dim 0
//let iOri , jOri = ref 0 , ref 0
//nxtGrid grid iOri jOri dim nFix pop
//let internal path = ref <| walkGrid !iOri !jOri !grid nFix
//
//while getCount nFix dim !path <> 1 do
//    nxtGrid grid iOri jOri dim nFix pop
//    path := walkGrid !iOri !jOri !grid nFix
//
//getCount nFix dim !path
//!grid
//
//[[0; 9; 0; 0; 0; 8]
// [0; 0; 0; 7; 0; 0]
// [10; 6; 0; 3; 0; 1]
// [0; 0; 4; 0; 0; 0]
// [0; 0; 0; 5; 0; 0]
// [0; 0; 0; 0; 2; 0]]
//
//[[0; 0; 5; 0; 0; 0]
// [0; 0; 0; 0; 0; 0]
// [0; 8; 10; 9; 0; 1]
// [0; 7; 4; 0; 0; 0]
// [0; 0; 0; 3; 0; 2]
// [6; 0; 0; 0; 0; 0]]