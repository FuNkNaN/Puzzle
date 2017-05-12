namespace Puzzle

module internal Check =

    /// The cell in the puzzle has not yet been passed.
    let toDo next x =
        x = 0 || x >= next

    /// The cell in the puzzle has already been passed.
    let passed next x =
        x < next && x <> 0

    /// Counts cells to do, even those that can no longer be reached.
    let nCellsToDo (grid :int[][]) next =
        grid 
        |> Seq.collect (Seq.filter (toDo next))
        |> Seq.length

    /// Counts cells to do, but only those that can still be reached.
    /// Assumes grid of equal dimensions.
    let nCellsWithinReach (grid :int[][]) next (i,j) =
        let rec loop (set :Set<int*int>) (i,j) =
            let isAvailable (i,j) =
                if i < 0 || i >= grid.Length || j < 0 || j >= grid.Length then false
                else set.Contains (i,j) |> not && toDo next grid.[i].[j]
                 
            [ 
                     (i-1),j ;          // _ ? _
                 i,(j-1) ; i,(j+1) ;    // ? x ?
                     (i+1),j            // _ ? _
            ] 
            |> List.filter isAvailable
            |> List.fold (fun (set :Set<int*int>) tup -> loop (set.Add tup) tup) set
        // Current cell is already passed, start recursion with empty set.
        loop Set.empty (i,j)
        |> Set.count

    /// If movement to given cell is possible, 
    /// return Some v (content of cell) or None otherwise.
    let move (grid: int[][]) dim nFix i j next =
        if i < 0 || i >= dim || j < 0 || j >= dim || next > nFix then None
        elif grid.[i].[j] = 0 then Some 0
        elif grid.[i].[j] = next then Some next
        else None

    /// Validate grid, check if path can become NP-complete.
    let isInvalidGrid grid next option i j =
        let nInReach = nCellsWithinReach grid next (i,j)
        let nToDo    = nCellsToDo grid next
        if nInReach = 0
        then not (nToDo = 1 && Option.isSome option)
        else nInReach < nToDo

    /// Counts adjacent cells that impose a boundary.
    /// Assumes grid of equal dimensions.
    let nCellBounds (grid :int[][]) next i j  =
        // ? ? ?
        // ? x ?
        // ? ? ?
        let isBound (i,j) =
            i < 0 || i >= grid.Length || j < 0 || j >= grid.Length || passed next grid.[i].[j]
        [ 
            (i-1),(j-1) ; (i-1),j ; (i-1),(j+1)
            ( i ),(j-1) ; ( i ),j ; ( i ),(j+1) 
            (i+1),(j-1) ; (i+1),j ; (i+1),(j+1) 
        ] 
        |> List.filter isBound 
        |> List.length

[<AutoOpen>]
module internal Path =

    /// Step n in path.
    type Depth = int

    /// Previous passed number.
    type Current = int

    /// Movement made.
    type Move =
      | S // Start
      | U // Up
      | R // Right
      | D // Down
      | L // Left

    /// Unbalanced Tree structure
    type Path =
      | End
      | Node of (Current * Depth * Move) * Path * Path * Path * Path

module internal Solve =

    /// Creates deep copies of inner arrays.
    /// Array.copy only creates a shallow copy.
    let cloneArrayOfArrays (array :int[][]) =
        Array.map Array.copy array

    /// Convert Option type to Tree structure. 
    let conv grid option f i j next depth dir = 
        if Check.nCellBounds grid next i j > 1 && Check.isInvalidGrid grid next option i j then End
        else
          match option with 
          | None -> End 
          | Some v -> 
                if v = next 
                then f grid i j (next+1) (depth+1) dir
                else let clone = cloneArrayOfArrays grid
                     clone.[i].[j] <- -1
                     f clone i j next (depth+1) dir

    /// Starting from point 1 iterate all plausible NP-complete paths
    /// and return them as a Path (Unbalanced Tree structure).
    let walkGrid i j (oriGrid: int[][]) nFix =
        let dim = oriGrid |> Array.length
        /// Expand the range to adjacent cells and recursively dive into valid paths.
        let rec cross (grid: int[][]) i j (next: int) (depth: int) (dir: Move) =
            let checkGrid = Check.move grid dim nFix
            let up      = checkGrid (i-1) j next
            let right   = checkGrid i (j+1) next
            let left    = checkGrid i (j-1) next
            let down    = checkGrid (i+1) j next
            Node((next-1,depth,dir),
                 (conv grid up    cross (i-1) j next depth U),
                 (conv grid right cross i (j+1) next depth R),
                 (conv grid left  cross i (j-1) next depth L),
                 (conv grid down  cross (i+1) j next depth D)
                 )
        cross oriGrid i j 2 0 S

    /// Implementation using continuations making algorithm tail recursive
    /// and an accumulator for efficient processing.
    /// Set-up required: Puzzle (project) -> Properties -> Build -> Generate Tail Calls.
    let rec getCountAcc nFix dim acc path cont =
        match path with
        | End -> cont acc
        | Node ((curr, depth, _), _, _, _, _) when depth = (dim * dim - 1) && curr = nFix
            ->  cont (acc + 1)
        | Node (_ , up , right , left , down) 
            ->  getCountAcc nFix dim acc up (fun accUp ->
                    getCountAcc nFix dim accUp right (fun accRight ->
                        getCountAcc nFix dim accRight left (fun accLeft ->
                            getCountAcc nFix dim accLeft down cont)))

    /// Number of solutions for given dimension and number of fixed points.
    let getCount nFix dim path = getCountAcc nFix dim 0 path id

/// Implements recursive functions that are not tail recursive,
/// utilized in Generate.newPuzzleOfScore.
module internal Analyse =

    /// Check if (sub)tree reaches certain depth.
    let rec contains depth = function
      | End -> false
      | Node ((_,d,_),t1,t2,t3,t4) ->
          if d = depth then true
          else contains depth t1 || contains depth t2 || contains depth t3 || contains depth t4

    /// Fold over the tree accumulating movements of the longest path.
    let rec pathFold x f acc = function
       | End -> acc
       | Node ((c,d,m),t1,t2,t3,t4) -> 
          if d = x then f acc (c,d,m)
          elif contains x t1 then f (pathFold x f acc t1) (c,d,m) 
          elif contains x t2 then f (pathFold x f acc t2) (c,d,m) 
          elif contains x t3 then f (pathFold x f acc t3) (c,d,m) 
          else f (pathFold x f acc t4) (c,d,m) 

    /// Collect movements of the longest path.
    let collect depth path = 
        if contains depth path 
        then pathFold depth (fun xs (_,_,m) -> m :: xs) [] path
        else []

    /// Calculate the complexity of the puzzle.
    let score depth path =
        let complexity (m1,m2) = if m1 = m2 then 1 else 2
        collect depth path
        |> List.pairwise
        |> List.sumBy complexity

module Generate =

    let private rnd = System.Random()

    /// Put closure on dim, return pop function.
    let internal rndGen dim =
        fun () -> rnd.Next(dim*dim)

    /// Generate list with locations of fixed points.
    let private constr nFix pop =
        let prev = ref Set.empty
        List.unfold
            (fun (n,x) ->
                if n <= nFix then
                    // add current value to Set of previous vals
                    prev := (!prev).Add x
                    // pop until there's a valid value
                    let v = ref <| pop()
                    while (!prev).Contains !v do
                        v := pop()
                    // return the current and next value
                    Some((n, x),(n+1, !v))
                else // terminator
                    None)
            (1,pop())

    /// Update grid, iOri and jOri references.
    /// Calculates a new random grid.
    let internal nxtGrid grid iOri jOri dim nFix =
        let list = constr nFix <| rndGen dim
        match list with
        | [] -> failwith "List with fixed points cannot be empty."
        | (_, x) :: _ -> iOri := (x / dim) ; jOri := (x % dim)
        // Use list to transform the zero value grid.
        grid := Array.init dim (fun _ -> Array.create dim 0)
        list 
        |> List.iter 
            (fun (n, x) 
                ->  let i = x / dim
                    let j = x % dim 
                    (!grid).[i].[j] <- n
            )

    /// Create a new puzzle
    /// of given dimension and number of fixed points.
    let newPuzzle dim nFix =
        let grid = ref <| Array.create dim Array.empty
        let iOri , jOri = ref 0 , ref 0
        nxtGrid grid iOri jOri dim nFix
        let path = ref <| Solve.walkGrid !iOri !jOri !grid nFix
        while Solve.getCount nFix dim !path <> 1 do
            nxtGrid grid iOri jOri dim nFix
            path := Solve.walkGrid !iOri !jOri !grid nFix
        !grid

    /// Create a new puzzle, with a minimum score, 
    /// of given dimension and number of fixed points.
    let newPuzzleOfScore minScore dim nFix =
        let grid = ref <| Array.create dim Array.empty
        let iOri , jOri = ref 0 , ref 0
        let depth = dim * dim - 1
        nxtGrid grid iOri jOri dim nFix
        let path = ref <| Solve.walkGrid !iOri !jOri !grid nFix
        while Solve.getCount nFix dim !path <> 1 
              && Analyse.score depth !path < minScore do
            nxtGrid grid iOri jOri dim nFix
            path := Solve.walkGrid !iOri !jOri !grid nFix
        !grid