namespace Puzzle

module internal Solve =

    /// Step n in path.
    type Depth = int

    /// Previous fixed number.
    type Current = int

    /// Movement made.
    type Direction =
      | S // Start
      | U // Up
      | R // Right
      | D // Down
      | L // Left

    /// Unbalanced Tree structure
    type Path =
      | End
      | Node of (Current * Depth * Direction) * Path * Path * Path * Path

    /// If movement to given cell is possible, 
    /// return Some v (content of cell) or None otherwise.
    let check (grid: int [,]) iMax jMax nFix i j next =
        if i < 0 || i >= iMax || j < 0 || j >= jMax || next > nFix then None
        elif grid.[i,j] = 0 then Some 0
        elif grid.[i,j] = next then Some next
        else None

    /// Convert Option type to Tree structure. 
    let conv grid option f i j next depth dir = 
      match option with 
      | None -> End 
      | Some v -> 
            if v = next 
            then f grid i j (next+1) (depth+1) dir
            else let clone = Array2D.copy grid
                 clone.[i,j] <- -1
                 f clone i j next (depth+1) dir

    /// Starting from point 1 iterate all possible paths
    /// and return them as a Path (Unbalanced Tree structure).
    let walkGrid i j (oriGrid: int [,]) nFix =
        let iMax  = oriGrid |> Array2D.length1
        let jMax  = oriGrid |> Array2D.length2
        /// Expand the range to adjacent cells and recursively dive into valid paths.
        let rec cross (grid: int [,]) i j (next: int) (depth: int) (dir: Direction) =
            let checkGrid = check grid iMax jMax nFix
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

    /// Get path depth = max achieved depth.
    let rec getDepth = function
        | End -> 0
        | Node ((_, depth , _) , up , right , left , down)
            -> List.max [depth ; getDepth up ; getDepth right ; getDepth left ; getDepth down]

    /// Number of solutions for given number of fixed points.
    let rec getCount nFix dim = function
        | End -> 0
        | Node ((curr, depth, _) , up , right , left , down) when depth = (dim * dim - 1) && curr = nFix
            -> 1 + getCount nFix dim up + getCount nFix dim right + getCount nFix dim left + getCount nFix dim down
        | Node (_ , up , right , left , down) 
            -> 0 + getCount nFix dim up + getCount nFix dim right + getCount nFix dim left + getCount nFix dim down

module Generate =

    let private rnd = System.Random()

    /// Put closure on dim, return pop function.
    let internal rndGen dim =
        fun () -> rnd.Next(dim*dim)

    /// Generate list with locations of fixed points.
    let private constr pop nFix =
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
    /// Calculates a random new grid.
    let internal nxtGrid grid iOri jOri dim nFix pop =
        let list = constr pop nFix
        match list with
        | [] -> failwith "List with fixed points cannot be empty."
        | (_, x) :: _ -> iOri := (x / dim) ; jOri := (x % dim)
        // Use list to transform the zero value grid.
        grid := Array2D.create dim dim 0
        list 
        |> List.iter 
            (fun (n, x) 
                ->  let i = x / dim
                    let j = x % dim 
                    (!grid).[i,j] <- n
            )

    /// Create a new puzzle of given dimension and number of fixed points.
    let newPuzzle dim nFix =
        if dim < 4 || dim > 6 || nFix < dim || nFix > 2 * dim then
            raise <| System.ArgumentException()
        else
            let pop  = rndGen dim
            let grid = ref <| Array2D.create dim dim 0
            let iOri , jOri = ref 0 , ref 0
            nxtGrid grid iOri jOri dim nFix pop
            let path = ref <| Solve.walkGrid !iOri !jOri !grid nFix
            while Solve.getCount nFix dim !path <> 1 do
                nxtGrid grid iOri jOri dim nFix pop
                path := Solve.walkGrid !iOri !jOri !grid nFix
            !grid