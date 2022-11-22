module Bowling

type Frame =
  | Strike
  | Spare of int
  | Partial of int * int
  | Bonus of int

module Frame =
  let private toInt =
    function
    | '-' -> 0
    | 'X' -> 10
    | c -> c |> System.Char.GetNumericValue |> int
  let parse =
    List.ofSeq
    >> function
        | 'X' :: [] -> Strike
        | c :: '/' :: [] -> c |> toInt |> Spare
        | x :: y :: [] -> Partial (toInt x, toInt y)
        | xs -> failwithf "Invalid Frame: %A" xs
  let parseBonus (xs: char seq) =
    xs
    |> List.ofSeq
    |> function
        | 'X' :: [] -> [ Bonus 10 ]
        | c :: '/' :: [] ->
          let x = toInt c
          [ Bonus x; Bonus (10 - x) ]
        | x :: y :: [] -> [ Bonus (toInt x); Bonus (toInt y) ]
        | c :: [] -> [ c |> toInt |> Bonus ]
        | xs -> failwithf "Invalid Frame: %A" xs

type Game = Frame list    

module Game =
  let parse (s: string) : Game =
    s.Split(' ')
    |> Array.indexed
    |> Array.map (fun (i,s') ->
      if i < 10 then
        [ Frame.parse s' ]
      else
        Frame.parseBonus s')
    |> List.ofArray
    |> List.collect id

  let rec private calc (game: Game) (score: int) =
    match game with
    | []
    | Bonus _ :: _ -> score
    | Strike :: tail ->
      match tail with
      | Bonus x :: Bonus y :: [] -> score + 10 + x + y
      | Spare _ :: _ -> (score + 20)
      | Strike :: Strike :: _ -> (score + 30)
      | Strike :: Spare x :: _
      | Strike :: Partial (x,_) :: _
      | Strike :: Bonus x :: _ -> (score + 20 + x)
      | Partial (x,y) :: _ -> (score + x + y)
      | _ -> failwithf "Invalid remaining frames: %A" game
      |> calc tail
    | Spare _ :: tail ->
      match tail with
      | Bonus x :: [] -> score + 10 + x
      | Strike :: _ -> (score + 20)
      | Spare x :: _
      | Partial (x,_) :: _ -> (score + 10 + x)
      | _ -> failwithf "Invalid remaining frames: %A" game
      |> calc tail
    | Partial (x,y) :: tail -> calc tail (score + x + y)

  let calculate (game: Game) = calc game 0

  let display (game: Game, score: int) =
    game |> List.iter (printfn "%A")
    score |> printfn "-----------------\n\rScore: %i\r\n******************\r\n"

[ "X X X X X X X X X X X X"
  "9- 9- 9- 9- 9- 9- 9- 9- 9- 9-"
  "5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5" ]
|> List.iter (fun s ->
  let game = Game.parse s
  (game, Game.calculate game) |> Game.display)
