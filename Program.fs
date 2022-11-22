module Bowling

type Frame =
  | Strike
  | Spare of int
  | Partial of int * int
  | Bonus of int
  //| TenthFrame of int * ((int * int option) option)

module Frame =
  let private toInt =
    function
    | '-' -> 0
    | 'X' -> 10
    | c -> c |> System.Char.GetNumericValue |> int
  let parse (s: string) =
    List.ofSeq s
    |> fun chrs ->
      match chrs with
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
        | xs -> failwithf "Invalid Bonus Frame: %A" xs

  let private rand = System.Random()

  let random () =
    match rand.Next(10) with
    | 10 -> Strike
    | x ->
      match (x, rand.Next(10 - x)) with
      | (x,y) when x+y=10 -> Spare x
      | (x,y) -> Partial (x,y)

  type BonusFrameCount = | Zero | One | Two

  let randomBonus (numFrames: BonusFrameCount) =
    match numFrames with
    | Zero -> []
    | One -> [ 10 |> rand.Next |> Bonus ]
    | Two ->
      let x = rand.Next(10)
      [ Bonus x; Bonus (rand.Next(10 - x)) ]

type Game = Frame list    

module Game =
  let parse (s: string) : Game =
    s.Split(' ')
    |> Array.mapi (fun i s' ->
      if i < 10 then
        [ Frame.parse s' ]
      else
        Frame.parseBonus s')
    |> List.ofArray
    |> List.collect id

  let private (|StrikeThrown|_|) (game: Game) =
    match game with
    | Strike :: tail ->
      let addScore (score: int) =
        match tail with
        | Bonus x :: Bonus y :: [] -> score + 10 + x + y
        | Spare _ :: _ -> score + 20
        | Strike :: Strike :: _ -> score + 30
        | Strike :: Spare x :: _
        | Strike :: Partial (x,_) :: _
        | Strike :: Bonus x :: _ -> score + 20 + x
        | Partial (x,y) :: _ -> score + x + y
        | _ -> failwithf "Invalid remaining frames: %A" game
      Some (addScore, tail)
    | _ -> None

  let private (|SpareThrown|_|) (game: Game) =
    match game with
    | Spare _ :: tail ->
      let addScore (score: int) =
        match tail with
        | Bonus x :: [] -> score + 10 + x
        | Strike :: _ -> score + 20
        | Spare x :: _
        | Partial (x,_) :: _ -> score + 10 + x
        | _ -> failwithf "Invalid remaining frames: %A" game
      Some (addScore, tail)
    | _ -> None

  let rec private calc (game: Game) (score: int) =
    match game with
    | StrikeThrown (addScore, tail) -> addScore score |> calc tail
    | SpareThrown (addScore, tail) -> addScore score |> calc tail
    | Partial (x,y) :: tail -> score + x + y |> calc tail
    | []
    // into bonus frames, so no additional scoring
    | Bonus _ :: _ -> score
    | _ -> failwithf "Invalid remaining frames: %A" game

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
