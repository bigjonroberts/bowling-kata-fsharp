module Bowling

type Frame =
  | Strike
  | Spare of int
  | Partial of int * int
  | TenthFrame of int * int * int option

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
  let parseTenth (xs: char seq) =
    xs
    |> List.ofSeq
    |> function
        | 'X' :: c2  :: '/' :: [] -> c2 |> toInt |> fun x2 -> TenthFrame (10,x2, Some (10 - x2))
        | 'X' :: c1  :: c2  :: [] -> TenthFrame (10, toInt c1, Some(toInt c2))
        | c1  :: '/' :: c2  :: [] -> c1 |> toInt |> fun x -> TenthFrame (x, 10 - x, c2 |> toInt |> Some)
        | c1  :: c2  :: []        -> TenthFrame (toInt c1, toInt c2, None)
        | _ -> failwithf "Invalid Tenth Frame: %A" xs

  let calcTenth (x1, x2, bonus) = (bonus |> Option.defaultValue 0) + x1 + x2

  let private rand = System.Random()

  let random () =
    match rand.Next 11 with
    | 10 -> Strike
    | x ->
      match (x, rand.Next(11 - x)) with
      | (x,y) when x+y=10 -> Spare x
      | (x,y) -> Partial (x,y)

  let randomTenth () : Frame =
    ( match rand.Next 11 with
      | 10 ->
        let y = rand.Next 11
        let z =
          if y = 10 then
            11 |> rand.Next
          else
            (11 - y) |> rand.Next
        (10,y,Some z)
      | x ->
        let y = rand.Next(11 - x)
        if x + y = 10 then
          (x,y, 11 |> rand.Next |> Some)
        else
          (x,y,None) )
    |> TenthFrame

type Game = Frame list    

module Game =
  let parse (s: string) : Game =
    s.Split(' ')
    |> Array.mapi (fun i s' ->
      if i < 9 then
        Frame.parse s'
      else
        Frame.parseTenth s')
    |> List.ofArray

  let private (|StrikeThrown|_|) (game: Game) =
    match game with
    | Strike :: tail ->
      let frameScore =
        match tail with
        | Spare _            :: _                        -> 20
        | Strike             :: Strike             :: _  -> 30
        | Strike             :: Spare x            :: _
        | Strike             :: Partial (x,_)      :: _ 
        | Strike             :: TenthFrame (x,_,_) :: _  -> 20 + x
        | Partial (x,y)      :: _
        | TenthFrame (x,y,_) :: []                       -> 10 + x + y
        | _ -> failwithf "Invalid remaining frames: %A" game
      Some (frameScore, tail)
    | _ -> None

  let private (|SpareThrown|_|) (game: Game) =
    match game with
    | Spare _ :: tail ->
      let frameScore =
        match tail with
        | Strike             :: _ -> 20
        | Spare x            :: _
        | TenthFrame (x,_,_) :: []
        | Partial (x,_)      :: _ -> 10 + x
        | _ -> failwithf "Invalid remaining frames: %A" game
      Some (frameScore, tail)
    | _ -> None

  let rec private calc (game: Game) (score: int) =
    match game with
    | StrikeThrown (frameScore, tail) -> score + frameScore |> calc tail
    | SpareThrown (frameScore, tail) -> score + frameScore |> calc tail
    | Partial (x,y) :: tail -> score + x + y |> calc tail
    | TenthFrame (x1,x2,bonus) :: [] -> score + Frame.calcTenth (x1,x2,bonus)
    | [] -> score
    | _ -> failwithf "Invalid remaining frames: %A" game

  let calculate (game: Game) = calc game 0

  let display (game: Game, score: int) =
    game |> List.iter (printfn "%A")
    score |> printfn "-----------------\n\rScore: %i\r\n******************\r\n"

  let random _ : Game = List.init 10 (function | 9 -> Frame.randomTenth () | _ -> Frame.random ())
    
[ "X X X X X X X X X XXX"
  "9- 9- 9- 9- 9- 9- 9- 9- 9- 9-"
  "5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/5" ]
|> List.map Game.parse
|> List.append (List.init 30 Game.random)
|> List.iter (fun game -> (game, Game.calculate game) |> Game.display)
