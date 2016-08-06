module Potter

type Book = One | Two | Three | Four | Five

let splitIntoLots bs =
  let selectBestCandidateLot f lots =
    let candidateLots, otherLots = lots |> List.partition (fun bs -> bs |> f)
    let sortedLots = candidateLots |> List.sort
    let selectedLot, unselectedLots =
      match sortedLots with
      | [] -> ([], [])
      | y :: ys -> (y, ys)
    (selectedLot, unselectedLots @ otherLots)

  let rec dispatch bs lots =
    match bs with
    | [] -> lots
    | x::xs ->
      let selectedLot, otherLots = lots |> selectBestCandidateLot (List.exists (fun y -> x = y) >> not)
      dispatch xs ((x :: selectedLot) :: otherLots)
  dispatch bs []

let calculateDiscount bs =
  match List.length bs with
  | 2 -> 0.05
  | 3 -> 0.1
  | 4 -> 0.2
  | 5 -> 0.25
  | _ -> 0.

let adornateWithDiscount bs =
  (bs, bs |> calculateDiscount)

let applyDiscount (bs, discount) =
  (float (List.length bs)) * 8. * (1. - discount)

let calculatePrice bs =
  bs |> splitIntoLots |> List.map (adornateWithDiscount >> applyDiscount) |> List.sum
