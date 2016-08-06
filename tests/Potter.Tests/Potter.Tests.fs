module Potter.Tests
open NUnit.Framework
open FsUnit
open Potter

[<Test>]
let ``empty basket costs 0``() =
  [] |> calculatePrice |> should equal 0.

[<Test>]
let ``1 book costs 8``() =
  [One] |> calculatePrice |> should equal 8.

[<Test>]
let ``2 different books gives 5% discount``() =
  [One; Two] |> calculatePrice |> should equal 15.2

[<Test>]
let ``3 different books gives 10% discount``() =
  [One; Two; Three] |> calculatePrice |> should equal 21.6

[<Test>]
let ``4 different books gives 20% discount``() =
  [One; Two; Three; Four] |> calculatePrice |> should equal 25.6

[<Test>]
let ``5 different books gives 25% discount``() =
  [One; Two; Three; Four; Five] |> calculatePrice |> should equal 30.

[<Test>]
let ``discount applies for distinct books``() =
  [One; One; Two] |> calculatePrice |> should equal (8. + 15.2)

[<Test>]
let ``optimize basket before calculating discount``() =
  [One; One; Two; Two; Three; Three; Four; Five] |> calculatePrice |> should equal 51.2
