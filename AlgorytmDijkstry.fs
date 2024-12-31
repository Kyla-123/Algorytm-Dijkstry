open System
open System.Collections.Generic

// Funkcja pomocnicza: Zbiera wszystkie wierzchołki w grafie
let zbierzWierzcholki (graf: Map<int, (int * int) list>) =
    let wierzcholki = HashSet<int>()
    for kvp in graf do
        wierzcholki.Add(kvp.Key) |> ignore
        for (sąsiad, _) in kvp.Value do
            wierzcholki.Add(sąsiad) |> ignore
    wierzcholki

// Funkcja algorytmu Dijkstry
let dijkstra (graf: Map<int, (int * int) list>) (wierzcholekStartowy: int) (wierzcholekKoncowy: int) =
    // 1. Zbieramy wszystkie wierzchołki w grafie
    let wszystkieWierzcholki = zbierzWierzcholki graf

    // 2. Inicjalizacja zmiennych
    let dystanse = Dictionary<int, int>() // Dystanse od wierzchołka początkowego
    let poprzednieWierzcholki = Dictionary<int, int option>() // Poprzednie wierzchołki na ścieżce
    let kolejkaPriorytetowa = SortedSet<(int * int)>(Comparer<(int * int)>.Create(fun (d1, n1) (d2, n2) ->
        if d1 <> d2 then d1.CompareTo(d2)
        else n1.CompareTo(n2)
    )) // Kolejka priorytetowa

    // 3. Inicjalizacja początkowa
    for wierzcholek in wszystkieWierzcholki do
        dystanse.[wierzcholek] <- if wierzcholek = wierzcholekStartowy then 0 else Int32.MaxValue
        poprzednieWierzcholki.[wierzcholek] <- None
        if wierzcholek = wierzcholekStartowy then
            kolejkaPriorytetowa.Add((0, wierzcholek)) |> ignore

    // 4. Pętla przeszukująca graf
    let mutable znaleziono = false // Flaga do zatrzymania pętli
    while kolejkaPriorytetowa.Count > 0 && not znaleziono do
        let (aktualnaOdleglosc, aktualnyWierzcholek) = kolejkaPriorytetowa.Min
        kolejkaPriorytetowa.Remove(kolejkaPriorytetowa.Min) |> ignore

        if aktualnyWierzcholek = wierzcholekKoncowy then
            znaleziono <- true // Zatrzymujemy algorytm, jeśli dotarliśmy do końcowego wierzchołka
        else
            match graf.TryFind aktualnyWierzcholek with
            | Some sasiedzi ->
                for (sąsiad, waga) in sasiedzi do
                    let nowaOdleglosc = aktualnaOdleglosc + waga
                    if nowaOdleglosc < dystanse.[sąsiad] then
                        kolejkaPriorytetowa.Remove((dystanse.[sąsiad], sąsiad)) |> ignore // Usuń stary dystans
                        dystanse.[sąsiad] <- nowaOdleglosc
                        poprzednieWierzcholki.[sąsiad] <- Some aktualnyWierzcholek
                        kolejkaPriorytetowa.Add((nowaOdleglosc, sąsiad)) |> ignore // Dodaj nowy dystans
            | None -> () // Jeśli brak sąsiadów, przechodzimy dalej

    // 5. Rekonstrukcja ścieżki
    let rec zbudujSciezke aktualnyWierzcholek sciezka =
        match poprzednieWierzcholki.[aktualnyWierzcholek] with
        | Some poprzedni -> zbudujSciezke poprzedni (aktualnyWierzcholek :: sciezka)
        | None -> aktualnyWierzcholek :: sciezka

    if dystanse.[wierzcholekKoncowy] = Int32.MaxValue then
        None // Brak ścieżki
    else
        Some (dystanse.[wierzcholekKoncowy], zbudujSciezke wierzcholekKoncowy [])

// Przykładowy graf
let graf = 
    Map.ofList [
        1, [(2, 2); (3, 4)]
        2, [(3, 1); (4, 7)]
        3, [(5, 3)]
        4, [(6, 1)]
        5, [(4, 2); (6, 5)]
    ]

// Wywołanie algorytmu
let wynik = dijkstra graf 1 4
match wynik with
| Some (odleglosc, sciezka) -> 
    printfn "Najkrótsza odległość: %d" odleglosc
    printfn "Ścieżka: %A" sciezka
| None -> 
    printfn "Brak ścieżki"
