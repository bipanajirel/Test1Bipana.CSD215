// For more information see https://aka.ms/fsharp-console-apps
//Bipana Jirel
//22071147
printfn "Bipana Jirel"
// Mapping, Filtering through Lists

let allSal = [75000; 48000; 120000; 190000; 300113; 92000; 36000]

// Filter high salary
let highSal = List.filter (fun salary -> salary > 100000)  allSal
printfn "High Sal: %A" highSal

// Federal income-tax calculation function
let calTax salary =
    match salary with
    | s when s < 49020 -> s + 20000
    | s when s >= 50000 && s <= 100000 -> s
    | _ -> salary


// Total tax for all salaries
let taxedSal = List.map calTax  allSal
printfn " Total taxes for all sal: %A" taxedSal

// Sum salaries between $50,000 and $100,000
let MedSal = List.filter (fun salary -> salary >= 50000 && salary <= 100000)  allSal |> List.sum
printfn "Sum of salaries between $50,000 and $100,000: %d" MedSal

// Tail-recursive function for calculating sum of multiples of 3
let rec ParameterThree n acc =
    match n with
    | 0 -> acc
    | _ -> ParameterThree (n - 3) (acc + n)

// Example
let result = ParameterThree 27 0
printfn "Sum of multiples of 3 up to 27: %d"result
