let salamount = [75000; 48000; 120000; 190000; 300113; 92000; 36000]
let HI salary = salary > 100000
let hISalaries = List.filter HI salamount
printfn "List of High-Income Salaries: %A" hISalaries


let calTax salary = match salary with
    | s when s <= 49020 -> 0.15 * float s
    | s when s <= 98040 -> 0.205 * float s
    | s when s <= 151978 -> 0.26 * float s
    | s when s <= 216511 -> 0.29 * float s
    | _ -> 0.205 * float salary
let T = List.map calTax salamount
printfn "Salaries list: %A" salamount
printfn "Taxes: %A" T


let add salary =
    if salary < 49020 then salary + 20000
    else salary
let addSal = List.map add salamount
printfn "Salaries list: %A" salamount
printfn "Salaries After Adding: %A" addSal


let filtersalBet salary =
    salary >= 50000 && salary <= 100000
let salBet = List.filter filtersalBet salamount
let SumsalBet = List.fold (fun acc salary -> acc + salary) 0 salBet
printfn "Salaries list: %A" salamount
printfn "Salaries in b/w 50,000 & 100,000: %A" salBet
printfn "Sum of Salaries in b/w 50,000 & 100,000: %d" SumsalBet





let sum k =
    let rec h cal u =
        if u > k then
            cal
        else
            h (cal + u) (u + 3)
    
    h 0 3

let result = sum 27
printfn "FinalOutput: %d" result