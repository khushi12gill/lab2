type Coach = {
  Name: string;
  FormerPlayer: bool
}

type Stats = {
  Wins: int;
  Losses: int;
}

type Team = {
  Name: string;
  Coach: Coach;
  Stats: Stats;
}

let teams : Team list = [
    { Name = "Denver Nuggets"; Coach = { Name = "Michael Malone"; FormerPlayer = true }; Stats = { Wins = 1897; Losses = 1890 } }
    { Name = "Milwaukee Bucks"; Coach = { Name = "Adrian Griffin"; FormerPlayer = false }; Stats = { Wins = 2340; Losses = 2103 } }
    { Name = "Indiana Pacers"; Coach = { Name = "Rick Carlisle"; FormerPlayer = false }; Stats = { Wins = 1883; Losses = 1903 } }
    { Name = "Miami Heat"; Coach = { Name = "Erik Spoelstra"; FormerPlayer = true }; Stats = { Wins = 2300; Losses = 2000 } }
    { Name = "Houston Rockets"; Coach = { Name = "Ime Udoka"; FormerPlayer = true }; Stats = { Wins = 2328; Losses = 2196 } }
]

let sTeams =
    teams
    |> List.filter (fun team -> team.Stats.Wins > team.Stats.Losses)
    |> List.map (fun team -> team.Name)

printfn "Names of Successful Teams: %A" sTeams


let calculateSuccessPercentage team =
    float team.Stats.Wins / float (team.Stats.Wins + team.Stats.Losses) * 100.0

let successPercentages =
    teams
    |> List.map (fun team -> (team.Name, calculateSuccessPercentage team))

printfn "Percentages of each teams: %A" successPercentages





type Cuisine =
    | Korean
    | Turkish

type MovieType =
    | Regular
    | IMAX
    | DBOX
    | RegularWithSnacks
    | IMAXWithSnacks
    | DBOXWithSnacks

type Activity =
    | BoardGame
    | Chill
    | Movie of MovieType * Cuisine  
    | Restaurant of Cuisine
    | LongDrive of int * float

let calculateBudget (activity : Activity) : float =
    match activity with
    | BoardGame | Chill -> 0.0
    | Movie (movieType, cuisine) ->  
        match movieType with
        | Regular -> 12.0
        | IMAX -> 17.0
        | DBOX -> 20.0
        | RegularWithSnacks | IMAXWithSnacks | DBOXWithSnacks -> 5.0 
    | Restaurant cuisine ->
        match cuisine with
        | Korean -> 70.0
        | Turkish -> 65.0
    | LongDrive (distance, fuel) -> float distance * fuel

let MBudget = calculateBudget (Movie (Regular, Korean))
printfn "Movie Budget: %.2f CAD" MBudget

let RBudget = calculateBudget (Restaurant Turkish)
printfn "Restaurant Budget: %.2f CAD" RBudget

let DriveBudget = calculateBudget (LongDrive (100, 0.5))
printfn "Long Drive Budget: %.2f CAD"DriveBudget










