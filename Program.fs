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
    teamss
    |> List.filter (fun team -> team.Stats.Wins > team.Stats.Losses)
    |> List.map (fun team -> team.Name)

printfn "Names of Successful Teams: %A" sTeams


let calculateSuccessPercentage team =
    float team.Stats.Wins / float (team.Stats.Wins + team.Stats.Losses) * 100.0

let successPercentages =
    teams
    |> List.map (fun team -> (team.Name, calculateSuccessPercentage team))

printfn "Percentages of each teams: %A" successPercentages
