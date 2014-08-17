// F# の詳細については、http://fsharp.net を参照してください
// 詳細については、'F# チュートリアル' プロジェクトを参照してください。
open Json
open System.Net

[<EntryPoint>]
let main argv = 
    let getCoord x : json =
        match x with
        | Object l -> l |> List.find(fun (k, v) -> k = "coord") |> snd
        | _ -> Null
    let wc = new WebClient() in
    let result = wc.DownloadString(@"http://api.openweathermap.org/data/2.5/weather?q=""Tokyo"",""jp""")
                |> Json.Parse |> Option.get |> getCoord
    
    printfn "%s" (result.ToString(true))
    0 // 整数の終了コードを返します
