module Json
    open System.Text
    open System.Text.RegularExpressions
    let private objectRegex = new Regex("^{(.*)}$")
    let private memberRegex = new Regex("^\"(.+?)\":(.+)$")
    let private stringRegex = new Regex("^\"(.*)\"$")
    let private arrayRegex = new Regex("^\[(.*)\]$")

    type json =
        | Object of (string * json) list
        | Array of json list
        | Number of float
        | String of string
        | Bool of bool
        | Null 
        (* Display json for readable format *)
        member private this.convString(lineBreak, lineTab, currentIndent) =
            let rec tv i = match i with 0 | -1 -> "" | _ -> lineTab + tv(i-1)
            let t = tv currentIndent
            let nt = tv (currentIndent + 1)
            let bt = tv (currentIndent - 1)
            let objToString (x:(string * json))=
                let (k,v) = x in
                nt + @"""" + k + @"""" + ":" + v.convString(lineBreak, lineTab, currentIndent + 1)
            match this with
            | Number f -> f.ToString()
            | String s -> @"""" + s + @""""
            | Bool b -> match b with true -> "true" | false -> "false"
            | Null -> "null"
            | Array arr -> bt + "[" + lineBreak + 
                                List.fold (fun (i:string) (j:json) -> (i + "," + lineBreak + (j.convString(lineBreak, lineTab, currentIndent + 1)))) (t + List.head(arr).convString(lineBreak, lineTab, currentIndent + 1)) (List.tail arr) +  lineBreak
                               + t + "]"
            | Object obj ->bt + "{" + lineBreak + 
                                List.fold (fun i j -> (i + "," + lineBreak + objToString(j))) (obj |> List.head |> objToString) (List.tail obj) + lineBreak
                               + t + "}"
        override this.ToString() =
            this.convString("", "", 0)
        member this.ToString(onFormat) =
            match onFormat with true -> this.convString("\n", "    ", 0) | false -> this.ToString()

        
    (* Define Active Pattern For Json *)
    let private (|Object|_|) (x:string) =
        let result = objectRegex.Match x in
        if result.Success then (result.Groups.[1].Value) |> Some else None

    let private (|Array|_|) (x:string) =
        let result = arrayRegex.Match x in
        if result.Success then (result.Groups.[1].Value) |> Some else None
        
    let private (|Number|_|) x =
        let result, value = System.Double.TryParse(x) 
        if result then value |> Some else None

    let private (|String|_|) x =
        let result = stringRegex.Match x in
        if result.Success then result.Groups.[1].Value |> Some else None

    let private (|Bool|_|) x =
        match x with "true" -> true |> Some | "false" -> false |> Some | _ -> None

    let private (|Null|_|) x =
        match x with | "null" -> Null |> Some | _ -> None

    let private (|Ignore|Element|) x =
        match x with | ' ' | '\t' | '\r' | '\n' -> Ignore | _ -> Element


    let rec private ignoreSpeceImpl (x:char list, isEscapedChar : bool, onString : bool, temp: char list) : char list=
        match x with 
        | c::rest ->
            if isEscapedChar then ignoreSpeceImpl (rest, false, onString, c::temp)
            else
                match c with | Ignore -> ignoreSpeceImpl (rest, false, onString, if onString then c::temp else temp)
                             | Element -> 
                                 match c with
                                 | '\\' -> ignoreSpeceImpl (rest, true, onString, c::temp)
                                 | '"' ->  ignoreSpeceImpl (rest, false, not onString, c::temp)
                                 | _ ->    ignoreSpeceImpl (rest, false, onString, c::temp)
        | [] -> temp

    let private joinString (x:char list):string =
        x |> List.rev |> List.fold (fun x y -> x + y.ToString()) ""

    let rec private splitMembersImpl(x: char list, onEscaped:bool, stack : char list, answer: string list, temp: char list) : string list =
        let closure x =
            match stack with
            | [] -> false
            | h::t -> match h with
                      | '"' -> '"' = x
                      | '{' -> '}' = x
                      | '[' -> ']' = x
                      | _ -> false
        match x with 
        | c::rest -> 
            if onEscaped then
                splitMembersImpl(rest, false, stack, answer, c::temp)
            else
                match c with
                | '\\' -> splitMembersImpl(rest, true, stack, answer, c::temp)
                | '"' | '{' | '['  ->  
                    splitMembersImpl(rest, false, (if closure(c) then (List.tail stack) else c::stack), answer, c::temp)
                | ']' | '}' ->
                    splitMembersImpl(rest, false, (if closure(c) then (List.tail stack) else stack), answer, c::temp)
                | ',' -> if stack = [] then splitMembersImpl(rest, false, stack, joinString(temp)::answer, [])
                         else splitMembersImpl(rest, false, stack, answer, c::temp)
                | _ -> splitMembersImpl(rest, false, stack, answer, c::temp)
        | [] -> joinString(temp)::answer

    let private splitMembers (x:string) : string list =
        splitMembersImpl(x.ToCharArray() |> Array.toList, false, [], [], []) |> List.rev

    let private getMember (x:string) =
        let result = memberRegex.Match x in
        if result.Success then (result.Groups.[1].Value, result.Groups.[2].Value) |> Some else None


    (* remove ignorable char from string *)
    let private ignoreSpece (x:string) = 
        let source = x.ToCharArray() |> Array.toList
        ignoreSpeceImpl (source, false, false, []) |> joinString

    (* try to parse json recursive *)
    let rec private parseImpl (x : string):(json option) =
        match x with 
        | Object itm -> 
            let items = splitMembers itm
            try
                let value = items |> List.map(fun (i) -> getMember(i).Value) |> List.map (fun (k,v) -> (k, parseImpl(v).Value))
                Object(value) |> Some
            with e->
                None
        | Array itm ->
            let items = splitMembers itm in
            try 
                let value = items |> List.map(fun i -> (parseImpl i).Value)
                Array(value) |> Some
            with e ->
                None
        | String value -> String(value) |> Some
        | Number value -> Number(value) |> Some
        | Bool value -> Bool(value) |> Some
        | Null -> Null |> Some
        | _ -> None

    /// <summary>
    /// Jsonをパースしてレコードを構築します<br/>
    /// パースできない場合, Noneが返される
    /// </summary>
    let Parse (x:string) : json option =
        x |> ignoreSpece |> parseImpl