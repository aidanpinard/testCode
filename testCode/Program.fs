// Learn more about F# at http://fsharp.org

[<EntryPoint>]
let main argv =
    // get each line of input from console 
    // terminates on empty line
    let getInputFromConsole lineNo =
        let text = System.Console.ReadLine()
        if System.String.IsNullOrEmpty(text) then None else Some (text,lineNo+1)
    
    let listUnfold = List.unfold getInputFromConsole 1
    printfn "%A" listUnfold
    printfn "Hello World from F#!"
    0 // return an integer exit code

