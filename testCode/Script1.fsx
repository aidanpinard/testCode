// A random collection of fragments of code from the
// fsharpforfunandprofit.com
// =============================================================
// Links for extra long sets of code
//// https://github.com/swlaschin/13-ways-of-looking-at-a-turtle




open System.Net
open System

let fetchUrlAsync url =
    async {
    let req = WebRequest.Create(Uri(url))
    use! resp = req.AsyncGetResponse() // new keyword "use!"
    use stream = resp.GetResponseStream()
    use reader = new IO.StreamReader(stream)
    let html = reader.ReadToEnd()
    printfn "finished downloading %s" url
    }

let sites = ["http://www.bing.com";
    "http://www.google.com";
    "http://www.microsoft.com";
    "http://www.amazon.com";
    "http://www.yahoo.com"]

#time // turn interactive timer on
sites
    |> List.map fetchUrlAsync // make a list of async tasks
    |> Async.Parallel // set up the tasks to run in parallel
    |> Async.RunSynchronously // start them off
#time


let childTask() =
    // chew up some CPU.
    for i in [1..2000] do
        for i in [1..1000] do
            do "Hello".Contains("H") |> ignore

#time
childTask()
#time

let parentTask =
    childTask
    |> List.replicate 20
    |> List.reduce (>>)
//test
#time
parentTask()
#time

let asyncChildTask = async { childTask() }

let asyncParentTask =
    asyncChildTask
    |> List.replicate 20
    |> Async.Parallel
//test
#time
asyncParentTask |> Async.RunSynchronously
#time

let slowConsoleWrite msg =
    msg |> String.iter (fun ch->
    System.Threading.Thread.Sleep(1)
    System.Console.Write ch
    )
// test in isolation
slowConsoleWrite "abc"

let makeTask logger taskId = async {
    let name = sprintf "Task%i" taskId
    for i in [1..3] do
        let msg = sprintf "-%s:Loop%i-" name i
        logger msg
    }
// test in isolation
let task = makeTask slowConsoleWrite 1
Async.RunSynchronously task

type UnserializedLogger() =
    // interface
    member this.Log msg = slowConsoleWrite msg

// test in isolation
let unserializedLogger = UnserializedLogger()    
unserializedLogger.Log "hello"

let unserializedExample =
    let logger = new UnserializedLogger()
    [1..5]
        |> List.map (fun i -> makeTask logger.Log i)
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore


type SerializedLogger() =
    // create the mailbox processor
    let agent = MailboxProcessor.Start(fun inbox ->
    // the message processing function
        let rec messageLoop () = async{
            // read a message
            let! msg = inbox.Receive()
            // write it to the log
            slowConsoleWrite msg
            // loop to top
            return! messageLoop ()
        }
        // start the loop
        messageLoop ()
        )
    // public interface
    member this.Log msg = agent.Post msg

// test in isolation
let serializedLogger = SerializedLogger()
serializedLogger.Log "hello"

let serializedExample =
    let logger = new SerializedLogger()
    [1..5]
    |> List.map (fun i -> makeTask logger.Log i)
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore




open System
open System.Threading
let createTimerAndObservable timerInterval =
    // setup a timer
    let timer = new System.Timers.Timer(float timerInterval)
    timer.AutoReset <- true
    // events are automatically IObservable
    let observable = timer.Elapsed
    // return an async task
    let task = async {
        timer.Start()
        do! Async.Sleep 5000
        timer.Stop()
    }
    // return a async task and the observable
    (task,observable)

type FizzBuzzEvent = {label:int; time: DateTime}

let areSimultaneous (earlierEvent,laterEvent) =
    let {label=_;time=t1} = earlierEvent
    let {label=_;time=t2} = laterEvent
    t2.Subtract(t1).Milliseconds < 50

// create the event streams and raw observables
let timer3, timerEventStream3 = createTimerAndObservable 300
let timer5, timerEventStream5 = createTimerAndObservable 500

// convert the time events into FizzBuzz events with the appropriate id
let eventStream3 = timerEventStream3 |> Observable.map (fun _ -> {label=3; time=DateTime.Now})
let eventStream5 = timerEventStream5 |> Observable.map (fun _ -> {label=5; time=DateTime.Now})

// combine the two streams
let combinedStream =
    Observable.merge eventStream3 eventStream5

// make pairs of events
let pairwiseStream =
    combinedStream |> Observable.pairwise

// split the stream based on whether the pairs are simultaneous
let simultaneousStream, nonSimultaneousStream =
    pairwiseStream |> Observable.partition areSimultaneous

// split the non-simultaneous stream based on the id
let fizzStream, buzzStream =
    nonSimultaneousStream
    // convert pair of events to the first event
    |> Observable.map (fun (ev1,_) -> ev1)
    // split on whether the event id is three
    |> Observable.partition (fun {label=id} -> id=3)
    //print events from the combinedStream
combinedStream
    |> Observable.subscribe (fun {label=id;time=t} ->
                                  printf "[%i] %i.%03i " id t.Second t.Millisecond)
//print events from the simultaneous stream
simultaneousStream
    |> Observable.subscribe (fun _ -> printfn "FizzBuzz")
//print events from the nonSimultaneous streams
fizzStream
    |> Observable.subscribe (fun _ -> printfn "Fizz")
buzzStream
    |> Observable.subscribe (fun _ -> printfn "Buzz")
// run the two timers at the same time
[timer3;timer5]
    |> Async.Parallel
    |> Async.RunSynchronously



open System.Windows.Forms
let form = new Form(Width= 400, Height = 300, Visible = true, Text = "Hello World")
form.TopMost <- true
form.Click.Add (fun args-> printfn "the form was clicked")
form.Show()





// ==================================
//     Stack Based Calculator
// Also known as the "reverse Polish
//            style".
// ==================================


// ===================================
//               Types
// ===================================
type Stack = StackContents of float list

// ==================================
//         Stack Primitives
// ==================================

/// Push a value onto the stack
let push x (StackContents contents) =
    StackContents (x::contents)

/// Pop a value from the stack and 
/// return it from the stack as a tuple
let pop (StackContents contents)=
    match contents with
    | top::rest -> 
        let newStack = StackContents rest
        (top, newStack)
    | [] ->
        failwith "Stack underflow"

// ==================================
//          Operator Core
// ==================================

/// pop the top two elements
/// do a binary operation on them
/// push the result
let binary mathFn stack= 
    let (y, stack') = pop stack
    let (x, stack'') = pop stack'
    let z = mathFn x y
    push z stack''

/// pop the top element
/// do a unary operation on it
/// push the result
let unary mathFn stack = 
    let (x, stack') = pop stack
    push (mathFn x) stack'

// ==================================
//            Other Core
// ==================================

/// pop and show the value on the stack
let SHOW stack = 
    let x,_ = pop stack
    printfn "the answer is %f" x
    stack

/// duplicate the value on top of the stack
let DUP stack = 
    let x, stack' = pop stack
    push x (push x stack')

/// swap the values on top of the stack
let SWAP stack = 
    let x, stack' = pop stack
    let y, stack'' = pop stack'
    push y (push x stack'')

/// remove value at the top of the stack
let DROP stack = 
    let _, stack' = pop stack
    stack'

// ==================================
//    Words based on primitives
// ==================================

/// Constants
/// ---------------------------------
let EMPTY = StackContents []
let START = EMPTY

/// Numbers
/// ---------------------------------
let ONE = push 1.0
let TWO = push 2.0
let THREE = push 3.0
let FOUR = push 4.0
let FIVE = push 5.0

/// Math Functions
/// ---------------------------------
let ADD = binary (+)
let SUB = binary (-)
let MUL = binary (*)
let DIV = binary (/)
let NEG = unary (fun x -> -x)

// ==================================
// Words based on composition
// ==================================

let SQUARE = 
    DUP >> MUL
let CUBE =
    DUP >> DUP >> MUL >> MUL
let SUM_UPTO =
    DUP                     // n
    >> ONE >> ADD           // n + 1
    >> MUL                  // n(n+1)
    >> TWO >> SWAP >> DIV   // n(n+1) / 2 [formula to sum upto]


START |> ONE |> TWO |> ADD |> FOUR |> SUB |> SHOW |> SQUARE |> CUBE |> FOUR |> SUM_UPTO |> SHOW




/// factorial on an array
let array = [1;2;3;4;5]

let summerArray array = 
    array |> List.fold (fun a x -> a * x) 1

printfn "%i" (summerArray array)



let makeResource name = 
    { new System.IDisposable
      with member this.Dispose() = printfn "%s disposed" name}

let exampleBinding name = 
    use potato = makeResource name
    printfn "done"

let exampleBindingWithLet name =
    let potato = makeResource (sprintf "%s with let" name)
    printfn "done with let"

exampleBinding "pot"
exampleBindingWithLet "pat"

let returnInvalidResource name = 
    use myResource = makeResource name
    myResource

let resource = returnInvalidResource "hello"



let randomNumbersUntilMatch matchValue max =
    let rng = new System.Random()
    let seqGen _ = rng.Next(max)
    let isNotMatch = (<>) matchValue

    Seq.initInfinite seqGen 
      |> Seq.takeWhile isNotMatch
      |> Seq.iter (printfn "%A")

    printfn "Found match %A" matchValue

randomNumbersUntilMatch 20 1000000

// throws a generic System.Exception
let f1 x =
    if x then "ok"
    else failwith "message"
    // throws an ArgumentException
let f2 x =
    if x then "ok"
    else invalidArg "paramName" "message"
    // throws a NullArgumentException
let f3 x =
    if x then "ok"
    else nullArg "paramName"

f3 false


// only shows your message. needs to 
// explicitly return the same as the
// try branch
let div x y = 
    try 
        (x+1)/y
    with 
    | :? System.DivideByZeroException as ex ->
        printfn "%s" ex.Message ; 0

// shows the exception error, automatically matches
// the try return type
let div2 x y = 
    try 
        (x+1)/y
    with 
    | :? System.DivideByZeroException as ex ->
        printfn "%s" ex.Message
        reraise()

div 1 1
div 1 0

// ----------------------------------
// pattern matching using regular 
// expressions
open System.Text.RegularExpressions

let checkEmail str = 
    match str with
    | x when Regex.Match(x, @".+@.+\.com").Success ->
        printfn "%s is a email bruh" x
    | x ->
        printfn "%s ain't a email" x

checkEmail "google@google.com"
checkEmail "mail.com"

// ----------------------------------
// pattern matching using regular 
// expressions and active patterns

open System.Text.RegularExpressions
// create an active pattern to match an email address
let (|EmailAddress|_|) input =
    let m = Regex.Match(input,@".+@.+")
    if (m.Success) then Some input else None
// use the active pattern in the match
let classifyString aString =
    match aString with
    | EmailAddress x ->
        printfn "%s is an email" x
    // otherwise leave alone
    | _ ->
        printfn "%s is something else" aString
//test
classifyString "alice@example.com"
classifyString "google.com"

// ----------------------------------
// pattern matching using arbitrary 
// conditionals
let fizzBuzz x =
    match x with
        | i when i % 15 = 0 ->
            printfn "fizzbuzz"
        | i when i % 3 = 0 ->
            printfn "fizz"
        | i when i % 5 = 0 ->
            printfn "buzz"
        | i ->
            printfn "%i" i

//test
[1..30] |> List.iter fizzBuzz

// ---------------------------------
let rows = [ (1,"a"); (-22,"bb"); (333,"ccc"); (-4444,"dddd") ]
// no alignment
for (i,s) in rows do
    printfn "|%i|%s|" i s
// with alignment
for (i,s) in rows do
    printfn "|%5i|%5s|" i s
// with left alignment for column 2
for (i,s) in rows do
    printfn "|%5i|%-5s|" i s
// with dynamic column width=20 for column 1
for (i,s) in rows do
    printfn "|%*i|%-5s|" 20 i s
// with dynamic column width for column 1 and column 2
for (i,s) in rows do
    printfn "|%*i|%-*s|" 20 i 10 s
// with alignment
for (i,s) in rows do
    printfn "|%5i|%5s|" i s
// with plus signs
for (i,s) in rows do
    printfn "|%+5i|%5s|" i s
// with zero pad
for (i,s) in rows do
    printfn "|%0+5i|%5s|" i s
// with left align
for (i,s) in rows do
    printfn "|%-5i|%5s|" i s
// with left align and plus
for (i,s) in rows do
    printfn "|%+-5i|%5s|" i s
// with left align and space instead of plus
for (i,s) in rows do
    printfn "|% -5i|%5s|" i s

let pi = 3.14
printfn "float: %f exponent: %e compact: %g" pi pi pi
let petabyte = pown 2.0 50
printfn "float: %f exponent: %e compact: %g" petabyte petabyte petabyte
let largeM = 123456789.123456789M // a decimal
printfn "float: %f decimal: %M" largeM largeM

open System.IO
//define the function
let printHello (tw:TextWriter) = tw.Write("hello")
//test it
printfn "custom function: %t" printHello

open System
open System.IO
//define the function using a closure
let printRand =
    let rand = new Random()
// return the actual printing function
    fun (tw:TextWriter) -> tw.Write(rand.Next(1,100))
//test it
for i in [1..5] do printfn "rand = %t" printRand

// function to format a date
let yymmdd1 (date:DateTime) = date.ToString("yy.MM.dd")
// function to format a date onto a TextWriter
let yymmdd2 (tw:TextWriter) (date:DateTime) = tw.Write("{0:yy.MM.dd}", date)
// test it
for i in [1..5] do
    let date = DateTime.Now.AddDays(float i)
    // using %s
    printfn "using ToString = %s" (yymmdd1 date)
    // using %a
    printfn "using a callback = %a" yymmdd2 date



// ----------------------------------
// using kprintf
open System
open System.IO
// a logging library such as log4net
// or System.Diagnostics.Trace
type Logger(name) =
    let currentTime (tw:TextWriter) =
        tw.Write("{0:s}",DateTime.Now)
    let logEvent level msg =
        printfn "%t %s [%s] %s" currentTime level name msg
    member this.LogInfo msg =
        logEvent "INFO" msg
    member this.LogError msg =
        logEvent "ERROR" msg
    static member CreateLogger name =
        new Logger(name)

// my application code
module MyApplication =
    let logger = Logger.CreateLogger("MyApp")
    // create a logInfo using the Logger class
    let logInfo format =
        let doAfter s =
            logger.LogInfo(s)
        Printf.ksprintf doAfter format
    // create a logError using the Logger class
    let logError format =
        let doAfter s =
            logger.LogError(s)
            System.Windows.Forms.MessageBox.Show(s) |> ignore
        Printf.ksprintf doAfter format
    // function to exercise the logging
    let test() =
        do logInfo "Message #%i" 1
        do logInfo "Message #%i" 2
        do logError "Oops! an error occurred in my app"

MyApplication.test()



// commandline arguments parser
// [fsharpforfunandforprofit pg. 461]
// Version 1 not show as it is nearly
// identical to 2 (2 has slight 
// improvements
open System.Windows.Forms
module CommandLineParserV2 = 
    type OrderByOption = OrderBySize | OrderByName
    type SubdirectoryOption = IncludeSubdirectories | ExcludeSubdirectories
    type VerboseOption = VerboseOutput | TerseOutput
    type CommandLineOptions = {
        verbose: VerboseOption;
        subDirectories: SubdirectoryOption;
        orderBy: OrderByOption
        }

    let parseCommandLine args =
        let defaultArgs = {
            verbose = TerseOutput;
            subDirectories = ExcludeSubdirectories;
            orderBy = OrderByName;
            }

        let rec parseCommandLineRec args optionsSoFar =
            match args with
            | [] -> 
                optionsSoFar
            | "/v"::xs ->
                let newOptionsSoFar = { optionsSoFar with verbose = VerboseOutput }
                parseCommandLineRec xs newOptionsSoFar
            | "/s"::xs ->
                let newOptionsSoFar = { optionsSoFar with subDirectories = IncludeSubdirectories }
                parseCommandLineRec xs newOptionsSoFar
            | "/o"::xs -> 
                match xs with
                | "S"::xss ->
                    let newOptionsSoFar = { optionsSoFar with orderBy = OrderBySize }
                    parseCommandLineRec xss newOptionsSoFar
                | "N"::xss ->
                    let newOptionsSoFar = { optionsSoFar with orderBy = OrderByName }
                    parseCommandLineRec xss newOptionsSoFar
                | _ -> 
                    let error = "OrderBy requires a second argument"
                    eprintfn "%s" error
                    MessageBox.Show(error, "Error") |> ignore
                    parseCommandLineRec xs optionsSoFar
            | x::xs -> 
                let error = sprintf "Argument %s is unrecognised" x
                eprintfn "%s" error
                MessageBox.Show(error, "Error") |> ignore
                parseCommandLineRec xs optionsSoFar

        parseCommandLineRec args defaultArgs

    parseCommandLine ["/v"; "/o"; "S";]
    parseCommandLine ["/s"; "/o";]
    parseCommandLine ["/s"; "/i";]


// Command line Parser With Fold
// This type is more complex and has 
// a few issues
/// Version 2 is more reccomended
module CommandLineParserV3 =
    type OrderByOption = OrderBySize | OrderByName
    type SubdirectoryOption = IncludeSubdirectories | ExcludeSubdirectories
    type VerboseOption = VerboseOutput | TerseOutput
    type CommandLineOptions = {
        verbose: VerboseOption;
        subdirectories: SubdirectoryOption;
        orderby: OrderByOption
        }
    type ParseMode = TopLevel | OrderBy
    type FoldState = {
        options: CommandLineOptions ;
        parseMode: ParseMode;
    }
    // parse the top-level arguments
    // return a new FoldState
    let parseTopLevel arg optionsSoFar =
        match arg with
        // match verbose flag
        | "/v" ->
            let newOptionsSoFar = {optionsSoFar with verbose=VerboseOutput}
            {options=newOptionsSoFar; parseMode=TopLevel}
        // match subdirectories flag
        | "/s"->
            let newOptionsSoFar = { optionsSoFar with subdirectories=IncludeSubdirectories}
            {options=newOptionsSoFar; parseMode=TopLevel}
        // match sort order flag
        | "/o" ->
            {options=optionsSoFar; parseMode=OrderBy}
        // handle unrecognized option and keep looping
        | x ->
            printfn "Option '%s' is unrecognized" x
            {options=optionsSoFar; parseMode=TopLevel}
    // parse the orderBy arguments
    // return a new FoldState
    let parseOrderBy arg optionsSoFar =
        match arg with
        | "S" ->
            let newOptionsSoFar = { optionsSoFar with orderby=OrderBySize}
            {options=newOptionsSoFar; parseMode=TopLevel}
        | "N" ->
            let newOptionsSoFar = { optionsSoFar with orderby=OrderByName}
            {options=newOptionsSoFar; parseMode=TopLevel}
        // handle unrecognized option and keep looping
        | _ ->
            printfn "OrderBy needs a second argument"
            {options=optionsSoFar; parseMode=TopLevel}
    // create a helper fold function
    let foldFunction state element =
        match state with
        | {options=optionsSoFar; parseMode=TopLevel} ->
            // return new state
            parseTopLevel element optionsSoFar
        | {options=optionsSoFar; parseMode=OrderBy} ->
            // return new state
            parseOrderBy element optionsSoFar
    // create the "public" parse function

    let parseCommandLine args =
        let defaultOptions = {
            verbose = TerseOutput;
            subdirectories = ExcludeSubdirectories;
            orderby = OrderByName
        }
        let initialFoldState =
            {options=defaultOptions; parseMode=TopLevel}
        // call fold with the initial state
        args |> List.fold foldFunction initialFoldState

// ==============================
// tests
// happy path
CommandLineParserV3.parseCommandLine ["/v"]
CommandLineParserV3.parseCommandLine ["/v"; "/s"]
CommandLineParserV3.parseCommandLine ["/o"; "S"]
// error handling
CommandLineParserV3.parseCommandLine ["/v"; "xyz"]
CommandLineParserV3.parseCommandLine ["/o"; "xyz"]
CommandLineParserV3.parseCommandLine ["/o"; "/v"] // note /v is not set




// Show an Error
let buttons = System.Windows.Forms.MessageBoxButtons.OK
let icon = System.Windows.Forms.MessageBoxIcon.Error
System.Windows.Forms.MessageBox.Show("Success!\nTask failed successfully!", "Error", buttons, icon)



[<Measure>] type m
[<Measure>] type s
[<Measure>] type kg
[<Measure>] type N = kg * m / s^2
[<Measure>] type inch
[<Measure>] type foot

let distance = 100.0<m>
let timeTaken = 20.0<s>
let speed = distance / timeTaken
let acceleration = speed / timeTaken
let mass = 5.0<kg>
let force1 = mass * acceleration
let force2 = 2.25<N>
let totalForce = force1 + force2


// dimensionless values are left as is
// or can be annotated with the measure <1>
let x = 4<1>
let y = 4
let compare = y = x

// Dimensionless values cannot be added or 
// subtracted from dimensioned values
// However, dimensioned values can be multiplied
// or divided by dimensionless values

// test addition & multiplication
3.0<foot> + 2.0<foot> // OK
// 3.0<foot> + 2.0    // error
3.0<foot> * 2.0       // OK

// to convert from dimensioned to dimensionless
// divide by 1 unit 
// for the opposite, multiply by 1 unit
let dimensionlessValue = 42
let dimensionlessValueInFeet = dimensionlessValue * 1<foot>
let dimensionlessValue' = dimensionlessValueInFeet / 1<foot>

//conversion
[<Measure>] type degC
[<Measure>] type degF
let convertDegCToF c =
    c * 1.8<degF/degC> + 32.0<degF> // note correct type inference
let convertDegFToC f =
    ( f - + 32.0<degF> ) * 1.8<degC/degF>
// test
let f = convertDegCToF 0.0<degC>
let c = convertDegFToC 32.0<degF>

// to make functions that work with any 
// unit of measure, the generic unit of 
// measure may be used ('u) 
// in this case, the wildcard shows that
// measure is allowed and will display the 
// 'u in its function signature
let squareMeasure (x:float<_>) = x * x
let degCSquared = squareMeasure 12.0<degC>
let feetSquared = squareMeasure 6.0<foot>

// for multiple parameters to a generic unit
// function, 'u may be used to indicate the 
// two measures must be the same
let distanceRatio (distance1:float<'u>) (distance2:float<'u>) =
    distance1 / distance2

// to show different measures are needed
// increment the letter as normal
let calculateSpeed (distance:float<'u>) (time:float<'v>) =
    distance / time

// Measures with Lists
// Measures cannot be defined in a list directly
// [1.0<foot>..10.0<foot>]    error
// however, the fix is to use the conversion fix 
// as seen above
// using List.map
[1.0..20.0] |> List.map (fun i -> i * 1.0<foot>)
// using a generator
[ for i in [1.0..20.0] -> i * 1.0<foot> ]


// when adding constants to dimensioned values
// the generic unit type must be used
// let value = 10<m> + 1    error
let value = 10<m> + 1<_>

// A similar situation occurs when passing in 
// constants to a higher order function such
// as fold
let feet = [ for i in [1.0..10.0] -> i * 1.0<foot> ]
// OK
feet |> List.sum
// Error
// feet |> List.fold (+) 0.0
// Fixed with generic 0
feet |> List.fold (+) 0.0<_>


// Sometimes type inference fails and the exact 
// type cannot be determined or is ignored
let add1 n = n + 1.0<_>
// add1 10.0<foot>
// error FS0001: This expression was expected to have type float
// but here has type float<foot>
// This says that n has no measure and hence was not expecting
// any values with measure. The generic unit 1.0<_> is ignored
// as it is not needed (as the type inferencer thinks)        
// explicitly annotating the measure type raises other errors 
// so the way to fix this is to use the LanguagePrimitives    
// module to fix ( eg FloatWithMeasure, Int32WithMeasure etc.)
// define the function
let add2 n =
    n + (LanguagePrimitives.FloatWithMeasure 1.0)
let add2Int n =
    n + (LanguagePrimitives.Int32WithMeasure 1)
// test
add1 10.0<foot> // Yes!

// Quicksort
let rec sort list = 
    match list with
    | [] -> 
        []
    | x::xs ->
        let less, more = List.partition ((>=) x) xs
        (sort less)@x::(sort more)

sort [3;4;1;7;3;5;2;6]

// look at signatures to see type inference
let inferInt x = x + 1
let inferFloat x = x + 1.0
let inferDecimal x = x + 1m // m suffix means decimal
let inferSByte x = x + 1y // y suffix means signed byte
let inferChar x = x + 'a' // a char
let inferString x = x + "my string"
// if..else implies a bool
let inferBool x = if x then false else true
// for..do implies a sequence
let inferStringList x = for y in x do printfn "%s" y
// :: implies a list
let inferIntList x = 99::x
// .NET library method is strongly typed
let inferStringAndBool x = System.String.IsNullOrEmpty(x)
let inferIntPrint x = printf "x is %i" x
let inferFloatPrint x = printf "x is %f" x
let inferGenericPrint x = printf "x is %A" x
let inferGeneric x = x
let inferIndirectGeneric x = inferGeneric x
let inferIndirectGenericAgain x = (inferIndirectGeneric x).ToString()

let doItTwice f = (f >> f)
let add3 x = x + 3
let add6 = doItTwice add3
// test
add6 5 // result = 11
let square x = x * x
let fourthPower = doItTwice square
// test
fourthPower 3 // result = 81
let chittyBang x = "Chitty " + x + " Bang"
let chittyChittyBangBang = doItTwice chittyBang
// test
chittyChittyBangBang "&" // result = "Chitty Chitty & Bang Bang"

// simultaneous functions that refer to each other
let rec showPositiveNumber x = // LET REC rather than LET
    match x with
    | x when x >= 0 -> printfn "%i is positive" x
    | _ -> showNegativeNumber x
and showNegativeNumber x = // AND rather than LET
    match x with
    | x when x < 0 -> printfn "%i is negative" x
    | _ -> showPositiveNumber x

// Simultaneous types
type A = NoneofA | AUsesB of B
and B = NoneofB | BUsesA of A // use AND instead of TYPE

let fibonacciUnfolder max (f1,f2) =
    if f1 > max then
        None
    else
        // return value and new threaded state
        let fNext = f1 + f2
        let newState = (f2,fNext)
        // f1 will be in the generated sequence
        Some (f1,newState)

let fibonacci max = List.unfold (fibonacciUnfolder max) (1I,1I)
fibonacci 10000000000000000000000I

let divideBy bottom top =
    if bottom = 0
    then None
    else Some(top/bottom)

type MaybeBuilder() =
    member this.Bind(x, f) =
        printfn "x is %A" x
        match x with
        | None -> None
        | Some a -> f a
    member this.Return(x) =
        Some x

let maybe = new MaybeBuilder()

let divideByWorkflow init x y z =
    maybe { 
        let! a = init |> divideBy x
        let! b = a |> divideBy y
        let! c = b |> divideBy z
        return c
        }

let zxc = divideByWorkflow 999 111 3 3
open System.Net
let req1 = HttpWebRequest.Create("https://try.fsharp.org")
let req2 = HttpWebRequest.Create("https://google.com")
let req3 = HttpWebRequest.Create("https://bing.com")
async {
    use! resp1 = req1.AsyncGetResponse()
    printfn "Downloaded %O" resp1.ResponseUri
    use! resp2 = req2.AsyncGetResponse()
    printfn "Downloaded %O" resp2.ResponseUri
    use! resp3 = req3.AsyncGetResponse()
    printfn "Downloaded %O" resp3.ResponseUri
    } |> Async.RunSynchronously

let (>>=) m f =
    printfn "expression is %A" m
    f m

let loggingWorkflow =
    1 >>= (+) 2 >>= (*) 42 >>= (-) 198 >>= id


let strToInt str = 
    let (b, x) = System.Int32.TryParse str
    if b
    then Some x
    else None

type MaybeInt() =
    member this.Bind (x, f)=
        printfn "x = %A" x
        Option.bind f x
    member this.Return(x) = 
        Some x

let work = MaybeInt()
let workFlow x y z = 
    work {
        let! a = strToInt x
        let! b = strToInt y
        let! c = strToInt z
        return a + b + c
    }

let good = workFlow "12" "3" "2"
let bad = workFlow "12" "xyz" "2"



type TraceBuilder() =
    member this.Bind(m, f) =
        match m with
        | None ->
            printfn "Binding with None. Exiting."
        | Some a ->
            printfn "Binding with Some(%A). Continuing" a
        Option.bind f m
    member this.Return(x) =
        printfn "Returning a unwrapped %A as an option" x
        Some x
    member this.ReturnFrom(m) =
        printfn "Returning an option (%A) directly" m
        m
    member this.Zero() =
        printfn "zero"
        None
    member this.Yield(x) =
        printfn "Yield an unwrapped %A as an option" x
        Some x
    member this.YieldFrom(m) =
        printfn "Yield an option (%A) directly" m
        m
    member this.Combine (a,b) =
        match a,b with
        | Some a', Some b' ->
            printfn "combining %A and %A" a' b'
            Some (a' + b')
        | Some a', None ->
            printfn "combining %A with None" a'
            Some a'
        | None, Some b' ->
            printfn "combining None with %A" b'
            Some b'
        | None, None ->
            printfn "combining None with None"
            None
    member this.Delay(f) =
        printfn "Delay"
        f()

// make an instance of the workflow
let trace = new TraceBuilder()

trace {
    return 1
    } |> printfn "Result 1: %A"
trace {
    return! Some 2
    } |> printfn "Result 2: %A"
trace {
    let! x = Some 1
    let! y = Some 2
    return x + y
    } |> printfn "Result 3: %A"
trace {
    let! x = None
    let! y = Some 1
    return x + y
    } |> printfn "Result 4: %A"
trace {
    do! Some (printfn "...expression that returns unit")
    do! Some (printfn "...another expression that returns unit")
    let! x = Some (1)
    return x
    } |> printfn "Result from do: %A"
trace {
    printfn "hello world"
    } |> printfn "Result for simple expression: %A"
trace {
    if false then return 1
    } |> printfn "Result for if without else: %A"
trace {
    yield "body"
    } |> printfn "Result for yield: %A"
trace {
    yield! Some 1
    } |> printfn "Result for yield!: %A"
trace {
    yield 1
    yield 2
    } |> printfn "Result for yield then yield: %A"

    
type ListBuilder() =
    member this.Bind(m, f) =
        m |> List.collect f
    member this.Zero() =
        printfn "Zero"
        []
    member this.Return(x) =
        printfn "Return an unwrapped %A as a list" x
        [x]
    member this.Yield(x) =
        printfn "Yield an unwrapped %A as a list" x
        [x]
    member this.YieldFrom(m) =
        printfn "Yield a list (%A) directly" m
        m
    member this.For(m,f) =
        printfn "For %A" m
        this.Bind(m,f)
    member this.Combine (a,b) =
        printfn "combining %A and %A" a b
        List.concat [a;b]
    member this.Delay(f) =
        printfn "Delay"
        f()
// make an instance of the workflow
let listbuilder = new ListBuilder()
listbuilder {
    let! x = [1..3]
    let! y = [10;20;30]
    return x + y
    } |> printfn "Result: %A"
listbuilder {
    for x in [1..3] do
    for y in [10;20;30] do
    return x + y
    } |> printfn "Result: %A"
listbuilder {
    yield 1
    yield 2
    } |> printfn "Result for yield then yield: %A"
listbuilder {
    yield 1
    yield! [2;3]
    } |> printfn "Result for yield then yield! : %A"
listbuilder {
    for i in ["red";"blue"] do
        yield i
        for j in ["hat";"tie"] do
            yield! [i + " " + j;"-"]
    } |> printfn "Result for for..in..do : %A"


let ``Nicasio is now the name of my variable`` = "Hi nicasio"
printfn "%s" ``Nicasio is now the name of my variable``


module ShoppingCartWithBug =
    let mutable itemQty = 1 // don't do this at home!
    let printQty() = 
        printfn "%i" itemQty
    let incrementClicked() =
        itemQty <- itemQty + 1
        printQty()
    let decrementClicked() =
        itemQty <- itemQty - 1
        printQty()

ShoppingCartWithBug.decrementClicked()
ShoppingCartWithBug.decrementClicked()
ShoppingCartWithBug.decrementClicked()
ShoppingCartWithBug.incrementClicked()


type Result<'TSuccess, 'TFailure> =
    | Success of 'TSuccess
    | Failure of 'TFailure

let henlo = Success "We did it"
let failureofachild = Failure "hi"
let henlos = Success "We did it"
let en = henlos = henlo
let test = Success [1;2;3;4;5;6;7;8]



// wack numbers
let x' = 1.0/0.0
let y' = x' * 2.0
let z' = 2.0 / x'


module WordCountTest =
    open System
    type Text = Text of string
    let addText (Text s1) (Text s2) =
        Text (s1 + s2)
    let wordCount (Text s) =
        System.Text.RegularExpressions.Regex.Matches(s,@"\S+").Count

    let page() =
        List.replicate 1000 "hello "
        |> List.reduce (+)
        |> Text
    let document() =
        page() |> List.replicate 10000

    let time f msg =
        let stopwatch = Diagnostics.Stopwatch()
        stopwatch.Start()
        f()
        stopwatch.Stop()
        printfn "Time taken for %s was %ims" msg stopwatch.ElapsedMilliseconds

    let wordCountViaAddText() =
        document()
        |> List.reduce addText
        |> wordCount
        |> printfn "The word count is %i"
    time wordCountViaAddText "reduce then count"

    let wordCountViaParallelAddCounts() =
        document()
        |> List.toArray
        |> Array.Parallel.map wordCount
        |> Array.reduce (+)
        |> printfn "The word count is %i"
    time wordCountViaParallelAddCounts "parallel map then reduce"

module FrequentWordTest =
    open System
    open System.Text.RegularExpressions
    type Text = Text of string
    let addText (Text s1) (Text s2) =
        Text (s1 + s2)
    let mostFrequentWord (Text s) =
        Regex.Matches(s,@"\S+")
        |> Seq.cast<Match>
        |> Seq.map (fun m -> m.ToString())
        |> Seq.groupBy id
        |> Seq.map (fun (k,v) -> k,Seq.length v)
        |> Seq.sortBy (fun (_,v) -> -v)
        |> Seq.head
        |> fst
    let page1() =
        List.replicate 1000 "hello world "
        |> List.reduce (+)
        |> Text
    let page2() =
        List.replicate 1000 "goodbye world "
        |> List.reduce (+)
        |> Text
    let page3() =
        List.replicate 1000 "foobar "
        |> List.reduce (+)
        |> Text
    let document() =
        [page1(); page2(); page3()]
    document()
    |> List.reduce addText
    |> mostFrequentWord
    |> printfn "Using add first, the most frequent word is %s"
    
    document()
    |> List.map mostFrequentWord
    |> List.reduce (+)
    |> printfn "Using map reduce, the most frequent word is %s"

let wack = (max System.Int32.MinValue System.Int32.MinValue) = System.Int32.MinValue