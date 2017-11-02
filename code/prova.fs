type Parsed =
    | Success of string * string
    | Failure of string

type Parser = string -> Parsed

let stringify x = x.ToString();

let pChar c =
    function (s:string) -> let f = s.[0]
                           if f = c then Success (stringify c, s.[1..])
                           else Failure s

let printParsed = printfn "%A"

pChar 'c' "hola" |> printParsed
pChar 'c' "cola" |> printParsed

let pc = pChar 'c'
let ph = pChar 'h'

let greedy r1 r2 =
    let len r = match r with
                | Success(f,rest) -> String.length f
                | _ -> 0
    let c1 = len r1
    let c2 = len r2
    if c1 > c2 then r1
    else r2

let pOr p1 p2 =
    function s -> match p1 s with
                  | Failure f -> p2 f
                  | _ -> greedy (p1 s) (p2 s)

let pcoh = pOr pc ph

pcoh "hola" |> printParsed
pcoh "cola" |> printParsed

let (<|>) p1 p2 = pOr p1 p2

let pab = pChar 'a' <|> pChar 'b'

pab "c" |> printParsed
pab "a" |> printParsed
pab "b" |> printParsed

let pAnd p1 p2 =
    function s -> match p1 s with
                  | Failure f -> Failure f
                  | Success (f,r) -> match p2 r with
                                     | Success (f2, r2) -> Success (f + f2, r2)
                                     | _ -> Failure f

pAnd (pChar 'a') (pChar 'b') "abc" |> printParsed
pAnd (pChar 'a') (pChar 'b') "bac" |> printParsed

let (<&>) p1 p2 = pAnd p1 p2

let psab = pChar 'a' <&> pChar 'b'
psab "abc" |> printParsed
psab "bac" |> printParsed

let pStar = function (s:string) -> Success (stringify s.[0], s.[1..])

pStar "anything" |> printParsed

let pSeq ps =
    let plist = ps |> Seq.toList
    function s ->
        let rec parse parsers acc rest =
            match parsers with
            | [] -> Success (acc, rest)
            | p::ps' -> let r = p rest
                        match r with
                        | Success (a, b) -> parse ps' (acc + a) b
                        | Failure f -> r
        parse plist "" s


let pseq1 = pSeq [pChar 'a'; pChar 'e'; pChar 'i'; pChar 'o'; pChar 'u' ];
pseq1 "aeiou" |> printParsed
pseq1 "aaiou" |> printParsed
pseq1 "aeiouaeiou" |> printParsed

let pseq2 = "aeiou" |> Seq.map pChar |> pSeq

pseq2 "aeiou" |> printParsed
pseq2 "aaiou" |> printParsed
pseq2 "aeiouaeiou" |> printParsed

let pAny ps =
    let plist = ps |> Seq.toList
    function s ->
        let rec parse parsers =
            match parsers with
            | [] -> Failure s
            | p::ps' -> match p s with
                        | Success(f,r) -> Success(f,r)
                        | _ -> parse ps'
        parse plist

let pAlpha = "abcdefghijklmnopqrstuvwxyz" |> Seq.map pChar |> pAny

printfn "pAlpha:"
pAlpha "123" |> printParsed
pAlpha "abra" |> printParsed


let pMany p =
    function s ->
        let rec parse r acc rest i =
            match r with
            | Failure f -> Success(acc, rest)
            | Success(f1, r1) -> match String.length r1 with
                                 | 0 -> Success(acc + f1, r1)
                                 | _ -> parse (p r1) (acc + f1) r1 (i + 1)
        parse (p s) "" s 0
let pWord = pMany pAlpha

printfn "pWord:"
pWord "word" |> printParsed
pWord "2324" |> printParsed

let pSpace = pChar ' ' <|> pChar '\t'
let pWhite = pChar ' '// <|> pMany (pChar ' ') //pSpace

pWhite "    " |> printParsed
let pText = pMany (pWord <|> pWhite)

printfn "pText:"
pText "this is a phrase" |> printParsed

let pBold = pChar '*' <&> pText <&> pChar '*'

pBold "this is not bold" |> printParsed
pBold "*this is bold* and this not" |> printParsed

let pApply f p =
    function s ->
        let r = p s
        match r with
        | Success(f', r') -> Success(f f', r')
        | _ -> r

let chopLeft (s:string) = s.[1..]
let chopRight (s:string) =
    let reverse s' = s' |> Array.ofSeq |> Array.rev |> Array.map (fun c -> c.ToString()) |> String.concat ""
    s |> reverse |> chopLeft |> reverse

let chop s = s |> chopLeft |> chopRight
let wrapWith w s = s |> chop |> (fun s' -> sprintf "<%s>%s</%s>" w s' w)
let b = pBold |> pApply (wrapWith "b")

b "*bold text* nonbold text" |> printParsed

let pItalic = pChar '_' <&> pText <&> pChar '_'
let i = pItalic |> pApply (wrapWith "i")

i "_this is italics_ and this is not" |> printParsed
let pMarkdown =
    i <|>
    b <|>
    pText <|>
    pWhite
    |> pMany

pMarkdown "this is *some* kind of _markdown_ parser" |> printParsed

pMarkdown "this *_wont_* succeed" |> printParsed

let rec pText' = pText <|> (fun s -> i' s) <|> (fun s -> b' s)
and i' = (pChar '_' <&> (fun s -> pText' s) <&> pChar '_') |> pApply (wrapWith "i")
and b' = (pChar '*' <&> (fun s -> pText' s) <&> pChar '*') |> pApply (wrapWith "b")

let pMarkdown' =
    i' <|>
    b' <|>
    pText' <|>
    pWhite
    |> pMany

pMarkdown' "this *_will_* succeed" |> printParsed
pMarkdown' "and _this_ _*will also*_ *succed*" |> printParsed

printfn "Done."



