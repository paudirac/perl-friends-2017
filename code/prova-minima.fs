(* Types *)
type Parsed =
    | Success of string * string
    | Failure of string

type Parser = string -> Parsed

(* Utils *)
let stringify x = x.ToString();
let printParsed = printfn "%A"
let chopLeft (s:string) = s.[1..]
let chopRight (s:string) =
    let reverse s' = s' |> Array.ofSeq |> Array.rev |> Array.map (fun c -> c.ToString()) |> String.concat ""
    s |> reverse |> chopLeft |> reverse
let chop s = s |> chopLeft |> chopRight
let wrapWith w s = s |> chop |> (fun s' -> sprintf "<%s>%s</%s>" w s' w)

(* Parser combinators *)
let pChar c =
    function (s:string) -> let f = s.[0]
                           if f = c then Success (stringify c, s.[1..])
                           else Failure s

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

let (<|>) p1 p2 = pOr p1 p2

let pAnd p1 p2 =
    function (s:string) -> match p1 s with
                  | Failure f -> Failure f
                  | Success (f,r) -> match p2 r with
                                     | Success (f2, r2) -> Success (f + f2, r2)
                                     | _ -> Failure f

let (<&>) p1 p2 = pAnd p1 p2

let pStar = function (s:string) -> Success (stringify s.[0], s.[1..])

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

let rec fac n =
    if n = 1 then 1
    else n * fac (n - 1)
let fac' n =
    let ns = [1..n]
    let folder acc curr = acc * curr
    in Seq.fold folder 1 ns

let reverse seq = seq |> Array.ofSeq |> Array.rev

let pUnit = 
    function s -> match explode s with
                    | [] -> Failure s
                    | c::cs -> Success (implode [c], implode cs)

let pSeq' ps = 
    let sp = reverse ps
    let folder acc curr = pAnd curr acc
    in Seq.fold folder pUnit sp

let pABC = [pChar 'a'; pChar 'b'; pChar 'c'] |> pSeq'
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

let pMany p =
    function s ->
        let rec parse r acc rest i =
            match r with
            | Failure f -> Success(acc, rest)
            | Success(f1, r1) -> match String.length r1 with
                                 | 0 -> Success(acc + f1, r1)
                                 | _ -> parse (p r1) (acc + f1) r1 (i + 1)
        parse (p s) "" s 0

let pApply f p =
    function s ->
        let r = p s
        match r with
        | Success(f', r') -> Success(f f', r')
        | _ -> r

(* Markdown parser *)

let pAlpha = "abcdefghijklmnopqrstuvwxyz" |> Seq.map pChar |> pAny
let pWord = pMany pAlpha
let pSpace = pChar ' ' <|> pChar '\t'
let pWhite = pChar ' '// <|> pMany (pChar ' ') //pSpace
let pText = pMany (pWord <|> pWhite)
let pBold = pChar '*' <&> pText <&> pChar '*'
let pnl = pChar '\010'
let rec pText' = pText <|> (fun s -> i' s) <|> (fun s -> b' s)
and i' = (pChar '_' <&> (fun s -> pText' s) <&> pChar '_') |> pApply (wrapWith "i")
and b' = (pChar '*' <&> (fun s -> pText' s) <&> pChar '*') |> pApply (wrapWith "b")
and ph1 = pChar '#' <&> (fun s -> pText' s) <&> pnl |> pApply (wrapWith "h1")
let p = pText' <&> pnl |> pApply (wrapWith "p")

let pMarkdown' =
(*     i' <|>
    b' <|>
    pText' <|>
    pWhite <|>
    pHeader1 *)
    ph1 <|>
    p
    |> pMany

let nomesnl = @"
"
pnl nomesnl |> printParsed
printfn "%s" nomesnl

(* pseudo tests *)
(* pChar 'c' "hola" |> printParsed
pChar 'c' "cola" |> printParsed
pAnd (pChar 'a') (pChar 'b') "abc" |> printParsed
pAnd (pChar 'a') (pChar 'b') "bac" |> printParsed
let pseq1 = pSeq [pChar 'a'; pChar 'e'; pChar 'i'; pChar 'o'; pChar 'u' ];
pseq1 "aeiou" |> printParsed
pseq1 "aaiou" |> printParsed
pseq1 "aeiouaeiou" |> printParsed
printfn "pAlpha:"
pAlpha "123" |> printParsed
pAlpha "abra" |> printParsed
printfn "pWord:"
pWord "word" |> printParsed
pWord "2324" |> printParsed
pWhite "    " |> printParsed
printfn "pText:"
pText "this is a phrase" |> printParsed
pBold "this is not bold" |> printParsed
pBold "*this is bold* and this not" |> printParsed
b' "*bold text* nonbold text" |> printParsed
i' "_this is italics_ and this is not" |> printParsed
pMarkdown' "this *_will_* succeed" |> printParsed
pMarkdown' "and _this_ _*will also*_ *succed*" |> printParsed
pMarkdown' "this *_will_* succeed" |> printParsed
pMarkdown' "and _this_ _*will also*_ *succed*" |> printParsed
pMarkdown' """# this is a title 
""" |> printParsed*)

let md = "@# This is a title

And hopefully those will be paragraphs.

And more paragraphs. But this is *another* phrase, not
a new paragraph. And this also.
"

(* md |> pMarkdown' |> printParsed
printfn md
 *)
printfn "Done."
