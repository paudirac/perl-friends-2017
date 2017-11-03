(* Types *)
type Parsed =
    | Success of string * string
    | Failure of string

type Parser = string -> Parsed

(* Utils *)

let explode (s:string) = [for c in s -> c]
let implode (xs:char list) =
    let sb = System.Text.StringBuilder(xs.Length)
    xs |> List.iter (sb.Append >> ignore)
    sb.ToString()

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
                | Success(f,_) -> String.length f
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
                                     | _ -> Failure s

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

let pAdvance = 
    function s -> match explode s with
                    | [] -> Failure s
                    | c::cs -> Success (implode [c], implode cs)

let succeed v = function s -> Success(v, s)

let pZero = function s -> Failure s
let pUnit = succeed ""

let pBind p f =
    function s -> match p s with
                    | Success (s', r) -> let p' = f s' in p' r
                    | _ -> Failure s

let (>>=) p f = pBind p f

let pAnd' pA pB = pA >>= fun a ->
                             pB >>= fun b ->
                                        succeed (a + b)

let pOr' pA pB = function s -> greedy (pA s) (pB s)

(* let pABC = pA >>= fun a -> 
                      pB >>= fun b -> 
                      pC >>= fun c -> 
                      succeed (a + b + c)
 *)


let pSeq' (ps: Parser seq) =
    let folder (acc: Parser) (p: Parser) : Parser = acc <&> p
    Seq.fold folder pUnit ps    


(* let pSeq' ps =   
    let sp = reverse ps
    let folder acc curr = pAnd curr acc
    in Seq.fold folder pZero sp
 *)
(* Seq.fold : ('State -> 'T -> 'State) -> 'State -> seq<'T> -> 'State *)

(* this doesn't work*)

let pA = pChar 'a'
let pB = pChar 'b'
let pC = pChar 'c'

let pABC = pA >>= fun a -> 
                      pB >>= fun b -> 
                      pC >>= fun c -> 
                      succeed (a + b + c)
(* let pSeq'' ps =
    let sp = reverse ps
    let folder acc p = p >>= (fun r -> succeed (acc + r))
    in Seq.fold folder "" sp *)
(* let pABC = [pChar 'a'; pChar 'b'; pChar 'c'] |> pSeq' *)
let pAny ps : Parser =
    let plist = ps |> Seq.toList
    function s ->
        if s.Length = 0 then Failure ""
        else 
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

let pMany1 p = p <&> pMany p

let pApply f p =
    function s ->
        let r = p s
        match r with
        | Success(f', r') -> Success(f f', r')
        | _ -> r

(* Markdown parser *)

let pAlpha = "abcdefghijklmnopqrstuvwxyz" |> Seq.map pChar |> pAny

let pSpace = pChar ' ' <|> pChar '\t'
let pWhite = pMany1 pSpace// <|> pMany (pChar ' ') //pSpace
let pWord = pMany1 pAlpha
let pText = pMany (pWord <|> pWhite)
let pBold = pChar '*' <&> pText <&> pChar '*'
let pnl = pChar '\010'
let pScapes = ['#';] |> Seq.map pChar |> pAny
let rec pText' = pText <|> (fun s -> i' s) <|> (fun s -> b' s) <|> pScapes
and i' = (pChar '_' <&> (fun s -> pText' s) <&> pChar '_') |> pApply (wrapWith "i")
and b' = (pChar '*' <&> (fun s -> pText' s) <&> pChar '*') |> pApply (wrapWith "b")
let ph1 = (pChar '#' <&> pSpace <&> pText') >>= (fun t -> succeed ("<h1>" + chopLeft t + "</h1>"))
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
