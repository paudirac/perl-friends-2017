(* Types *)
type Parsed<'a> =
    | Success of 'a * string
    | Failure of string

type Parser<'a> = string -> Parsed<'a>
let pResult v =
    function s -> Success (v, s)

let explode (s:string) = [for c in s -> c]
let implode (xs:char list) =
    let sb = System.Text.StringBuilder(xs.Length)
    xs |> List.iter (sb.Append >> ignore)
    sb.ToString()

let pDot = 
    function s -> match explode s with
                    | [] -> Failure s
                    | c::cs -> Success (implode [c], implode cs)

let bind (p: Parser<'a>) (f: 'a -> Parser<'b>) : Parser<'b> =
    function s -> match p s with
                    | Success (a, s') -> let pb = f a in pb s'
                    | Failure s' -> Failure s'

let (</) a b = a |> b
let (/>) a b = a <| b

let modulo a b = a % b

let kk = 10 </modulo/> 3


pResult "hola" "hola"
pResult 42 "hola"
