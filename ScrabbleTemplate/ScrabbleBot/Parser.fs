// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

open StateMonad
open ScrabbleUtil // NEW. KEEP THIS LINE.
open Eval
open FParsecLight.TextParser // Industrial parser-combinator library. Use for Scrabble Project.


let pIntToChar = pstring "intToChar"
let pPointValue = pstring "pointValue"

let pCharToInt = pstring "charToInt"
let pToUpper = pstring "toUpper"
let pToLower = pstring "toLower"
let pCharValue = pstring "charValue"

let pTrue = pstring "true"
let pFalse = pstring "false"
let pIsDigit = pstring "isDigit"
let pIsLetter = pstring "isLetter"
let pIsVowel = pstring "isVowel"

let pif = pstring "if"
let pthen = pstring "then"
let pelse = pstring "else"
let pwhile = pstring "while"
let pdo = pstring "do"
let pdeclare = pstring "declare"

let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
let pletter = satisfy System.Char.IsLetter <?> "letter"
let palphanumeric = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"
//______
let spaces = many whitespaceChar <?> "space"
let spaces1 = many1 whitespaceChar <?> "space1"

let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2
let (.>*>) p1 p2 = p1 .>> spaces .>> p2
let (>*>.) p1 p2 = p1 .>> spaces >>. p2

let parenthesise p = pchar '(' >*>. p .>*> pchar ')'

let pid =
    (pchar '_' <|> pletter .>>. many (palphanumeric <|> pchar '_')) //first check _ or letter, then _ or alphanumerical again and again... (many)
    |>> (fun (a, b) -> System.String.Concat(a :: b)) //in the end - apply transformation to result: concat a, b (first character and last characters) to a single string.



let unop op a = op >*>. a
let binop op p1 p2 = p1 .>*> op .>*>. p2 //get both operands without operator

let TermParse, tref = createParserForwardedToRef<aExp> ()
let ProdParse, pref = createParserForwardedToRef<aExp> ()
let AtomParse, aref = createParserForwardedToRef<aExp> ()

let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"

do tref := choice [ AddParse; SubParse; ProdParse ]

let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"

do pref := choice [ MulParse; DivParse; ModParse; AtomParse ]

let NParse = pint32 |>> N <?> "Int"
let ParParse = parenthesise TermParse
let PVParse = unop pPointValue AtomParse |>> PV <?> "PV"
let NegParse = unop (pchar '-') AtomParse |>> (fun a -> Mul((N -1), a)) <?> "Neg"
let VParse = pid |>> V <?> "V"
let AexpParse = TermParse

let CParse, cref = createParserForwardedToRef<cExp> ()

let chParse =
    between (pchar ''') (pchar ''') (palphanumeric <|> whitespaceChar) |>> C <?> "C"

let toUpParse = unop pToUpper (parenthesise CParse) |>> ToUpper <?> "ToUpper"
let toLowParse = unop pToLower (parenthesise CParse) |>> ToLower <?> "ToLower"

let intToChParse =
    unop pIntToChar (parenthesise AexpParse) |>> IntToChar <?> "IntToChar"

let chValParse = unop pCharValue (parenthesise AexpParse) |>> CV <?> "CV"

let CharToIntParser = unop pCharToInt (parenthesise CParse) |>> CharToInt <?> "V"


do aref := choice [ CharToIntParser; NegParse; PVParse; VParse; NParse; ParParse ]

do cref := choice [ chValParse; intToChParse; toUpParse; toLowParse; chParse ]

let CexpParse = CParse


//_________

let BexpParse = pstring "not implemented"

let stmParse = pstring "not implemented"

(* The rest of your parser goes here *)
type word = (char * int) list
type squareFun = word -> int -> int -> Result<int, Error>
type square = Map<int, squareFun>

type boardFun2 = coord -> Result<square option, Error>

type board =
    { center: coord
      defaultSquare: square
      squares: boardFun2 }

// Default (unusable) board in case you are not implementing a parser for the DSL.
let mkBoard: boardProg -> board =
    fun _ ->
        { center = (0, 0)
          defaultSquare = Map.empty
          squares = fun _ -> Success(Some Map.empty) }
