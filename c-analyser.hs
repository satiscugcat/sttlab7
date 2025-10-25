{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use asum" #-}
{-# HLINT ignore "Fuse foldr/map" #-}

import Data.Char

import Data.List
import Control.Applicative ( Alternative(..) )

import System.IO
import System.Environment
import Control.Monad
import System.FilePath.Posix

data Assn =
    JustAssn |
    PlusAssn |
    MinusAssn |
    MultAssn |
    DivAssn |
    ModAssn |
    AndAssn |
    XorAssn |
    OrAssn |
    LShiftAssn |
    RShiftAssn
    deriving Eq
instance Show Assn where
    show a =
        case a of
            JustAssn -> "="
            PlusAssn -> "+="
            MinusAssn -> "-="
            DivAssn -> "/="
            ModAssn -> "%="
            AndAssn -> "&="
            XorAssn -> "^="
            OrAssn -> "|="
            LShiftAssn -> "<<="
            RShiftAssn -> ">>="
data BinOp =
    Plus |
    Minus |
    Mult |
    Div |
    BitAnd |
    BitOr |
    BoolAnd |
    BoolOr |
    Eq |
    NEq |
    Gr |
    Ls |
    GrEq |
    LsEq |
    BitXor |
    Mod |
    LShift |
    RShift |
    AssnOp Assn
    deriving Eq

instance Show BinOp where
    show :: BinOp -> String
    show b =
        case b of
            Mod -> "%"
            LShift -> "<<"
            RShift -> ">>"
            AssnOp a -> show a
            Plus -> "+"
            Minus -> "-"
            Mult -> "*"
            Div -> "/"
            BitAnd -> "&"
            BitOr -> "|"
            BoolAnd -> "&&"
            BoolOr -> "||"
            BitXor -> "^"
            Eq -> "=="
            NEq -> "!="
            Gr -> ">"
            Ls -> "<"
            GrEq -> ">="
            LsEq -> "<="

data CType =
    I8 |
    I16 |
    I32 |
    I64 |
    U8 |
    U16 |
    U32 |
    U64 |
    F32 |
    F64 |
    F128 |
    Pointer CType |
    Array CType Int |
    Ident String
    deriving Eq
instance Show CType where
    show c =
        case c of
            I8 -> "char"
            I16 -> "int"
            I32 -> "long int"
            I64 -> "long long int"
            U8 -> "unsigned char"
            U16 -> "unsigned int"
            U32 -> "long int"
            U64 -> "long long int"
            F32 -> "float"
            F64 -> "double"
            F128 -> "long double"
            Pointer c' -> "(" ++ show c' ++ ")" ++ "*"
            Array c' n -> "(" ++ show c' ++ ")" ++ "[" ++ show n ++ "]"
            Ident s -> s
data UnOp =
    Ref |
    Deref |
    Neg |
    BoolNot |
    BitNot |
    PostInc |
    PostDec |
    PreInc |
    PreDec
    deriving Eq
instance Show UnOp where
    show :: UnOp -> String
    show u =
        case u of
            Neg -> "-"
            BoolNot -> "!"
            Ref -> "&"
            Deref -> "*"
            BitNot -> "~"
            PostInc -> "++"
            PreInc -> "++"
            PostDec -> "--"
            PreDec -> "--"

data Expr =
    ConstExpr String |
    VarExpr String |
    FCallExpr Expr [Expr] |
    SubscriptExpr Expr Expr |
    UnOpExpr UnOp Expr |
    BinOpExpr Expr BinOp Expr |
    ArrayExpr [Expr]
    deriving Eq
instance Show Expr where
    show e =
        case e of
            ConstExpr s -> s
            VarExpr s -> s
            FCallExpr e' es -> show e' ++ "(" ++ foldr (\i f -> show i ++ "," ++ f) ")" es
            SubscriptExpr e1 e2 -> show e1 ++ "[" ++ show e2 ++ "]"
            UnOpExpr u e' ->
                case u of
                    PostInc -> show e' ++ "++"
                    PostDec -> show e' ++ "--"
                    _ -> show u ++ show e'
            BinOpExpr e1 b e2 -> show e1 ++ show b ++ show e2
            ArrayExpr es -> "{" ++ foldr (\i f -> show i ++ "," ++ f) "}" es
charToHex :: Char -> Int
charToHex char =
    if isDigit char
        then read [char]
        else let num = ord char in
            if num <= ord 'F' then num - ord 'A' else num - ord 'a'

hexStrToDec :: String -> Int
hexStrToDec string = helper (reverse string)
helper "" = 0
helper (x:xs) = charToHex x + 16 * hexStrToDec (reverse xs)
data Result error value =
    Ok value |
    Err error
    deriving Show


newtype Parser a = P ((String, Int, Int) -> Result (String, Int, Int) (a, (String, Int, Int)))

parse :: Parser a -> (String, Int, Int) -> Result (String, Int, Int) (a, (String, Int, Int))
parse (P parser) = parser

parseSimp :: Parser a -> String -> Result (String, Int, Int) (a, (String, Int, Int))
parseSimp (P parser) s = parser (s,0,0)

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f pa = P (\inp -> case parse pa inp of
            Ok (a, str) -> Ok (f a, str)
            Err error -> Err error
        )

instance Applicative Parser where
    pure :: a -> Parser a
    pure somevalue_of_type_a = P (\inp -> Ok (somevalue_of_type_a, inp))
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> pa = P (\inp -> case parse pf inp of
            Ok (ab, out) -> parse (fmap ab pa) out
            Err error -> Err error
        )

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  pa >>= apb = P (\inp -> case parse pa inp of
        Ok (aval, str) -> parse (apb aval) str
        Err error -> Err error
    )

instance Alternative Parser where
  empty = P (\(_, a, b) -> Err ("Empty error", a, b))
  (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp -> case parse p inp of
        Ok (val, str) -> Ok (val, str)
        Err _ -> parse q inp
    )

(?!) :: Parser a -> String -> Parser a
p ?! errorMessage =
    P (\(inp, ln, char) ->
        let val = parse p (inp, ln, char) in
        case val of
        Ok _ -> val
        Err (_, ln', char') -> Err (errorMessage, ln', char')
        )

item :: Parser Char
item = P (\inp -> case inp of
        ([], a , b) -> Err ("End of string", a, b)
        (x:xs, lineNum, charNum) ->
            case x of
                '\n' -> Ok (x, (xs, lineNum + 1, 0))
                _ -> Ok (x, (xs, lineNum, charNum + 1))
    )
sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x then return x else empty ?! "Unsatisfied sat predicate"
token :: Parser a -> Parser a
token p =
    do
        space
        x <- p
        space
        return x
digit :: Parser Char
digit = sat isDigit ?! "Expected digit"

char :: Char -> Parser Char
char x = sat (==x) ?! ("Expected character " ++ [x])

string :: String -> Parser String
string [] = return []
string (x:xs) = do
                char x
                string xs
                return (x:xs)
                ?! ("Expected string " ++ x:xs)

symbol :: String -> Parser String
symbol s = token (string s)

symbols :: [String] -> Parser [String]
symbols ls =
    case ls of
        hd:tl ->
            do
                x <- symbol hd
                xs <- symbols tl
                return (x:xs)
        [] -> return []

parseNumber :: Parser String
parseNumber =
    some digit

parseInteger :: Parser String
parseInteger =
    token (
    (do
    neg <- char '-'
    num <- parseNumber
    return (neg:num))
    <|>
    parseNumber)


parseFloat :: Parser String
parseFloat =
    token (startWithDot <|> startWithNum)
    where
        startWithNum =
            do
                left <- parseInteger
                (do
                    char '.'
                    right <- many digit
                    return (left ++ "." ++ right))
                    <|>
                    return left

        startWithDot =
            do
                char '.'
                right <- parseNumber
                return ('0':"." ++ right)


space :: Parser ()
space = do
    many (sat isSpace)
    return ()

hex :: Parser Char
hex = digit
    <|>
    sat (\char -> ord char <= ord 'F' && ord char >= ord 'A' ||
                  ord char <= ord 'a' && ord char >= ord 'a'
    )

validStringChar :: Parser Char
validStringChar = P (\inp -> case inp of
                        ([], a, b) -> Err ("End of Input", a, b)
                        (x:xs, a, b) -> case x of
                                '"' -> Err ("Invalid String Character", a, b)
                                '\n' -> Err ("NewLine in String", a, b)
                                _ -> Ok (x, (xs, a, b+1))
    )

parseString :: Parser String
parseString =
    token (
        do
            char '"'
            s <- many validStringChar
            char '"'
            return s
        )
        ?! "Invalid String"

parseIdent :: Parser String
parseIdent =
    token (
        do
            c <- sat isAlpha
            cs <- many (sat (\c -> isAlphaNum c || c == '_'))
            return (c:cs)
    )


parsePrimType :: Parser CType
parsePrimType =
        do
        symbol "char"
        return I8
    <|>
        do
        symbols ["signed", "char"]
        return I8
    <|>
        do
        symbols ["unsigned", "char"]
        return U8
    <|>
        do
        symbols ["short", "int"] <|> symbols ["short"]  <|> symbols ["signed", "short", "int"] <|> symbols ["signed", "short"]
        return I16
    <|>
        do
        symbols ["unsigned", "short", "int"] <|> symbols ["unsigned", "short"]
        return U16
    <|>
        do
        symbols ["long", "long", "int"] <|> symbols ["long", "long"]  <|> symbols ["signed", "long", "long", "int"] <|> symbols ["signed", "long", "long"]
        return I64
    <|>
        do
        symbols ["unsigned", "long", "long", "int"] <|> symbols ["unsigned", "long", "long"]
        return U64
    <|>
        do
        symbols ["long", "int"] <|> symbols ["long"]  <|> symbols ["signed", "long", "int"] <|> symbols ["signed", "long"]
        return I32
    <|>
        do
        symbols ["unsigned", "long", "int"] <|> symbols ["unsigned", "long"]
        return U32
    <|>
        do
        symbols ["signed", "int"] <|> symbols ["signed"] <|> symbols ["int"]
        return I16
    <|>
        do
        symbols ["unsigned", "int"] <|> symbols ["unsigned"]
        return I16
    <|>
        do
        symbols ["float"]
        return F32
    <|>
        do
        symbols ["double"]
        return F64
    <|>
        do
        symbols ["long", "double"]
        return F128

data IdentExpr =
    JustIdent String |
    PointerIdent IdentExpr |
    SubscriptIdent IdentExpr Int
parseDecl :: Parser (String, CType)
parseDecl =
        do
        prim <- parsePrimType <|> parseIdentType
        ident <- parseIdentExpr
        return (helper (prim, ident))
    where
        parseIdentType = Ident <$> parseIdent
        parseIdentExpr =
                do
                symbol "*"
                PointerIdent <$> parseIdentExpr
            <|>
                parseIdentExprSubscript
        parseIdentExprSubscript =
                do
                x <- parseIdentExprPrimary
                ns <- many (
                    do
                    symbol "["
                    n <-  read <$> parseNumber
                    symbol "]"
                    return n
                    )
                return  (foldl  SubscriptIdent x ns)
            <|>
                parseIdentExprPrimary
        parseIdentExprPrimary =
                do
                symbol "("
                x <- parseIdentExpr
                symbol ")"
                return x
            <|>
                JustIdent <$> parseIdent
        helper :: (CType, IdentExpr) -> (String, CType)
        helper (ct, ie)=
            case ie of
                JustIdent s -> (s, ct)
                PointerIdent i-> helper ( Pointer ct, i)
                SubscriptIdent i n -> helper (Array ct n, i)

parsePrimary :: Parser Expr
parsePrimary =
        do
        ConstExpr <$> (parseInteger <|> parseString <|> parseFloat)
    <|>
        do
        VarExpr <$> parseIdent
    <|>
        do
        symbol "("
        x <- parseExpr
        symbol ")"
        return x
    <|>
        ArrayExpr <$> parseArray

parseTuple :: Parser [Expr]
parseTuple =
        do
        symbol "("
        (do symbol ")";  return []) <|> ( do x <- parseExpr; xs <- parseTupleHelper ; symbol ")" ; return (x:xs))

    where
        parseTupleHelper = many (
                do
                symbol ","
                parseExpr
                )

parseArray :: Parser [Expr]
parseArray =
        do
        symbol "{"
        (do symbol "}"; return []) <|> ( do x <- parseExpr; xs <- parseTupleHelper; symbol "}"; return (x:xs))
    where
        parseTupleHelper = many (
                do
                symbol ","
                parseExpr
                )
parseCall :: Parser Expr
parseCall =
        do
        x <- parsePrimary
        foldl
            (\f i ->
                case i of
                    Left (Left n) -> SubscriptExpr f n
                    Left (Right es) -> FCallExpr f es
                    Right True -> UnOpExpr PostInc f
                    Right False -> UnOpExpr PostDec f
            )
            x <$> parseCallHelper
    where
        parseCallHelper :: Parser [Either (Either Expr [Expr]) Bool]
        parseCallHelper =many ((
            do
            symbol "["; n <-  parseExpr ; symbol "]"
            return (Left (Left n))
            )
            <|>
            (do
            Left . Right <$> parseTuple
            )
            <|>
            (
            do
            symbol "++"
            return (Right True)
            )
            <|>
            (
            do
            symbol "--"
            return (Right False)
            ))

parseUnary :: Parser Expr
parseUnary =
        do
        un <- parseBoolNot <|> parseDec <|> parseNeg <|> parseRef <|> parseDeref <|> parseBitNot <|> parseInc
        UnOpExpr un <$> parseUnary
    <|>
        parseCall

    where
        parseBoolNot = do symbol "!"; return BoolNot
        parseNeg = do symbol "-"; return Neg
        parseRef = do symbol "&"; return Ref
        parseDeref = do symbol "*" ;return Deref
        parseBitNot = do symbol "~"; return BitNot
        parseInc = do symbol "++"; return PreInc
        parseDec = do symbol "--"; return PreDec

buildBinopParser :: [(String, BinOp)] -> Parser Expr -> Parser Expr
buildBinopParser ls childParser =
    do
    x <- childParser
    bs <- many ( do
        sym <- foldr (<|>) empty
                (map (\p ->
                        let (s, b) = p in
                        (do symbol s; return b)
                    ) ls)
        x' <- childParser
        return (sym, x')
        )
    return (foldl
            (\f i ->
            case i of
                (sym, x') -> BinOpExpr f sym x'
            )
            x
            bs
        )
parseProd :: Parser Expr
parseProd = buildBinopParser [("*", Mult), ("/", Div), ("%", Mod)] parseUnary
parseSum :: Parser Expr
parseSum = buildBinopParser [("+", Plus), ("-", Minus)] parseProd
parseShift :: Parser Expr
parseShift = buildBinopParser [("<<", LShift), (">>", RShift)] parseSum
parseComp :: Parser Expr
parseComp = buildBinopParser [("<=", LsEq), (">=", GrEq), ("<", Ls), (">", Gr)] parseShift
parseCompEq :: Parser Expr
parseCompEq = buildBinopParser [("==", Eq), ("!=", NEq)] parseComp
parseBitAnd :: Parser Expr
parseBitAnd = buildBinopParser [("&", BitAnd)] parseCompEq
parseBitXor :: Parser Expr
parseBitXor = buildBinopParser [("^", BitXor)] parseBitAnd
parseBitOr :: Parser Expr
parseBitOr = buildBinopParser [("|", BitOr)] parseBitXor
parseBoolAnd :: Parser Expr
parseBoolAnd = buildBinopParser [("&&", BoolAnd)] parseBitOr
parseBoolOr :: Parser Expr
parseBoolOr = buildBinopParser [("||", BoolOr)] parseBoolAnd

parseAssn :: Parser Expr
parseAssn =
        do
        x <- parseBoolOr
        assn <- (do symbol "="; return JustAssn) <|>
                (do symbol "+="; return PlusAssn) <|>
                (do symbol "-="; return MinusAssn) <|>
                (do symbol "*="; return MultAssn) <|>
                (do symbol "/="; return DivAssn) <|>
                (do symbol "%="; return ModAssn) <|>
                (do symbol "&="; return AndAssn) <|>
                (do symbol "^="; return XorAssn) <|>
                (do symbol "|="; return OrAssn) <|>
                (do symbol "<<="; return LShiftAssn) <|>
                (do symbol ">>="; return RShiftAssn)
        BinOpExpr x (AssnOp assn) <$> parseAssn
    <|>
        parseBoolOr
    where
        parseJustAssn = do symbol "="; return JustAssn

parseExpr :: Parser Expr
parseExpr = parseAssn

data Command =
    EmptyCommand |
    ExprCommand Expr |
    BlockCommand [Command] |
    DeclCommand CType String |
    InitCommand CType String Expr |
    WhileCommand Expr Command |
    ITECommand Expr Command Command
    deriving Eq
instance Show Command where
    show c =
        case c of
            EmptyCommand -> ";"
            ExprCommand e -> show e ++ ";"
            BlockCommand cs -> "{" ++ concatMap show cs ++ "}"
            DeclCommand c s -> show c ++ " " ++ s ++ ";";
            InitCommand c s e -> show c ++ " " ++ s ++ " = " ++ show e ++ ";"
            WhileCommand e c -> "while(" ++ show e ++ ")" ++ show c
            ITECommand e c1 c2 -> "if(" ++ show e ++ ")" ++ show c1  ++ "else" ++ show c2
type Program = [Command]



parseWhileCommand :: Parser Command
parseWhileCommand =
    do
        symbol "while" ; symbol "("; e <- parseExpr; symbol ")"
        WhileCommand e <$> parseCommand

parseITECommand :: Parser Command
parseITECommand =
    do
        symbol "if"; symbol "("; e <- parseExpr; symbol ")"
        c1 <- parseCommand
        (do
            symbol "else"
            ITECommand e c1 <$> parseCommand
            )
            <|>
            return (ITECommand e c1 EmptyCommand)
parseEmptyCommand :: Parser Command
parseEmptyCommand =
    do
        symbol ";"
        return EmptyCommand
parseExprCommand :: Parser Command
parseExprCommand =
    do
        e <- parseExpr
        symbol ";"
        return (ExprCommand e)

parseInitCommand :: Parser Command
parseInitCommand =
    do
        p <- parseDecl
        symbol "="
        e <- parseExpr
        symbol ";"
        let (s, c) = p in
            return (InitCommand c s e)
parseDeclCommand :: Parser Command
parseDeclCommand =
    do
        p <- parseDecl
        symbol ";"
        let (s, c) = p in
            return (DeclCommand c s)

parseBlockCommand :: Parser Command
parseBlockCommand =
    do
        symbol "{"
        x <- parseProgram
        symbol "}"
        return (BlockCommand x)

parseCommand :: Parser Command
parseCommand =
    do
    parseEmptyCommand <|>
        parseITECommand  <|>
        parseBlockCommand <|>
        parseInitCommand <|>
        parseDeclCommand <|>
        parseWhileCommand <|>
        parseExprCommand

parseProgram :: Parser Program
parseProgram = many parseCommand

parseInclude :: Parser String
parseInclude =
    do
        symbol "#include"
        symbol "<"
        x <- parseIdent
        symbol ".h>"
        return x

parseCProgram :: Parser Program
parseCProgram =
    do
        many parseInclude
        symbols ["int", "main", "(", ")", "{"]
        p <- parseProgram
        symbol "}"
        return p

type Label = String
type Data = (String, [Command])
data Extendability =
    Extendable | Unextendable
    deriving (Show, Eq)
type Node = (Label, Data, Extendability)
getCommands :: Node -> [Command]
getCommands (_, (_, cs), _) = cs
type Edge = (Label, Label)
type Graph = ([Node], [Edge])

end:: Node
end = ("END", ("(End of Program)",[]), Extendable)

natStream:: [Integer]
natStream = 0: map (+1) natStream
labelStream :: [String]
labelStream = map (\n -> "B" ++ show n) natStream
odds :: LabelStream -> LabelStream
odds (x:_:tl) = x:odds tl
evens :: LabelStream -> LabelStream
evens (_:x:tl) = x:evens tl
type LabelStream = [String]
generator :: [Command] -> LabelStream -> Graph -> (LabelStream, Graph)
generator [] ls g = (ls, g)
generator (BlockCommand cs: tl) ls g =
    generator
        (case cs of
            [] -> EmptyCommand:tl
            _ -> cs ++ tl)
        ls
        g
generator (WhileCommand e c: tail) ls g =
    let (ls', g') = generator tail ls g in
    let (ns, es) = g' in
    let ((l_temp, _, _):_) = ns in
    let (l:ls'') = ls' in
    let (ls_new, g_new) = generator [c] ls'' ((l, ("while_end", []), Unextendable):ns, es) in
    let (hd_new:ns_new, es_new) = g_new in
    let (l_temp_new, _, _) = hd_new in
    let (l_new:ls_new') = ls_new in
    (ls_new', ((l_new, ("while" ++ "(" ++ show e ++ ")", [ExprCommand e]), Unextendable):hd_new:ns_new, (l,l_new):(l_new,l_temp):(l_new, l_temp_new):es_new))
generator (ITECommand e c1 c2: tail) ls g =
    let (ls', g') = generator tail ls g in
    let ((l, d, _):tl,es) = g' in
    let (ls1, g1) = generator [c1] (odds ls') ((l, d, Unextendable):tl, es) in
    let (ls2, g2) = generator [c2] (evens ls') ((l, d, Unextendable):tl, es) in
    let (hd1:ns1, es1) = g1 in
    let (hd2:ns2, es2) = g2 in
    let (ns, es) = (ns1 `union` ns2, es1 `union` es2) in
    let (l:ls) = ls1 `union` ls2 in
    let (l1, d1, _) = hd1 in
    let (l2, d2, _) = hd2 in
    (ls, ((l, ("if (" ++ show e++")", [ExprCommand e]), Unextendable):hd1:hd2:ns, (l,l1):(l,l2):es))

generator (c:tail) ls g =
    let (ls', g') = generator tail ls g in
    let (hn:tn,es) = g' in

    case hn of
        (l, (sd, cd), Extendable) -> (ls', ((l, (" \\n " ++ show c ++ sd, c:cd), Extendable):tn, es))
        (l, d, Unextendable) ->
            let (l':ls'') = ls' in
                (ls'',((l', (" \\n " ++ show c, [c]), Extendable):hn:tn, (l', l):es) )

genGraph :: [Command] -> Graph
genGraph cs =
    snd (generator cs labelStream ([end], []))

dotNode :: Node -> String
dotNode (l, (s, _), _) =
    l ++ " [label = \"" ++ l ++ ":" ++ s ++ "\"];"
dotEdge :: Edge -> String
dotEdge (l1, l2) =
    l1 ++ " -> "++ l2 ++ ";"
dotGraph :: Graph -> String
dotGraph (ns, es)=
    foldr (\i f -> i ++ "\n" ++ f) "" (map dotNode ns) ++ "\n" ++
    foldr (\i f -> i ++ "\n" ++ f) "" (map dotEdge es)
dotGen :: String -> String
dotGen p =
    case parseSimp parseCProgram p of
        Ok (cs, ([], _ , _)) ->
            "digraph { \n" ++ dotGraph (genGraph cs) ++ "\n}"

cyclometricComplexity :: Graph -> Int
cyclometricComplexity (ns, es) =
    length es - length ns + 2

type Definition = (Expr, Assn, Expr)

(!!!) :: [Definition] -> [Definition] -> [Definition]
a !!! [] = a
a !!!(hd:tl) =
    let (e2, _, _) = hd in
    filter (\i -> let (e1, _, _) = i in e1 /= e2) a !!! tl

getKilled :: [Definition] -> [Definition] -> [Definition]
getKilled a [] = []
getKilled a (hd:tl) =
    let (e2, _, _) = hd in
    union (filter (\i -> let (e1, _, _) = i in e1 == e2) a) (getKilled a tl)
(-->) :: [Definition] -> [Definition] -> [Definition]
a --> b =
    (a !!! b) `union` b

genExpression :: Expr -> [Definition]
genExpression e =
    case e of
        FCallExpr e1 es -> foldr (-->) [] (map genExpression es) --> genExpression e1
        SubscriptExpr e1 e2 -> genExpression e1 --> genExpression e2
        UnOpExpr _ e' -> genExpression e'
        BinOpExpr e1 (AssnOp a) e2 ->  genExpression e1 --> (genExpression e2 --> [(e1, a, e2)])
        BinOpExpr e1 _ e2 -> genExpression e1 --> genExpression e2
        ArrayExpr es -> foldr (-->) [] (map genExpression es)
        _ -> []

-- data Command =
--     EmptyCommand |
--     ExprCommand Expr |
--     BlockCommand [Command] |
--     DeclCommand CType String |
--     InitCommand CType String Expr |
--     WhileCommand Expr Command |
--     ITECommand Expr Command Command
--     deriving Eq
genCommand :: Command -> [Definition]
genCommand c =
    case c of
        EmptyCommand -> []
        ExprCommand e -> genExpression e
        BlockCommand cs ->  foldr (-->) [] (map genCommand cs)
        DeclCommand _ _ -> []
        InitCommand _ s e -> genExpression e --> [(VarExpr s, JustAssn, e)]
        _ -> []

genCommands :: [Command] -> [Definition]
genCommands cs = foldr (-->) [] (map genCommand cs)

findIncoming :: Label -> [Edge] -> [Label]
findIncoming l es =
    map fst (filter (\e -> snd e == l) es)

findOutgoing :: Label -> [Edge] -> [Label]
findOutgoing l es =
    map snd (filter (\e -> fst e == l) es)

findNode :: Label -> [Node] -> Maybe Node
findNode l [] = Nothing
findNode l1 ((l2, d, e):ns) =
    if l1 == l2
        then Just (l2, d, e)
        else findNode l1 ns

unwrap :: Maybe a -> a
unwrap (Just n) = n

type ReachingData = (Label, [Definition], [Definition], [Definition], [Definition])

emptyData :: [Node] -> [ReachingData]
emptyData = map (\n -> let (l, _, _) = n in (l, [], [], [], []))


gen :: ReachingData -> [Definition]
gen (_, g, _, _, _) = g
kill :: ReachingData -> [Definition]
kill (_, _, k, _, _) = k
inn :: ReachingData -> [Definition]
inn (_, _, _, i, _) = i
out :: ReachingData -> [Definition]
out (_, _, _, _, o) = o
label :: ReachingData -> Label
label (l, _, _, _, _) = l

findData :: Label -> [ReachingData] -> Maybe ReachingData
findData l [] = Nothing
findData l1 ((l2, g, k, i, o):ns) =
    if l1 == l2
        then Just (l1, g, k, i, o)
        else findData l1 ns
updateData :: [ReachingData] -> Graph -> [ReachingData]
updateData rd g =
    let (ns, es) = g in
        map (\r ->
                let (l, g, k, i, o) = r in
                let g' = genCommands (getCommands (unwrap (findNode l ns))) in
                let i' = foldr union [] (map ( out . unwrap . (`findData` rd)) (findIncoming l es)) in
                let k' = getKilled i' g' in
                let o' = i' --> g' in
                    (l, g', k', i', o')
             ) rd

updateDataLoop :: [ReachingData] -> Int -> Graph -> [ReachingData]
updateDataLoop rd i  g =
    let rd' = updateData rd g in
        if rd' == rd || (i > 300)
            then rd
            else updateDataLoop rd' (i+1) g


analyseGraph :: Graph -> [ReachingData]
analyseGraph g =
    let (ns, es) = g in
        updateDataLoop (emptyData ns) 0 g

analyseProgram :: String -> [ReachingData]
analyseProgram p =
    case parseSimp parseCProgram p of
        Ok (cs, ([], _ , _)) -> analyseGraph (genGraph cs)
printDef :: Definition -> String
printDef (e1,a,e2) = show e1 ++ show a ++ show e2 ++ ";"
printDefs :: [Definition] -> String
printDefs = concatMap printDef
csvFormat :: [ReachingData] -> String
csvFormat rd =
    foldr (\i f -> i ++ "\n" ++ f) ""
        (map (\r -> 
            let (l, g, k, i, o) = r in
                l ++ "," ++ printDefs g ++ "," ++ printDefs k ++ "," ++ printDefs i ++ "," ++ printDefs o
            ) rd)
main :: IO ()
main =
    do
        args <- getArgs
        case args of
            [x] ->
                do
                    input <- readFile x
                    case parse parseCProgram (input, 0, 0) of
                        Ok (cs, ([], _, _)) ->
                            do
                                let dotPath = x -<.> "dot";
                                let csvPath = x -<.> "csv";
                                let g = genGraph cs;
                                putStrLn ("The cyclometric complexity, #nodes, #edges is: "++ show (cyclometricComplexity g) ++ "," ++ show (length ns) ++ "," ++ show (length es))
                                writeFile dotPath (dotGen input)
                                let rd = analyseGraph g;
                                writeFile csvPath (csvFormat rd)
                                return ()
                        Ok (_, (_, line, char)) -> putStrLn ("Parse Error at Line: " ++ show line ++ " and Character: " ++ show char)
                        Err (string, line, char) -> putStrLn ("Parse Error with Message: " ++ string ++ "at Line: " ++ show line ++ "at Character: " ++ show char)
            _ -> putStrLn "Only one argument allowed"
