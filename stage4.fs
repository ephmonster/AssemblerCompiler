
module CompilationEngine
open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic


[<StructuredFormatDisplay("<{Type}>{Lexeme}</{Type}>")>]
type public Token(typ:string, lex:string) = 
    let xmlTok typ lex =
        if typ = "symbol" then
            match lex with
            |"<" -> "&lt;"
            |">" -> "&gt;"
            |"&" -> "&amp;"
            |_ -> lex
        else
            lex

    member val Type = typ 
    member val Lexeme = xmlTok typ lex
    new() = Token(null, null)

let inline (>=<) a (b,c) = a >= b && a <= c
let inline (+=) a b = a := a.Value + b 

let Keywords =
    ["class"; "constructor"; "function"; "method"; "field"; 
     "static"; "var"; "int"; "char"; "boolean"; "void"; "true"; 
     "false"; "null"; "this"; "let"; "do"; "if"; "else"; 
     "while"; "return"] |> Set

let Symbols = 
    ['{'; '}'; '('; ')'; '['; ']'; '.'; ','; ';'; '+'; '-'; 
     '*'; '/'; '&'; '|'; '<'; '>'; '='; '~'] |> Set

let Ops = 
    ['+'; '-'; '*'; '/'; '&'; '|'; '<'; '>'; '='] |> Set

let UniaryOps = 
    ['-'; '~'] |> Set

let isKeyword(word: string) = 
        Keywords.Contains(word)
        
let isSymbol(word:string) =
    word.Length = 1 && Symbols.Contains(char word)

let isIntegerConstant(word: string) = 
    let success, value = Int32.TryParse(word)
    success && value >=< (0, 32767) // value between 0 and 2^15

let isStringConstant(word: string) =
    Regex.IsMatch(word, "^\"[^\n\"]*\"$")

let isIdentifier(word: string) =
    Regex.IsMatch(word, "^[a-zA-Z_][a-zA-Z0-9_]*$")

type Tokenizer(filepath: string) as self = 
    let mutable _tokens = new List<Token>()
    let mutable _curToken = Token()
    let mutable _chars: char array = Array.empty  
    let mutable _position = ref -1
    do 
        self.Tokenizer(filepath)

    member _.Tokens=_tokens

    member _.Tokenizer(filepath:string) =
        let i = ref 0
        let mutable input = File.ReadAllText(filepath).Split('\n')
        let mutable lines = ResizeArray<string>()
        while i.Value < input.Length do
            let mutable curLine = Regex.Replace(Regex.Replace(input.[i.Value].Trim(), @"\/\/.*$", ""), @"\/\*.*?\*\/", "");
            if String.length(curLine) = 0 || curLine.StartsWith "//" || curLine.StartsWith "\r" then
                i += 1
            elif curLine.StartsWith "/*" then
                while not(input.[i.Value].EndsWith("*/")) && not(input.[i.Value].EndsWith("*/\r"))  do   
                    i += 1
                i += 1
            else 
                i += 1
                lines.Add(curLine)

        let mutable chars = ResizeArray<char>()
        for i in lines.ToArray() do
            let x = Regex.Replace($"{i}", @"\\n", "")
            chars.AddRange(x.Trim().ToCharArray())

        _chars <- chars.ToArray()

    member _.NextChar() =
        _position += 1
        _chars.[_position.Value]

    member _.Peek() =
        _chars.[_position.Value + 1]
    
    member _.hasMoreTokens() = 
        _position.Value + 1 < _chars.Length

    member _.tokenType() =
        match _curToken.Type with
            | "keyword" -> "KEYWORD"
            | "symbol" -> "SYMBOL"
            | "identifier" -> "IDENTIFIER"
            | "integerConstant" -> "INT_CONST"
            | "stringConstant" -> "STRING_CONST" 
            | "null" -> "NULL_TYPE"

    member _.keyWord() = 
        let mutable t = ""
        if _curToken.Type = "keyword" then
            t <- _curToken.Lexeme.ToUpper()  
        t       
    
    member _.symbol() =
        let mutable t = ""
        if _curToken.Type = "symbol" then
            t <- _curToken.Lexeme
        t 

    member _.identifier() =
        let mutable t = ""
        if _curToken.Type = "identifier" then
            t <- _curToken.Lexeme
        t 

    member _.intVal() =
        let mutable t = ""
        if _curToken.Type = "integerConstant" then
            t <- _curToken.Lexeme
        t 
    
    member _.stringVal() =
        let mutable t = ""
        if _curToken.Type = "stringConstant" then
            t <- _curToken.Lexeme
        t 
    
    member this.advance() = 
        let mutable current_word = ""
        let mutable current_char = this.NextChar()
        let mutable next_char = char 0

        // whitespace
        if current_char = ' ' then 
            this.advance()   

        // keywords or identifiers
        elif Char.IsLetter(current_char) || current_char = '_' then
            current_word <- current_word + $"{current_char}"

            while this.hasMoreTokens() && (Char.IsLetterOrDigit(this.Peek()) || this.Peek() = '_') do
                next_char <- this.NextChar()
                current_word <- current_word + $"{next_char}"
                    
            // reserved keywords
            if isKeyword(current_word) then
                this.AddToken(Token("keyword", current_word))
            // identifier 
            elif isIdentifier(current_word) then
                this.AddToken(Token("identifier", current_word))
            else exit(-1)
        
        // integers
        elif Char.IsDigit(current_char) then
            current_word <- current_word + $"{current_char}"

            while this.hasMoreTokens() && Char.IsDigit(this.Peek()) do
                next_char <- this.NextChar()
                current_word <- current_word + $"{next_char}"

            if isIntegerConstant(current_word) then
                this.AddToken(Token("integerConstant", current_word))
            else exit(-1)
        
        // strings
        elif current_char = '"' then
            let mutable next_char = this.NextChar()
            
            while this.hasMoreTokens() && next_char <> '"' do
                current_word <- current_word + $"{next_char}"
                next_char <- this.NextChar()
            
            this.AddToken(Token("stringConstant", current_word))

        elif Symbols.Contains(current_char) then
            this.AddToken(Token("symbol", $"{current_char}"))

        else exit(-1)

    // get tokens
    member this.Tokenize() =
        while this.hasMoreTokens() do
            this.advance()

        _tokens

     // Also prints the token to console
    member _.AddToken(token:Token) =
        _curToken <- token        
        _tokens.Add(token)
        //printfn "%A" token

type Kind = NONE | STATIC | FIELD | ARG | VAR

type public Symbol(name:string, typ:string, kind:Kind, index:int) = 
    member val Name = name
    member val Type = typ 
    member val Kind = kind
    member val Index = index
    new() = Symbol(null, null, NONE, -1)

type public SymbolTable()=
    let mutable _classTable = new Dictionary<string, Symbol>()
    let mutable _functionTable = new Dictionary<string, Symbol>()  
    let _staticIndex = ref 0 
    let _fieldIndex = ref 0 
    let _argIndex = ref 0 
    let _varIndex = ref 0

    member this.reset() =
        _functionTable.Clear()
        _argIndex := 0
        _varIndex := 0

    member _.Define(name:string, typ:string, kind:Kind) =
        match kind with 
        | Kind.STATIC ->
            _classTable[name] <- Symbol(name, typ, Kind.STATIC, _staticIndex.Value)
            _staticIndex += 1
        | Kind.FIELD ->
            _classTable[name] <- Symbol(name, typ, Kind.FIELD, _fieldIndex.Value)
            _fieldIndex += 1
        | Kind.ARG ->
            _functionTable[name] <- Symbol(name, typ, Kind.ARG, _argIndex.Value)
            _argIndex += 1
        | Kind.VAR ->
            _functionTable[name] <- Symbol(name, typ, Kind.VAR, _varIndex.Value)
            _varIndex += 1
        | _ -> failwith("error")

    member _.VarCount(kind:Kind) =
        match kind with
        | Kind.STATIC -> _staticIndex.Value
        | Kind.FIELD -> _fieldIndex.Value
        | Kind.ARG -> _argIndex.Value
        | Kind.VAR -> _varIndex.Value
        | NONE -> -1

    member _.KindOf(name:string) : Kind =
        if _functionTable.ContainsKey(name) then
            _functionTable[name].Kind
        elif _classTable.ContainsKey(name) then
            _classTable[name].Kind
        else Kind.NONE  

    member _.TypeOf(name:string) : string =
        if _functionTable.ContainsKey(name) then
            _functionTable[name].Type
        elif _classTable.ContainsKey(name) then
            _classTable[name].Type
        else "NONE"

    member _.IndexOf(name:string) =
        if _functionTable.ContainsKey(name) then
            _functionTable[name].Index
        elif _classTable.ContainsKey(name) then
            _classTable[name].Index
        else -1   



type Segment =
    | CONSTANT
    | ARGUMENT
    | LOCAL
    | STATIC
    | THIS
    | THAT
    | POINTER
    | TEMP

type Command = 
    | ADD
    | SUB
    | NEG
    | EQ
    | GT
    | LT
    | AND
    | OR
    | NOT

 type VMWriter(outputFile:string) =
    let writer = new StreamWriter(outputFile)
    member this.writePush(segment: Segment, index: int) =
        writer.WriteLine("push " + string(segment).ToLower() + " " + string(index))
    member this.writePop(segment: Segment, index: int) =
        writer.WriteLine("pop " + string(segment).ToLower() + " " + string(index))
    member this.writeArithmetic(command: Command) =
        writer.WriteLine(string(command).ToLower())
    member this.writeLabel(label: string) =
        writer.WriteLine("label " + label)
    member this.writeGoto(label: string) =
        writer.WriteLine("goto " + label)
    member this.writeIf(label: string) =
        writer.WriteLine("if-goto " + label)
    member this.writeCall(name: string, nVars: int) =
        writer.WriteLine("call " + name + " " + string(nVars))
    member this.writeFunction(name: string, nVars: int) =
        writer.WriteLine("function " + name + " " + string(nVars))
    member this.writeReturn()=
        writer.WriteLine("return")
    member this.close() =
        writer.Flush()
        writer.Close()


type CompilationEngine(inputFile: string, outputFile: string, VMOutput: string) = 
    let writer = new StreamWriter(outputFile)
    let tokenizer = new Tokenizer(inputFile)
    let VMW = new VMWriter(VMOutput)
    let ST = new SymbolTable()
    let mutable ifCounter = 0
    let mutable className = ""
    let mutable routineType=""
    let mutable returnType = ""
    let mutable functionName =""
    member this.mapKindToSegment (kind: Kind) : Segment =
        match kind with
        | Kind.STATIC -> Segment.STATIC
        | Kind.FIELD -> Segment.THIS
        | Kind.ARG -> Segment.ARGUMENT
        | Kind.VAR -> Segment.LOCAL
        | Kind.NONE -> failwith "None is not a proper segment"
    member this.compileClass() = 
        writer.WriteLine("<class>")
        Console.WriteLine("<class>")
        tokenizer.advance()
        match tokenizer.keyWord() with 
            | "CLASS" -> 
                writer.WriteLine("<keyword> class </keyword>")
                Console.WriteLine("<keyword> class </keyword>")
            | _ -> failwith("Incorrect compileClass")
        tokenizer.advance()
        match tokenizer.tokenType() with 
            | "IDENTIFIER" -> 
                className <- tokenizer.identifier()
                writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
            | _ -> failwith("Incorrect compileClass")
        tokenizer.advance()
        match tokenizer.symbol() with 
            | "{" -> 
                writer.WriteLine("<symbol> { </symbol>")
                Console.WriteLine("<symbol> { </symbol>")
            | _ -> failwith("Incorrect compileClass")
        tokenizer.advance()
        while tokenizer.keyWord() = "STATIC" || tokenizer.keyWord() = "FIELD" do
            writer.WriteLine("<classVarDec>")
            Console.WriteLine("<classVarDec>")
            this.compileClassVarDec()
            writer.WriteLine("</classVarDec>")
            Console.WriteLine("</classVarDec>")
            tokenizer.advance()
        while tokenizer.keyWord() = "CONSTRUCTOR" || tokenizer.keyWord() = "FUNCTION" || tokenizer.keyWord() = "METHOD" do
            writer.WriteLine("<subroutineDec>")
            Console.WriteLine("<subroutineDec>")
            this.compileSubroutine()
            writer.WriteLine("</subroutineDec>")
            Console.WriteLine("</subroutineDec>")
            tokenizer.advance()
        match tokenizer.symbol() with 
            | "}" -> 
                writer.WriteLine("<symbol> } </symbol>")
                Console.WriteLine("<symbol> } </symbol>")
            | _ -> failwith("Incorrect compileClass")
        writer.WriteLine("</class>")
        Console.WriteLine("</class>")
        writer.Flush()
        writer.Close()
        VMW.close()
        


    member this.compileClassVarDec() =
        let mutable _type = ""
        let mutable kind = Kind.NONE
        if tokenizer.keyWord() = "STATIC" then
            kind <- Kind.STATIC
            writer.WriteLine("<keyword> static </keyword>")
            Console.WriteLine("<keyword> static </keyword>")
        elif tokenizer.keyWord() = "FIELD" then
            kind <- Kind.FIELD
            writer.WriteLine("<keyword> field </keyword>")
            Console.WriteLine("<keyword> field </keyword>")
        tokenizer.advance()
        match tokenizer.tokenType() with
            | "KEYWORD" -> 
                match tokenizer.keyWord() with
                    | "INT" -> 
                        _type <- "int"
                        writer.WriteLine("<keyword> int </keyword>")
                        Console.WriteLine("<keyword> int </keyword>")
                    | "CHAR" -> 
                        _type <- "char"
                        writer.WriteLine("<keyword> char </keyword>")
                        Console.WriteLine("<keyword> char </keyword>")
                    | "BOOLEAN" -> 
                        _type <- "boolean"
                        writer.WriteLine("<keyword> boolean </keyword>")
                        Console.WriteLine("<keyword> boolean </keyword>")
                    | _ -> failwith("Incorrect ClassVarDec")
            | "IDENTIFIER" -> 
                _type <- tokenizer.identifier()
                writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
            | _ -> failwith("Incorrect ClassVarDec")
        tokenizer.advance()
        match tokenizer.tokenType() with 
            | "IDENTIFIER" -> 
                ST.Define(tokenizer.identifier(), _type, kind)
                writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
            | _ -> failwith("Incorrect ClassVarDec")
        tokenizer.advance()
        if tokenizer.symbol() = "," then
            while tokenizer.symbol() = "," do
                writer.WriteLine("<symbol> , </symbol>")
                Console.WriteLine("<symbol> , </symbol>")
                tokenizer.advance()
                match tokenizer.tokenType() with 
                    |"IDENTIFIER" -> 
                        ST.Define(tokenizer.identifier(), _type, kind)
                        writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                        Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                    | _ -> failwith("Incorrect ClassVarDec")
                tokenizer.advance()
        match tokenizer.symbol() with 
            |";" -> 
                writer.WriteLine("<symbol> ; </symbol>")
                Console.WriteLine("<symbol> ; </symbol>")
            | _ -> failwith("Incorrect ClassVarDec")

    member this.compileSubroutine() =
        ST.reset()
        match tokenizer.keyWord() with
            |"CONSTRUCTOR" -> 
                routineType <- "Constructor"
                writer.WriteLine("<keyword> constructor </keyword>")
                Console.WriteLine("<keyword> constructor </keyword>")
            |"METHOD" -> 
                routineType <- "Method"
                ST.Define("this", className, ARG)
                writer.WriteLine("<keyword> method </keyword>")
                Console.WriteLine("<keyword> method </keyword>")
            |"FUNCTION" -> 
                routineType <- "Function"
                writer.WriteLine("<keyword> function </keyword>")
                Console.WriteLine("<keyword> function </keyword>")
            | _ -> failwith("Incorrect SubRoutine")
        tokenizer.advance()
        match tokenizer.tokenType() with
                   | "KEYWORD" -> 
                        match tokenizer.keyWord() with
                            | "VOID" -> 
                                writer.WriteLine("<keyword> void </keyword>")
                                Console.WriteLine("<keyword> void </keyword>")
                            | "INT" -> 
                                writer.WriteLine("<keyword> int </keyword>")
                                Console.WriteLine("<keyword> int </keyword>")
                            | "CHAR" -> 
                                writer.WriteLine("<keyword> char </keyword>")
                                Console.WriteLine("<keyword> char </keyword>")
                            | "BOOLEAN" -> 
                                writer.WriteLine("<keyword> boolean </keyword>")
                                Console.WriteLine("<keyword> boolean </keyword>")
                            | _ -> failwith("Incorrect SubRoutine")
                   | "IDENTIFIER" -> 
                        writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                        Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                   | _ -> failwith("Incorrect SubRoutine")
        tokenizer.advance()
        match tokenizer.tokenType() with 
            | "IDENTIFIER" -> 
                functionName <- tokenizer.identifier()
                writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
            | _ -> failwith("Incorrect SubRoutine")
        tokenizer.advance()
        match tokenizer.symbol() with 
            | "(" -> 
                writer.WriteLine("<symbol> ( </symbol>")
                Console.WriteLine("<symbol> ( </symbol>")
            | _ -> failwith("Incorrect SubRoutine")
        tokenizer.advance()
        writer.WriteLine("<parameterList>")
        Console.WriteLine("<parameterList>")
        this.compileParameterList()
        writer.WriteLine("</parameterList>")
        Console.WriteLine("</parameterList>")
        match tokenizer.symbol() with
            | ")" -> 
                writer.WriteLine("<symbol> ) </symbol>")
                Console.WriteLine("<symbol> ) </symbol>")
            | _ -> failwith("Incorrect SubRoutine")
        tokenizer.advance()
        writer.WriteLine("<subroutineBody>")
        Console.WriteLine("<subroutineBody>")
        this.compileSubroutineBody()
        writer.WriteLine("</subroutineBody>")
        Console.WriteLine("</subroutineBody>")

        member this.compileParameterList()=
            let mutable typ = ""
            if tokenizer.tokenType() = "IDENTIFIER" || tokenizer.keyWord() = "INT" ||tokenizer.keyWord()="CHAR" || tokenizer.keyWord() = "BOOLEAN" then
                match tokenizer.tokenType() with
                | "KEYWORD" -> 
                     match tokenizer.keyWord() with
                         | "INT" -> 
                            typ <- "int"
                            writer.WriteLine("<keyword> int </keyword>")
                            Console.WriteLine("<keyword> int </keyword>")
                         | "CHAR" -> 
                            typ <- "char"
                            writer.WriteLine("<keyword> char </keyword>")
                            Console.WriteLine("<keyword> char </keyword>")
                         | "BOOLEAN" -> 
                            typ <- "boolean"
                            writer.WriteLine("<keyword> boolean </keyword>")
                            Console.WriteLine("<keyword> boolean </keyword>")
                         | _ -> failwith("Incorrect paramList")
                | "IDENTIFIER" -> 
                    typ <- tokenizer.identifier()
                    writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                    Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                tokenizer.advance()
                match tokenizer.tokenType() with 
                    | "IDENTIFIER" -> 
                        ST.Define(tokenizer.identifier(), typ, ARG)
                        writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                        Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                    | _ -> failwith("Incorrect ParamList")
                tokenizer.advance()
                while tokenizer.symbol() = "," do
                    writer.WriteLine("<symbol> , </symbol>")
                    Console.WriteLine("<symbol> , </symbol>")
                    tokenizer.advance()
                    match tokenizer.tokenType() with
                    | "KEYWORD" -> 
                         match tokenizer.keyWord() with
                             | "INT" -> 
                                typ <- "int"
                                writer.WriteLine("<keyword> int </keyword>")
                                Console.WriteLine("<keyword> int </keyword>")
                             | "CHAR" -> 
                                typ <- "char"
                                writer.WriteLine("<keyword> char </keyword>")
                                Console.WriteLine("<keyword> char </keyword>")
                             | "BOOLEAN" -> 
                                typ <- "boolean"
                                writer.WriteLine("<keyword> boolean </keyword>")
                                Console.WriteLine("<keyword> boolean </keyword>")
                             | _ -> failwith("Incorrect paramList")
                    | "IDENTIFIER" -> 
                        typ <- tokenizer.identifier()
                        writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                        Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                    |_ -> failwith("Incorrect paramList")
                    tokenizer.advance()
                    match tokenizer.tokenType() with 
                        | "IDENTIFIER" -> 
                            ST.Define(tokenizer.identifier(), typ, ARG)
                            writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                            Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                        | _ -> failwith("Incorrect ParamList")
                    tokenizer.advance()

       

        member this.compileSubroutineBody()=
            match tokenizer.symbol() with 
                | "{" -> 
                    writer.WriteLine("<symbol> { </symbol>")
                    Console.WriteLine("<symbol> { </symbol>")
                | _ -> failwith("Incorrect subRoutineBody")
            tokenizer.advance()
            while tokenizer.keyWord() = "VAR" do
                writer.WriteLine("<varDec>")
                Console.WriteLine("<varDec>")
                this.compileVarDec()
                writer.WriteLine("</varDec>")
                Console.WriteLine("</varDec>")
            VMW.writeFunction(className + "." + functionName, ST.VarCount(VAR))
            match routineType with
            | "Constructor" ->
                VMW.writePush(CONSTANT, ST.VarCount(FIELD))
                VMW.writeCall("Memory.alloc", 1)
                VMW.writePop(POINTER, 0)
            |"Method" ->
                VMW.writePush(ARGUMENT, 0)
                VMW.writePop(POINTER, 0)
            |_->()
            writer.WriteLine("<statements>")
            Console.WriteLine("<statements>")
            this.compileStatements()
            writer.WriteLine("</statements>")
            Console.WriteLine("</statements>")
            match tokenizer.symbol() with 
                | "}" -> 
                    writer.WriteLine("<symbol> } </symbol>")
                    Console.WriteLine("<symbol> } </symbol>")
                | _ -> failwith("Incorrect subRoutineBody")

        member this.compileVarDec() =
            let mutable type_ = ""
            writer.WriteLine("<keyword> var </keyword>")
            Console.WriteLine("<keyword> var </keyword>")
            tokenizer.advance()
            match tokenizer.tokenType() with
            | "KEYWORD" -> 
                 match tokenizer.keyWord() with
                     | "INT" -> 
                        type_ <- "int"
                        writer.WriteLine("<keyword> int </keyword>")
                        Console.WriteLine("<keyword> int </keyword>")
                     | "CHAR" -> 
                        type_<-"char"
                        writer.WriteLine("<keyword> char </keyword>")
                        Console.WriteLine("<keyword> char </keyword>")
                     | "BOOLEAN" -> 
                        type_<-"boolean"
                        writer.WriteLine("<keyword> boolean </keyword>")
                        Console.WriteLine("<keyword> boolean </keyword>")
                     | _ -> failwith("Incorrect varDec")
            | "IDENTIFIER" -> 
                type_<-tokenizer.identifier()
                writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
            | _ -> failwith("Incorrect varDec")
            tokenizer.advance()
            match tokenizer.tokenType() with 
                | "IDENTIFIER" -> 
                    ST.Define(tokenizer.identifier(), type_, VAR)
                    writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                    Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                | _ -> failwith("Incorrect varDec")
            tokenizer.advance()
            while tokenizer.symbol() = "," do
                writer.WriteLine("<symbol> , </symbol>")
                Console.WriteLine("<symbol> , </symbol>")
                tokenizer.advance()
                match tokenizer.tokenType() with 
                    | "IDENTIFIER" -> 
                        ST.Define(tokenizer.identifier(), type_, VAR)
                        writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                        Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                    | _ -> failwith("Incorrect varDec")
                tokenizer.advance()
            match tokenizer.tokenType() with
                | "SYMBOL" ->
                    match tokenizer.symbol() with
                    |";" -> 
                        writer.WriteLine("<symbol> ; </symbol>")
                        Console.WriteLine("<symbol> ; </symbol>")
                    | _ -> failwith("Incorrect varDec")
                |_-> failwith("Incorrect varDec")
            tokenizer.advance()

        member this.compileStatements()=
            if tokenizer.keyWord() = "LET" || tokenizer.keyWord() = "IF" || tokenizer.keyWord() = "WHILE" || tokenizer.keyWord() = "DO" || tokenizer.keyWord() = "RETURN" then
                while tokenizer.keyWord() = "LET" || tokenizer.keyWord() = "IF" || tokenizer.keyWord() = "WHILE" || tokenizer.keyWord() = "DO" || tokenizer.keyWord() = "RETURN" do
                    match tokenizer.keyWord() with
                        | "LET" -> 
                            writer.WriteLine("<letStatement>")
                            Console.WriteLine("<letStatement>")
                            this.compileLet()
                            writer.WriteLine("</letStatement>")
                            Console.WriteLine("</letStatement>")
                        | "IF" -> 
                            writer.WriteLine("<ifStatement>")
                            Console.WriteLine("<ifStatement>")
                            this.compileIf()
                            writer.WriteLine("</ifStatement>")
                            Console.WriteLine("</ifStatement>")
                        | "WHILE" -> 
                            writer.WriteLine("<whileStatement>")
                            Console.WriteLine("<whileStatement>")
                            this.compileWhile()
                            writer.WriteLine("</whileStatement>")
                            Console.WriteLine("</whileStatement>")
                        | "DO" -> 
                            writer.WriteLine("<doStatement>")
                            Console.WriteLine("<doStatement>")
                            this.compileDo()
                            writer.WriteLine("</doStatement>")
                            Console.WriteLine("</doStatement>")
                        | "RETURN" -> 
                            writer.WriteLine("<returnStatement>")
                            Console.WriteLine("<returnStatement>")
                            this.compileReturn()
                            writer.WriteLine("</returnStatement>")
                            Console.WriteLine("</returnStatement>")
            else
               ()

        member this.compileLet()=
            let mutable id = ""
            let mutable ty_ = ""
            writer.WriteLine("<keyword> let </keyword>")
            Console.WriteLine("<keyword> let </keyword>")
            tokenizer.advance()
            match tokenizer.tokenType() with
                | "IDENTIFIER" -> 
                    id <- tokenizer.identifier()
                    writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                    Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                | _ -> failwith("Incorrect let")
            tokenizer.advance()
            if tokenizer.symbol() = "[" then
                ty_ <- "Array"
                VMW.writePush(this.mapKindToSegment(ST.KindOf(id)), ST.IndexOf(id))
                writer.WriteLine("<symbol> [ </symbol>")
                Console.WriteLine("<symbol> [ </symbol>")
                tokenizer.advance()
                writer.WriteLine("<expression>")
                Console.WriteLine("<expression>")
                this.compileExpression()
                writer.WriteLine("</expression>")
                Console.WriteLine("</expression>")
                VMW.writeArithmetic(ADD)
                match tokenizer.symbol() with
                    | "]" -> 
                        writer.WriteLine("<symbol> ] </symbol>")
                        Console.WriteLine("<symbol> ] </symbol>")
                    | _ -> failwith("Incorrect let")
                tokenizer.advance()
            match tokenizer.symbol() with
                | "=" -> 
                    writer.WriteLine("<symbol> = </symbol>")
                    Console.WriteLine("<symbol> = </symbol>")
                | _ -> failwith("Incorrect let")
            tokenizer.advance()
            writer.WriteLine("<expression>")
            Console.WriteLine("<expression>")
            this.compileExpression()
            writer.WriteLine("</expression>")
            Console.WriteLine("</expression>")
            match tokenizer.symbol() with
                | ";" -> 
                    writer.WriteLine("<symbol> ; </symbol>")
                    Console.WriteLine("<symbol> ; </symbol>")
                | _ -> failwith("Incorrect let")
            match ty_ with
            |"Array" -> 
                VMW.writePop(TEMP, 0)
                VMW.writePop(POINTER, 1)
                VMW.writePush(TEMP, 0)
                VMW.writePop(THAT, 0)
            |_ -> VMW.writePop(this.mapKindToSegment(ST.KindOf(id)), ST.IndexOf(id))
            tokenizer.advance()

        member this.compileIf()=
            let mutable ourCounter1 = ifCounter + 1
            let mutable ourCounter2 = ifCounter + 2
            ifCounter <- ifCounter+2
            writer.WriteLine("<keyword> if </keyword>")
            Console.WriteLine("<keyword> if </keyword>")
            tokenizer.advance()
            match tokenizer.symbol() with 
                | "(" -> 
                    writer.WriteLine("<symbol> ( </symbol>")
                    Console.WriteLine("<symbol> ( </symbol>")
                |_ -> failwith("Incorrect if")
            tokenizer.advance()
            writer.WriteLine("<expression>")
            Console.WriteLine("<expression>")
            this.compileExpression()
            writer.WriteLine("</expression>")
            Console.WriteLine("</expression>")
            match tokenizer.symbol() with
                | ")" -> 
                    writer.WriteLine("<symbol> ) </symbol>")
                    Console.WriteLine("<symbol> ) </symbol>")
                | _ -> failwith("Incorrect if")
            VMW.writeArithmetic(NOT)
            VMW.writeIf("L"+string(ourCounter1))
            tokenizer.advance()
            match tokenizer.symbol() with
                | "{" -> 
                    writer.WriteLine("<symbol> { </symbol>")
                    Console.WriteLine("<symbol> { </symbol>")
                | _ -> failwith("Incorrect if")
            tokenizer.advance()
            writer.WriteLine("<statements>")
            Console.WriteLine("<statements>")
            this.compileStatements()
            writer.WriteLine("</statements>")
            Console.WriteLine("</statements>")
            match tokenizer.symbol() with
                | "}" -> 
                    writer.WriteLine("<symbol> } </symbol>")
                    Console.WriteLine("<symbol> } </symbol>")
                | _ -> failwith("Incorrect if")
            tokenizer.advance()
            VMW.writeGoto("L" + string(ourCounter2))
            VMW.writeLabel("L" + string(ourCounter1))
            if tokenizer.keyWord() = "ELSE" then
                writer.WriteLine("<keyword> else </keyword>")
                Console.WriteLine("<keyword> else </keyword>")
                tokenizer.advance()
                match tokenizer.symbol() with
                    | "{" -> 
                        writer.WriteLine("<symbol> { </symbol>")
                        Console.WriteLine("<symbol> { </symbol>")
                    | _ -> failwith("Incorrect if")
                tokenizer.advance()
                writer.WriteLine("<statements>")
                Console.WriteLine("<statements>")
                this.compileStatements()
                writer.WriteLine("</statements>")
                Console.WriteLine("</statements>")
                match tokenizer.symbol() with
                    | "}" -> 
                        writer.WriteLine("<symbol> } </symbol>")
                        Console.WriteLine("<symbol> } </symbol>")
                    | _ -> failwith("Incorrect if")
                tokenizer.advance()
            VMW.writeLabel("L" + string(ourCounter2))



        member this.compileWhile()=
            let mutable whileCounter1 = ifCounter + 1
            let mutable whileCounter2 = ifCounter + 2
            ifCounter <- ifCounter+2
            VMW.writeLabel("L" + string(whileCounter1))
            writer.WriteLine("<keyword> while </keyword>")
            Console.WriteLine("<keyword> while </keyword>")
            tokenizer.advance()
            match tokenizer.symbol() with 
                | "(" -> 
                    writer.WriteLine("<symbol> ( </symbol>")
                    Console.WriteLine("<symbol> ( </symbol>")
                |_ -> failwith("Incorrect while")
            tokenizer.advance()
            writer.WriteLine("<expression>")
            Console.WriteLine("<expression>")
            this.compileExpression()
            writer.WriteLine("</expression>")
            Console.WriteLine("</expression>")
            match tokenizer.symbol() with
                | ")" -> 
                    writer.WriteLine("<symbol> ) </symbol>")
                    Console.WriteLine("<symbol> ) </symbol>")
                | _ -> failwith("Incorrect while")
            VMW.writeArithmetic(NOT)
            VMW.writeIf("L"+string(whileCounter2))
            tokenizer.advance()
            match tokenizer.symbol() with
                | "{" -> 
                    writer.WriteLine("<symbol> { </symbol>")
                    Console.WriteLine("<symbol> { </symbol>")
                | _ -> failwith("Incorrect while")
            tokenizer.advance()
            writer.WriteLine("<statements>")
            Console.WriteLine("<statements>")
            this.compileStatements()
            writer.WriteLine("</statements>")
            Console.WriteLine("</statements>")
            match tokenizer.symbol() with
                | "}" -> 
                    writer.WriteLine("<symbol> } </symbol>")
                    Console.WriteLine("<symbol> } </symbol>")
                | _ -> failwith("Incorrect while")
            VMW.writeGoto("L" + string(whileCounter1))
            VMW.writeLabel("L" + string(whileCounter2))
            tokenizer.advance()

        member this.compileDo()=
            writer.WriteLine("<keyword> do </keyword>")
            Console.WriteLine("<keyword> do </keyword>")
            tokenizer.advance()
            match tokenizer.tokenType() with
                |"IDENTIFIER" -> 
                    let mutable id = tokenizer.identifier()
                    writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                    Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                    tokenizer.advance()
                    if tokenizer.tokenType() = "SYMBOL" then
                        match tokenizer.symbol() with
                            | "(" -> 
                                VMW.writePush(POINTER, 0)
                                writer.WriteLine("<symbol> ( </symbol>")
                                Console.WriteLine("<symbol> ( </symbol>")
                                tokenizer.advance()
                                writer.WriteLine("<expressionList>")
                                Console.WriteLine("<expressionList>")
                                let mutable ar = this.compileExpressionList()
                                writer.WriteLine("</expressionList>")
                                Console.WriteLine("</expressionList>")
                                match tokenizer.symbol() with
                                    | ")" -> 
                                        VMW.writeCall(className + "." + id, ar + 1)
                                        writer.WriteLine("<symbol> ) </symbol>")
                                        Console.WriteLine("<symbol> ) </symbol>")
                                    |_ -> failwith("incorrect do")
                                tokenizer.advance()
                                match tokenizer.symbol() with
                                    | ";" -> 
                                        writer.WriteLine("<symbol> ; </symbol>")
                                        Console.WriteLine("<symbol> ; </symbol>")
                                    |_ -> 
                                        failwith("incorrect do")
                            | "." ->
                                let mutable methodName = ""
                                writer.WriteLine("<symbol> . </symbol>")
                                Console.WriteLine("<symbol> . </symbol>")
                                tokenizer.advance()
                                match tokenizer.tokenType() with
                                    |"IDENTIFIER" -> 
                                        methodName <- tokenizer.identifier()
                                        writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                                        Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                                    |_->failwith("incorrect do")
                                if not(ST.KindOf(id) = NONE) then
                                    VMW.writePush(this.mapKindToSegment(ST.KindOf(id)), ST.IndexOf(id))
                                tokenizer.advance()
                                match tokenizer.symbol() with
                                    | "(" -> 
                                        writer.WriteLine("<symbol> ( </symbol>")
                                        Console.WriteLine("<symbol> ( </symbol>")
                                    |_ -> failwith("incorrect do")
                                tokenizer.advance()
                                writer.WriteLine("<expressionList>")
                                Console.WriteLine("<expressionList>")
                                let mutable ar = this.compileExpressionList()
                                writer.WriteLine("</expressionList>")
                                Console.WriteLine("</expressionList>")
                                match tokenizer.symbol() with
                                    | ")" -> 
                                        writer.WriteLine("<symbol> ) </symbol>")
                                        Console.WriteLine("<symbol> ) </symbol>")
                                    |_ -> failwith("incorrect do")
                                if not(ST.KindOf(id) = NONE) then
                                    VMW.writeCall(ST.TypeOf(id) + "." + methodName, ar + 1)
                                else
                                    VMW.writeCall(id + "." + methodName, ar)
                                tokenizer.advance()
                                match tokenizer.symbol() with
                                    | ";" -> 
                                        writer.WriteLine("<symbol> ; </symbol>")
                                        Console.WriteLine("<symbol> ; </symbol>")
                                    |_ -> failwith("incorrect do")                                 
                            |_ -> failwith("Incorrect do")
                |_ -> failwith("Incorrect do")
            VMW.writePop(TEMP, 0)
            tokenizer.advance()
                


        member this.compileReturn()=
            writer.WriteLine("<keyword> return </keyword>")
            Console.WriteLine("<keyword> return </keyword>")
            tokenizer.advance()
            match tokenizer.tokenType() with
                |"INT_CONST" | "STRING_CONST" | "IDENTIFIER" -> 
                    writer.WriteLine("<expression>")
                    Console.WriteLine("<expression>")
                    this.compileExpression()
                    writer.WriteLine("</expression>")
                    Console.WriteLine("</expression>")
                | "KEYWORD" ->
                    match tokenizer.keyWord() with
                        |"TRUE" |"FALSE"|"NULL" |"THIS" ->     
                            writer.WriteLine("<expression>")
                            Console.WriteLine("<expression>")
                            this.compileExpression()
                            writer.WriteLine("</expression>")
                            Console.WriteLine("</expression>")
                        |_->VMW.writePush(CONSTANT, 0)
                | "SYMBOL" ->
                    match tokenizer.symbol() with
                        |"(" | "-" | "~" -> 
                            writer.WriteLine("<expression>")
                            Console.WriteLine("<expression>")
                            this.compileExpression()
                            writer.WriteLine("</expression>")
                            Console.WriteLine("</expression>")
                        |_-> VMW.writePush(CONSTANT, 0)
                | _->VMW.writePush(CONSTANT, 0)
            match tokenizer.tokenType() with
                |"SYMBOL" ->
                    match tokenizer.symbol() with
                        | ";" ->  
                            writer.WriteLine("<symbol> ; </symbol>")
                            Console.WriteLine("<symbol> ; </symbol>")
                        |_ -> failwith("incorrect return")
                |_ -> failwith("incorrect return")
            match routineType with
            |"Constructor" ->
                VMW.writePush(POINTER, 0)
            |_->()
            VMW.writeReturn()
            tokenizer.advance()



        member this.compileExpression()=
            let mutable stop = true
            writer.WriteLine("<term>")
            Console.WriteLine("<term>")
            this.compileTerm()
            writer.WriteLine("</term>")
            Console.WriteLine("</term>")
            while tokenizer.tokenType() = "SYMBOL" && stop do
                match tokenizer.symbol() with 
                    | "+" | "-" |"*" | "/" | "&amp;" |"|"|"&lt;"|"&gt;"|"=" ->
                        let mutable op = tokenizer.symbol()
                        writer.WriteLine("<symbol> " + tokenizer.symbol() + " </symbol>")
                        Console.WriteLine("<symbol> " + tokenizer.symbol() + " </symbol>")
                        tokenizer.advance()
                        writer.WriteLine("<term>")
                        Console.WriteLine("<term>")
                        this.compileTerm()
                        writer.WriteLine("</term>")
                        Console.WriteLine("</term>")
                        match op with 
                        | "+" -> VMW.writeArithmetic(ADD)
                        | "-" -> VMW.writeArithmetic(SUB)
                        |"*" -> VMW.writeCall("Math.multiply", 2)
                        | "/" -> VMW.writeCall("Math.divide", 2)
                        | "&amp;" -> VMW.writeArithmetic(AND)
                        |"|" -> VMW.writeArithmetic(OR)
                        |"&lt;" -> VMW.writeArithmetic(LT)
                        |"&gt;" -> VMW.writeArithmetic(GT)
                        |"=" -> VMW.writeArithmetic(EQ)
                    |_->
                        stop <- false
                   


        member this.compileTerm() =
            let rec termRec() =
                match tokenizer.tokenType() with
                    |"INT_CONST" -> 
                        VMW.writePush(CONSTANT, int(tokenizer.intVal()))
                        writer.WriteLine("<integerConstant> " + string(tokenizer.intVal()) + " </integerConstant>")
                        Console.WriteLine("<integerConstant> " + string(tokenizer.intVal()) + " </integerConstant>")
                        tokenizer.advance()
                    | "STRING_CONST" -> 
                        let mutable stringLength = tokenizer.stringVal().Length
                        VMW.writePush(CONSTANT, stringLength)
                        VMW.writeCall("String.new", 1)
                        for c in tokenizer.stringVal() do
                            VMW.writePush(CONSTANT, int(c))
                            VMW.writeCall("String.appendChar", 2)
                        writer.WriteLine("<stringConstant> " + string(tokenizer.stringVal()) + " </stringConstant>")
                        Console.WriteLine("<stringConstant> " + string(tokenizer.stringVal()) + " </stringConstant>")
                        tokenizer.advance()
                    | "KEYWORD" ->
                        match tokenizer.keyWord() with
                            |"TRUE" -> 
                                VMW.writePush(CONSTANT, 0)
                                VMW.writeArithmetic(NOT)
                                writer.WriteLine("<keyword> true </keyword>")
                                Console.WriteLine("<keyword> true </keyword>")
                                tokenizer.advance()
                            |"FALSE" -> 
                                VMW.writePush(CONSTANT, 0)
                                writer.WriteLine("<keyword> false </keyword>")
                                Console.WriteLine("<keyword> false </keyword>")
                                tokenizer.advance()
                            |"NULL" -> 
                                VMW.writePush(CONSTANT, 0)
                                writer.WriteLine("<keyword> null </keyword>")
                                Console.WriteLine("<keyword> null </keyword>")
                                tokenizer.advance()
                            |"THIS" -> 
                                VMW.writePush(POINTER, 0)
                                writer.WriteLine("<keyword> this </keyword>")
                                Console.WriteLine("<keyword> this </keyword>")
                                tokenizer.advance()
                            |_ -> failwith("Incorrect term")
                    |"IDENTIFIER" -> 
                        let mutable id = tokenizer.identifier()
                        writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                        Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                        tokenizer.advance()
                        if tokenizer.tokenType() = "SYMBOL" then
                            match tokenizer.symbol() with
                                | "[" -> 
                                    VMW.writePush(this.mapKindToSegment(ST.KindOf(id)), ST.IndexOf(id))
                                    writer.WriteLine("<symbol> [ </symbol>")
                                    Console.WriteLine("<symbol> [ </symbol>")
                                    tokenizer.advance()
                                    writer.WriteLine("<expression>")
                                    Console.WriteLine("<expression>")
                                    this.compileExpression()
                                    writer.WriteLine("</expression>")
                                    Console.WriteLine("</expression>")
                                    VMW.writeArithmetic(ADD)
                                    VMW.writePop(POINTER, 1)
                                    VMW.writePush(THAT, 0)
                                    match tokenizer.symbol() with
                                        | "]" -> 
                                            writer.WriteLine("<symbol> ] </symbol>")
                                            Console.WriteLine("<symbol> ] </symbol>")
                                        |_ -> failwith("incorrect term")
                                    tokenizer.advance()
                                | "(" -> 
                                    VMW.writePush(POINTER, 0)
                                    writer.WriteLine("<symbol> ( </symbol>")
                                    Console.WriteLine("<symbol> ( </symbol>")
                                    tokenizer.advance()
                                    writer.WriteLine("<expressionList>")
                                    Console.WriteLine("<expressionList>")
                                    let mutable args = this.compileExpressionList()
                                    writer.WriteLine("</expressionList>")
                                    Console.WriteLine("</expressionList>")
                                    match tokenizer.symbol() with
                                        | ")" -> 
                                            VMW.writeCall(className + "." + id, args + 1)
                                            writer.WriteLine("<symbol> ) </symbol>")
                                            Console.WriteLine("<symbol> ) </symbol>")
                                        |_ -> failwith("incorrect term")
                                    tokenizer.advance()
                                | "." ->
                                    let mutable methodName = ""
                                    writer.WriteLine("<symbol> . </symbol>")
                                    Console.WriteLine("<symbol> . </symbol>")
                                    tokenizer.advance()
                                    match tokenizer.tokenType() with
                                        |"IDENTIFIER" -> 
                                            methodName <- tokenizer.identifier()
                                            writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                                            Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                                        |_->failwith("incorrect term")
                                    if not(ST.KindOf(id) = NONE) then
                                        VMW.writePush(this.mapKindToSegment(ST.KindOf(id)), ST.IndexOf(id))
                                    tokenizer.advance()
                                    match tokenizer.symbol() with
                                        | "(" -> 
                                            writer.WriteLine("<symbol> ( </symbol>")
                                            Console.WriteLine("<symbol> ( </symbol>")
                                        |_ -> failwith("incorrect term")
                                    tokenizer.advance()
                                    writer.WriteLine("<expressionList>")
                                    Console.WriteLine("<expressionList>")
                                    let mutable args = this.compileExpressionList()
                                    writer.WriteLine("</expressionList>")
                                    Console.WriteLine("</expressionList>")
                                    match tokenizer.symbol() with
                                        | ")" -> 
                                            writer.WriteLine("<symbol> ) </symbol>")
                                            Console.WriteLine("<symbol> ) </symbol>")
                                        |_ -> failwith("incorrect term")
                                    if not(ST.KindOf(id) = NONE) then
                                        VMW.writeCall(ST.TypeOf(id) + "." + methodName, args + 1)
                                    else
                                        VMW.writeCall(id + "." + methodName, args)
                                    tokenizer.advance()

                                |_ -> 
                                    VMW.writePush(this.mapKindToSegment(ST.KindOf(id)), ST.IndexOf(id))
                    |"SYMBOL" ->
                        match tokenizer.symbol() with
                            |"(" -> 
                                writer.WriteLine("<symbol> ( </symbol>")
                                Console.WriteLine("<symbol> ( </symbol>")
                                tokenizer.advance()
                                writer.WriteLine("<expression>")
                                Console.WriteLine("<expression>")
                                this.compileExpression()
                                writer.WriteLine("</expression>")
                                Console.WriteLine("</expression>")
                                match tokenizer.symbol() with
                                    | ")" -> 
                                        writer.WriteLine("<symbol> ) </symbol>")
                                        Console.WriteLine("<symbol> ) </symbol>")
                                    |_ -> failwith("incorrect term")
                                tokenizer.advance()
                            |"-" ->
                                writer.WriteLine("<symbol> - </symbol>")
                                Console.WriteLine("<symbol> - </symbol>")
                                tokenizer.advance()
                                writer.WriteLine("<term>")
                                Console.WriteLine("<term>")
                                termRec()
                                writer.WriteLine("</term>")
                                Console.WriteLine("</term>")
                                VMW.writeArithmetic(NEG)
                            |"~" ->
                                writer.WriteLine("<symbol> ~ </symbol>")
                                Console.WriteLine("<symbol> ~ </symbol>")
                                tokenizer.advance()
                                writer.WriteLine("<term>")
                                Console.WriteLine("<term>")
                                termRec()
                                writer.WriteLine("</term>")
                                Console.WriteLine("</term>")
                                VMW.writeArithmetic(NOT)
                            |_ -> failwith("Incorrect term")
                    |_-> failwith("Incorrect term")
            termRec()
        
        
        member this.compileExpressionList() : int =
            let mutable counter = 0
            let mutable stop = true
            if tokenizer.tokenType() = "INT_CONST" || tokenizer.tokenType() = "STRING_CONST" || tokenizer.tokenType() = "IDENTIFIER" then 
                writer.WriteLine("<expression>")
                Console.WriteLine("<expression>")
                this.compileExpression()
                writer.WriteLine("</expression>")
                Console.WriteLine("</expression>")
                counter <- counter + 1
                while tokenizer.tokenType() = "SYMBOL" && stop do
                    match tokenizer.symbol() with
                        | "," -> 
                            writer.WriteLine("<symbol> , </symbol>")
                            Console.WriteLine("<symbol> , </symbol>")
                            tokenizer.advance()
                            writer.WriteLine("<expression>")
                            Console.WriteLine("<expression>")
                            this.compileExpression()
                            writer.WriteLine("</expression>")
                            Console.WriteLine("</expression>")
                            counter <- counter + 1
                        |_ -> stop <- false
            if tokenizer.tokenType() = "KEYWORD" then 
                match tokenizer.keyWord() with
                    | "TRUE" | "FALSE" | "NULL" | "THIS" ->
                        writer.WriteLine("<expression>")
                        Console.WriteLine("<expression>")
                        this.compileExpression()
                        writer.WriteLine("</expression>")
                        Console.WriteLine("</expression>")
                        counter <- counter + 1
                        while tokenizer.tokenType() = "SYMBOL" && stop do
                            match tokenizer.symbol() with
                                | "," -> 
                                    writer.WriteLine("<symbol> , </symbol>")
                                    Console.WriteLine("<symbol> , </symbol>")
                                    tokenizer.advance()
                                    writer.WriteLine("<expression>")
                                    Console.WriteLine("<expression>")
                                    this.compileExpression()
                                    writer.WriteLine("</expression>")
                                    Console.WriteLine("</expression>")
                                    counter <- counter + 1
                                |_ -> stop <- false
                    |_-> ()
            if tokenizer.tokenType() = "SYMBOL" then 
                match tokenizer.symbol() with
                    | "~" | "-" | "("  ->
                        writer.WriteLine("<expression>")
                        Console.WriteLine("<expression>")
                        this.compileExpression()
                        writer.WriteLine("</expression>")
                        Console.WriteLine("</expression>")
                        counter <- counter + 1
                        while tokenizer.tokenType() = "SYMBOL" && stop do
                            match tokenizer.symbol() with
                                | "," -> 
                                    writer.WriteLine("<symbol> , </symbol>")
                                    Console.WriteLine("<symbol> , </symbol>")
                                    tokenizer.advance()
                                    writer.WriteLine("<expression>")
                                    Console.WriteLine("<expression>")
                                    this.compileExpression()
                                    writer.WriteLine("</expression>")
                                    Console.WriteLine("</expression>")
                                    counter <- counter + 1
                                |_ -> stop <- false
                    |_-> ()
            counter



 type jackAnalyzer() =
    member this.analyze() =
        printfn "Please enter the path: "
        let path = Console.ReadLine()
        let files = Directory.GetFiles(path, "*.jack")
        let XmlWriter(outputDir:string, className:string, tokens:List<Token>) =
            let xmlFilePath = Path.Combine(outputDir, $"{className}Tokenized.xml")
            use xmlFile = new StreamWriter(xmlFilePath)
            xmlFile.WriteLine("<tokens>")
            for token in tokens do
                xmlFile.Write("\t")
                xmlFile.WriteLine($"<{token.Type}> {token.Lexeme} </{token.Type}>")
            xmlFile.WriteLine("</tokens>")
            xmlFile.Flush()
        for file in files do
            let mutable tokenizer = new Tokenizer(file)
            let className = Path.GetFileNameWithoutExtension(file)
            XmlWriter(path, className, tokenizer.Tokenize())
            let compileFilePath = Path.Combine(path, $"{className}Compiled.xml")
            let vmFilePath = Path.Combine(path, $"{className}.vm")
            let mutable engine = new CompilationEngine(file, compileFilePath, vmFilePath)
            engine.compileClass()
            ()

[<EntryPoint>]
let main argv =
    let analyzer = new jackAnalyzer()
    analyzer.analyze()
    0

                    
