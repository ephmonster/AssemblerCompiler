// Learn more about F# at http://fsharp.org

open System
open System.IO
  
let output = new StreamWriter("C:\Temp\Tar0.asm")
let mutable counter = 0
let initialSetPointers() = output.WriteLine("@256
D=A
@SP
M=D
@300
D=A
@LCL
M=D
@400
D=A
@ARG
M=D
@3000
D=A
@THIS
M=D
@3010
D=A
@THAT
M=D
")
let handleAdd() = output.WriteLine("@SP
AM=M-1 
D=M
@SP
AM=M-1
M=M+D
@SP
M=M+1")
let handleSub() = output.WriteLine("@SP
AM=M-1 
D=M
@SP
AM=M-1
M=M-D
@SP
M=M+1")
let handleNeg() = output.WriteLine("@SP
AM=M-1
M=-M
@SP
M=M+1")
let handleEq() =  
    counter <- counter + 1
    output.WriteLine("@SP
AM=M-1 
D=M
@SP
AM=M-1
D=D-M
@EQUAL" + counter.ToString() + "
D;JEQ
@SP
A=M
M=0
@RESUME" + counter.ToString() + "
0; JMP
(EQUAL" + counter.ToString() + ")
@SP
A=M
M=-1
@RESUME" + counter.ToString() + "
0; JMP
(RESUME" + counter.ToString() + ")
@SP
M=M+1")

let handleGt() =  
    counter <- counter + 1
    output.WriteLine("@SP
AM=M-1 
D=M
@SP
AM=M-1
D=M-D
@TRUE" + counter.ToString() + "
D;JGT
@SP
A=M
M=0
@RESUME" + counter.ToString() + "
0; JMP
(TRUE" + counter.ToString() + ")
@SP
A=M
M=-1
@RESUME" + counter.ToString() + "
0; JMP
(RESUME" + counter.ToString() + ")
@SP
M=M+1")

let handleLt() = 
    counter <- counter + 1
    output.WriteLine("@SP
AM=M-1 
D=M
@SP
AM=M-1
D=M-D
@FALSE" + counter.ToString() + "
D;JGE
@SP
A=M
M=-1
@RESUME" + counter.ToString() + "
0; JMP
(FALSE" + counter.ToString() + ")
@SP
A=M
M=0
@RESUME" + counter.ToString() + "
0; JMP
(RESUME" + counter.ToString() + ")
@SP
M=M+1")

let handleAnd() = output.WriteLine("@SP
AM=M-1 
D=M
@SP
AM=M-1
M=D&M
@SP
M=M+1")

let handleOr() = output.WriteLine("@SP
AM=M-1 
D=M
@SP
AM=M-1
M=D|M
@SP
M=M+1")

let handleNot() = output.WriteLine("@SP
AM=M-1 
D=M
M=!D
@SP
M=M+1")

let handlePopStatic(className: string,  x: string) = output.WriteLine("@SP
AM=M-1
D=M 
@" + className + "." + x + "
M=D")

let handlePush(x :string) (y: string) =
    output.WriteLine("command: push segment: " + x + " index: " + y)

let handlePop(x :string) (y: string) =
    output.WriteLine("command: pop segment: " + x + " index: " + y)

let handlePushConstant(x: string) =
     output.WriteLine("@"+x)
     output.WriteLine("D=A
@SP
A=M
M=D
@SP
M=M+1")

let handlePushStatic(className: string, x: string) =
    output.WriteLine("@" + className + "." + x + "
D=M
@SP                     
A=M                       
M=D
@SP
M=M+1")

let handlePushThis(x) =
    output.WriteLine("@" + x + "
D=A
@THIS
A=M
D=D+A
A=D
D=M
@SP
A=M
M=D
@SP
M=M+1")

let handlePushThat(x) =
    output.WriteLine("@" + x + "
D=A
@THAT
A=M
D=D+A
A=D
D=M
@SP
A=M
M=D
@SP
M=M+1")

let handlePushLocal(x) =
    output.WriteLine("@" + x + "
D=A
@LCL
A=M
D=D+A
A=D
D=M
@SP
A=M
M=D
@SP
M=M+1")

let handlePushArg(x) =
    output.WriteLine("@" + x + "
D=A
@ARG
A=M
D=D+A
A=D
D=M
@SP
A=M
M=D
@SP
M=M+1")

let handlePushTemp(x) =
    output.WriteLine("@" + x + "
D=A
@5
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1")

let handlePushPointer0() = 
    output.WriteLine("@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1")

let handlePushPointer1() = 
    output.WriteLine("@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1")

let handlePopThis(x)=
    output.WriteLine("@" + x + "
D=A
@THIS
A=M
D=D+A
@THIS
M=D
@SP
M=M-1
A=M
D=M
@THIS
A=M
M=D
@" + x + "
D=A
@THIS
A=M
D=A-D
@THIS
M=D")

let handlePopThat(x)=
    output.WriteLine("@" + x + "
D=A
@THAT
A=M
D=D+A
@THAT
M=D
@SP
M=M-1
A=M
D=M
@THAT
A=M
M=D
@" + x + "
D=A
@THAT
A=M
D=A-D
@THAT
M=D")

let handlePopArg(x)=
    output.WriteLine("@" + x + "
D=A
@ARG
A=M
D=D+A
@ARG
M=D
@SP
M=M-1
A=M
D=M
@ARG
A=M
M=D
@" + x + "
D=A
@ARG
A=M
D=A-D
@ARG
M=D")

let handlePopLocal(x)=
    output.WriteLine("@" + x + "
D=A
@LCL
A=M
D=D+A
@LCL
M=D
@SP
M=M-1
A=M
D=M
@LCL
A=M
M=D
@" + x + "
D=A
@LCL
A=M
D=A-D
@LCL
M=D")

let handlePopPointer0()=
    output.WriteLine("@SP
AM=M-1
D=M
@THIS
M=D")

let handlePopPointer1()=
    output.WriteLine("@SP
AM=M-1
D=M
@THAT
M=D")

let handlePopTemp(x)=
    let offset = System.Int32.Parse(x)
    let newX = offset + 5
    let stringX = newX.ToString()
    output.WriteLine("AM=M-1
D=M
@" + stringX + "
M=D")

let GetFileName(filepath : string) = Path.GetFileNameWithoutExtension(filepath)
    

[<EntryPoint>]
let main argv =
    printfn "Please enter the path: "
    let path = Console.ReadLine()
    let files = Directory.GetFiles(path)
    initialSetPointers()
    for file in files do
        use reader =new StreamReader(file)
        let className = GetFileName(file)
        let mutable line = ""
        counter <- 0
        while not (reader.EndOfStream) do
            line <- reader.ReadLine()
            let words = line.Split([|' '; '\t'|], StringSplitOptions.RemoveEmptyEntries)
            if words.Length > 0 then
                match words.[0] with
                | "add" -> handleAdd() 
                | "sub" -> handleSub()
                | "neg" -> handleNeg()
                | "eq" -> handleEq()
                | "gt" -> handleGt()
                | "lt" -> handleLt()
                | "push" -> 
                    match words.[1] with
                    |"constant" -> handlePushConstant(words.[2])
                    |"static" -> handlePushStatic(className, words.[2])
                    |"this" -> handlePushThis(words.[2])
                    |"that" -> handlePushThat(words.[2])
                    |"local" -> handlePushLocal(words.[2])
                    |"argument" -> handlePushArg(words.[2])
                    |"temp" -> handlePushTemp(words.[2])
                    |"pointer" ->
                        match words.[2] with
                        |"0" -> handlePushPointer0()
                        |"1" -> handlePushPointer1()
                | "pop" -> 
                    match words.[1] with
                    |"static" -> handlePopStatic(className, words.[2])
                    |"this" -> handlePopThis(words.[2])
                    |"that" -> handlePopThat(words.[2])
                    |"argument" -> handlePopArg(words.[2])
                    |"local" -> handlePopLocal(words.[2])
                    |"temp" -> handlePopTemp(words.[2])
                    |"pointer" ->
                        match words.[2] with
                        |"0" -> handlePopPointer0()
                        |"1" -> handlePopPointer1()
                | "and" -> handleAnd()
                | "or" -> handleOr()
                | "not" -> handleNot()
        let currentFileName = file
        printf "End of input file: %s\n" currentFileName 
    output.Flush() // Flush the buffer to ensure all data is written to the file
    output.Close() // Close the StreamWriter to release resources
    printf "Output file is ready: Tar0.asm"

    0
