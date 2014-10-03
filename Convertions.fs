module Convertions
open System
open System.Text.RegularExpressions



type System.String with

    member this.ChunksOf2 = 
        let matches = Regex.Matches(this,"(..)")
        [| for x in matches -> x.Groups.[1].Value |]

    member this.ChunksOf8 = 
        let matches = Regex.Matches(this,"(........)")
        [| for x in matches -> x.Groups.[1].Value |]



////////////////////////////////////////////////////////////////// CONVERTIONS /////////////////////////////////////////////////////////
//ByteToX
let private ByteToHex (b : byte) = 
    sprintf "%02x" b

let private ByteToInt (b : byte) = 
    (int) b

let private ByteToChar(b : byte) = 
    (char) b

let private ByteToBin (b : byte) = 
    Convert.ToString(b,2).PadLeft(8,'0')
    
//XToByte
let private HexToByte (s : string) = 
    let suc, res = Byte.TryParse(s,Globalization.NumberStyles.HexNumber, System.Globalization.CultureInfo.GetCultureInfo(1))
    res

let private IntToByte (i : int) = 
    (byte) i

let private BinToByte (s : string) = 
    Convert.ToByte(s,2)

let private CharToByte (c : char) = 
    (byte) c


//HexToX
let private HexToInt   = HexToByte >> ByteToInt 
let private HexToBin   = HexToByte >> ByteToBin 
let private HexToChar  = HexToByte >> ByteToChar 

//IntToX
let private IntToHex   = IntToByte >> ByteToHex 
let private IntToBin   = IntToByte >> ByteToBin 
let private IntToChar  = IntToByte >> ByteToChar 

//CharToX
let private CharToBin  = CharToByte >> ByteToBin 
let private CharToHex  = CharToByte >> ByteToHex 
let private CharToInt  = CharToByte >> ByteToInt 

//BinToX
let private BinToHex   = BinToByte >> ByteToHex 
let private BinToInt   = BinToByte >> ByteToInt 
let private BinToChar  = BinToByte >> ByteToChar 

//ByteArrToX
let private ByteArrToBase64Str (b : byte[]) = 
    Convert.ToBase64String b

let private ByteArrToHexStr (b : byte[]) =
    Array.fold(fun acc elem -> acc + (ByteToHex elem) ) "" b

let private ByteArrToBinStr (b : byte[]) =
    Array.fold(fun acc elem -> acc + (ByteToBin elem) ) "" b

let private ByteArrToCharStr (b : byte[]) =
    Array.fold(fun acc elem -> acc + (ByteToChar elem).ToString() ) "" b
//StrToByteArr
let private Base64StrToByteArr (s : string) = 
    Convert.FromBase64String s

let private HexStrToByteArr (s : string) = 
    s.ChunksOf2
    |> Array.map HexToByte

let private BinStrToByteArr (s : string) = 
    s.ChunksOf8
    |> Array.map BinToByte

let private CharStrToByteArr (s : string ) =
    s
    |> Seq.map CharToByte
    |> Array.ofSeq

//HexStringToXStrin
let private HexStrToBinStr   = HexStrToByteArr >> ByteArrToBinStr 
let private HexStrToCharStr       = HexStrToByteArr >> ByteArrToCharStr 
let private HexStrToBase64Str = HexStrToByteArr >> ByteArrToBase64Str 

//BinStringToXString
let private BinStrToHexStr   = BinStrToByteArr >> ByteArrToHexStr 
let private BinStrToCharStr      = BinStrToByteArr >> ByteArrToCharStr 
let private BinStrToBase64Str = BinStrToByteArr >> ByteArrToBase64Str 

//Base64StrToXString
let private Base64StrToHexStr  = Base64StrToByteArr >> ByteArrToHexStr 
let private Base64StrToBinStr  = Base64StrToByteArr >> ByteArrToBinStr
let private Base64StrToCharStr    = Base64StrToByteArr >> ByteArrToCharStr

//CharStrToXString
let private CharStrToHexStr  = CharStrToByteArr >> ByteArrToHexStr 
let private CharStrToBinCharStr    = CharStrToByteArr >> ByteArrToBinStr 
let private CharStrToBase64Str  = CharStrToByteArr >> ByteArrToBase64Str 

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

type System.String with 
    member this.toBytes = 
        CharStrToByteArr this

module String =
    let toBytes (s : string) = s.toBytes

module Hex =

    type _T = Hex of string with

        member this.value  = 
            let stringof (Hex str) = str
            stringof this

        member this.toBytes = 
            this.value |> HexStrToByteArr

        member this.print = 
            printfn "%s" this.value



    let create (s : string) = 
        let hexchars =  ['a';'b';'c';'d';'e';'f';'0';'1';'2';'3';'4';'5';'6';'7';'8';'9']
        let ishex = s |> Seq.forall (fun char -> List.exists (fun x -> x = char) hexchars)
        if not ishex || s.Length % 2 = 1
        then failwith "Provided string is not valid hex"
        else Hex s

    let createOption (s : string) = 
        try 
            Some (create s)
        with 
            | exn -> None

    let value (hex : _T) = hex.value

    
    let fromInt i = 
        Hex (IntToHex i)

    let fromByte i =
        Hex (ByteToHex i)

    let toBytes (hex : _T) = hex.toBytes

    let fromBytes (b : byte[] ) = 
        Hex <| ByteArrToHexStr b
        
        


module Binary = 

    type _T = Binary of string with 
        
        member this.value  = 
            let stringof (Binary str) = str
            stringof this

        member this.toBytes = 
            this.value |> BinStrToByteArr

        member this.print = 
            printfn "%s" this.value

    let create (s : string) = 
        let isbin = s |> Seq.forall (fun char -> char = '0' || char = '1')
        if not isbin || s.Length % 8 <> 0
        then failwith "Provided string is not valid binary"
        else Binary s

    let createOption (s : string) = 
        try 
            Some (create s)
        with 
            | exn -> None

    let value (bin : _T) = bin.value

    let toBytes (bin : _T) = bin.toBytes

    let fromInt i = 
        Binary (IntToBin i)

    let fromByte i =
        Binary (ByteToBin i)

    let fromBytes (b : byte[] ) = 
        Binary <| ByteArrToBinStr b

module Base64 = 


    type _T = Base64 of string with 
        
        member this.value  = 
            let stringof (Base64 str) = str
            stringof this

        member this.toBytes = 
            this.value |> Base64StrToByteArr

        member this.print = 
            printfn "%s" this.value
    // some recognizion logic
    let create (s : string) = 
        Base64 s
    // redundant for now
    let createOption (s : string) = 
        try 
            Some (create s)
        with 
            | exn -> None

    let value (str : _T) = str.value

    let toBytes (str : _T) = str.toBytes

    let fromBytes (b : byte[] ) = 
        Base64 <| ByteArrToBase64Str b


module Array = 

    let toBinary (b : byte[]) = 
           ByteArrToBinStr b 

    let toHex (b : byte[]) = 
           ByteArrToHexStr b 

    let toString (b : byte[]) = 
           ByteArrToCharStr b 

    let toBase64 (b : byte[]) = 
           ByteArrToBase64Str b 
    
