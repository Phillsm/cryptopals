// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

(*
#r @"C:\Users\Phill\Dropbox\crypto\matasano\Matasano Crypto Challenges\packages\FSharp.Data.2.0.14\lib\net40\FSharp.Data.dll"
#load @"C:\Users\Phill\Dropbox\crypto\matasano\Matasano Crypto Challenges\Matasano Crypto Challenges\Convertions.fs"
#load @"C:\Users\Phill\Dropbox\crypto\matasano\Matasano Crypto Challenges\Matasano Crypto Challenges\MyCollectionLib.fs"
*)
open System
open System.Text.RegularExpressions
open System.Linq
open FSharp.Data
open System.Text
open System.Security.Cryptography
open System.IO
open MyCollectionLib
open Convertions




/////////////////////////////////////////////
///                                       ///          
///                 SET 1                 ///
///                                       ///      
/////////////////////////////////////////////


/////////////// CHALLENGE 1 /////////////////
//Convert hex to base64
//The string:
//
//49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d
//Should produce:
//
//SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t
/////////////////////////////////////////////

let testSet1Chal1 = 
    let testhex = Hex.toBytes <| Hex.create "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    let product = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

    testhex
    |> Base64.fromBytes
    |> Base64.value  = product // true
        


/////////////// CHALLENGE 2  /////////////////
//Write a function that takes two equal-length buffers and produces their XOR combination.
//
//If your function works properly, then when you feed it the string:
//
//1c0111001f010100061a024b53535009181c
//... after hex decoding, and when XOR'd against:
//
//686974207468652062756c6c277320657965
//... should produce:
//
//746865206b696420646f6e277420706c6179
///////////////////////////////////////////////

let xorArrays (b1 : byte[]) (b2 : byte[]) = 
    Array.zip b1 b2
    |> Array.map(fun (b1,b2) -> b1 ^^^ b2)    

let testSet1Chal2 = 
    let s1 = Hex.toBytes <| Hex.create "1c0111001f010100061a024b53535009181c"
    let s2 = Hex.toBytes <| Hex.create "686974207468652062756c6c277320657965"
    let result = Hex.toBytes <| Hex.create "746865206b696420646f6e277420706c6179"

    let xord = xorArrays s1 s2

    xord = result


/////////////// CHALLENGE 3  /////////////////
//The hex encoded string:
//
//1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736
//... has been XOR'd against a single character. Find the key, decrypt the message.
//
//You can do this by hand. But don't: write code to do it for you.
//
//How? Devise some method for "scoring" a piece of English plaintext. Character frequency is a good metric. Evaluate each output and choose the one with the best score.
//
//////////////////////////////////////////////

let chal3cypher = Hex.toBytes <| Hex.create "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

let XorWithSingleChar (array : byte[]) (c : char) = 
    let chararray = [|for x in array -> (byte c)|]
    xorArrays array chararray


let FrequencyTable = [|('a',8.2);('b',1.5);('c',2.8);('d',4.3);('e',12.7);('f',2.2);('g',2.0);('h',6.1);('i',7.0);
                       ('j',0.2);('k',0.8);('l',4.0);('m',2.4);('n',6.7);('o',7.5);('p',1.9);('q',0.1);('r',6.0);
                       ('s',6.3);('t',9.1);('q',2.8);('v',1.0);('w',2.4);('x',0.2);('y',2.0);('z',0.1);(' ',12.8)|] 
                       |> Map.ofArray

let frequencyscore (b : byte) =
    let char = (char) b
    let isEnglishLetter (c : char) = 
        try
            Map.containsKey (Char.ToLower c) FrequencyTable
        with
            exn -> false

    if isEnglishLetter char
    then FrequencyTable.Item (Char.ToLower char) 
    else -12.0          
    
    
let BruteForceCaesar (b : byte[]) = 
      [ for c in 0..255 -> char c]
      |> List.fold (fun (accmap : Map<char,byte[]>) elemchar -> accmap.Add(elemchar,(XorWithSingleChar b elemchar)) ) Map.empty
      |> Map.map (fun key value -> (value , value |> Seq.fold(fun acc elem -> acc + frequencyscore elem ) 0.0 ) ) 
      |> Map.fold(fun (out,max,char) key (value, score) -> if score > max then (value,score,key) else (out,max,char)) (Array.empty<byte>, 0.0, 'c')



let testSet1Chal3 = BruteForceCaesar chal3cypher |> (fun (x,y,z) -> (Array.toString x,y,z))


/////////////// CHALLANGE 4  /////////////////
//One of the 60-character strings in this file has been encrypted by single-character XOR.
//
//Find it.
//
//(Your code from #3 should help.)
/////////////////////////////////////////////
let file = Http.RequestString("""http://cryptopals.com/static/challenge-data/4.txt""")
let lines = file.Split [|'\n'|]

let testSet1Chal4 =
    lines
    |> Array.map (Hex.create >> Hex.toBytes)
    |> Array.Parallel.map BruteForceCaesar
    |> Array.maxBy (fun (string, score, key) -> score)
    |> (fun (x,y,z) -> (Array.toString x,y,z))



/////////////// CHALLANGE 5 /////////////////
//Here is the opening stanza of an important work of the English language:
//
//Burning 'em, if you ain't quick and nimble
//I go crazy when I hear a cymbal
//Encrypt it, under the key "ICE", using repeating-key XOR.
//
//In repeating-key XOR, you'll sequentially apply each byte of the key; the first byte of plaintext will be XOR'd against I, the next C, the next E, then I again for the 4th byte, and so on.
//
//It should come out to:
//
//0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272
//a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f
///////////////////////////////////////////

let plaintext = "Burning 'em, if you ain't quick and nimble
I go crazy when I hear a cymbal" |> String.toBytes
let key = "ICE" |> String.toBytes

let expected = Hex.toBytes <| Hex.create "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"



//key repeated to fit plaintext
let repeat (array : byte[]) ( i : int) =
    let repeatinner items = 
        seq { while true do yield! items }
    (repeatinner array)
    |> Seq.take i

let xorPlainTextWithRepeatKey (plaintext : byte[]) ( key : byte[]) = 
    Seq.zip plaintext (repeat key plaintext.Length)
    |> Seq.map (fun (plain,key) -> plain ^^^ key )
    |> Array.ofSeq
    
let testSet1Chal5 = xorPlainTextWithRepeatKey plaintext key = expected


/////////////// CHALLENGE 6 /////////////////
//There's a file here. It's been base64'd after being encrypted with repeating-key XOR.
//
//Decrypt it.
//
//Here's how:
//
//Let KEYSIZE be the guessed length of the key; try values from 2 to (say) 40.
//Write a function to compute the edit distance/Hamming distance between two strings. The Hamming distance is just the number of differing bits. The distance between:
//this is a test
//and
//wokka wokka!!!
//is 37. Make sure your code agrees before you proceed.
//For each KEYSIZE, take the first KEYSIZE worth of bytes, and the second KEYSIZE worth of bytes, and find the edit distance between them. Normalize this result by dividing by KEYSIZE.
//The KEYSIZE with the smallest normalized edit distance is probably the key. You could proceed perhaps with the smallest 2-3 KEYSIZE values. Or take 4 KEYSIZE blocks instead of 2 and average the distances.
//Now that you probably know the KEYSIZE: break the ciphertext into blocks of KEYSIZE length.
//Now transpose the blocks: make a block that is the first byte of every block, and a block that is the second byte of every block, and so on.
//Solve each block as if it was single-character XOR. You already have code to do this.
//For each block, the single-byte XOR key that produces the best looking histogram is the repeating-key XOR key byte for that block. Put them together and you have the key.
//////////////////////////////////////////////
let chal6CypherString = Http.RequestString("""http://cryptopals.com/static/challenge-data/6.txt""") |> Base64.create |> Base64.toBytes

let keysizes = [2..40]

let hamming (arr1 : byte[]) (arr2 : byte[]) = 
    Array.zip arr1 arr2
    |> Array.map (fun (fst,snd) -> fst ^^^ snd |> (Binary.fromByte >> Binary.value) )
    |> Array.fold (fun acc str -> str.Count (fun x -> x = '1') + acc ) 0

let hammingtest = hamming ("this is a test" |> String.toBytes) ("wokka wokka!!!" |> String.toBytes) = 37

let splitEvery (n : int) s =
    seq { let r = ResizeArray<_>(n)
          for x in s do
              r.Add(x)
              if r.Count = n then
                  yield r.ToArray() |> Seq.ofArray
                  r.Clear()
          if r.Count <> 0 then
              yield r.ToArray() |> Seq.ofArray }

let getDifferenceList (input : byte[]) = 
    let keysizes = [2..40]
    [ for keysize in keysizes do
        let chunks =
            (splitEvery keysize input) |> Seq.pairwise
        let hammedchunks =
            chunks
            |> Seq.take ((chunks |> Seq.length) - 1)
            |> Seq.fold (fun acc (first,second) -> acc + (hamming (Array.ofSeq first) (Array.ofSeq second))) 0

        let difference = (float) hammedchunks / (float) (chunks |> Seq.length)
        yield (keysize,((float)difference / (float)keysize))]
    |> List.sortBy snd

let makeUniform matrix = 
    if (Seq.head matrix |> Seq.length) > (Seq.last matrix |> Seq.length)
    then Seq.take ((Seq.length matrix) - 1) matrix
    else matrix

let transpose (matrix : seq<seq<'a>>) = 
    let length = (matrix |> Seq.head) |> Seq.length

    seq {for x in 0..length-1 ->
             seq {for innerpart in matrix ->
                      innerpart |> Seq.nth x}}

let solveTransposedMatrix (input : byte[]) = 
    [for currentkeysize in Seq.take 3 (getDifferenceList input) ->  
        splitEvery (fst currentkeysize) input
        |> makeUniform
        |> transpose
        |> Seq.map (fun elem -> BruteForceCaesar <| Array.ofSeq elem )
        |> Seq.fold (fun acc (str,score,key) -> acc + key.ToString() ) "" ]


let testSet1Chal6 = 
    let key = 
        chal6CypherString
        |> solveTransposedMatrix
        |> List.head
        |> String.toBytes

    xorPlainTextWithRepeatKey chal6CypherString key
    |> Array.toString
/////////////// CHALLENGE 7 /////////////////
//AES in ECB mode
//The Base64-encoded content in this file has been encrypted via AES-128 in ECB mode under the key
//
//"YELLOW SUBMARINE".
//(case-sensitive, without the quotes; exactly 16 characters; I like "YELLOW SUBMARINE" because it's exactly 16 bytes long, and now you do too).
//
//Decrypt it. You know the key, after all.
//
//Easiest way: use OpenSSL::Cipher and give it AES-128-ECB as the cipher.
////////////////////////////////////////////
let chal7input = Http.RequestString("""http://cryptopals.com/static/challenge-data/7.txt""") |> Base64.create |> Base64.toBytes


let decryptAesEcb (key : byte[]) (input : byte[])  = 
    let aes = AesManaged.Create()
    aes.Key <- key 
    aes.Mode <- CipherMode.ECB
    aes.BlockSize <- 128
    let decryptor = aes.CreateDecryptor()
    use msDecrypt = new MemoryStream(input)
    use csDecrypt = new CryptoStream(msDecrypt,decryptor,CryptoStreamMode.Read)
    use srDecrypt = new StreamReader(csDecrypt)
    srDecrypt.ReadToEnd()
    |> String.toBytes;;



let chal7output = 
    chal7input 
    |> decryptAesEcb (String.toBytes "YELLOW SUBMARINE")
    |> Array.toString

/////////////// CHALLENGE 8 /////////////////
//Detect AES in ECB mode
//In this file are a bunch of hex-encoded ciphertexts.
//
//One of them has been encrypted with ECB.
//
//Detect it.
//
//Remember that the problem with ECB is that it is stateless and deterministic; the same 16 byte plaintext block will always produce the same 16 byte ciphertext.
/////////////////////////////////////////////
let chal8input = Http.RequestString("""http://cryptopals.com/static/challenge-data/8.txt""").Split[|'\n'|] |> Array.map (Hex.create >> Hex.toBytes)


let countduplicates (input) =
    input
    |> Seq.fold (fun (acc: Map<_,_>) item -> if acc.ContainsKey item then acc.Add(item,((acc.Item item) + 1 )) else acc.Add(item,0) )  Map.empty
    |> Map.fold (fun count item duplicates -> duplicates + count ) 0

let detectecb (b : byte[]) = 
    let count = countduplicates ((splitEvery 16 b) |> Seq.map (Seq.toArray))
    if count > 0 then true else false

let chal8output = 
    chal8input
    |> Array.filter detectecb

/////////////////////////////////////////////
///                                       ///          
///                 SET 2                ///
///                                       ///      
/////////////////////////////////////////////

/////////////// CHALLENGE 9 /////////////////
//Implement PKCS#7 padding
//A block cipher transforms a fixed-sized block (usually 8 or 16 bytes) of plaintext into ciphertext. But we almost never want to transform a single block; we encrypt irregularly-sized messages.
//
//One way we account for irregularly-sized messages is by padding, creating a plaintext that is an even multiple of the blocksize. The most popular padding scheme is called PKCS#7.
//
//So: pad any block to a specific block length, by appending the number of bytes of padding to the end of the block. For instance,
//
//"YELLOW SUBMARINE"
//... padded to 20 bytes would be:
//
//"YELLOW SUBMARINE\x04\x04\x04\x04"
////////////////////////////////////////////
type System.String with
    /// <summary>
    /// pads the string using PKCS#7
    /// </summary>
    /// <param name="i">the length the string should be padded to</param>
    member this.pad (i : int) = 
        let paddistance = i - this.Length
        if paddistance < 1 
        then this
        else 
            let padstring = seq { for c in 1..paddistance -> (char) paddistance }
            sprintf "%s%s" this (String.Concat padstring)

let pad (i : int) (b:byte[]) =  
    let paddistance = i - b.Length
    if paddistance < 1
    then b
    else
        let padding = [| for c in 1..paddistance -> (byte) paddistance |]
        Array.append b padding
    
let autopad (b : byte[]) = 
    if b.Length % 16 = 0
    then pad (b.Length + 16) b
    else 
        let mutable loopnum = b.Length
        let nearestmult = 
            while loopnum % 16 <> 0 do
                loopnum <- loopnum + 1
        pad loopnum b



let testSet2Chal9 = 
    ("YELLOW SUBMARINE".pad 20).Length = 20
    && ((pad 20 ("YELLOW SUBMARINE" |> String.toBytes)) |> Array.toString).ToString().Length = 20


/////////////// CHALLENGE 10 /////////////////
//Implement CBC mode
//CBC mode is a block cipher mode that allows us to encrypt irregularly-sized messages,
// despite the fact that a block cipher natively only transforms individual blocks.
//
//In CBC mode, each ciphertext block is added to the next plaintext block before the next call to the cipher core.
//
//The first plaintext block, which has no associated previous ciphertext block,
//is added to a "fake 0th ciphertext block" called the initialization vector, or IV.
//
//Implement CBC mode by hand by taking the ECB function you wrote earlier,
//making it encrypt instead of decrypt (verify this by decrypting whatever you encrypt to test),
//and using your XOR function from the previous exercise to combine them.
//
//The file here is intelligible (somewhat) when CBC decrypted against "YELLOW SUBMARINE" with an IV of all ASCII 0 (\x00\x00\x00 &c)
///////////////////////////////////////////
let chal10input = File.ReadAllBytes("""C:\Users\Phill\Dropbox\crypto\matasano\Matasano Crypto Challenges\Matasano Crypto Challenges\10.txt""")

let encryptAesEcbNoPadding(key : byte[]) (input : byte[])  = 
    let aes = AesManaged.Create()
    aes.Key <- key 
    aes.Mode <- CipherMode.ECB
    aes.BlockSize <- 128
    aes.Padding <- PaddingMode.None
    let encryptor = aes.CreateEncryptor()
    use msEncrypt = new MemoryStream()
    (
        use csEncrypt = new CryptoStream(msEncrypt,encryptor,CryptoStreamMode.Write)
        csEncrypt.Write(input,0,input.Length)
    )
    msEncrypt.ToArray()

let decryptAesEcbNoPadding (key : byte[]) (input : byte[])  = 
    let aes = AesManaged.Create()
    aes.Key <- key 
    aes.Mode <- CipherMode.ECB
    aes.BlockSize <- 128
    aes.Padding <- PaddingMode.None
    let decryptor = aes.CreateDecryptor()
    use msDecrypt = new MemoryStream(input)
    use csDecrypt = new CryptoStream(msDecrypt,decryptor,CryptoStreamMode.Read)
    csDecrypt.Read(input,0,input.Length) |> ignore
    msDecrypt.ToArray()

let midtest = 
    let string = ("SUBMARINE YELLOW" |> String.toBytes)
    let key = "YELLOW SUBMARINE" |> String.toBytes
    let ecb = encryptAesEcbNoPadding key string
    let decrypt = decryptAesEcbNoPadding key ecb
    decrypt |> Array.toString

let midtest2 = 
    let string = autopad ( "SUBMARINE YELLOW Yo Giga puddi testest" |> String.toBytes)
    let key = "YELLOW SUBMARINE" |> String.toBytes
    let ecb = encryptAesEcbNoPadding key string
    let decrypt = decryptAesEcbNoPadding key ecb
    decrypt |> Array.toString

let myCBCencryptor (key : byte[]) (input : byte[]) = 
    let IV = [| for x in 1..16 -> 0uy|]
    let paddedAndBlocked = Array.chunksOf 16 (autopad input)
    let previous = ref IV
    [| yield IV;
      for x in 0..(paddedAndBlocked.Length - 1) do
        let plaintext = xorArrays !previous paddedAndBlocked.[x]
        let encrypted = encryptAesEcbNoPadding key plaintext
        previous := encrypted
        yield encrypted |]
    |> Array.concat

let myCBCdecrypter (key : byte[]) (input : byte[]) =
    let paddedAndBlocked = Array.chunksOf 16 (input)
    let IV = paddedAndBlocked.[0]
    let previous = ref IV
    [| for x in 1..(paddedAndBlocked.Length - 1) do    
        let decrypted = decryptAesEcbNoPadding key paddedAndBlocked.[x]
        let plaintext = xorArrays decrypted !previous
        previous := paddedAndBlocked.[x]
        yield plaintext |]
    |> Array.concat

let testmyCBC =
    let encrypted = ("SUBMARINE YELLOW and more" |> String.toBytes) |> myCBCencryptor ("YELLOW SUBMARINE" |> String.toBytes) 
    let decrrypted = myCBCdecrypter ("YELLOW SUBMARINE" |> String.toBytes) encrypted
    decrrypted |> Array.toString

let test = 
    chal10input
    |> myCBCencryptor ("YELLOW SUBMARINE" |> String.toBytes) 
    |> Array.toString

let testCBC (key : byte[]) (input : byte[])  = 
    let aes = AesManaged.Create()
    aes.Key <- key 
    aes.Mode <- CipherMode.CBC
    aes.BlockSize <- 128
    aes.Padding <- PaddingMode.None
    aes.IV <- [| for x in 1..16 -> 0uy|]
    let encryptor = aes.CreateEncryptor()
    use msEncrypt = new MemoryStream()
    (
        use csEncrypt = new CryptoStream(msEncrypt,encryptor,CryptoStreamMode.Write)
        csEncrypt.Write(input,0,input.Length)
    )
    msEncrypt.ToArray()

let test123 = 
    chal10input
    |> myCBCencryptor ("YELLOW SUBMARINE" |> String.toBytes) 
    |> Array.toString

let test2 =
    chal10input
    |> testCBC ("YELLOW SUBMARINE" |> String.toBytes)
    |> Array.toString


[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code​
