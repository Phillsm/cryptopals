module Convertions

module Hex =
    type _T
    val create : string -> _T
    val createOption : string -> _T option
    val toBytes : _T -> byte[]
    val value  : _T -> string
    val fromBytes : byte[] -> _T
    val fromByte : byte -> _T
    val fromInt : int -> _T

module Binary =
    type _T
    val create : string -> _T
    val createOption : string -> _T option
    val toBytes : _T -> byte[]
    val value  : _T -> string
    val fromBytes : byte[] -> _T
    val fromByte : byte -> _T
    val fromInt : int -> _T

module Base64 =
    type _T
    val create : string -> _T
    val createOption : string -> _T option
    val toBytes : _T -> byte[]
    val value  : _T -> string
    val fromBytes : byte[] -> _T

module Array = 
    val toBinary : byte[] -> string
    val toHex : byte[] -> string
    val toBase64 : byte[] -> string
    val toString : byte[] -> string

module String = 
    val toBytes : string -> byte[]
