#r "##FSHOME##\packages\FSharp.Data\lib\net45\FSharp.Data.dll"
#r "##FSHOME##\packages\FSharp.Data\lib\net45\FSharp.Data.DesignTime.dll"
#r @"System.Xml.Linq"

open System.Xml
open FSharp.Data

let inline (<==) a b = 
    (a:>XmlNode).AppendChild b |> ignore
    a

let url  = "http://localhost:8212"


let doc         = new XmlDocument()
let SoapUri     = "http://www.w3.org/2003/05/soap-envelope/"
let SecurityUri = "http://schemas.xmlsoap.org/ws/2002/04/secext"
let XMLAUri     = "urn:schemas-microsoft-com:xml-analysis"
let AleaUri     = "http://www.misag.com"

let createSoap     elem = doc.CreateElement("soap", elem,     SoapUri)
let createSecurity elem = doc.CreateElement("wsse", elem, SecurityUri)
let createXMLA     elem = doc.CreateElement("xmla", elem,     XMLAUri)
let createAlea     elem = doc.CreateElement("Alea", elem,     AleaUri)

let security user pwd   = 
    createSecurity "Security"
    <==(createSecurity "UsernameToken"
        <==(createSecurity "Username"
            <== doc.CreateTextNode user)
        <==(createSecurity "Password"
            <== doc.CreateTextNode pwd))

let envelope hdr body   = 
    createSoap "Envelope"
    <==(createSoap "Header"
        <== hdr
        <== security "Admin" "" )
    <==(createSoap "Body"
        <== body)

let properties = 
    createXMLA "Properties"

let execute cmd = 
    createXMLA "Execute"
    <== cmd
    <== properties

let beginSession =
    let hdr = createXMLA "BeginSession"
    let bdy = 
        createXMLA "Command"
        <== createXMLA "Statement"
        |> execute 
    envelope hdr bdy

let request (cnt:string) =
        try
            let txt = cnt.Replace("\n\r","\n").Replace("\r\n","\n").Replace("\r","\n")
            Http.RequestString(url, body = TextRequest txt )
        with e -> 
            printfn "%A" e.Message
            ""

beginSession.OuterXml
|> request 
|> printfn "%s"
