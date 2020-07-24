#r "##FSHOME##\packages\FSharp.Data\lib\net45\FSharp.Data.dll"
#r "##FSHOME##\packages\FSharp.Data\lib\net45\FSharp.Data.DesignTime.dll"
#r @"System.Xml.Linq"

open System.Xml
open FSharp.Data

let inline (<==) a b = 
    (a :> XmlNode).AppendChild b |> ignore
    a

let inline (<--) a b =
    (a :> XmlElement).SetAttributeNode b |> ignore
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

let requestElem     = createAlea "Request"
let dimensionElem   = createAlea "Dimension"
let descriptionElem = createAlea "Description"
let elementsElem    = createAlea "Elements"

let requestId id = 
    let idAtt = doc.CreateAttribute "RequestID"
    idAtt.Value <- id
    idAtt

let requestClass cls =
    let classAtt = doc.CreateAttribute "Class"
    classAtt.Value <- cls
    classAtt

let requestMethod mtd =
    let methodAtt = doc.CreateAttribute "Method"
    methodAtt.Value <- mtd
    methodAtt

let dimName name =
    let nameAtt = doc.CreateAttribute "Name"
    nameAtt.Value <- name
    nameAtt

let dimFirstBatch fb =
    let fbAtt = doc.CreateAttribute "FirstBatch"
    fbAtt.Value <- fb
    fbAtt

let dimLastBatch lb =
    let lbAtt = doc.CreateAttribute "LastBatch"
    lbAtt.Value <- lb
    lbAtt

requestElem   <-- requestId "001"
requestElem   <-- requestClass "Dimension"
requestElem   <-- requestMethod "Create"

dimensionElem <-- dimName "prueba"
dimensionElem <-- dimFirstBatch "true"
dimensionElem <-- dimLastBatch "true"

let descriptionText text = descriptionElem <== doc.CreateTextNode text

descriptionText "prueba"

let insertElement elementType elementName = 
    let element = elementType + "\t" + elementName + "\n"
    elementsElem <== doc.CreateTextNode element

let MyElement1 = insertElement "N" "MyElement1"
let MyElement2 = insertElement "N" "MyElement2"
let MyElement3 = insertElement "S" "MyElement3"

let insertElementWithChild elementParentType elementParentName elementChildName =
    let element = elementParentType + "\t" + elementParentName + "\n" + "\t" + elementChildName + "\t-1"
    elementsElem <== doc.CreateTextNode element

let MyElement4 = insertElementWithChild "N" "MyElement4" "MyElement5"

let dimensionCreate =
    createAlea "Document"
    <==(requestElem
        <==(dimensionElem
            <==(descriptionElem)
            <==(elementsElem)))

let dimCreateRequest =
    let hdr = createXMLA "Header"
    let bdy = 
        createXMLA "Command"
        <== dimensionCreate
        |> execute 
    envelope hdr bdy

let request (cnt:string) =
        try
            let txt = cnt.Replace("\n\r","\n").Replace("\r\n","\n").Replace("\r","\n")
            Http.RequestString(url, body = TextRequest txt )
        with e -> 
            printfn "%A" e.Message
            ""

// beginSession.OuterXml
// |> request 
// |> printfn "%s"

dimCreateRequest.OuterXml
|> request
|> printfn "%s"
