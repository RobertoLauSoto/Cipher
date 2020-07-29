#r "##FSHOME##\packages\FSharp.Data\lib\net45\FSharp.Data.dll"
#r "##FSHOME##\packages\FSharp.Data\lib\net45\FSharp.Data.DesignTime.dll"
#r @"System.Xml.Linq"

module Prueba = 
    open System.Xml
    open FSharp.Data

    let inline (<==) a b = 
        (a :> XmlNode).AppendChild b |> ignore
        a

    let inline (<--) a b =
        (a :> XmlElement).SetAttributeNode b |> ignore
        a

    let inline (<*=) a b = a <== (b : XmlNode).CloneNode true
    let inline (<*-) a b = a <-- ((b : XmlAttribute).Clone() :?> XmlAttribute)  

    let outerXml (e:XmlElement) = e.OuterXml

    type openSessionResp = XmlProvider<Sample="""<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema"><soap:Header><Session SessionId="23c4-4-1140" xmlns="urn:schemas-microsoft-com:xml-analysis"/></soap:Header><soap:Body><ExecuteResponse xmlns="urn:schemas-microsoft-com:xml-analysis"><return><root xmlns="urn:schemas-microsoft-com:xml-analysis:empty" /></return></ExecuteResponse></soap:Body></soap:Envelope>""">
    type aleaResp        = XmlProvider<Sample="""<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema"><soap:Header><Session SessionId="7fb6-7-1170" xmlns="urn:schemas-microsoft-com:xml-analysis"/></soap:Header><soap:Body><ExecuteResponse xmlns="urn:schemas-microsoft-com:xml-analysis"><return><root xmlns="urn:schemas-microsoft-com:xml-analysis:mddataset"><Alea:DocumentWrapper xmlns:Alea="http://www.misag.com">&lt;Alea:Document xmlns:Alea=&quot;http://www.misag.com&quot;&gt;&lt;Alea:Request RequestID=&quot;17&quot;&gt;&lt;Alea:Return&gt;&lt;Alea:Properties&gt;&lt;Alea:Events Generate=&quot;true&quot;/&gt;&lt;Alea:Splashing/&gt;&lt;/Alea:Properties&gt;&lt;/Alea:Return&gt;&lt;/Alea:Request&gt;&lt;/Alea:Document&gt;</Alea:DocumentWrapper></root></return></ExecuteResponse></soap:Body></soap:Envelope>""">
    type aleaProperties  = XmlProvider<Sample="""<Alea:Document xmlns:Alea="http://www.misag.com"><Alea:Request RequestID="17"><Alea:Return><Alea:Properties><Alea:Events Generate="true"/><Alea:Splashing/></Alea:Properties></Alea:Return></Alea:Request></Alea:Document>""">



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

    let createAttribute att v =
        let idAtt = doc.CreateAttribute att
        idAtt.Value <- v
        idAtt

    let security user pwd   = 
        createSecurity "Security"
        <==(createSecurity "UsernameToken"
            <==(createSecurity "Username"
                <== doc.CreateTextNode user)
            <==(createSecurity "Password"
                <== doc.CreateTextNode pwd))

    let envelope hdrs body   =
        let hdr = createSoap "Header"
        for (h : XmlElement) in hdrs do 
            hdr <*= h
            |> ignore
        createSoap "Envelope"
        <== hdr
        <==(createSoap "Body"
            <*= body)

    let properties = 
        createXMLA "Properties"

    let execute cmd = 
        createXMLA "Execute"
        <== (createXMLA "Command" <*= cmd)
        <== properties

    let beginSession =    
        createXMLA "Statement"
        |> execute 
        |> envelope [ createXMLA "BeginSession" ; security "Admin" "" ]

    let sessionId     = createAttribute "SessionID"
    let requestId     = createAttribute "RequestID"
    let requestClass  = createAttribute "Class"
    let requestMethod = createAttribute "Method"
    let dimName       = createAttribute "Name"
    let dimFirstBatch = createAttribute "FirstBatch"
    let dimLastBatch  = createAttribute "LastBatch"

    let createSession sid = createXMLA "Session" <-- sessionId sid

    let endSession sid =
        createXMLA "Statement"
        |> execute 
        |> envelope [ createXMLA "EndSession" ; sid ]        

    let dimensionName dim  = 
        createAlea "Dimension"
        <-- dimName       dim
        <-- dimFirstBatch "true"
        <-- dimLastBatch  "true"
    let descriptionElem = createAlea "Description"
    let elementsElem    = createAlea "Elements"

    let descriptionText text = descriptionElem <== doc.CreateTextNode text

    descriptionText "prueba"

    let insertElement elementType elementName = 
        let element = elementType + "\t" + elementName + "\n"
        elementsElem <== doc.CreateTextNode element

    let MyElement1 = insertElement "N" "MyElement1"
    let MyElement2 = insertElement "N" "MyElement2"
    let MyElement3 = insertElement "S" "MyElement3"


    [
        "texto1" 
        "texto2"
        "texto3"
    ]
    |> String.concat "\n"
    |> print

    let insertElementWithChild elementParentType elementParentName elementChildName =
        let element = elementParentType + "\t" + elementParentName + "\n" + "\t" + elementChildName + "\t-1"
        elementsElem <== doc.CreateTextNode element

    let MyElement4 = insertElementWithChild "C" "MyElement4" "MyElement5"

    let aleaRequest = 
        let mutable rid = 1
        fun cls mtd ->
            rid <- rid + 1
            createAlea "Request"
            <-- requestId     (sprintf "%03d" rid)
            <-- requestClass  cls
            <-- requestMethod mtd

    let dimensionCreate dim =
        createAlea "Document" 
        <==(aleaRequest "Dimension" "Create"
            <==(dimensionName dim
                <==(descriptionElem)
                <==(elementsElem)))

    let dimensionDelete dim =
        createAlea "Document"
            <==(aleaRequest "Dimension" "Delete"
                <== dimensionName dim)


    type ErrorRequest = 
        | HttpError     of string
        | EnvelopeError of string
        | XMLAError     of string 
        | AleaError     of string

    let request (cnt:string) =
            try
                let txt = cnt.Replace("\n\r","\n").Replace("\r\n","\n").Replace("\r","\n")
                Http.RequestString(url, body = TextRequest txt )
                |> Ok
            with e -> 
                e.Message
                |> HttpError
                |> Error                

    let session reqs =
        beginSession
        |> outerXml
        |> request            
        |> Result.map openSessionResp.Parse
        |> Result.bind (fun resp ->  
            let sid () = createSession resp.Header.Session.SessionId 
            [
                yield! reqs |> Seq.map (envelope [ sid () ])
                yield  endSession (sid ())
            ]
            // |> Seq.map (tee (outerXml >> print))
            |> Seq.map (outerXml >> request)
            |> Seq.toArray
            |> Result.sequenceSeq)

    [ execute (dimensionDelete "prueba") 
      execute (dimensionCreate "prueba")
      ]
    |> session 
    |> printfn "%A"
