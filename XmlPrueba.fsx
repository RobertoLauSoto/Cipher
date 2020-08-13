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

    type Server    = Server    of string
    type Dimension = Dimension of string
    type Cube      = Cube      of string
    type Element   = Element   of string with member this.Id = match this with Element id -> id
    [< RequireQualifiedAccess >]
    type ElemType  = String | Numeric | Consolidated
    type Relation  = {
        parent : Element
        child  : Element
        weight : float
    }
    type TableId   = TableId1 | TableId2 | TableId3
    [< RequireQualifiedAccess >]
    type FieldType = String | Numeric | Date | Logical
    type Field     = {
        fldName        : string
        fldTable       : TableId
        fldDescription : string
        fldType        : FieldType
        fldLength      : int
        fldDecimals    : int
    }
    type Handle    = Handle    of int

    let TableIdDecl =
        function 
        | TableId1 -> "1"
        | TableId2 -> "2"
        | TableId3 -> "3"

    let ElemTypeDecl =
        function 
        | ElemType.String       -> "S"
        | ElemType.Numeric      -> "N"
        | ElemType.Consolidated -> "C"

    let FieldTypeDecl =
        function 
        | FieldType.String  -> "C"
        | FieldType.Numeric -> "N"
        | FieldType.Date    -> "D"
        | FieldType.Logical -> "L"

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

    let sessionId          = createAttribute "SessionID"
    let requestId          = createAttribute "RequestID"
    let requestClass       = createAttribute "Class"
    let requestMethod      = createAttribute "Method"
    let dimName            = createAttribute "Name"
    let dimFirstBatch      = createAttribute "FirstBatch"
    let dimLastBatch       = createAttribute "LastBatch"
    let dimWithHierarchies = createAttribute "WithHierarchies"
    let attributeName      = createAttribute "Name"
    let attributeDesc      = createAttribute "Description"
    let attributeWidth   : int -> _ = string        >> createAttribute "Width"
    let attributeDecimal : int -> _ = string        >> createAttribute "Decimal"
    let attributeId                 = TableIdDecl   >> createAttribute "ID"
    let attributeType               = FieldTypeDecl >> createAttribute "Type"



    let createSession sid = createXMLA "Session" <-- sessionId sid

    let endSession sid =
        createXMLA "Statement"
        |> execute 
        |> envelope [ createXMLA "EndSession" ; sid ]

    let aleaRequest = 
        let mutable rid = 1
        fun cls mtd ->
            rid <- rid + 1
            createAlea "Request"
            <-- requestId     (sprintf "%03d" rid)
            <-- requestClass  cls
            <-- requestMethod mtd

    let dimensionName (Dimension dim)  = 
        createAlea "Dimension"
        <-- dimName       dim
        <-- dimFirstBatch "true"
        <-- dimLastBatch  "true"

    let dimensionNameWithHierarchies (Dimension dim) =
        createAlea "Dimension"
        <-- dimName            dim
        <-- dimWithHierarchies "true"


    let descriptionCreate desc =
        createAlea "Description"
        <== doc.CreateTextNode desc

    let concatElements (elementsList : seq<Element * ElemType>) =
        elementsList
        |> Seq.map (fun (Element elm, elmType) -> ElemTypeDecl elmType + "\t" + elm)
        |> String.concat "\n"

    let concatRelations (relationsList : seq<Relation>) =
        relationsList
        |> Seq.groupBy (fun rel -> rel.parent)
        |> Seq.map (fun (prn, rels) -> 
            rels
            |> Seq.map (fun rel -> sprintf "\t%s\t%g" rel.child.Id rel.weight)
            |> String.concat "\n"
            |> sprintf "C\t%s\n%s\n" prn.Id
        )
        |> String.concat "\n"
        

    let elementsCreate (elementsList : seq<Element * ElemType>) (relationsList : seq<Relation>) = 
        createAlea "Elements"
        <== doc.CreateTextNode (concatElements elementsList + "\n" + concatRelations relationsList) 

    let dimensionCreate (dim : Dimension) desc (elementsList : seq<Element * ElemType>) (relationsList : seq<Relation>) =
        createAlea "Document" 
        <==(aleaRequest "Dimension" "Create"
            <==(dimensionName dim
                <==(descriptionCreate desc)
                <==(elementsCreate elementsList relationsList)))

    let dimensionDelete dim =
        createAlea "Document"
            <==(aleaRequest "Dimension" "Delete"
                <== dimensionName dim)
    
    let dimAttributeCreate (att : Field) =
        createAlea "Attribute"
        <-- attributeName    att.fldName
        <-- attributeType    att.fldType
        <-- attributeDesc    att.fldDescription
        <-- attributeWidth   att.fldLength
        <-- attributeDecimal att.fldDecimals

    let attributeTableCreateMethod id atts =
        let tbl = createAlea "AttributeTable"                
                  <-- attributeId id
        for att in atts do
            if att.fldTable = id then
                tbl <== dimAttributeCreate att
                |> ignore
        tbl

    let dimensionCreateAttributeTable dim id atts =
        createAlea "Document"
        <==(aleaRequest "Dimension" "CreateAttributeTable"
            <==(dimensionName dim
                <==(attributeTableCreateMethod id atts)))

    let dimensionDeleteAttributeTable dim id =
        createAlea "Document"
        <==(aleaRequest "Dimension" "DeleteAttributeTable"
            <==(dimensionName dim
                <==(createAlea "AttributeTable"
                    <-- attributeId id)))

    // let dimensionImportAttributeValues dim id (elementsList : seq<Element * seq<Field * string>>) =
    //     let tbl = createAlea "AttributeTable"
    //     createAlea "Document"
    //         <==(aleaRequest "Dimension" "ImportAttributeValues"
    //             <==(dimensionNameWithHierarchies dim
    //             ))

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

    // [ execute (dimensionDelete "prueba") 
    //   execute (dimensionCreate "prueba" "prueba description" ["N\tMyElement1"; "N\tMyElement2"; "S\tMyElement3"; "C\tMyElement4\n\tMyElement5"])
    //   ]
    // |> session 
    // |> printfn "%A"

    let prueba2 = Dimension "prueba2"
    let elements = [ 
                     Element "MyElement1" , ElemType.String
                     Element "MyElement2" , ElemType.Numeric
                     Element "MyElement3" , ElemType.Consolidated
                    ]
    let relations = [
        {
            parent = Element "MyElement3"
            child  = Element "MyElement2"
            weight = 1.0
        }               
    ]
    let attributes = [
        {
            fldName        = "Attribute11"
            fldType        = FieldType.String
            fldDescription = "Attribute11"
            fldLength      = 10
            fldDecimals    = 0
            fldTable       = TableId1
        }
        {
            fldName        = "Attribute12"
            fldType        = FieldType.Numeric
            fldDescription = "Attribute12"
            fldLength      = 10
            fldDecimals    = 0
            fldTable       = TableId1
        }
        {
            fldName        = "Attribute13"
            fldType        = FieldType.Logical
            fldDescription = "Attribute13"
            fldLength      = 10
            fldDecimals    = 0
            fldTable       = TableId1
        }        

        {
            fldName        = "Attribute21"
            fldType        = FieldType.String
            fldDescription = "Attribute21"
            fldLength      = 10
            fldDecimals    = 0
            fldTable       = TableId2
        }
        {
            fldName        = "Attribute22"
            fldType        = FieldType.Numeric
            fldDescription = "Attribute22"
            fldLength      = 10
            fldDecimals    = 0
            fldTable       = TableId2
        }
        {
            fldName        = "Attribute23"
            fldType        = FieldType.Logical
            fldDescription = "Attribute23"
            fldLength      = 10
            fldDecimals    = 0
            fldTable       = TableId2
        }
        {
            fldName        = "Attribute31"
            fldType        = FieldType.String
            fldDescription = "Attribute31"
            fldLength      = 10
            fldDecimals    = 0
            fldTable       = TableId3
        }
        {
            fldName        = "Attribute32"
            fldType        = FieldType.Numeric
            fldDescription = "Attribute32"
            fldLength      = 10
            fldDecimals    = 0
            fldTable       = TableId3
        }
        {
            fldName        = "Attribute33"
            fldType        = FieldType.Logical
            fldDescription = "Attribute33"
            fldLength      = 10
            fldDecimals    = 0
            fldTable       = TableId3
        }
    ]

    [ execute (dimensionDelete prueba2)
      execute (dimensionCreate prueba2 "prueba2 description" elements relations )
      execute (dimensionCreateAttributeTable prueba2 TableId1 attributes )
      execute (dimensionCreateAttributeTable prueba2 TableId2 attributes )
      execute (dimensionCreateAttributeTable prueba2 TableId3 attributes )
    //   execute (dimensionDeleteAttributeTable prueba2 TableId3 )
      ]
    |> session 
    |> printfn "%A"

