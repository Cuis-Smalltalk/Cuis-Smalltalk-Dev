'From Cuis 5.0 [latest update: #4503] on 29 December 2020 at 2:48:16 pm'!
'Description '!
!provides: 'YAXO' 1 17!
SystemOrganization addCategory: 'YAXO'!


!classDefinition: #SAXException category: 'YAXO'!
Error subclass: #SAXException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'YAXO'!
!classDefinition: 'SAXException class' category: 'YAXO'!
SAXException class
	instanceVariableNames: ''!

!classDefinition: #SAXMalformedException category: 'YAXO'!
SAXException subclass: #SAXMalformedException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'YAXO'!
!classDefinition: 'SAXMalformedException class' category: 'YAXO'!
SAXMalformedException class
	instanceVariableNames: ''!

!classDefinition: #SAXParseException category: 'YAXO'!
SAXException subclass: #SAXParseException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'YAXO'!
!classDefinition: 'SAXParseException class' category: 'YAXO'!
SAXParseException class
	instanceVariableNames: ''!

!classDefinition: #XMLException category: 'YAXO'!
Error subclass: #XMLException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'YAXO'!
!classDefinition: 'XMLException class' category: 'YAXO'!
XMLException class
	instanceVariableNames: ''!

!classDefinition: #XMLInvalidException category: 'YAXO'!
XMLException subclass: #XMLInvalidException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'YAXO'!
!classDefinition: 'XMLInvalidException class' category: 'YAXO'!
XMLInvalidException class
	instanceVariableNames: ''!

!classDefinition: #XMLMalformedException category: 'YAXO'!
XMLException subclass: #XMLMalformedException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'YAXO'!
!classDefinition: 'XMLMalformedException class' category: 'YAXO'!
XMLMalformedException class
	instanceVariableNames: ''!

!classDefinition: #XMLWarningException category: 'YAXO'!
XMLException subclass: #XMLWarningException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'YAXO'!
!classDefinition: 'XMLWarningException class' category: 'YAXO'!
XMLWarningException class
	instanceVariableNames: ''!

!classDefinition: #SAXWarning category: 'YAXO'!
Warning subclass: #SAXWarning
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'YAXO'!
!classDefinition: 'SAXWarning class' category: 'YAXO'!
SAXWarning class
	instanceVariableNames: ''!

!classDefinition: #DTDEntityDeclaration category: 'YAXO'!
Object subclass: #DTDEntityDeclaration
	instanceVariableNames: 'name value ndata'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'YAXO'!
!classDefinition: 'DTDEntityDeclaration class' category: 'YAXO'!
DTDEntityDeclaration class
	instanceVariableNames: 'contextBehavior'!

!classDefinition: #DTDExternalEntityDeclaration category: 'YAXO'!
DTDEntityDeclaration subclass: #DTDExternalEntityDeclaration
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'YAXO'!
!classDefinition: 'DTDExternalEntityDeclaration class' category: 'YAXO'!
DTDExternalEntityDeclaration class
	instanceVariableNames: ''!

!classDefinition: #DTDParameterEntityDeclaration category: 'YAXO'!
DTDEntityDeclaration subclass: #DTDParameterEntityDeclaration
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'YAXO'!
!classDefinition: 'DTDParameterEntityDeclaration class' category: 'YAXO'!
DTDParameterEntityDeclaration class
	instanceVariableNames: ''!

!classDefinition: #SAXHandler category: 'YAXO'!
Object subclass: #SAXHandler
	instanceVariableNames: 'document driver eod'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'YAXO'!
!classDefinition: 'SAXHandler class' category: 'YAXO'!
SAXHandler class
	instanceVariableNames: ''!

!classDefinition: #XMLDOMParser category: 'YAXO'!
SAXHandler subclass: #XMLDOMParser
	instanceVariableNames: 'entity stack incremental'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'YAXO'!
!classDefinition: 'XMLDOMParser class' category: 'YAXO'!
XMLDOMParser class
	instanceVariableNames: ''!

!classDefinition: #XMLNamespaceScope category: 'YAXO'!
Object subclass: #XMLNamespaceScope
	instanceVariableNames: 'scope currentBindings useNamespaces validateAttributes'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'YAXO'!
!classDefinition: 'XMLNamespaceScope class' category: 'YAXO'!
XMLNamespaceScope class
	instanceVariableNames: ''!

!classDefinition: #XMLNode category: 'YAXO'!
Object subclass: #XMLNode
	instanceVariableNames: ''
	classVariableNames: 'CanonicalTable'
	poolDictionaries: ''
	category: 'YAXO'!
!classDefinition: 'XMLNode class' category: 'YAXO'!
XMLNode class
	instanceVariableNames: ''!

!classDefinition: #XMLNodeWithElements category: 'YAXO'!
XMLNode subclass: #XMLNodeWithElements
	instanceVariableNames: 'elements uri namespace'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'YAXO'!
!classDefinition: 'XMLNodeWithElements class' category: 'YAXO'!
XMLNodeWithElements class
	instanceVariableNames: ''!

!classDefinition: #XMLDocument category: 'YAXO'!
XMLNodeWithElements subclass: #XMLDocument
	instanceVariableNames: 'dtd version encoding requiredMarkup'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'YAXO'!
!classDefinition: 'XMLDocument class' category: 'YAXO'!
XMLDocument class
	instanceVariableNames: ''!

!classDefinition: #XMLElement category: 'YAXO'!
XMLNodeWithElements subclass: #XMLElement
	instanceVariableNames: 'name contents attributes'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'YAXO'!
!classDefinition: 'XMLElement class' category: 'YAXO'!
XMLElement class
	instanceVariableNames: ''!

!classDefinition: #XMLPI category: 'YAXO'!
XMLNode subclass: #XMLPI
	instanceVariableNames: 'target data'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'YAXO'!
!classDefinition: 'XMLPI class' category: 'YAXO'!
XMLPI class
	instanceVariableNames: ''!

!classDefinition: #XMLStringNode category: 'YAXO'!
XMLNode subclass: #XMLStringNode
	instanceVariableNames: 'string'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'YAXO'!
!classDefinition: 'XMLStringNode class' category: 'YAXO'!
XMLStringNode class
	instanceVariableNames: ''!

!classDefinition: #XMLTokenizer category: 'YAXO'!
Object subclass: #XMLTokenizer
	instanceVariableNames: 'stream nestedStreams entities externalEntities parameterEntities parsingMarkup markedPosition peekChar validating nameBuffer attributeBuffer'
	classVariableNames: 'CharEscapes DigitTable LiteralChars NameDelimiters SeparatorTable'
	poolDictionaries: ''
	category: 'YAXO'!
!classDefinition: 'XMLTokenizer class' category: 'YAXO'!
XMLTokenizer class
	instanceVariableNames: ''!

!classDefinition: #SAXDriver category: 'YAXO'!
XMLTokenizer subclass: #SAXDriver
	instanceVariableNames: 'saxHandler scope useNamespaces validateAttributes'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'YAXO'!
!classDefinition: 'SAXDriver class' category: 'YAXO'!
SAXDriver class
	instanceVariableNames: ''!

!classDefinition: #XMLParser category: 'YAXO'!
XMLTokenizer subclass: #XMLParser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'YAXO'!
!classDefinition: 'XMLParser class' category: 'YAXO'!
XMLParser class
	instanceVariableNames: ''!

!classDefinition: #XMLWriter category: 'YAXO'!
Object subclass: #XMLWriter
	instanceVariableNames: 'stream stack scope scanner canonical'
	classVariableNames: 'XMLTranslation XMLTranslationMap'
	poolDictionaries: ''
	category: 'YAXO'!
!classDefinition: 'XMLWriter class' category: 'YAXO'!
XMLWriter class
	instanceVariableNames: ''!


!XMLDOMParser commentStamp: '<historical>' prior: 0!
See class side for examples.!

!XMLTokenizer commentStamp: '<historical>' prior: 0!
XMLTokenizer

bolot@cc.gatech.edu

breaks the stream of characters into a stream of XMLnodes (aka token stream)
token stream is used by XMLparser to generate XMLdocument tree!

!DTDEntityDeclaration methodsFor: 'accessing' stamp: 'mir 1/4/2002 19:40'!
name
	^name! !

!SAXHandler methodsFor: 'lexical' stamp: 'mir 8/11/2000 18:52'!
comment: commentString
	"This call corresponds to the Java SAX ext call
	comment(char[] ch, int start, int length)."! !

!XMLNode methodsFor: 'printing' stamp: 'mir 1/17/2002 15:45'!
printOn: stream
	self printXMLOn: (XMLWriter on: stream)! !

!XMLElement methodsFor: 'accessing' stamp: 'sd 5/25/2003 18:25'!
name
	^ self qualifiedName! !

!XMLWriter methodsFor: 'writing xml' stamp: 'mir 12/8/2000 17:56'!
comment: aString
	self startComment.
	self stream nextPutAll: aString.
	self endComment! !

!DTDEntityDeclaration methodsFor: 'behaviors' stamp: 'mir 1/15/2002 11:29'!
bypass
	"Return my reference as is."
	^self reference! !

!DTDEntityDeclaration methodsFor: 'behaviors' stamp: 'mir 1/15/2002 11:29'!
forbidden
	self error: 'Forbidden reference usage'! !

!DTDEntityDeclaration methodsFor: 'behaviors' stamp: 'mir 1/15/2002 18:01'!
include
	"Return my expanded value."
	^value ifNil: [SAXWarning signal: 'XML undefined entity ' , name printString]! !

!DTDEntityDeclaration methodsFor: 'behaviors' stamp: 'mir 1/15/2002 18:06'!
includedInLiteral
	"Return my expanded value."
	^self include! !

!DTDEntityDeclaration methodsFor: 'behaviors' stamp: 'mir 1/15/2002 11:30'!
reference
	"Return my reference as is."
	^self class leadIn , self name , ';'! !

!DTDEntityDeclaration methodsFor: 'accessing' stamp: 'mir 1/17/2002 15:25'!
name: aString
	name _ aString asSymbol! !

!DTDEntityDeclaration methodsFor: 'accessing' stamp: 'mir 12/8/2000 17:22'!
ndata
	^ndata! !

!DTDEntityDeclaration methodsFor: 'accessing' stamp: 'mir 12/8/2000 17:22'!
ndata: aString
	ndata _ aString! !

!DTDEntityDeclaration methodsFor: 'accessing' stamp: 'mir 11/16/2000 10:54'!
value
	^value! !

!DTDEntityDeclaration methodsFor: 'accessing' stamp: 'mir 11/16/2000 10:55'!
value: aString
	value _ aString! !

!DTDEntityDeclaration methodsFor: 'invocation' stamp: 'mir 11/16/2000 21:23'!
registerIn: aParser
	aParser entity: self name put: self! !

!DTDEntityDeclaration methodsFor: 'invocation' stamp: 'mir 1/15/2002 15:08'!
valueForContext: aContext
	^self perform: (self class behaviorForContext: aContext)! !

!DTDEntityDeclaration class methodsFor: 'accessing' stamp: 'mir 11/16/2000 20:14'!
behaviorForContext: aContext
	^self contextBehavior at: aContext! !

!DTDEntityDeclaration class methodsFor: 'accessing' stamp: 'mir 11/16/2000 20:15'!
contextBehavior
	^contextBehavior! !

!DTDEntityDeclaration class methodsFor: 'accessing' stamp: 'mir 11/16/2000 20:27'!
leadIn
	^'&'! !

!DTDEntityDeclaration class methodsFor: 'class initialization' stamp: 'mir 1/15/2002 18:02'!
initialize
	"DTDEntityDeclaration initialize"

	contextBehavior _ Dictionary new.
	contextBehavior
		at: #content put: #include ;
		at: #attributeValueContent put: #includedInLiteral ;
		at: #attributeValue put: #forbidden ;
		at: #entityValue put: #bypass ;
		at: #dtd put: #forbidden ! !

!DTDEntityDeclaration class methodsFor: 'instance creation' stamp: 'mir 11/16/2000 20:13'!
name: aString value: aValueString
	^self new
		name: aString;
		value: aValueString! !

!DTDExternalEntityDeclaration class methodsFor: 'class initialization' stamp: 'mir 1/14/2002 18:15'!
initialize
	"DTDExternalEntityDeclaration initialize"

	contextBehavior _ Dictionary new.
	contextBehavior
		at: #content put: #include ;
		at: #attributeValueContent put: #includedInLiteral ;
		at: #attributeValue put: #forbidden ;
		at: #entityValue put: #bypass ;
		at: #dtd put: #forbidden ! !

!DTDParameterEntityDeclaration methodsFor: 'behaviors' stamp: 'mir 1/15/2002 11:30'!
includePE
	"Return my expanded value."
	^self include! !

!DTDParameterEntityDeclaration methodsFor: 'behaviors' stamp: 'mir 1/15/2002 23:21'!
notRecognized
	SAXMalformedException signal: 'Malformed entity.'! !

!DTDParameterEntityDeclaration methodsFor: 'invocation' stamp: 'mir 11/28/2000 17:26'!
registerIn: aParser
	aParser parameterEntity: self name put: self! !

!DTDParameterEntityDeclaration class methodsFor: 'class initialization' stamp: 'mir 1/14/2002 18:15'!
initialize
	"DTDParameterEntityDeclaration initialize"

	contextBehavior _ Dictionary new.
	contextBehavior
		at: #content put: #notRecognized: ;
		at: #attributeValueContent put: #notRecognized: ;
		at: #attributeValue put: #notRecognized: ;
		at: #entityValue put: #include: ;
		at: #dtd put: #includePE:! !

!DTDParameterEntityDeclaration class methodsFor: 'accessing' stamp: 'mir 11/16/2000 20:27'!
leadIn
	^'%'! !

!SAXHandler methodsFor: 'content' stamp: 'mir 1/8/2002 18:27'!
characters: aString
	"This call corresponds to the Java SAX call
	characters(char[] ch, int start, int length)."! !

!SAXHandler methodsFor: 'content' stamp: 'mir 1/8/2002 18:24'!
checkEOD
	"Check if the document shouldn't be ended already"
	self eod
		ifTrue: [self driver errorExpected: 'No more data expected,']! !

!SAXHandler methodsFor: 'content' stamp: 'mir 1/17/2002 13:12'!
documentAttributes: attributeList! !

!SAXHandler methodsFor: 'content' stamp: 'mir 1/8/2002 18:26'!
endDocument
	"This call corresponds to the Java SAX call
	endDocument()."
	eod _ true! !

!SAXHandler methodsFor: 'content' stamp: 'mir 8/14/2000 18:07'!
endElement: elementName
! !

!SAXHandler methodsFor: 'content' stamp: 'mir 6/5/2003 19:05'!
endElement: elementName namespace: namespace namespaceURI: namespaceURI qualifiedName: qualifiedName
	"This call corresponds to the Java SAX call
	endElement(java.lang.String namespaceURI, java.lang.String localName, java.lang.String qName).
	By default this call is mapped to the following more convenient call:"

	self endElement: elementName! !

!SAXHandler methodsFor: 'content' stamp: 'mir 8/11/2000 16:25'!
endPrefixMapping: prefix
	"This call corresonds to the Java SAX call
	endPrefixMapping(java.lang.String prefix)."! !

!SAXHandler methodsFor: 'content' stamp: 'mir 8/11/2000 16:25'!
ignorableWhitespace: aString
	"This call corresonds to the Java SAX call
	ignorableWhitespace(char[] ch, int start, int length)."! !

!SAXHandler methodsFor: 'content' stamp: 'mir 8/11/2000 16:26'!
processingInstruction: piName data: dataString
	"This call corresonds to the Java SAX call
	processingInstruction(java.lang.String target, java.lang.String data)."! !

!SAXHandler methodsFor: 'content' stamp: 'mir 8/11/2000 16:45'!
skippedEntity: aString
	"This call corresonds to the Java SAX call
	skippedEntity(java.lang.String name)."! !

!SAXHandler methodsFor: 'content' stamp: 'mir 8/11/2000 16:45'!
startDocument
	"This call corresonds to the Java SAX call
	startDocument()."! !

!SAXHandler methodsFor: 'content' stamp: 'mir 8/14/2000 18:07'!
startElement: elementName attributeList: attributeList
! !

!SAXHandler methodsFor: 'content' stamp: 'mir 6/5/2003 16:50'!
startElement: localName namespaceURI: namespaceUri namespace: namespace attributeList: attributeList
	"This call corresonds to the Java SAX call
	startElement(java.lang.String namespaceURI, java.lang.String localName, java.lang.String qName, Attributes atts).
	By default this call is mapped to the following more convenient call:"

	self startElement: localName attributeList: attributeList! !

!SAXHandler methodsFor: 'content' stamp: 'mir 8/11/2000 16:47'!
startPrefixMapping: prefix uri: uri
	"This call corresonds to the Java SAX call
	startPrefixMapping(java.lang.String prefix, java.lang.String uri)."! !

!SAXHandler methodsFor: 'lexical' stamp: 'mir 8/11/2000 18:53'!
endEntity: entityName
	"This call corresponds to the Java SAX ext call
	endEntity(java.lang.String name)."! !

!SAXHandler methodsFor: 'lexical' stamp: 'mir 8/11/2000 18:53'!
startCData
	"This call corresponds to the Java SAX ext call
	startCData()."! !

!SAXHandler methodsFor: 'lexical' stamp: 'mir 8/11/2000 18:54'!
startDTD: declName publicID: publicID systemID: systemID
	"This call corresponds to the Java SAX ext call
	startDTD(java.lang.String name, java.lang.String publicId, java.lang.String systemId)."! !

!SAXHandler methodsFor: 'lexical' stamp: 'mir 8/11/2000 18:54'!
startEntity: entityName
	"This call corresponds to the Java SAX ext call
	startEntity(java.lang.String name)."! !

!SAXHandler methodsFor: 'accessing' stamp: 'mir 11/30/2000 18:12'!
document
	^document! !

!SAXHandler methodsFor: 'accessing' stamp: 'mir 11/30/2000 18:12'!
document: aDocument
	document _ aDocument! !

!SAXHandler methodsFor: 'accessing' stamp: 'mir 12/7/2000 15:34'!
driver
	^driver! !

!SAXHandler methodsFor: 'accessing' stamp: 'mir 12/7/2000 15:34'!
driver: aDriver
	driver _ aDriver.
	driver saxHandler: self! !

!SAXHandler methodsFor: 'accessing' stamp: 'mir 1/8/2002 18:18'!
eod
	^eod! !

!SAXHandler methodsFor: 'accessing' stamp: 'mir 6/5/2003 19:28'!
useNamespaces: aBoolean
	self driver useNamespaces: aBoolean! !

!SAXHandler methodsFor: 'initialization' stamp: 'mir 1/8/2002 18:18'!
initialize
	eod _ false! !

!SAXHandler methodsFor: 'parsing' stamp: 'mir 1/8/2002 18:18'!
parseDocument
	[self driver nextEntity isNil or: [self eod]] whileFalse! !

!SAXHandler methodsFor: 'entity' stamp: 'mir 8/11/2000 17:33'!
resolveEntity: publicID systemID: systemID
	"This call corresonds to the Java SAX call
	resolveEntity(java.lang.String publicId, java.lang.String systemId)."! !

!SAXHandler class methodsFor: 'instance creation' stamp: 'sd 5/23/2003 15:19'!
on: aStream
	| driver parser |
	driver _ SAXDriver on: aStream.
	driver validating: true.
	parser _ self new driver: driver.
	^parser! !

!SAXHandler class methodsFor: 'instance creation' stamp: 'cwp 5/28/2003 02:08'!
parseDTDFrom: aStream
	| driver parser |
	driver _ SAXDriver on: aStream.
	driver validating: true.
	driver startParsingMarkup.
	parser _ self new driver: driver.
	parser startDocument.
	parser parseDocument.
	^parser! !

!SAXHandler class methodsFor: 'instance creation' stamp: 'mir 6/5/2003 19:13'!
parseDocumentFrom: aStream
	|  parser |
	parser _ self on: aStream.
	parser startDocument.
	parser parseDocument.
	^parser! !

!SAXHandler class methodsFor: 'instance creation' stamp: 'mir 1/8/2002 15:55'!
parseDocumentFromFileNamed: fileName
	^self parseDocumentFromFileNamed: fileName readIntoMemory: false! !

!SAXHandler class methodsFor: 'instance creation' stamp: 'pb 5/25/2016 01:33'!
parseDocumentFromFileNamed: fileName readIntoMemory: readIntoMemory

	^fileName asFileEntry readStreamDo: [ :stream |
		self parseDocumentFrom: 
			(readIntoMemory
				ifTrue: [ stream contentsOfEntireFile readStream ]
				ifFalse: [ stream ])]! !

!SAXHandler class methodsFor: 'instance creation' stamp: 'mir 6/5/2003 19:15'!
parserOnFileNamed: fileName
	^self parserOnFileNamed: fileName readIntoMemory: false! !

!SAXHandler class methodsFor: 'instance creation' stamp: 'pb 5/25/2016 01:51'!
parserOnFileNamed: fileName readIntoMemory: readIntoMemory
	| stream  |
	stream _ (DirectoryEntry currentDirectory // fileName) readStream.
	readIntoMemory
		ifTrue: [stream _ stream contentsOfEntireFile readStream].
	^self on: stream! !

!XMLDOMParser methodsFor: 'content' stamp: 'mir 10/25/2000 11:30'!
characters: aString
	| newElement |
	newElement _ XMLStringNode string: aString.
	self top addContent: newElement.
! !

!XMLDOMParser methodsFor: 'content' stamp: 'mir 1/17/2002 13:13'!
documentAttributes: attributeList
	self document version: (attributeList at: 'version' ifAbsent: [nil]).
	self document encoding: (attributeList at: 'encoding' ifAbsent: [nil]).
	self document requiredMarkup: (attributeList at: 'requiredMarkup' ifAbsent: [nil]).
! !

!XMLDOMParser methodsFor: 'content' stamp: 'mir 1/8/2002 18:19'!
endDocument
	self pop.
	super endDocument! !

!XMLDOMParser methodsFor: 'content' stamp: 'mir 1/8/2002 18:11'!
endElement: elementName
	| currentElement |
	currentElement _ self pop.
	currentElement name = elementName
		ifFalse: [self driver errorExpected: 'End tag "', elementName , '" doesn''t match "' , currentElement name , '".']! !

!XMLDOMParser methodsFor: 'content' stamp: 'mir 6/24/2003 14:53'!
endElement: localName namespace: namespace namespaceURI: uri qualifiedName: qualifiedName
	| currentElement |
	currentElement _ self pop.
	(currentElement namespace isNil
		or: [currentElement namespace = self defaultNamespace])
		ifTrue: [
			currentElement localName = localName
				ifFalse: [self driver errorExpected: 'End tag "', localName , '" doesn''t match "' , currentElement localName  , '".']]
		ifFalse: [
			currentElement qualifiedName = qualifiedName
				ifFalse: [self driver errorExpected: 'End tag "', qualifiedName , '" doesn''t match "' , currentElement qualifiedName  , '".']]! !

!XMLDOMParser methodsFor: 'content' stamp: 'mir 3/6/2002 10:49'!
processingInstruction: piName data: dataString
	| newElement |
	newElement _ XMLPI target: piName data: dataString.
	self top addElement: newElement! !

!XMLDOMParser methodsFor: 'content' stamp: 'mir 11/30/2000 18:14'!
startDocument
	self document: XMLDocument new.
	self push: self document ! !

!XMLDOMParser methodsFor: 'content' stamp: 'mir 3/6/2002 10:49'!
startElement: elementName attributeList: attributeList
	| newElement |
	newElement _ XMLElement named: elementName attributes: attributeList.
	self incremental
		ifFalse: [self stack isEmpty
			ifFalse: [self top addElement: newElement]].
	self push: newElement! !

!XMLDOMParser methodsFor: 'content' stamp: 'mir 6/24/2003 18:52'!
startElement: localName namespaceURI: namespaceUri namespace: namespace attributeList: attributeList
	| newElement |
	"newElement _ namespace = self defaultNamespace
		ifTrue: [XMLElement named: localName namespace: nil uri: nil attributes: attributeList]
		ifFalse: [XMLElement named: localName namespace: namespace uri: namespaceUri attributes: attributeList]."
	newElement _ XMLElement named: localName namespace: namespace uri: namespaceUri attributes: attributeList.
	self incremental
		ifFalse: [self stack isEmpty
			ifFalse: [self top addElement: newElement]].
	self push: newElement! !

!XMLDOMParser methodsFor: 'private' stamp: 'mir 6/16/2003 17:20'!
defaultNamespace
	^self top
		ifNotNil: [self top namespace]! !

!XMLDOMParser methodsFor: 'private' stamp: 'mir 1/8/2001 12:04'!
pop
	| oldTop |
	oldTop _ self stack removeLast.
	entity _ oldTop.
	^oldTop! !

!XMLDOMParser methodsFor: 'private' stamp: 'mir 1/8/2001 12:02'!
push: anObject
	self stack add: anObject.
	entity _ anObject
! !

!XMLDOMParser methodsFor: 'private' stamp: 'mir 8/14/2000 18:28'!
stack
	^stack! !

!XMLDOMParser methodsFor: 'private' stamp: 'mir 1/8/2001 11:46'!
top
	^self stack isEmpty
		ifTrue: [nil]
		ifFalse: [self stack last]! !

!XMLDOMParser methodsFor: 'parsing' stamp: 'mir 6/5/2003 19:29'!
domDocument
	[self startDocument; parseDocument]
		ensure: [self driver stream close].
	^document! !

!XMLDOMParser methodsFor: 'parsing' stamp: 'mir 6/28/2001 18:45'!
nextEntity
	| currentTop |
	currentTop _ self top.
	[self driver nextEntity isNil
		or: [self top ~~ currentTop]] whileTrue.
	^entity! !

!XMLDOMParser methodsFor: 'parsing' stamp: 'mir 12/21/2000 14:02'!
nextEntityStart
	[self driver nextEntity.
	self stack isEmpty] whileTrue.
	^entity! !

!XMLDOMParser methodsFor: 'accessing' stamp: 'mir 1/8/2001 12:05'!
incremental
	^incremental! !

!XMLDOMParser methodsFor: 'accessing' stamp: 'mir 1/8/2001 12:05'!
incremental: aBoolean
	incremental _ aBoolean! !

!XMLDOMParser methodsFor: 'initialization' stamp: 'mir 1/8/2001 12:05'!
initialize
	super initialize.
	stack _ OrderedCollection new.
	incremental _ false! !

!XMLDOMParser class methodsFor: 'examples' stamp: 'mir 8/14/2000 18:36'!
addressBookXMLWithDTD
	"XMLDOMParser addressBookXMLWithDTD"
	^self parseDocumentFrom: XMLTokenizer addressBookXMLWithDTD readStream! !

!XMLDOMParser class methodsFor: 'instance creation' stamp: 'mir 12/7/2000 16:29'!
parseDocumentFrom: aStream
	^(super parseDocumentFrom: aStream) document! !

!XMLNamespaceScope methodsFor: 'private' stamp: 'mir 6/4/2003 15:51'!
currentScope
	^self scope last! !

!XMLNamespaceScope methodsFor: 'private' stamp: 'mir 6/4/2003 16:08'!
establishLocalBindings
	(self currentScope at: 2)
		ifNil: [
			currentBindings := currentBindings copy.
			self currentScope at: 2 put: currentBindings]! !

!XMLNamespaceScope methodsFor: 'private' stamp: 'mir 6/24/2003 14:25'!
initScope
	scope := OrderedCollection new: 20.
	currentBindings := Dictionary new.
	scope addLast: {'http://www.w3.org/TR/REC-xml-names'. currentBindings. nil. }.
! !

!XMLNamespaceScope methodsFor: 'private' stamp: 'mir 6/5/2003 19:32'!
namespaceAliases: namespace
	"Locate all namespaces that are aliases of the given URI."

	| aliases uri |
	aliases _ Set new.
	uri _ self namespaceURIOf: namespace ifAbsent: [self parseError: 'Attribute refers to undefined namespace ' , namespace asString ].
	currentBindings keysAndValuesDo: [:ns :u |
		(u = uri
			and: [ns ~= namespace])
			ifTrue: [aliases add: ns]].
	^ aliases! !

!XMLNamespaceScope methodsFor: 'private' stamp: 'mir 6/24/2003 14:26'!
scope
	scope ifNil: [self initScope].
	^scope! !

!XMLNamespaceScope methodsFor: 'scope' stamp: 'mir 6/16/2003 16:37'!
declareNamespace: ns uri: uri
	"Declare the given name space prefix with the given URL"

	ns = 'xmlns'
		ifTrue: [^self defaultNamespace: uri].
	self establishLocalBindings.
	currentBindings removeKey: ns ifAbsent: [].
	currentBindings at: ns put: uri! !

!XMLNamespaceScope methodsFor: 'scope' stamp: 'mir 6/5/2003 19:28'!
enterScope
	self scope addLast: { self defaultNamespace. nil. currentBindings. }! !

!XMLNamespaceScope methodsFor: 'scope' stamp: 'mir 6/4/2003 16:05'!
leaveScope
	| leftScope |
	leftScope := self scope removeLast.
	currentBindings := (self currentScope at: 2) ifNil: [leftScope at: 3]! !

!XMLNamespaceScope methodsFor: 'accessing' stamp: 'mir 6/24/2003 14:22'!
defaultNamespace
	^self currentScope first! !

!XMLNamespaceScope methodsFor: 'accessing' stamp: 'mir 6/24/2003 14:23'!
defaultNamespace: ns
	"Declare the default namespace."
	self currentScope at: 1 put: ns! !

!XMLNamespaceScope methodsFor: 'accessing' stamp: 'sd 5/28/2003 09:33'!
namespaceURIOf: ns
	"Retrieve the URI of the given namespace prefix, if it is defined. A nil namespace
	returns the global namespace"

	^ self namespaceURIOf: ns ifAbsent: [ nil ]! !

!XMLNamespaceScope methodsFor: 'accessing' stamp: 'mir 6/24/2003 13:33'!
namespaceURIOf: ns ifAbsent: aBlock
	"Retrieve the URI of the given namespace prefix, if it is defined. 
	A nil namespace returns the default namespace. 
	If no namespace can be found the value of the block is returned"

	^ns
		ifNil: [self defaultNamespace]
		ifNotNil: [currentBindings at: ns ifAbsent: aBlock]! !

!XMLNamespaceScope methodsFor: 'accessing' stamp: 'mir 6/5/2003 19:32'!
namespaces
	^currentBindings! !

!XMLNamespaceScope methodsFor: 'validation' stamp: 'mir 6/5/2003 17:16'!
validateAttributes: attributeList
	"check all attribute namespaces are defined and not duplicated by aliasing"
	| namespace localName |
	attributeList keysDo: [:attrName |
		self splitName: attrName into: [:ns :ln |
			namespace _ ns.
			localName _ ln].
		namespace ifNotNil: [
			(self namespaceAliases: namespace) do: [:alias |
				(attributeList includesKey: alias , ':' , localName)
					ifTrue: [self parseError: 'Attributes ' , attrName , ' and ' , alias , ':' , localName , ' are aliased to namespace ' , (self namespaceURIOf: namespace) ]]]]! !

!XMLNode methodsFor: 'accessing' stamp: 'mir 1/8/2002 18:44'!
addContent: contentString
	SAXParseException signal: 'Illegal string data.'! !

!XMLNode methodsFor: 'enumerating' stamp: 'mir 1/17/2002 14:49'!
contentsDo: aBlock! !

!XMLNode methodsFor: 'enumerating' stamp: 'mir 3/6/2002 10:56'!
elementsDo: aBlock! !

!XMLNode methodsFor: 'searching' stamp: 'mir 3/6/2002 10:52'!
firstTagNamed: aSymbol 
	"Return the first encountered node with the specified tag. Pass the message on"

	| answer |

	self elementsDo: [:node | (answer _ node firstTagNamed: aSymbol) ifNotNil: [^answer]].
	^nil! !

!XMLNode methodsFor: 'searching' stamp: 'mir 3/6/2002 10:53'!
firstTagNamed: aSymbol with: aBlock
	"Return the first encountered node with the specified tag that
	allows the block to evaluate to true. Pass the message on"

	| answer |

	self elementsDo: [:node |
		(answer _ node firstTagNamed: aSymbol with: aBlock) ifNotNil: [^answer]].
	^nil! !

!XMLNode methodsFor: 'searching' stamp: 'mir 3/6/2002 10:53'!
tagsNamed: aSymbol childrenDo: aOneArgumentBlock
	"Evaluate aOneArgumentBlock for all children who match"

	self elementsDo: [:each | 
		each tagsNamed: aSymbol ifReceiverDo: aOneArgumentBlock]! !

!XMLNode methodsFor: 'searching' stamp: 'mir 3/6/2002 10:53'!
tagsNamed: aSymbol childrenDoAndRecurse: aOneArgumentBlock
	"Evaluate aOneArgumentBlock for all children who match and recurse"

	self elementsDo: [:each | 
		each tagsNamed: aSymbol ifReceiverDoAndRecurse: aOneArgumentBlock]! !

!XMLNode methodsFor: 'searching' stamp: 'mir 3/6/2002 10:53'!
tagsNamed: aSymbol contentsDo: aBlock
	"Evaluate aBlock for all of the contents of the receiver.
	The receiver has no tag, so pass the message on"

	self elementsDo: [:each | each tagsNamed: aSymbol contentsDo: aBlock]! !

!XMLNode methodsFor: 'searching' stamp: 'mir 3/6/2002 10:53'!
tagsNamed: aSymbol do: aOneArgumentBlock
	"Search for nodes with tag aSymbol. When encountered evaluate aOneArgumentBlock"

	self elementsDo: [:each | each tagsNamed: aSymbol do: aOneArgumentBlock]! !

!XMLNode methodsFor: 'searching' stamp: 'bp 12/4/2009 10:29'!
tagsNamed: aSymbol ifReceiverDo: aOneArgumentBlock
	"Handled only by XMLElement subclass"

! !

!XMLNode methodsFor: 'searching' stamp: 'mir 3/6/2002 10:53'!
tagsNamed: aSymbol ifReceiverDoAndRecurse: aOneArgumentBlock
	"Recurse all children"

	self elementsDo: [:each | each tagsNamed: aSymbol ifReceiverDoAndRecurse: aOneArgumentBlock]! !

!XMLNode methodsFor: 'searching' stamp: 'mir 3/6/2002 10:53'!
tagsNamed: aSymbol ifReceiverOrChildDo: aOneArgumentBlock
	"Recurse all children"

	self elementsDo: [:each | each tagsNamed: aSymbol ifReceiverDo: aOneArgumentBlock]! !

!XMLNode methodsFor: 'testing' stamp: 'mir 1/17/2002 15:28'!
isProcessingInstruction
	^false! !

!XMLNode methodsFor: 'testing' stamp: 'mir 1/17/2002 15:26'!
isTag
	^false! !

!XMLNode methodsFor: 'testing' stamp: 'mir 1/17/2002 15:26'!
isText
	^false! !

!XMLNode methodsFor: 'printing' stamp: 'mir 1/17/2002 15:45'!
printXMLOn: writer
	self subclassResponsibility! !

!XMLNodeWithElements methodsFor: 'accessing' stamp: 'mir 3/6/2002 11:01'!
addElement: element
	self elements add: element! !

!XMLNodeWithElements methodsFor: 'accessing' stamp: 'mir 10/25/2000 11:22'!
addEntity: entityName value: entityValue
	self entities add: entityName->entityValue! !

!XMLNodeWithElements methodsFor: 'accessing' stamp: 'mir 3/6/2002 10:46'!
elementAt: entityName
	^self elementAt: entityName ifAbsent: [nil]! !

!XMLNodeWithElements methodsFor: 'accessing' stamp: 'mir 6/25/2003 13:27'!
elementAt: entityName ifAbsent: aBlock
	elements
		ifNil: [^aBlock value].
	^self elements detect: [:each | each name = entityName or: [each localName = entityName]] ifNone: [^aBlock value]! !

!XMLNodeWithElements methodsFor: 'accessing' stamp: 'mir 6/16/2003 17:36'!
elementUnqualifiedAt: entityName
	^self elementUnqualifiedAt: entityName ifAbsent: [nil]! !

!XMLNodeWithElements methodsFor: 'accessing' stamp: 'mir 6/16/2003 17:36'!
elementUnqualifiedAt: entityName ifAbsent: aBlock
	elements
		ifNil: [^aBlock value].
	^self elements detect: [:each | each localName = entityName] ifNone: [^aBlock value]! !

!XMLNodeWithElements methodsFor: 'accessing' stamp: 'mir 3/6/2002 10:54'!
elements
	elements ifNil: [elements _ OrderedCollection new].
	^elements! !

!XMLNodeWithElements methodsFor: 'accessing' stamp: 'jdb 2/16/2009 11:04'!
topElement

	self elements ifEmpty: [^self].
	^self elements first! !

!XMLNodeWithElements methodsFor: 'enumerating' stamp: 'mir 3/6/2002 10:45'!
elementsDo: aBlock
	elements
		ifNotNil: [
			self elements do: [:each | aBlock value: each]]! !

!XMLNodeWithElements methodsFor: 'name space' stamp: 'mir 6/5/2003 15:20'!
namespace
	^ namespace! !

!XMLNodeWithElements methodsFor: 'name space' stamp: 'mir 6/16/2003 16:21'!
namespace: ns uri: u
	namespace _ ns.
	uri _ u! !

!XMLNodeWithElements methodsFor: 'name space' stamp: 'mir 6/5/2003 15:20'!
namespaceURI
	^ uri! !

!XMLNodeWithElements methodsFor: 'printing' stamp: 'mir 3/6/2002 10:49'!
printXMLOn: writer
	self elementsDo: [:element | element printXMLOn: writer]! !

!XMLDocument methodsFor: 'accessing' stamp: 'mir 11/30/2000 17:48'!
dtd
	^dtd! !

!XMLDocument methodsFor: 'accessing' stamp: 'mir 11/30/2000 17:48'!
dtd: aDTD
	dtd _ aDTD! !

!XMLDocument methodsFor: 'accessing' stamp: 'mir 5/16/2003 14:09'!
encoding	
	^encoding ifNil: ['UTF-8']! !

!XMLDocument methodsFor: 'accessing' stamp: 'mir 1/17/2002 12:57'!
encoding: aString	
	encoding _ aString! !

!XMLDocument methodsFor: 'accessing' stamp: 'mir 1/17/2002 12:57'!
requiredMarkup	
	^requiredMarkup! !

!XMLDocument methodsFor: 'accessing' stamp: 'mir 1/17/2002 12:57'!
requiredMarkup: aString	
	requiredMarkup _ aString! !

!XMLDocument methodsFor: 'accessing' stamp: 'mir 1/17/2002 12:57'!
version	
	^version! !

!XMLDocument methodsFor: 'accessing' stamp: 'mir 1/17/2002 12:57'!
version: aString	
	version _ aString! !

!XMLDocument methodsFor: 'printing' stamp: 'mir 1/17/2002 16:44'!
printCanonicalOn: aStream

	| writer |
	writer _ XMLWriter on: aStream.
	writer canonical: true.
	self printXMLOn: writer! !

!XMLDocument methodsFor: 'printing' stamp: 'mir 5/16/2003 14:08'!
printXMLOn: writer
	version ifNotNil: [writer xmlDeclaration: self version encoding: self encoding].
	super printXMLOn: writer! !

!XMLElement methodsFor: 'initialization' stamp: 'mir 8/14/2000 17:58'!
addContent: contentString
	self contents add: contentString! !

!XMLElement methodsFor: 'initialization' stamp: 'mir 1/17/2002 15:24'!
name: aString
	name _ aString asSymbol! !

!XMLElement methodsFor: 'initialization' stamp: 'mir 3/7/2000 16:43'!
setAttributes: newAttributes
	attributes _ newAttributes! !

!XMLElement methodsFor: 'accessing' stamp: 'mir 1/18/2001 16:55'!
attributeAt: attributeName
	^self attributeAt: attributeName ifAbsent: [nil]! !

!XMLElement methodsFor: 'accessing' stamp: 'jmv 5/7/2010 16:46'!
attributeAt: attributeName ifAbsent: aBlock
	^self attributes at: attributeName ifAbsent: aBlock! !

!XMLElement methodsFor: 'accessing' stamp: 'mir 1/17/2002 15:24'!
attributeAt: attributeName put: attributeValue
	self attributes at: attributeName asSymbol put: attributeValue! !

!XMLElement methodsFor: 'accessing' stamp: 'mas 10/15/2003 09:18'!
attributes
	^attributes ifNil: [attributes _ Dictionary new]! !

!XMLElement methodsFor: 'accessing' stamp: 'mir 3/7/2000 16:24'!
characterData
	^self contentString! !

!XMLElement methodsFor: 'accessing' stamp: 'mir 1/18/2001 16:27'!
contentString
	^(self contents size == 1
		and: [self contents first isKindOf: XMLStringNode])
		ifTrue: [self contents first string]
		ifFalse: ['']! !

!XMLElement methodsFor: 'accessing' stamp: 'mir 6/18/2003 13:47'!
contentStringAt: entityName
	^(self elementAt: entityName ifAbsent: [^'']) contentString! !

!XMLElement methodsFor: 'accessing' stamp: 'mir 8/14/2000 17:58'!
contents
	contents ifNil: [contents _ OrderedCollection new].
	^contents! !

!XMLElement methodsFor: 'accessing' stamp: 'sd 5/25/2003 18:25'!
tag
	^ self name asSymbol! !

!XMLElement methodsFor: 'accessing' stamp: 'mir 6/5/2003 12:02'!
valueFor: aSymbol 
	^self valueFor: aSymbol ifAbsent: ['']! !

!XMLElement methodsFor: 'accessing' stamp: 'mir 1/17/2002 15:28'!
valueFor: aSymbol ifAbsent: aBlock 
	^self attributes at: aSymbol ifAbsent: aBlock! !

!XMLElement methodsFor: 'enumerating' stamp: 'jmv 4/26/2010 15:19'!
attributesNamesDo: aBlock

	attributes ifNotNil: [
		attributes keysDo: aBlock ]! !

!XMLElement methodsFor: 'enumerating' stamp: 'mir 10/25/2000 11:15'!
contentsDo: aBlock
	contents
		ifNotNil: [
			self contents do: [:each | aBlock value: each]]! !

!XMLElement methodsFor: 'searching' stamp: 'mir 6/25/2003 13:34'!
firstTagNamed: aSymbol 
	"Return the first encountered node with the specified tag.
	If it is not the receiver, pass the message on"

	(self localName == aSymbol
		or: [self tag == aSymbol])
		ifTrue: [^self].
	^super firstTagNamed: aSymbol ! !

!XMLElement methodsFor: 'searching' stamp: 'mir 6/25/2003 13:34'!
firstTagNamed: aSymbol with: aBlock
	"Return the first encountered node with the specified tag that allows
	the block to evaluate to true. Pass the message on"

	((self localName == aSymbol
		or: [self tag == aSymbol])
 		and: [aBlock value: self])
		ifTrue: [^self].
	^super firstTagNamed: aSymbol with: aBlock.! !

!XMLElement methodsFor: 'searching' stamp: 'mir 6/25/2003 13:33'!
tagsNamed: aSymbol contentsDo: aBlock
	"Evaluate aBlock for all of the contents of the receiver
	if the receiver tag equals aSymbol. Pass the message on"

	(self localName == aSymbol
		or: [self tag == aSymbol])
		ifTrue: [self contentsDo: aBlock].
	super tagsNamed: aSymbol contentsDo: aBlock! !

!XMLElement methodsFor: 'searching' stamp: 'mir 6/25/2003 13:35'!
tagsNamed: aSymbol do: aOneArgumentBlock
	"If the receiver tag equals aSymbol, evaluate aOneArgumentBlock
	with the receiver. Continue the search"

	(self localName == aSymbol
		or: [self tag == aSymbol])
		ifTrue: [aOneArgumentBlock value: self].
	super tagsNamed: aSymbol do: aOneArgumentBlock! !

!XMLElement methodsFor: 'searching' stamp: 'mir 6/25/2003 13:35'!
tagsNamed: aSymbol ifReceiverDo: aOneArgumentBlock
	"If the receiver tag equals aSymbol, evaluate aOneArgumentBlock with the receiver"

	(self localName == aSymbol
		or: [self tag == aSymbol])
		ifTrue: [aOneArgumentBlock value: self]
! !

!XMLElement methodsFor: 'searching' stamp: 'mir 6/25/2003 13:35'!
tagsNamed: aSymbol ifReceiverDoAndRecurse: aOneArgumentBlock
	"If the receiver tag equals aSymbol, evaluate aOneArgumentBlock
	with the receiver. Then recurse through all the children"

	(self localName == aSymbol
		or: [self tag == aSymbol])
		ifTrue: [aOneArgumentBlock value: self].
	super tagsNamed: aSymbol ifReceiverDoAndRecurse: aOneArgumentBlock! !

!XMLElement methodsFor: 'searching' stamp: 'mir 6/25/2003 13:35'!
tagsNamed: aSymbol ifReceiverOrChildDo: aOneArgumentBlock
	"If the receiver tag equals aSymbol, evaluate aOneArgumentBlock with the receiver.
	For each of the receivers children do the same. Do not go beyond direct children"

	(self localName == aSymbol
		or: [self tag == aSymbol])
		ifTrue: [aOneArgumentBlock value: self].
	super tagsNamed: aSymbol ifReceiverDo: aOneArgumentBlock! !

!XMLElement methodsFor: 'testing' stamp: 'mir 3/6/2002 10:48'!
isEmpty
	^self elements isEmpty
		and: [self contents isEmpty]! !

!XMLElement methodsFor: 'testing' stamp: 'mir 1/17/2002 15:26'!
isTag
	^true! !

!XMLElement methodsFor: 'name space' stamp: 'sd 5/25/2003 18:24'!
localName
	^ name! !

!XMLElement methodsFor: 'name space' stamp: 'mir 6/5/2003 15:20'!
qualifiedName
	^self namespace
		ifNil: [self localName]
		ifNotNil: [self namespace , ':' , self localName]! !

!XMLElement methodsFor: 'printing' stamp: 'mir 1/17/2002 16:58'!
printXMLOn: writer
	writer startElement: self name attributeList: self attributes.
	(writer canonical not
		and: [self isEmpty and: [self attributes isEmpty not]])
		ifTrue: [writer endEmptyTag: self name]
		ifFalse: [
			writer endTag.
			self contentsDo: [:content | content printXMLOn: writer].
			super printXMLOn: writer.
			writer endTag: self name]! !

!XMLElement class methodsFor: 'instance creation' stamp: 'pb 1/30/2016 18:36'!
named: aString
	^ self basicNew name: aString.! !

!XMLElement class methodsFor: 'instance creation' stamp: 'pb 1/30/2016 18:37'!
named: aString attributes: attributeList
	^ self basicNew
		name: aString;
		setAttributes: attributeList.! !

!XMLElement class methodsFor: 'instance creation' stamp: 'pb 1/30/2016 18:37'!
named: aString namespace: ns uri: uri attributes: attributeList
	^ self basicNew
		name: aString;
		
			namespace: ns
			uri: uri;
		setAttributes: attributeList.! !

!XMLElement class methodsFor: 'instance creation' stamp: 'pb 1/30/2016 19:38'!
new
	"Call instance creation helper methods (such as #named:) instead"
	self shouldNotImplement.! !

!XMLPI methodsFor: 'accessing' stamp: 'mir 1/17/2002 13:02'!
data
	^data! !

!XMLPI methodsFor: 'accessing' stamp: 'mir 1/17/2002 13:02'!
data: aString
	data _ aString! !

!XMLPI methodsFor: 'accessing' stamp: 'mir 1/17/2002 13:02'!
target
	^target! !

!XMLPI methodsFor: 'accessing' stamp: 'mir 1/17/2002 13:02'!
target: aString
	target _ aString! !

!XMLPI methodsFor: 'testing' stamp: 'mir 1/17/2002 15:28'!
isProcessingInstruction
	^true! !

!XMLPI methodsFor: 'printing' stamp: 'mir 1/17/2002 15:53'!
printXMLOn: writer
	writer pi: self target data: self data! !

!XMLPI class methodsFor: 'instance creation' stamp: 'pb 1/30/2016 19:38'!
new
	"Call #target:data: instead"
	self shouldNotImplement.! !

!XMLPI class methodsFor: 'instance creation' stamp: 'pb 1/30/2016 18:36'!
target: targetName data: aString
	^ self basicNew
		target: targetName;
		data: aString.! !

!XMLStringNode methodsFor: 'accessing'!
characterData
	^self string! !

!XMLStringNode methodsFor: 'accessing' stamp: 'mir 10/25/2000 11:28'!
string
	^string ifNil: ['']! !

!XMLStringNode methodsFor: 'accessing' stamp: 'mir 10/25/2000 11:28'!
string: aString
	string _ aString! !

!XMLStringNode methodsFor: 'testing' stamp: 'mir 1/17/2002 15:27'!
isText
	^true! !

!XMLStringNode methodsFor: 'printing' stamp: 'mir 1/17/2002 15:53'!
printXMLOn: writer
	writer pcData: self string! !

!XMLStringNode class methodsFor: 'instance creation' stamp: 'pb 1/30/2016 19:38'!
new
	"Call #string: instead"
	self shouldNotImplement.! !

!XMLStringNode class methodsFor: 'instance creation' stamp: 'pb 1/30/2016 18:36'!
string: aString
	^ self basicNew string: aString.! !

!XMLTokenizer methodsFor: 'streaming' stamp: 'mir 3/14/2003 22:58'!
atEnd
	nestedStreams == nil
		ifTrue: [^peekChar == nil and: [stream atEnd]].
	^stream atEnd
		ifTrue: [
			self popNestingLevel.
			self atEnd]
		ifFalse: [false]! !

!XMLTokenizer methodsFor: 'streaming' stamp: 'mir 3/14/2003 22:59'!
checkNestedStream
	nestedStreams == nil
		ifFalse: [(peekChar == nil and: [self stream atEnd])
			ifTrue: [
				self popNestingLevel.
				self checkNestedStream]]
! !

!XMLTokenizer methodsFor: 'streaming' stamp: 'mir 6/28/2001 16:45'!
hasNestedStreams
	^nestedStreams notNil! !

!XMLTokenizer methodsFor: 'streaming' stamp: 'mir 3/14/2003 23:04'!
next
	"Return the next character from the current input stream. If the current stream is at end pop to next nesting level if there is one.
	Due to the potential nesting of original document, included documents and replacment texts the streams are held in a stack representing the nested streams. The current stream is the top one."
	| nextChar |
	peekChar
		ifNil: [
			nestedStreams ifNotNil: [self checkNestedStream].
			^nextChar _ stream next]
		ifNotNil: [
			nextChar _ peekChar.
			peekChar _ nil.
			^nextChar].
	! !

!XMLTokenizer methodsFor: 'streaming' stamp: 'jmv 12/12/2017 22:05:08'!
nextTrimmedBlanksUpTo: delimiter
	| resultStream nextChar |
	resultStream := WriteStream on: (String new: 10).
	nextChar := nil.
	[(nextChar := self next) == delimiter]
		whileFalse: [
			nextChar isSeparator ifFalse: [
				resultStream nextPut: nextChar]].
	nextChar == delimiter
		ifFalse: [self parseError: 'XML no delimiting ' , delimiter printString , ' found'].
	^resultStream contents
! !

!XMLTokenizer methodsFor: 'streaming' stamp: 'mir 5/14/2003 18:44'!
nextUpTo: delimiter
	| resultStream nextChar |
	resultStream _ WriteStream on: (String new: 10).
	[self atEnd or: [(nextChar _ self next) == delimiter]]
		whileFalse: [resultStream nextPut: nextChar].
	nextChar == delimiter
		ifFalse: [self parseError: 'XML no delimiting ' , delimiter printString , ' found'].
	^resultStream contents
! !

!XMLTokenizer methodsFor: 'streaming' stamp: 'mir 1/9/2002 15:29'!
nextUpToAll: delimitingString
	| string |
	self unpeek.
	string _ self stream upToAll: delimitingString.
	self stream skip: delimitingString size negated.
	(self stream next: delimitingString size) = delimitingString
		ifFalse: [self parseError: 'XML no delimiting ' , delimitingString printString , ' found'].
	^string
! !

!XMLTokenizer methodsFor: 'streaming' stamp: 'mir 3/14/2003 23:05'!
peek
	"Return the next character from the current input stream. If the current stream poop to next nesting level if there is one.
	Due to the potential nesting of original document, included documents and replacment texts the streams are held in a stack representing the nested streams. The current stream is the top one."
	peekChar
		ifNil: [
			nestedStreams ifNotNil: [self checkNestedStream].
			^peekChar _ stream next]
		ifNotNil: [^peekChar]! !

!XMLTokenizer methodsFor: 'streaming' stamp: 'mir 6/29/2001 00:36'!
popNestingLevel
	self hasNestedStreams
		ifTrue: [
			self stream close.
			self stream: self nestedStreams removeLast.
			self nestedStreams size > 0
				ifFalse: [nestedStreams _ nil]]! !

!XMLTokenizer methodsFor: 'streaming' stamp: 'mir 1/16/2002 10:54'!
pushStream: newStream
	"Continue parsing from the new nested stream."
	self unpeek.
	self nestedStreams addLast: self stream.
	self stream: newStream! !

!XMLTokenizer methodsFor: 'streaming' stamp: 'mir 3/14/2003 22:54'!
skipSeparators
	| nextChar |
	[((nextChar _ self peek) == nil)
		or: [SeparatorTable at: nextChar asciiValue+1]]
		whileFalse: [self next].
	(nestedStreams == nil or: [self atEnd not])
		ifFalse: [
			self checkNestedStream.
			self skipSeparators]! !

!XMLTokenizer methodsFor: 'streaming' stamp: 'mir 1/16/2002 10:42'!
skipUpTo: delimiter
	| nextChar |
	self unpeek.
	[self atEnd or: [(nextChar _ self next) == delimiter]]
		whileFalse: [].
	nextChar == delimiter
		ifFalse: [self parseError: 'XML no delimiting ' , delimiter printString , ' found']
! !

!XMLTokenizer methodsFor: 'streaming' stamp: 'mir 1/17/2002 14:31'!
topStream
	^self hasNestedStreams
		ifTrue: [self nestedStreams first]
		ifFalse: [self stream]! !

!XMLTokenizer methodsFor: 'streaming' stamp: 'jmv 6/15/2016 14:35'!
unpeek
	peekChar
		ifNotNil: [
			self stream skip: -1.
			peekChar _ nil]! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'jmv 6/7/2018 11:48:21'!
checkAndExpandReference: parsingContext
	| referenceString nextChar |
	nextChar _ self peek.
	self validating
		ifFalse: [^nil].
	nextChar == $&
		ifTrue: [
			self next.
			self peek == $#
				ifTrue: [
					"Disabled. Keep Numeric Character References verbatim."
					false ifTrue: [^self pushStream: (ReadStream on: self nextCharReference asString)].
					self pushStream: (ReadStream on: '&').
					^ self ].
			referenceString _ self nextLiteral.
			self next == $;
				ifFalse: [self errorExpected: ';'].
			self handleEntity: referenceString in: parsingContext ]
		ifFalse: [
			((nextChar == $%
				and: [self parsingMarkup])
				and: [parsingContext == #entityValue])
				ifTrue: [
					self skipSeparators.
					referenceString _ self nextLiteral.
					self handleEntity: referenceString in: parsingContext]].

	self atEnd ifTrue: [self errorExpected: 'Character expected.']! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 11/16/2000 21:41'!
conditionalInclude: conditionalKeyword
	conditionalKeyword = 'INCLUDE'
		ifTrue: [^true].
	conditionalKeyword = 'IGNORE'
		ifTrue: [^false].
	^self conditionalInclude: (self parameterEntity: conditionalKeyword) value! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 6/5/2003 16:32'!
nextAttributeInto: attributes namespaces: namespaces

	| attrName attrValue |
	attrName _ self nextName.
	self skipSeparators.
	self next == $=
		ifFalse: [self errorExpected: '='].
	self skipSeparators.
	attrValue _ self nextAttributeValue.

	(self usesNamespaces
		and: [(attrName findString: 'xmlns') = 1])
		ifTrue: [attrName size > 6
			ifTrue: [namespaces at: (attrName copyFrom: 7 to: attrName size) put: attrValue]
			ifFalse: [namespaces at: attrName put: attrValue]]
		ifFalse: [attributes at: attrName put: attrValue]! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'jmv 6/24/2009 12:56'!
nextAttributeValue
	| delimiterChar attributeValueStream nextChar nextPeek referenceString entity entityValue c |
	delimiterChar _ self next.
	(delimiterChar == $"
		or: [delimiterChar == $'])
		ifFalse: [self errorExpected: 'Attribute value delimiter expected.'].
	attributeValueStream _ attributeBuffer reset.
	[
		nextPeek _ nextChar _ self next.
		nextChar ifNil: [self errorExpected: 'Character expected.'].
		nextChar == $&
			ifTrue: [
				self peek == $#
					ifTrue: [
						nextPeek _ nil.
						nextChar _ self nextCharReference]
					ifFalse: [
						referenceString _ self nextLiteral.
						c _ self next.
						c == $;
							ifFalse: [
								"self errorExpected: ';'" "Just ignore errors. consider badformed xml as a simple literal string"
								attributeValueStream
									nextPut: nextChar;
									nextPutAll: referenceString;	"si anda, es de casualidad"
									nextPut: c.
								nextChar _ nil
								]
							ifTrue: [
								entity _ self entity: referenceString.
								entityValue _ entity valueForContext: #content.
								(self class isCharEscape: entityValue)
									ifTrue: [
										nextPeek _ nil.
										nextChar _ entityValue]
									ifFalse: [
										entityValue _ entityValue asString.
										entityValue isEmpty
											ifTrue: [nextPeek _ nextChar _ nil]
											ifFalse: [
												self pushStream: (ReadStream on: entityValue asString).
												nextPeek _ nextChar _ self next]]]]].
		nextPeek == delimiterChar
	]
		whileFalse: [
			nextChar ifNotNil: [attributeValueStream nextPut: nextChar]].
	^self fastStreamStringContents: attributeValueStream
"	^attributeValueStream contents"! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 1/17/2002 17:00'!
nextCDataContent
	| cdata |
	"Skip $[ "
	self next.
	cdata _ self nextUpToAll: ']]>'.
	self handleCData: cdata
! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 12/6/2000 14:29'!
nextCDataOrConditional

	| nextChar conditionalKeyword |
	"Skip ["
	self next.
	self skipSeparators.
	nextChar _ self peek.
	nextChar == $%
		ifTrue: [
			self checkAndExpandReference: (self parsingMarkup ifTrue: [#dtd] ifFalse: [#content]).
			conditionalKeyword _ self nextLiteral.
			self skipSeparators.
			^self next == $[
				ifTrue: [
						self skipSeparators.
						self nextIncludeSection: (self conditionalInclude: conditionalKeyword)]
				ifFalse: [self errorExpected: '[' ]].

	nextChar == $C
		ifTrue: [
			^self nextLiteral = 'CDATA'
				ifTrue: [self peek == $[
							ifTrue: [self nextCDataContent]
							ifFalse: [self errorExpected: '[' ]]
				ifFalse: [self errorExpected: 'CData']].
	self errorExpected: 'CData or declaration'
! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'jmv 12/29/2020 14:47:38'!
nextCharReference
	"Note: Should not be used inside attribute values. Leave them verbatim.
	For uses elsewhere, I really don't know. (jmv)"
	| base asciiCodeOrCodePoint |

	self next == $#
		ifFalse: [self errorExpected: 'character reference'].
	base _ self peek == $x
		ifTrue: [
			self next.
			16]
		ifFalse: [10].

	asciiCodeOrCodePoint _ [self readNumberBase: base] on: Error do: [:ex | self errorExpected: 'Number.'].
	(self next) == $;
		ifFalse: [self errorExpected: '";"'].

	^Character codePoint: asciiCodeOrCodePoint trySimilar: true! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 11/28/2000 17:54'!
nextComment
	| string |
	"Skip first -"
	self next.
	self next == $-
		ifFalse: [self errorExpected: 'second comment $-'].
	string _ self nextUpToAll: '-->'.
	self handleComment: string! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 3/14/2003 23:27'!
nextEndTag
	| string |
	"Skip /"
	self next.
	self skipSeparators.
	string _ self nextTrimmedBlanksUpTo: $>.
	"string _ (self nextUpTo: $>) withBlanksTrimmed."
	self handleEndTag: string! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'cwp 6/17/2003 21:04'!
nextEntity
	"return the next XMLnode, or nil if there are no more"

	"branch, depending on what the first character is"
	self nextWhitespace.
	self atEnd ifTrue: [self handleEndDocument. ^ nil].
	self checkAndExpandReference: (self parsingMarkup ifTrue: [#dtd] ifFalse: [#content]).
	^self peek = $<
		ifTrue: [self nextNode]
		ifFalse: [self nextPCData]! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 1/17/2002 18:14'!
nextEntityValue
	| delimiterChar entityValueStream nextChar nextPeek referenceString entity entityValue |
	delimiterChar _ self next.
	(delimiterChar == $"
		or: [delimiterChar == $'])
		ifFalse: [self errorExpected: 'Entity value delimiter expected.'].

	entityValueStream _ WriteStream on: (String new).
	[
	nextPeek _ nextChar _ self peek.
	nextChar ifNil: [self errorExpected: 'Character expected.'].
	nextChar == $&
		ifTrue: [
			self next.
			self peek == $#
				ifTrue: [
					nextPeek _ nil.
					nextChar _ self nextCharReference]
				ifFalse: [
					referenceString _ self nextLiteral.
					self next == $;
						ifFalse: [self errorExpected: ';'].
					entity _ self entity: referenceString.
					entityValue _ entity valueForContext: #entityValue.
					self pushStream: (ReadStream on: entityValue asString).
					nextPeek _ nextChar _ self next]]
		ifFalse: [
			nextChar == $%
				ifTrue: [
					self skipSeparators.
					referenceString _ self nextLiteral.
					nextChar _ self handleEntity: referenceString in: #entityValue.
					nextPeek _ nextChar _ self next]
				ifFalse: [self next]].
	nextPeek == delimiterChar]
		whileFalse: [
			nextChar ifNotNil: [entityValueStream nextPut: nextChar]].
	^entityValueStream contents! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 6/28/2001 16:38'!
nextIncludeSection: parseSection
	| section |
	"Read the file up to the next include section delimiter and parse it if parseSection is true"

	
	section _ self nextUpToAll: ']]>'.
	parseSection
		ifTrue: [
			self pushStream: (ReadStream on: section)]! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'jmv 6/7/2018 11:43:40'!
nextLiteral
	| resultStream nextChar resultString |
	resultStream _ (String new: 10) writeStream.
	((nextChar _ self peek) isLetter
		or: [nextChar == $_])
		ifFalse: [self errorExpected: 'Name literal.'].
	[nextChar _ self peek.
	(LiteralChars at: nextChar asciiValue+1)
		ifTrue: [
			nextChar == $&
				ifTrue: [
					nextChar _ self next.
					self peek == $#
						ifTrue: [resultStream nextPut: self nextCharReference]
						ifFalse: [^resultStream contents]]
				ifFalse: [
					resultStream nextPut: self next]]
		ifFalse: [resultString _ resultStream contents.
			resultString isEmpty
				ifTrue: [self errorExpected: 'Name literal']
				ifFalse: [^resultString]]] repeat! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 3/14/2003 23:10'!
nextName
	| nextChar |
	nameBuffer reset.
	self peek == $.
		ifTrue: [self malformedError: 'Character expected.'].
	[(nextChar _ self peek)
		ifNil: [self errorExpected: 'Character expected.'].
	NameDelimiters at: nextChar asciiValue] whileFalse: [
			nameBuffer nextPut: self next].
	^self fastStreamStringContents: nameBuffer
"	^nameBuffer contents"! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 11/28/2000 17:52'!
nextNode
	| nextChar |
	"Skip < "
	self next.
	nextChar _ self peek.
	nextChar == $!! ifTrue: [
		"Skip !!"
		self next.
		nextChar _ self peek.
		nextChar == $- ifTrue: [^self nextComment].
		nextChar == $[ ifTrue: [^self nextCDataOrConditional].
		^self parsingMarkup
			ifTrue: [self nextMarkupDeclaration]
			ifFalse: [self nextDocType]].
	nextChar == $? ifTrue: [^self nextPI].
	^self nextTag! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'jmv 6/7/2018 11:52:30'!
nextPCData
	| resultStream nextChar referenceString entity entityValue nextPeek p |
	resultStream _ (String new: 10) writeStream.
	self validating
		ifFalse: [
			[
				p _ self peek.
				p isNil or: [p == $<]
				]
				whileFalse: [resultStream nextPut: self next].
			^self handlePCData: resultStream contents].

	[
	nextPeek _ nextChar _ self peek.
	nextChar ifNil: [self errorExpected: 'Character expected.'].
	nextChar == $&
		ifTrue: [
			self next.
			self peek == $#
				ifTrue: [
					"Disabled. Keep Numeric Character References verbatim."
					false ifTrue: [
						nextPeek _ nil.
						nextChar _ self nextCharReference]]
				ifFalse: [
					referenceString _ self nextLiteral.
					self next == $;
						ifFalse: [self errorExpected: ';'].
					entity _ self entity: referenceString.
					entityValue _ entity valueForContext: #content.
					(self class isCharEscape: entityValue)
						ifTrue: [
							nextPeek _ nil.
							nextChar _ entityValue]
						ifFalse: [
							entityValue _ entityValue asString.
							entityValue isEmpty
								ifTrue: [nextPeek _ nextChar _ nil]
								ifFalse: [
									self pushStream: (ReadStream on: entityValue asString).
									nextPeek _ nextChar _ self peek]]]]
		ifFalse: [nextPeek == $< ifFalse: [self next]].
	nextPeek == $<]
		whileFalse: [
			nextChar ifNotNil: [resultStream nextPut: nextChar]].
	self handlePCData: resultStream contents! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 1/17/2002 13:00'!
nextPI
	| piTarget piData |
	"Skip ?"
	self next.
	piTarget _ self nextLiteral.
	piTarget asUppercase = 'XML'
		ifTrue: [^self nextXMLDecl].
	self skipSeparators.
	piData _ self nextUpToAll: '?>'.
	self handlePI: piTarget data: piData! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 1/17/2002 14:25'!
nextPubidLiteral
	^self nextAttributeValue! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 1/17/2002 14:25'!
nextSystemLiteral
	^self nextAttributeValue! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 6/5/2003 22:37'!
nextTag
	| tagName attributes nextChar namespaces |
	(self peek = $/)
		ifTrue: [^self nextEndTag].
	tagName _ self nextName.
	self skipSeparators.
	attributes _ Dictionary new: 33.
	namespaces _ Dictionary new: 5.
	[(nextChar _ self peek) == $> or: [nextChar == $/]] whileFalse: [
		self checkAndExpandReference: #content.
		self nextAttributeInto: attributes namespaces: namespaces.
		self skipSeparators.].
	self handleStartTag: tagName attributes: attributes namespaces: namespaces.
	self next == $/
		ifTrue: [
			self handleEndTag: tagName.
			self next].
	! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mu 11/9/2005 09:43'!
nextWhitespace
	| nextChar resultStream resultString|
	resultStream _ (String new: 10) writeStream.
	[((nextChar _ self peek) == nil)
		or: [SeparatorTable at: nextChar asciiValue+1 ifAbsent:[true]]]
		whileFalse: [resultStream nextPut: nextChar. self next].
	(nestedStreams == nil or: [self atEnd not])
		ifFalse: [self checkNestedStream.
				self nextWhitespace].
	resultString _ resultStream contents.
	resultString isEmpty ifFalse: [self handleWhitespace: resultString].! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 6/5/2003 14:52'!
nextXMLDecl
	| attributes nextChar namespaces |
	self skipSeparators.
	attributes _ Dictionary new.
	namespaces _ Dictionary new.
	[(nextChar _ self peek) == $?] whileFalse: [
		self nextAttributeInto: attributes namespaces: namespaces.
		self skipSeparators.].
	self next.
	self next == $>
		ifFalse: [self errorExpected: '> expected.'].
	self handleXMLDecl: attributes namespaces: namespaces! !

!XMLTokenizer methodsFor: 'tokenizing dtd' stamp: 'mir 6/29/2001 00:08'!
endDocTypeDecl
	"Skip ]>"
	self next; next.
	^nil! !

!XMLTokenizer methodsFor: 'tokenizing dtd' stamp: 'mir 1/8/2002 13:54'!
nextDocType
	| declType |
	declType _ self nextLiteral.
	declType = 'DOCTYPE'
		ifTrue: [
			self startParsingMarkup.
			^self nextDocTypeDecl].
	self errorExpected: 'markup declaration, not ' , declType printString! !

!XMLTokenizer methodsFor: 'tokenizing dtd' stamp: 'mir 1/17/2002 17:29'!
nextDocTypeDecl
	| nextChar |
	self skipSeparators.
	self nextLiteral.
	self skipSeparators.
	self peek == $[
		ifFalse: [[nextChar _ self peek.
				nextChar == $> or: [nextChar == $[ ]] whileFalse: [self next]].
	self peek == $[
		ifTrue: [
			self next.
			[self skipSeparators.
			self peek == $]] whileFalse: [
				self checkAndExpandReference: #dtd.
				self nextNode].
			self next == $] 
				ifFalse: [self errorExpected: ']' ]].
	self skipSeparators.
	self next == $>
		ifFalse: [self errorExpected: '>' ].

	self endParsingMarkup! !

!XMLTokenizer methodsFor: 'tokenizing dtd' stamp: 'mir 1/17/2002 14:24'!
nextEntityDeclaration
	| entityName entityDef referenceClass reference |
	self skipSeparators.
	referenceClass _ self peek == $%
		ifTrue: [
			self next.
			self skipSeparators.
			DTDParameterEntityDeclaration]
		ifFalse: [DTDEntityDeclaration].
	entityName _ self nextLiteral.
	self skipSeparators.
	entityDef _ (self peek == $" or: [self peek == $'])
		ifTrue: [self nextEntityValue]
		ifFalse: [self nextExternalId].
	self skipUpTo: $>.
	reference _ referenceClass name: entityName value: entityDef.
	reference registerIn: self.
	^reference! !

!XMLTokenizer methodsFor: 'tokenizing dtd' stamp: 'jmv 10/27/2015 18:09'!
nextExternalId
	| extDefType systemId entry |
	extDefType _ self nextLiteral.
	extDefType = 'PUBLIC'
		ifTrue: [
			self skipSeparators.
			self nextPubidLiteral.
			self skipSeparators.
			self peek == $>
				ifFalse: [
					systemId _ self nextSystemLiteral]].

	extDefType = 'SYSTEM'
		ifTrue: [
			self skipSeparators.
			systemId _ self nextSystemLiteral].

	systemId
		ifNil: [^nil].

	"The rest of this method only applies if we're reading aFileStream"
	(self topStream isKindOf: FileStream)
		ifFalse: [^''].
	entry _ self topStream directory // systemId.
	^entry exists
		ifTrue: [entry fileContents]
		ifFalse: ['']! !

!XMLTokenizer methodsFor: 'tokenizing dtd' stamp: 'mir 1/17/2002 13:49'!
nextMarkupDeclaration
	| declType |
	declType _ self nextLiteral.
	self validating
		ifFalse: [^self skipMarkupDeclaration].
	declType = 'ENTITY'
		ifTrue: [self nextEntityDeclaration]
		ifFalse: [self skipMarkupDeclaration]! !

!XMLTokenizer methodsFor: 'tokenizing dtd' stamp: 'mir 1/4/2002 11:05'!
skipMarkupDeclaration
	self skipUpTo: $>! !

!XMLTokenizer methodsFor: 'private' stamp: 'mir 11/13/2000 18:19'!
endParsingMarkup
	parsingMarkup _ false! !

!XMLTokenizer methodsFor: 'private' stamp: 'mir 3/14/2003 23:09'!
fastStreamStringContents: writeStream
	| newSize |
	newSize _ writeStream position.
	^(String new: newSize)
		replaceFrom: 1
		to: newSize
		with: writeStream originalContents
		startingAt: 1! !

!XMLTokenizer methodsFor: 'private' stamp: 'mir 12/7/2000 16:46'!
log: aString
	"Transcript show: aString; cr"! !

!XMLTokenizer methodsFor: 'private' stamp: 'mir 6/28/2001 16:54'!
nestedStreams
	nestedStreams ifNil: [nestedStreams _ OrderedCollection new].
	^nestedStreams! !

!XMLTokenizer methodsFor: 'private' stamp: 'mir 11/13/2000 18:19'!
parsingMarkup
	^parsingMarkup! !

!XMLTokenizer methodsFor: 'private' stamp: 'mu 11/14/2005 14:12'!
readNumberBase: base
	"Read a hex number from stream until encountering $; "

	| value digit |

	base = 10 ifFalse: [	| numberString | 
		numberString _ self nextUpTo: $;.
		self stream skip: -1.
		^Integer readFrom: numberString asUppercase readStream base: base. 
	].

	value _ 0.
	digit _ DigitTable at: self peek asciiValue.
	digit < 0
		ifTrue: [self error: 'At least one digit expected here'].
	self next.
	value _ digit.
	[digit _ DigitTable at: self peek asciiValue.
	digit < 0
		ifTrue: [^value]
		ifFalse: [
			self next.
			value _ value * base + digit]
		] repeat.
	^ value! !

!XMLTokenizer methodsFor: 'private' stamp: 'mir 11/13/2000 18:19'!
startParsingMarkup
	parsingMarkup _ true! !

!XMLTokenizer methodsFor: 'private' stamp: 'mir 6/28/2001 16:50'!
stream
	^stream! !

!XMLTokenizer methodsFor: 'private' stamp: 'mir 6/28/2001 16:50'!
stream: newStream
	"Continue parsing from the new nested stream."
	stream _ newStream! !

!XMLTokenizer methodsFor: 'entities' stamp: 'mir 1/14/2002 15:06'!
entities
	entities ifNil: [entities _ self initEntities].
	^entities! !

!XMLTokenizer methodsFor: 'entities' stamp: 'mir 1/17/2002 13:53'!
entity: refName
	^self validating
		ifTrue: [self entities
			at: refName
			ifAbsentPut: [self parseError: 'XML undefined entity ' , refName printString]]
		ifFalse: [DTDEntityDeclaration name: refName value: '']
! !

!XMLTokenizer methodsFor: 'entities' stamp: 'mir 11/16/2000 21:43'!
entity: refName put: aReference
	"Only the first declaration of an entity is valid so if there is already one don't register the new value."
	self entities at: refName ifAbsentPut: [aReference]! !

!XMLTokenizer methodsFor: 'entities' stamp: 'mir 1/14/2002 17:59'!
externalEntities
	externalEntities ifNil: [externalEntities _ Dictionary new].
	^externalEntities! !

!XMLTokenizer methodsFor: 'entities' stamp: 'mir 1/14/2002 17:59'!
externalEntity: refName
	^self entities
		at: refName
		ifAbsentPut: ['']! !

!XMLTokenizer methodsFor: 'entities' stamp: 'mir 1/17/2002 18:12'!
handleEntity: referenceString in: parsingContext 

	| entity entityValue |
	entity _ self entity: referenceString.
	entityValue _ entity valueForContext: parsingContext.
	(self class isCharEscape: entityValue)
		ifTrue: [entityValue _ entity reference].
	self pushStream: (ReadStream on: entityValue asString)! !

!XMLTokenizer methodsFor: 'entities' stamp: 'mir 8/17/2004 16:52'!
initEntities
	| ents |
	ents _ Dictionary new.
	ents
		at: 'amp' put: (DTDEntityDeclaration name: 'amp' value: $&);
		at: 'quot' put: (DTDEntityDeclaration name: 'quot' value: $");
		at: 'apos' put: (DTDEntityDeclaration name: 'apos' value: $');
		at: 'gt' put: (DTDEntityDeclaration name: 'gt' value: $>);
		at: 'lt' put: (DTDEntityDeclaration name: 'lt' value: $<).
	^ents! !

!XMLTokenizer methodsFor: 'entities' stamp: 'mir 11/16/2000 21:20'!
parameterEntities
	parameterEntities ifNil: [parameterEntities _ Dictionary new].
	^parameterEntities! !

!XMLTokenizer methodsFor: 'entities' stamp: 'mir 11/16/2000 21:40'!
parameterEntity: refName
	^self parameterEntities
		at: refName
		ifAbsent: [self parseError: 'XML undefined parameter entity ' , refName printString]! !

!XMLTokenizer methodsFor: 'entities' stamp: 'mir 11/16/2000 21:42'!
parameterEntity: refName put: aReference
	"Only the first declaration of an entity is valid so if there is already one don't register the new value."
	self parameterEntities at: refName ifAbsentPut: [aReference]! !

!XMLTokenizer methodsFor: 'errors' stamp: 'mir 5/14/2003 18:27'!
errorExpected: expectedString
	| actualString |
	actualString := ''.
	self atEnd
		ifFalse: [
			[actualString := self next: 20]
				on: Error
				do: [:ex | ]].
	self parseError: 'XML expected ' , expectedString printString , ': ' , actualString! !

!XMLTokenizer methodsFor: 'errors' stamp: 'mir 1/9/2002 15:26'!
malformedError: errorString
	SAXMalformedException signal: errorString! !

!XMLTokenizer methodsFor: 'errors' stamp: 'mir 1/8/2002 15:37'!
parseError: errorString
	SAXParseException signal: errorString! !

!XMLTokenizer methodsFor: 'handling tokens' stamp: 'mir 11/13/2000 16:04'!
handleCData: aString
	self log: 'CData: ' , aString! !

!XMLTokenizer methodsFor: 'handling tokens' stamp: 'mir 8/14/2000 11:37'!
handleComment: aString
	self log: 'Comment: ' , aString! !

!XMLTokenizer methodsFor: 'handling tokens' stamp: 'mir 8/14/2000 18:27'!
handleEndDocument
	self log: 'End Doc '! !

!XMLTokenizer methodsFor: 'handling tokens' stamp: 'mir 8/14/2000 11:38'!
handleEndTag: aString
	self log: 'End tag: ' , aString! !

!XMLTokenizer methodsFor: 'handling tokens' stamp: 'mir 8/14/2000 11:38'!
handlePCData: aString
	self log: 'PCData: ' , aString! !

!XMLTokenizer methodsFor: 'handling tokens' stamp: 'mir 12/11/2000 16:10'!
handlePI: piTarget data: piData
	self log: 'PI: ' , piTarget , ' data ' , piData! !

!XMLTokenizer methodsFor: 'handling tokens' stamp: 'mir 8/14/2000 18:26'!
handleStartDocument
	self log: 'Start Doc'! !

!XMLTokenizer methodsFor: 'handling tokens' stamp: 'mir 8/14/2000 11:39'!
handleStartTag: tagName attributes: attributes
	self log: 'Start tag: ' , tagName.
	attributes keysAndValuesDo: [:key :value |
		self log: key , '->' , value]! !

!XMLTokenizer methodsFor: 'handling tokens' stamp: 'cwp 6/17/2003 21:08'!
handleWhitespace: aString
	self log: 'Whitespace: ' , aString! !

!XMLTokenizer methodsFor: 'handling tokens' stamp: 'mir 6/5/2003 14:53'!
handleXMLDecl: attributes namespaces: namespaces
	attributes keysAndValuesDo: [:key :value |
		self log: key , '->' , value]! !

!XMLTokenizer methodsFor: 'initialization' stamp: 'mir 3/14/2003 19:27'!
initialize
	parsingMarkup _ false.
	validating _ false.
	attributeBuffer _ WriteStream on: (String new: 128).
	nameBuffer _ WriteStream on: (String new: 128)! !

!XMLTokenizer methodsFor: 'accessing' stamp: 'mir 6/28/2001 16:51'!
parseStream: aStream
	self stream: aStream! !

!XMLTokenizer methodsFor: 'accessing' stamp: 'mir 1/14/2002 17:51'!
validating: aBoolean
	validating _ aBoolean! !

!XMLTokenizer methodsFor: 'testing' stamp: 'mir 6/5/2003 16:33'!
usesNamespaces
	^false! !

!XMLTokenizer methodsFor: 'testing' stamp: 'mir 1/14/2002 17:51'!
validating
	^validating! !

!XMLTokenizer class methodsFor: 'examples' stamp: 'mir 8/14/2000 11:41'!
addressBookXML
	^'<addressbook>
  <person employee-number="A0000" family-name="Gates" first-name="Bob">
    <contact-info><!!--Confidential--></contact-info>
    <address city="Los Angeles" number="1239" state="CA" street="Pine Rd."/>
    <job-info employee-type="Full-Time" is-manager="no" job-description="Manager"/>
    <manager employee-number="A0000"/>
  </person>
  <person employee-number="A7000" family-name="Brown"
    first-name="Robert" middle-initial="L.">
    <contact-info>
      <email address="robb@iro.ibm.com"/>
      <home-phone number="03-3987873"/>
    </contact-info>
    <address city="New York" number="344" state="NY" street="118 St."/>
    <job-info employee-type="Full-Time" is-manager="yes" job-description="Group Leader"/>
    <manager employee-number="A0000"/>
  </person>
  <person employee-number="A7890" family-name="DePaiva"
    first-name="Kassie" middle-initial="W.">
    <contact-info><!!-- Kassie''s agent phone: 03-987654 --></contact-info>
    <address city="Los Angeles" number="1234" state="CA" street="Pine Rd."/>
    <job-info employee-type="Full-Time" is-manager="no" job-description="Actor"/>
    <manager employee-number="A0000"/>
    <misc-info>One of the most talented actresses on Daytime. Kassie
      plays the devious and beautiful Blair Cramer on ABC&apos;s
      &quot;One Life To Live.&quot;</misc-info>
  </person>
  <person employee-number="A7987" family-name="Smith" first-name="Joe">
    <contact-info>
      <email address="joes@iro.ibm.com"/>
      <mobile-phone number="888-7657765"/>
      <home-phone number="03-8767898"/>
      <home-phone number="03-8767871"/>
    </contact-info>
    <address city="New York" number="12789" state="NY" street="W. 15th Ave."/>
    <job-info employee-type="Part-Time" is-manager="no" job-description="Hacker"/>
    <manager employee-number="A7000"/>
  </person>
</addressbook>
'! !

!XMLTokenizer class methodsFor: 'examples' stamp: 'mir 12/19/2005 16:10'!
addressBookXMLWithDTD
	^'<?xml version="1.0" encoding="UTF-8"?>
<!!DOCTYPE addressbook SYSTEM "addressbook.dtd">
<addressbook>
  <person employee-number="A0000" family-name="Gates" first-name="Bob">
    <contact-info><!!--Confidential--></contact-info>
    <address city="Los Angeles" number="1239" state="CA" street="Pine Rd."/>
    <job-info employee-type="Full-Time" is-manager="no" job-description="Manager"/>
    <manager employee-number="A0000"/>
  </person>
  <person employee-number="A7000" family-name="Brown"
    first-name="Robert" middle-initial="L.">
    <contact-info>
      <email address="robb@iro.ibm.com"/>
      <home-phone number="03-3987873"/>
    </contact-info>
    <address city="New York" number="344" state="NY" street="118 St."/>
    <job-info employee-type="Full-Time" is-manager="yes" job-description="Group Leader"/>
    <manager employee-number="A0000"/>
  </person>
  <person employee-number="A7890" family-name="DePaiva"
    first-name="Kassie" middle-initial="W.">
    <contact-info><!!-- Kassie''s agent phone: 03-987654 --></contact-info>
    <address city="Los Angeles" number="1234" state="CA" street="Pine Rd."/>
    <job-info employee-type="Full-Time" is-manager="no" job-description="Actor"/>
    <manager employee-number="A0000"/>
    <misc-info>One of the most talented actresses on Daytime. Kassie
      plays the devious and beautiful Blair Cramer on ABC&apos;s
      &quot;One Life To Live.&quot;</misc-info>
  </person>
  <person employee-number="A7987" family-name="Smith" first-name="Joe">
    <contact-info>
      <email address="joes@iro.ibm.com"/>
      <mobile-phone number="888-7657765"/>
      <home-phone number="03-8767898"/>
      <home-phone number="03-8767871"/>
    </contact-info>
    <address city="New York" number="12789" state="NY" street="W. 15th Ave."/>
    <job-info employee-type="Part-Time" is-manager="no" job-description="Hacker"/>
    <manager employee-number="A7000"/>
  </person>
</addressbook>
'! !

!XMLTokenizer class methodsFor: 'examples' stamp: 'mir 8/14/2000 11:41'!
exampleAddressBook
	| tokenizer |
	"XMLTokenizer exampleAddressBook"

	tokenizer _ XMLTokenizer on: self addressBookXML readStream.
	[tokenizer next notNil]
		whileTrue: []! !

!XMLTokenizer class methodsFor: 'examples' stamp: 'mir 8/14/2000 16:23'!
exampleAddressBookWithDTD
	| tokenizer |
	"XMLTokenizer exampleAddressBookWithDTD"

	tokenizer _ XMLTokenizer on: self addressBookXMLWithDTD readStream.
	[tokenizer next notNil]
		whileTrue: []! !

!XMLTokenizer class methodsFor: 'class initialization' stamp: 'mir 3/14/2003 22:54'!
initialize
	"XMLTokenizer initialize"

	| nameDelimiters |

	CharEscapes _ #( $& $" $' $> $< ) asSet.

	SeparatorTable  _ Array new: 256.
	SeparatorTable atAllPut: true.
	#(9 10 12 13 32) do: [:each | SeparatorTable at: each+1 put: false].

	LiteralChars _ Array new: 256.
	LiteralChars atAllPut: false.
	':-_.' do: [:each | LiteralChars at: each asciiValue put: true].
	1 to: 256 do: [:i | ((i-1) asCharacter isDigit or: [(i-1) asCharacter isLetter])
		ifTrue: [LiteralChars at: i put: true]].

	nameDelimiters _ #(9 10 12 13 32 61 "$= asInteger 61" 62 "$> asInteger" 47 "$/ asInteger").
	NameDelimiters _ Array new: 256.
	NameDelimiters atAllPut: false.
	nameDelimiters do: [:each | NameDelimiters at: each put: true].

	DigitTable _ Array new: 256.
	DigitTable atAllPut: -1.
	($0 to: $9) do: [:each | DigitTable at: each asciiValue put: each digitValue].
	($A to: $F) do: [:each | DigitTable at: each asciiValue put: each digitValue].
! !

!XMLTokenizer class methodsFor: 'accessing' stamp: 'mir 1/15/2002 21:39'!
isCharEscape: aChar
	^CharEscapes includes: aChar! !

!XMLTokenizer class methodsFor: 'instance creation' stamp: 'mir 11/16/2000 07:58'!
on: aStream
	^self new parseStream: aStream! !

!SAXDriver methodsFor: 'handling tokens' stamp: 'mir 1/16/2002 00:33'!
handleCData: aString
	self saxHandler
		checkEOD; 
		characters: aString! !

!SAXDriver methodsFor: 'handling tokens' stamp: 'cwp 6/17/2003 18:26'!
handleComment: aString
	self saxHandler
		checkEOD; 
		comment: aString! !

!SAXDriver methodsFor: 'handling tokens' stamp: 'mir 1/8/2002 18:38'!
handleEndDocument
	self saxHandler endDocument! !

!SAXDriver methodsFor: 'handling tokens' stamp: 'mir 6/24/2003 13:36'!
handleEndTag: elementName
	| namespace localName namespaceURI qualifiedName |

	self usesNamespaces
		ifTrue: [
			self splitName: elementName into: [:ns :ln |
				namespace _ ns.
				localName _ ln].

			"ensure our namespace is defined"
			namespace
				ifNil: [
					namespace := self scope defaultNamespace.
					qualifiedName := namespace , ':' , elementName]
				ifNotNil: [
					namespaceURI := self scope namespaceURIOf: namespace.
					namespaceURI
						ifNil: [self parseError: 'Start tag ' , elementName , ' refers to undefined namespace ' , namespace asString].
					qualifiedName := elementName].

			"call the handler"
			self saxHandler
				checkEOD; 
				endElement: localName namespace: namespace namespaceURI: namespaceURI qualifiedName: qualifiedName.
			self scope leaveScope]
		ifFalse: [
			"call the handler"
			self saxHandler
				checkEOD; 
				endElement: elementName namespace: nil namespaceURI: nil qualifiedName: elementName]! !

!SAXDriver methodsFor: 'handling tokens' stamp: 'mir 1/8/2002 18:24'!
handlePCData: aString
	self saxHandler
		checkEOD; 
		characters: aString! !

!SAXDriver methodsFor: 'handling tokens' stamp: 'mir 1/8/2002 18:24'!
handlePI: piTarget data: piData
	self saxHandler
		checkEOD; 
		processingInstruction: piTarget data: piData! !

!SAXDriver methodsFor: 'handling tokens' stamp: 'mir 8/14/2000 18:29'!
handleStartDocument
	self saxHandler startDocument! !

!SAXDriver methodsFor: 'handling tokens' stamp: 'mir 6/24/2003 13:35'!
handleStartTag: elementName attributes: attributeList namespaces: namespaces

	| localName namespace namespaceURI |

	self usesNamespaces
		ifTrue: [
			self scope enterScope.
				"declare any namespaces"	
				namespaces keysAndValuesDo: [:ns :uri |
					self scope declareNamespace: ns uri: uri].

			self splitName: elementName into: [:ns :ln |
				namespace _ ns.
				localName _ ln].

			"ensure our namespace is defined"
			namespace
				ifNil: [namespace := self scope defaultNamespace]
				ifNotNil: [
					namespaceURI := self scope namespaceURIOf: namespace.
					namespaceURI
						ifNil: [self parseError: 'Start tag ' , elementName , ' refers to undefined namespace ' , namespace asString]].

			self validatesAttributes
				ifTrue: [self scope validateAttributes: attributeList].
			"call the handler"
			self saxHandler
				checkEOD; 
				startElement: localName namespaceURI: namespaceURI namespace: namespace attributeList: attributeList]
		ifFalse: [
			"call the handler"
			self saxHandler
				checkEOD; 
				startElement: elementName namespaceURI: nil namespace: nil attributeList: attributeList]! !

!SAXDriver methodsFor: 'handling tokens' stamp: 'cwp 6/18/2003 01:00'!
handleWhitespace: aString
	self saxHandler
		checkEOD; 
		ignorableWhitespace: aString! !

!SAXDriver methodsFor: 'handling tokens' stamp: 'mir 6/24/2003 13:39'!
handleXMLDecl: attributes namespaces: namespaces
	self saxHandler
		checkEOD; 
		documentAttributes: attributes.
	self usesNamespaces
		ifTrue: [
			namespaces keysAndValuesDo: [:ns :uri |
				self scope declareNamespace: ns uri: uri]]! !

!SAXDriver methodsFor: 'initialization' stamp: 'mir 6/5/2003 16:29'!
initialize
	super initialize.
	useNamespaces := false.
	validateAttributes := false! !

!SAXDriver methodsFor: 'accessing' stamp: 'mir 8/11/2000 17:51'!
saxHandler
	^saxHandler! !

!SAXDriver methodsFor: 'accessing' stamp: 'mir 8/11/2000 17:52'!
saxHandler: aHandler
	saxHandler _ aHandler! !

!SAXDriver methodsFor: 'accessing' stamp: 'mir 6/24/2003 14:51'!
useNamespaces: aBoolean
	useNamespaces := aBoolean! !

!SAXDriver methodsFor: 'namespaces' stamp: 'mir 6/24/2003 13:40'!
scope
	scope ifNil: [scope := XMLNamespaceScope new].
	^scope! !

!SAXDriver methodsFor: 'namespaces' stamp: 'mir 6/16/2003 13:09'!
splitName: aName into: twoArgsBlock
	"Split the name into namespace and local name (the block arguments).
	Handle both qualified and unqualified names using the default name space"

	| i ns ln |
	i _ aName lastIndexOf: $:.
	i = 0
		ifTrue: [
			ns := nil.
			ln := aName]
		ifFalse: [
			ns := aName copyFrom: 1 to: (i - 1).
			ln := aName copyFrom: i+1 to: aName size].
	twoArgsBlock value: ns value: ln! !

!SAXDriver methodsFor: 'testing' stamp: 'mir 6/5/2003 16:30'!
usesNamespaces
	^useNamespaces! !

!SAXDriver methodsFor: 'testing' stamp: 'mir 6/5/2003 17:06'!
validatesAttributes
	^validateAttributes! !

!XMLParser methodsFor: 'callbacks' stamp: 'SqR 7/2/2000 16:51'!
attribute: aSymbol value: aString
	"This method is called for each attribute/value pair in a start tag"

	^self subclassResponsibility! !

!XMLParser methodsFor: 'callbacks' stamp: 'SqR 7/2/2000 16:52'!
beginStartTag: aSymbol asPI: aBoolean
	"This method is called for at the beginning of a start tag.
	The asPI parameter defines whether or not the tag is a 'processing
	instruction' rather than a 'normal' tag."

	^self subclassResponsibility! !

!XMLParser methodsFor: 'callbacks' stamp: 'SqR 7/2/2000 16:52'!
endStartTag: aSymbol
	"This method is called at the end of the start tag after all of the
	attributes have been processed"

	^self subclassResponsibility! !

!XMLParser methodsFor: 'callbacks' stamp: 'SqR 7/2/2000 16:52'!
endTag: aSymbol
	"This method is called when the parser encounters either an
	end tag or the end of a unary tag"

	^self subclassResponsibility! !

!XMLParser methodsFor: 'callbacks' stamp: 'SqR 7/2/2000 16:52'!
text: aString
	"This method is called for the blocks of text between tags.
	It preserves whitespace, but has all of the enclosed entities expanded"

	^self subclassResponsibility! !

!XMLParser methodsFor: 'handling tokens' stamp: 'mir 1/17/2002 09:27'!
handleCData: aString
	self text: aString! !

!XMLParser methodsFor: 'handling tokens' stamp: 'mir 1/17/2002 09:26'!
handleEndTag: aString
	self endTag: aString! !

!XMLParser methodsFor: 'handling tokens' stamp: 'mir 1/17/2002 09:27'!
handlePCData: aString
	self text: aString! !

!XMLParser methodsFor: 'handling tokens' stamp: 'mir 1/17/2002 09:26'!
handleStartTag: tagName attributes: attributes
	self beginStartTag: tagName asPI: false.
	attributes keysAndValuesDo: [:key :value |
		self attribute: key value: value].
	self endStartTag: tagName! !

!XMLWriter methodsFor: 'writing xml' stamp: 'mir 5/20/2003 11:04'!
attribute: attributeName value: attributeValue
	self stream
		space;
		nextPutAll: attributeName.
	self
		eq;
		putAsXMLString: attributeValue! !

!XMLWriter methodsFor: 'writing xml' stamp: 'mir 12/8/2000 17:55'!
cdata: aString
	self startCData.
	self stream nextPutAll: aString.
	self endCData! !

!XMLWriter methodsFor: 'writing xml' stamp: 'mir 12/8/2000 17:56'!
endEmptyTag: tagName
	self popTag: tagName.
	self stream nextPutAll: '/>'.
	self canonical
		ifFalse: [self stream space]! !

!XMLWriter methodsFor: 'writing xml' stamp: 'mir 5/20/2003 15:25'!
endTag
	self stream nextPutAll: '>'.
	"self canonical
		ifFalse: [self stream space]"! !

!XMLWriter methodsFor: 'writing xml' stamp: 'mir 5/20/2003 11:52'!
endTag: tagName
	self endTag: tagName xmlns: nil! !

!XMLWriter methodsFor: 'writing xml' stamp: 'mir 6/24/2003 14:46'!
endTag: tagName xmlns: xmlns
	self popTag: tagName.
	self stream
		nextPutAll: '</'.
	(xmlns notNil
		and: [xmlns ~= self defaultNamespace])
		ifTrue: [self stream
			nextPutAll: xmlns;
			nextPut: $:].
	stream nextPutAll: tagName.
	self endTag.
! !

!XMLWriter methodsFor: 'writing xml' stamp: 'mir 5/20/2003 11:13'!
flush
	self stream flush! !

!XMLWriter methodsFor: 'writing xml' stamp: 'ar 12/15/2002 15:56'!
pcData: aString
	| lastIndex nextIndex |
	lastIndex := 1.
	"Unroll the first search to avoid copying"
	nextIndex := String findFirstInString: aString inSet: XMLTranslationMap startingAt: lastIndex.
	nextIndex = 0 ifTrue:[^self stream nextPutAll: aString].
	[self stream nextPutAll: (aString copyFrom: lastIndex to: nextIndex-1).
	self stream nextPutAll: (XMLTranslation at: (aString at: nextIndex)).
	lastIndex := nextIndex + 1.
	nextIndex := String findFirstInString: aString inSet: XMLTranslationMap startingAt: lastIndex.
	nextIndex = 0] whileFalse.
	self stream nextPutAll: (aString copyFrom: lastIndex to: aString size).! !

!XMLWriter methodsFor: 'writing xml' stamp: 'mir 12/11/2000 16:12'!
pi: piTarget data: piData
	self startPI: piTarget.
	self stream nextPutAll: piData.
	self endPI! !

!XMLWriter methodsFor: 'writing xml' stamp: 'pb 3/1/2015 18:02'!
startElement: elementName attributeList: attributeList
	self canonical
		ifFalse: [self stream lf].
	self startTag: elementName.
	attributeList keys asArray sort do: [:key |
		self attribute: key value: (attributeList at: key)]! !

!XMLWriter methodsFor: 'writing xml' stamp: 'mir 5/20/2003 11:51'!
startTag: tagName
	self startTag: tagName xmlns: nil! !

!XMLWriter methodsFor: 'writing xml' stamp: 'mir 6/24/2003 14:10'!
startTag: tagName xmlns: xmlns
	self stream
		nextPut: $<.
	(xmlns notNil
		and: [xmlns ~= self scope defaultNamespace])
		ifTrue: [self stream
			nextPutAll: xmlns;
			nextPut: $:].
	self stream
		nextPutAll: tagName.
	"self canonical
		ifFalse: [self stream space]."
	self pushTag: tagName! !

!XMLWriter methodsFor: 'writing xml' stamp: 'ar 2/23/2005 16:35'!
xmlDeclaration: versionString
	self canonical
		ifFalse: [
			self
				startPI: 'xml';
				attribute: 'version' value: versionString;
				endPI]! !

!XMLWriter methodsFor: 'writing xml' stamp: 'mir 5/20/2003 12:17'!
xmlDeclaration: versionString encoding: encodingString
	self canonical
		ifFalse: [
			self
				startPI: 'xml';
				attribute: 'version' value: versionString;
				attribute: 'encoding' value: encodingString;
				endPI.
			self stream flush]! !

!XMLWriter methodsFor: 'accessing' stamp: 'mir 12/7/2000 15:54'!
canonical
	^canonical! !

!XMLWriter methodsFor: 'accessing' stamp: 'mir 12/7/2000 15:54'!
canonical: aBoolean
	canonical _ aBoolean! !

!XMLWriter methodsFor: 'accessing' stamp: 'mir 12/8/2000 17:54'!
stream
	^stream! !

!XMLWriter methodsFor: 'accessing' stamp: 'mir 12/8/2000 17:54'!
stream: aStream
	stream _ aStream! !

!XMLWriter methodsFor: 'namespaces' stamp: 'mir 6/24/2003 15:09'!
declareNamespace: ns uri: uri
	self scope declareNamespace: ns uri: uri! !

!XMLWriter methodsFor: 'namespaces' stamp: 'mir 6/24/2003 14:23'!
defaultNamespace
	^self scope defaultNamespace! !

!XMLWriter methodsFor: 'namespaces' stamp: 'mir 6/24/2003 14:23'!
defaultNamespace: ns
	"Declare the default namespace."
	self scope defaultNamespace: ns! !

!XMLWriter methodsFor: 'namespaces' stamp: 'mir 6/24/2003 15:02'!
enterScope
	self scope enterScope! !

!XMLWriter methodsFor: 'namespaces' stamp: 'mir 6/24/2003 14:47'!
leaveScope
	self scope leaveScope! !

!XMLWriter methodsFor: 'private tags' stamp: 'mir 12/8/2000 18:01'!
endCData
	self stream nextPutAll: ']]>'! !

!XMLWriter methodsFor: 'private tags' stamp: 'mir 12/8/2000 18:01'!
endComment
	self stream nextPutAll: ' -->'! !

!XMLWriter methodsFor: 'private tags' stamp: 'mir 5/20/2003 12:13'!
endPI
	self stream nextPutAll: ' ?>'! !

!XMLWriter methodsFor: 'private tags' stamp: 'mir 12/8/2000 18:01'!
startCData
	self stream nextPutAll: '<!![CDATA['! !

!XMLWriter methodsFor: 'private tags' stamp: 'mir 12/8/2000 18:01'!
startComment
	self stream nextPutAll: '<-- '! !

!XMLWriter methodsFor: 'private tags' stamp: 'mir 12/8/2000 18:01'!
startPI: identifier
	self stream
		nextPutAll: '<?';
		nextPutAll: identifier;
		space! !

!XMLWriter methodsFor: 'writing dtd' stamp: 'mir 8/8/2000 18:13'!
endDecl: type
	self endTag! !

!XMLWriter methodsFor: 'writing dtd' stamp: 'jmv 3/3/2015 10:03'!
endDeclaration
	self stream
		lf;
		nextPut: $].
	self endTag! !

!XMLWriter methodsFor: 'writing dtd' stamp: 'mir 12/8/2000 18:02'!
startDecl: type
	self stream
		nextPutAll: '<!!';
		nextPutAll: type asUppercase;
		space! !

!XMLWriter methodsFor: 'writing dtd' stamp: 'mir 12/8/2000 18:02'!
startDecl: type named: aString
	self stream
		nextPutAll: '<!!';
		nextPutAll: type asUppercase;
		space;
		nextPutAll: aString;
		space! !

!XMLWriter methodsFor: 'writing dtd' stamp: 'jmv 3/3/2015 10:03'!
startDeclaration: dtdName
	self startDecl: 'DOCTYPE' named: dtdName.
	self stream
		nextPut: $[;
		lf! !

!XMLWriter methodsFor: 'private' stamp: 'mir 12/8/2000 18:00'!
eq
	self stream nextPut: $=! !

!XMLWriter methodsFor: 'private' stamp: 'mir 8/7/2000 16:23'!
popTag: tagName
	| stackTop |
	stackTop _ self stack isEmpty
		ifTrue: ['<empty>']
		ifFalse: [self stack last].
	^stackTop = tagName
		ifTrue: [self stack removeLast]
		ifFalse: [self error: 'Closing tag "' , tagName , '" does not match "' , stackTop]! !

!XMLWriter methodsFor: 'private' stamp: 'mir 8/7/2000 16:18'!
pushTag: tagName
	self stack add: tagName! !

!XMLWriter methodsFor: 'private' stamp: 'mir 12/11/2000 16:24'!
putAsXMLString: aValue
	self stream nextPut: $".
	self pcData: aValue.
	self stream nextPut: $"! !

!XMLWriter methodsFor: 'private' stamp: 'mir 6/24/2003 14:22'!
scope
	^scope! !

!XMLWriter methodsFor: 'private' stamp: 'mir 8/8/2000 17:02'!
stack
	^stack! !

!XMLWriter methodsFor: 'initialization' stamp: 'mir 6/24/2003 13:41'!
initialize
	stack _ OrderedCollection new.
	canonical _ false.
	scope := XMLNamespaceScope new! !

!XMLWriter class methodsFor: 'class initialization' stamp: 'ar 12/13/2002 01:04'!
initialize
	"XMLWriter initialize"

	XMLTranslation := Dictionary new.
	XMLTranslation
		at: Character cr put: '&#13;';
		at: Character lf put: '&#10;';
		at: Character tab put: '&#9;';
		at: $& put: '&amp;';
		at: $< put: '&lt;';
		at: $> put: '&gt;';
"		at: $' put: '&apos;'; "
		at: $" put: '&quot;'.
	XMLTranslationMap := ByteArray new: 256.
	XMLTranslation keysDo:[:ch| XMLTranslationMap at: ch asciiValue+1 put: 1].
! !

!XMLWriter class methodsFor: 'instance creation' stamp: 'jmv 2/2/2014 00:42'!
on: aStream
	^self new stream: aStream! !
DTDEntityDeclaration initialize!
DTDExternalEntityDeclaration initialize!
DTDParameterEntityDeclaration initialize!
XMLTokenizer initialize!
XMLWriter initialize!
