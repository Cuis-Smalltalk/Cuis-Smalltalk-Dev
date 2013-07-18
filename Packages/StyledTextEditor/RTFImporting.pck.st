'From Cuis 4.1 of 12 December 2012 [latest update: #1759] on 17 July 2013 at 12:56:06.44085 am'!
'Description Please enter a description for this package.'!
!provides: 'RTFImporting' 1 0!
!requires: 'Graphics-Files-Additional' 1 nil!
!classDefinition: #RTFException category: #RTFimporting!
Error subclass: #RTFException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RTFimporting'!
!classDefinition: 'RTFException class' category: #RTFimporting!
RTFException class
	instanceVariableNames: ''!

!classDefinition: #RTFChunkScanner category: #RTFimporting!
Object subclass: #RTFChunkScanner
	instanceVariableNames: 'destX lastIndex xTable rightEdge stopConditions prevIndex bufferStream buffer chunk'
	classVariableNames: 'BufferStream ScannerTable XTable'
	poolDictionaries: ''
	category: 'RTFimporting'!
!classDefinition: 'RTFChunkScanner class' category: #RTFimporting!
RTFChunkScanner class
	instanceVariableNames: ''!

!classDefinition: #RTFColorDef category: #RTFimporting!
Object subclass: #RTFColorDef
	instanceVariableNames: 'red green blue'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RTFimporting'!
!classDefinition: 'RTFColorDef class' category: #RTFimporting!
RTFColorDef class
	instanceVariableNames: ''!

!classDefinition: #RTFFontInfo category: #RTFimporting!
Object subclass: #RTFFontInfo
	instanceVariableNames: 'name num family charset cpg'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RTFimporting'!
!classDefinition: 'RTFFontInfo class' category: #RTFimporting!
RTFFontInfo class
	instanceVariableNames: ''!

!classDefinition: #RTFParser category: #RTFimporting!
Object subclass: #RTFParser
	instanceVariableNames: 'tokenizer state builder'
	classVariableNames: 'HandleMessages'
	poolDictionaries: ''
	category: 'RTFimporting'!
!classDefinition: 'RTFParser class' category: #RTFimporting!
RTFParser class
	instanceVariableNames: ''!

!classDefinition: #RTFParserDestination category: #RTFimporting!
Object subclass: #RTFParserDestination
	instanceVariableNames: 'block type'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RTFimporting'!
!classDefinition: 'RTFParserDestination class' category: #RTFimporting!
RTFParserDestination class
	instanceVariableNames: ''!

!classDefinition: #RTFParserState category: #RTFimporting!
Object subclass: #RTFParserState
	instanceVariableNames: 'stack destination context'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RTFimporting'!
!classDefinition: 'RTFParserState class' category: #RTFimporting!
RTFParserState class
	instanceVariableNames: ''!

!classDefinition: #RTFStylesheet category: #RTFimporting!
Object subclass: #RTFStylesheet
	instanceVariableNames: 'additive name type num style basedon'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RTFimporting'!
!classDefinition: 'RTFStylesheet class' category: #RTFimporting!
RTFStylesheet class
	instanceVariableNames: ''!

!classDefinition: #RTFSophieStylesheet category: #RTFimporting!
RTFStylesheet subclass: #RTFSophieStylesheet
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RTFimporting'!
!classDefinition: 'RTFSophieStylesheet class' category: #RTFimporting!
RTFSophieStylesheet class
	instanceVariableNames: ''!

!classDefinition: #RTFTextBuilder category: #RTFimporting!
Object subclass: #RTFTextBuilder
	instanceVariableNames: 'textConverter colorTable fontTable styleTable skipNextCharacters defaultSkipNextCharacters textStream currentFgColor fontFamilyName fontPointSize bold italic underline align stateStack firstIndent leftIndent rightIndent spaceBefore spaceAfter'
	classVariableNames: 'CodePageConverterTable'
	poolDictionaries: ''
	category: 'RTFimporting'!
!classDefinition: 'RTFTextBuilder class' category: #RTFimporting!
RTFTextBuilder class
	instanceVariableNames: ''!

!classDefinition: #RTFTextConverter category: #RTFimporting!
Object subclass: #RTFTextConverter
	instanceVariableNames: 'acceptingEncodings'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RTFimporting'!
!classDefinition: 'RTFTextConverter class' category: #RTFimporting!
RTFTextConverter class
	instanceVariableNames: ''!

!classDefinition: #RTFLatin1TextConverter category: #RTFimporting!
RTFTextConverter subclass: #RTFLatin1TextConverter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RTFimporting'!
!classDefinition: 'RTFLatin1TextConverter class' category: #RTFimporting!
RTFLatin1TextConverter class
	instanceVariableNames: ''!

!classDefinition: #RTFMappingUnicodeTextConverter category: #RTFimporting!
RTFTextConverter subclass: #RTFMappingUnicodeTextConverter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RTFimporting'!
!classDefinition: 'RTFMappingUnicodeTextConverter class' category: #RTFimporting!
RTFMappingUnicodeTextConverter class
	instanceVariableNames: ''!

!classDefinition: #RTFCP1250UnicodeTextConverter category: #RTFimporting!
RTFMappingUnicodeTextConverter subclass: #RTFCP1250UnicodeTextConverter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RTFimporting'!
!classDefinition: 'RTFCP1250UnicodeTextConverter class' category: #RTFimporting!
RTFCP1250UnicodeTextConverter class
	instanceVariableNames: ''!

!classDefinition: #RTFCP1251UnicodeTextConverter category: #RTFimporting!
RTFMappingUnicodeTextConverter subclass: #RTFCP1251UnicodeTextConverter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RTFimporting'!
!classDefinition: 'RTFCP1251UnicodeTextConverter class' category: #RTFimporting!
RTFCP1251UnicodeTextConverter class
	instanceVariableNames: ''!

!classDefinition: #RTFCP1252UnicodeTextConverter category: #RTFimporting!
RTFMappingUnicodeTextConverter subclass: #RTFCP1252UnicodeTextConverter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RTFimporting'!
!classDefinition: 'RTFCP1252UnicodeTextConverter class' category: #RTFimporting!
RTFCP1252UnicodeTextConverter class
	instanceVariableNames: ''!

!classDefinition: #RTFMacRomanUnicodeTextConverter category: #RTFimporting!
RTFMappingUnicodeTextConverter subclass: #RTFMacRomanUnicodeTextConverter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RTFimporting'!
!classDefinition: 'RTFMacRomanUnicodeTextConverter class' category: #RTFimporting!
RTFMacRomanUnicodeTextConverter class
	instanceVariableNames: ''!

!classDefinition: #RTFToken category: #RTFimporting!
Object subclass: #RTFToken
	instanceVariableNames: 'type content arg'
	classVariableNames: 'DefaultArgs'
	poolDictionaries: ''
	category: 'RTFimporting'!
!classDefinition: 'RTFToken class' category: #RTFimporting!
RTFToken class
	instanceVariableNames: ''!

!classDefinition: #RTFTokenizer category: #RTFimporting!
Object subclass: #RTFTokenizer
	instanceVariableNames: 'stream buffer chunkBuffer last next afterNext afterAfter'
	classVariableNames: 'ControlSymbolSet EndOfArgumentSet EndOfKeywordSet'
	poolDictionaries: ''
	category: 'RTFimporting'!
!classDefinition: 'RTFTokenizer class' category: #RTFimporting!
RTFTokenizer class
	instanceVariableNames: ''!

!classDefinition: #RTFUnicode category: #RTFimporting!
Object subclass: #RTFUnicode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RTFimporting'!
!classDefinition: 'RTFUnicode class' category: #RTFimporting!
RTFUnicode class
	instanceVariableNames: ''!


!RTFColorDef commentStamp: '<historical>' prior: 0!
Carries color information for the color table!

!RTFFontInfo commentStamp: '<historical>' prior: 0!
Carries RTF font information to be passed to a builder!

!RTFParser commentStamp: '<historical>' prior: 0!
An RTF parser!

!RTFParserState commentStamp: '<historical>' prior: 0!
A state of the RTF parser contains:
	- a destination block (to send plain text to)
	- a context (a currently built object for the builder, e.g. RTFFontInfo)
The state has abilities for saving and restoring states on a stack!

!RTFStylesheet commentStamp: '<historical>' prior: 0!
Carries information about RTF styles!

!RTFSophieStylesheet commentStamp: '<historical>' prior: 0!
Adds style applying functionality to stylesheets!

!RTFTextBuilder commentStamp: '<historical>' prior: 0!
To do:
- Ensure consistency between rtf export and import
- write tests for that consistency
- integrate with StyledTextBuilder (el parser der rtf seria un transcodificador???)!!
	(el builder es solo para StyledText... Yo quisiera que esto sea mas general!!)!

!RTFTextConverter commentStamp: '<historical>' prior: 0!
The abstract class for all different type of text converters.  nextFromStream: and nextPut:toStream: are the public accessible methods.  If you are going to make a subclass for a stateful text conversion, you should override restoreStateOf:with: and saveStateOf: along the line of CompoundTextConverter.
!

!RTFLatin1TextConverter commentStamp: '<historical>' prior: 0!
Text converter for ISO 8859-1.  An international encoding used in Western Europe.!

!RTFMappingUnicodeTextConverter commentStamp: '<historical>' prior: 0!
Base class for Unicode converters based on mappings as defined in
http://www.unicode.org/Public/MAPPINGS/!

!RTFCP1250UnicodeTextConverter commentStamp: '<historical>' prior: 0!
CP1250 to Unicode converter based on
http://www.unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP1250.TXT!

!RTFCP1251UnicodeTextConverter commentStamp: '<historical>' prior: 0!
CP1251 to Unicode converter based on
http://www.unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP1251.TXT!

!RTFCP1252UnicodeTextConverter commentStamp: '<historical>' prior: 0!
CP1251 to Unicode converter based on
http://www.unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP1252.TXT!

!RTFMacRomanUnicodeTextConverter commentStamp: '<historical>' prior: 0!
True MacRoman to Unicode converter based on
http://www.unicode.org/Public/MAPPINGS/VENDORS/APPLE/ROMAN.TXT!

!RTFToken commentStamp: '<historical>' prior: 0!
token types:
    #blockOpen - '{'
    #blockClose - '}'
    #keyword - e.g. '\alabala'
    #string - e.g. 'alabala'

content stores keyword or string
arg stores keyword argument
if any field unapplicable for type, it returns nil
DafaultArgs: All keywords have argument value. Default keyowrd parameters is defined in DefaultArgs. If no default argument is fount, the default is assumed 0. !

!RTFTokenizer commentStamp: '<historical>' prior: 0!
Because of getAfterNext, don't expect the stream of this token to behave as expected. Two tokens ahead are actually read from the stream

At initialization last is nil. Must getToken to get first

newline is a Mac-hack. No newlines are expected in a RTF, but Mac makes escaped newlines!

!RTFUnicode commentStamp: '<historical>' prior: 0!
Not a real Unicode implementation. Just compatibility for Sophie-RTF. Answers instances of Character (i.e. ISO-8859-15).
Based on http://www.unicode.org/Public/MAPPINGS/ISO8859/8859-15.TXT!

!RTFChunkScanner methodsFor: 'private' stamp: 'mir 8/14/2006 16:25'!
addScannedString
	| scannedString |
	scannedString := ''.
	prevIndex < lastIndex
		ifTrue: [scannedString := chunk copyFrom: prevIndex to: lastIndex-1].
	self bufferStream isEmpty
		ifFalse: [
			self bufferStream nextPutAll: scannedString.
			scannedString := self bufferStream contents.
			self bufferStream reset].
	scannedString isEmpty
		ifFalse: [buffer add: (RTFToken newString: scannedString)]
! !

!RTFChunkScanner methodsFor: 'private' stamp: 'mir 8/12/2006 15:51'!
addScannedStringToBuffer
	prevIndex < lastIndex
		ifFalse: [^self].
	self bufferStream nextPutAll: (chunk copyFrom: prevIndex to: lastIndex-1)! !

!RTFChunkScanner methodsFor: 'stop conditions' stamp: 'mir 8/12/2006 15:44'!
blockCloseAt: index
	self addScannedString.
	buffer add: (RTFToken newBlockClose).
	lastIndex := lastIndex  + 1.
	prevIndex := lastIndex! !

!RTFChunkScanner methodsFor: 'stop conditions' stamp: 'mir 8/12/2006 15:44'!
blockOpenAt: index
	self addScannedString.
	buffer add: (RTFToken newBlockOpen).
	lastIndex := lastIndex  + 1.
	prevIndex := lastIndex! !

!RTFChunkScanner methodsFor: 'private' stamp: 'mir 8/12/2006 14:59'!
bufferStream
	^BufferStream! !

!RTFChunkScanner methodsFor: 'stop conditions' stamp: 'mir 8/12/2006 15:46'!
crAt: index
	self addScannedStringToBuffer.
	lastIndex := lastIndex  + 1.
	prevIndex := lastIndex! !

!RTFChunkScanner methodsFor: 'stop conditions' stamp: 'mir 8/12/2006 15:50'!
lfAt: index
	self addScannedStringToBuffer.
	lastIndex := lastIndex  + 1.
	prevIndex := lastIndex! !

!RTFChunkScanner methodsFor: 'initialize' stamp: 'mir 8/12/2006 16:05'!
scan: chunkString into: aBuffer startingAt: index
	chunk := chunkString.
	buffer := aBuffer.
	self scanStartingAt: index! !

!RTFChunkScanner methodsFor: 'private' stamp: 'mir 8/12/2006 15:59'!
scanCharactersFrom: startIndex to: stopIndex in: sourceString rightX: rightX stopConditions: stops kern: kernDelta
	"Primitive. This is the inner loop of text display--but see 
	scanCharactersFrom: to:rightX: which would get the string, 
	stopConditions and displaying from the instance. March through source 
	String from startIndex to stopIndex. If any character is flagged with a 
	non-nil entry in stops, then return the corresponding value. Determine 
	width of each character from xTable, indexed by map. 
	If dextX would exceed rightX, then return stops at: 258. 
	Advance destX by the width of the character. If stopIndex has been
	reached, then return stops at: 257. Optional. 
	See Object documentation whatIsAPrimitive."
	| ascii char |
	<primitive: 103>

	lastIndex _ startIndex.
	[lastIndex <= stopIndex]
		whileTrue: 
			[char _ (sourceString at: lastIndex).
			ascii _ char asciiValue + 1.
			(stops at: ascii) == nil ifFalse: [^stops at: ascii].
			lastIndex _ lastIndex + 1].
	lastIndex _ stopIndex.
	^stops at: CharacterScanner endOfRunCode! !

!RTFChunkScanner methodsFor: 'private' stamp: 'mir 8/12/2006 15:16'!
scanFrom: startIndex to: stopIndex
	| stopCondition |
	stopCondition := self scanCharactersFrom: startIndex to: stopIndex in: chunk rightX: SmallInteger maxVal stopConditions: stopConditions kern: 0.
	stopCondition
		ifNil: [^nil]
		ifNotNil: [stopCondition == #scanFinished
			ifTrue: [^nil]
			ifFalse: [self perform: stopCondition with: lastIndex]]! !

!RTFChunkScanner methodsFor: 'private' stamp: 'mir 8/14/2006 16:32'!
scanStartingAt: index
	| chunkSize |
	self bufferStream reset.
	stopConditions := ScannerTable.
	xTable := XTable.
	destX := 0.
	lastIndex := index.
	prevIndex := index.
	rightEdge := SmallInteger maxVal.
	chunkSize := chunk size.
	[(self scanFrom: lastIndex to: chunkSize) isNil]
		whileFalse.
	prevIndex <= lastIndex
		ifTrue: [lastIndex := lastIndex + 1].
	self addScannedString! !

!RTFChunkScanner class methodsFor: 'class initialization' stamp: 'mir 8/12/2006 15:51'!
initialize
	"RTFChunkScanner initialize"

	BufferStream := (String new: 4096) writeStream.
	XTable := Array new: 258 withAll: 0.

	ScannerTable := Array new: 258.
	ScannerTable atAllPut: nil.
	ScannerTable at: CharacterScanner endOfRunCode put: #scanFinished.
	ScannerTable at: CharacterScanner crossedXCode put: #scanFinished.

	ScannerTable at: ${ asciiValue + 1 put: #blockOpenAt:.
	ScannerTable at: $} asciiValue + 1 put: #blockCloseAt:.
	ScannerTable at: Character lfCharacter asciiValue + 1 put: #lfAt:.
	ScannerTable at: Character crCharacter asciiValue + 1 put: #crAt:.
! !

!RTFChunkScanner class methodsFor: 'instance creation' stamp: 'mir 8/12/2006 16:04'!
scan: chunk into: buffer startingAt: index
	^self new scan: chunk into: buffer startingAt: index! !

!RTFColorDef methodsFor: 'accessing' stamp: 'tat 5/5/2006 13:49'!
blue
	^blue! !

!RTFColorDef methodsFor: 'accessing' stamp: 'tat 5/5/2006 13:49'!
blue: b
	blue := b! !

!RTFColorDef methodsFor: 'accessing' stamp: 'tat 5/5/2006 13:48'!
green
	^green! !

!RTFColorDef methodsFor: 'accessing' stamp: 'tat 5/5/2006 13:48'!
green: g
	green := g! !

!RTFColorDef methodsFor: 'accessing' stamp: 'tat 5/5/2006 13:48'!
red
	^red! !

!RTFColorDef methodsFor: 'accessing' stamp: 'tat 5/5/2006 13:48'!
red: r
	red := r! !

!RTFFontInfo methodsFor: 'accessing' stamp: 'tat 5/5/2006 01:40'!
charset
	^charset! !

!RTFFontInfo methodsFor: 'accessing' stamp: 'tat 5/5/2006 01:40'!
charset: c
	charset := c! !

!RTFFontInfo methodsFor: 'accessing' stamp: 'tat 5/5/2006 01:44'!
cpg
	^cpg! !

!RTFFontInfo methodsFor: 'accessing' stamp: 'tat 5/5/2006 01:44'!
cpg: n
	cpg := n! !

!RTFFontInfo methodsFor: 'accessing' stamp: 'tat 5/5/2006 01:31'!
family
	^family! !

!RTFFontInfo methodsFor: 'accessing' stamp: 'tat 5/5/2006 01:31'!
family: f
	family := f! !

!RTFFontInfo methodsFor: 'accessing' stamp: 'tat 5/3/2006 02:21'!
name
	^name! !

!RTFFontInfo methodsFor: 'accessing' stamp: 'tat 5/3/2006 02:21'!
name: fn
	name := fn! !

!RTFFontInfo methodsFor: 'accessing' stamp: 'tat 5/3/2006 02:22'!
num
	^num! !

!RTFFontInfo methodsFor: 'accessing' stamp: 'tat 5/3/2006 02:22'!
num: n
	num := n! !

!RTFParser methodsFor: 'handlers' stamp: 'tat 11/1/2006 15:00'!
addContents
	"assuming that the next RTF token is a string - add the string to the content tree
	using current style settings"

	|token|
	"self break."

	token := tokenizer getToken.
	state destination value: (token string)
! !

!RTFParser methodsFor: 'utilities' stamp: 'mir 8/12/2006 16:56'!
createHandleMessage: token
	"builds a handle message from a keyword token"
	^HandleMessages at: token word ifAbsent: [('handle' , token word , ':') asSymbol]! !

!RTFParser methodsFor: 'utilities' stamp: 'jmv 4/4/2011 16:28'!
digitValue: char

	| value |
	value _ char asciiValue.
	value <= $9 asciiValue 
		ifTrue: [^value - $0 asciiValue].
	value >= $a asciiValue 
		ifTrue: [value <= $z asciiValue ifTrue: [^value - $a asciiValue + 10]].
	value >= $A asciiValue 
		ifTrue: [value <= $Z asciiValue ifTrue: [^value - $A asciiValue + 10]].
! !

!RTFParser methodsFor: 'utilities' stamp: 'tat 5/3/2006 02:18'!
emptyToken
	^tokenizer tokenClass emptyToken
	! !

!RTFParser methodsFor: 'utilities' stamp: 'jmv 4/4/2011 16:26'!
getAddress: string

	"expects a string of the type 
	HYPERLINK<whitespaces>('|dblquot)<some link text>('|dblquot)
	and answers the link text"

	|count countEnd first last |

	"self break."

	"skip the HYPERLINK part"
	count := 10.
	[(((string at: count) = $') or: [(string at: count) = $"]) or: [count >= (string size)]] whileFalse:
		[count := count +1].
	
	"skip potential whitechars after the address"

	countEnd := string size.
	[(((string at: countEnd) = $') or: [(string at: countEnd) = $"] or: [countEnd <= (count+1)])] whileFalse:
		[countEnd := countEnd - 1].

	first _ count + 1 max: 1.
	last _ countEnd-1 min: string size.
	(first > string size or: [first > last ]) ifTrue: [ ^''].
	^string copyFrom: first to: last! !

!RTFParser methodsFor: 'handlers-general' stamp: 'tat 5/5/2006 13:53'!
handleAll: token! !

!RTFParser methodsFor: 'handlers-characters' stamp: 'tat 4/11/2007 15:07'!
handleLeftCurlyBracket: token
	builder addUnicodeContents: '{'! !

!RTFParser methodsFor: 'handlers-characters' stamp: 'tat 4/11/2007 14:30'!
handleRightCurlyBracket: token
	builder addUnicodeContents: '}'! !

!RTFParser methodsFor: 'handlers-characters' stamp: 'tat 11/1/2006 15:06'!
handleSomeCodePageHexValue: token
	"send text to destination if not nil"
	state destination value: (String value: token arg)
! !

!RTFParser methodsFor: 'handlers-general' stamp: 'tat 5/5/2006 14:36'!
handleStar: token
	"check if the parser can respond to the command following the *"
	(self respondsTo: (self createHandleMessage: (tokenizer lookAhead: 1))) ifFalse: [
		"if not, suppress plain text within this block"
		self handlerSupressText
	]! !

!RTFParser methodsFor: 'handlers-stylesheet' stamp: 'tat 5/5/2006 14:45'!
handleadditive: token
	state context additive! !

!RTFParser methodsFor: 'handlers-skipping' stamp: 'kalin 7/9/2006 16:58'!
handleaftnsep: token

	self skipUntilBlockClose.! !

!RTFParser methodsFor: 'handlers-general' stamp: 'tat 7/11/2006 22:26'!
handleansi: token
	builder buildAnsiCharacterSet! !

!RTFParser methodsFor: 'handlers-general' stamp: 'tat 7/27/2006 11:38'!
handleansicpg: token
	builder buildCodePage: (token arg)! !

!RTFParser methodsFor: 'handlers-charformat' stamp: 'tat 7/10/2006 23:31'!
handleb: token

	builder buildSetBold: (token arg = 1)! !

!RTFParser methodsFor: 'handlers-general' stamp: 'tat 6/24/2006 00:37'!
handlebin: token
	"ignore binary data in RTF"
	self skipUntilBlockClose! !

!RTFParser methodsFor: 'handlers-colortable' stamp: 'tat 5/5/2006 13:50'!
handleblue: token
	state context blue: (token arg)! !

!RTFParser methodsFor: 'handlers-characters' stamp: 'tat 6/22/2006 22:54'!
handlebullet: token
	builder buildAddBullet! !

!RTFParser methodsFor: 'handlers-charformat' stamp: 'kalin 5/3/2006 12:00'!
handlecaps: token

	builder buildSetCaps! !

!RTFParser methodsFor: 'handlers-charformat' stamp: 'kalin 5/3/2006 12:03'!
handlecb: token

	builder buildSetBkColor: (token arg)! !

!RTFParser methodsFor: 'handlers-charformat' stamp: 'tat 3/12/2007 19:22'!
handlecbpat: token
	"treat this as changing the background color"
	self handlecb: token! !

!RTFParser methodsFor: 'handlers-charformat' stamp: 'kalin 5/3/2006 12:03'!
handlecf: token

	builder buildSetFgColor: (token arg)! !

!RTFParser methodsFor: 'handlers-charformat' stamp: 'tat 3/1/2007 15:37'!
handlechcbpat: token
	"treat this as changing the background color"
	self handlecb: token! !

!RTFParser methodsFor: 'handlers-skipping' stamp: 'kalin 7/9/2006 16:57'!
handlechftnsep: token

	self skipUntilBlockClose.! !

!RTFParser methodsFor: 'handlers-colortable' stamp: 'tat 11/1/2006 15:09'!
handlecolortbl: token
	state destination block: [:string | string = ';' ifTrue: [
		builder buildAddColorDef: state context.
		state context: RTFColorDef new
	]] type: #colortable.
	state context: RTFColorDef new
	! !

!RTFParser methodsFor: 'handlers-fonttable' stamp: 'tat 5/5/2006 01:45'!
handlecpg: token
	state context cpg: (token arg)! !

!RTFParser methodsFor: 'handlers-stylesheet' stamp: 'tat 5/6/2006 17:08'!
handlecs: token
	self handles: token.
	state context isNil ifFalse: [state context type: #character]! !

!RTFParser methodsFor: 'handlers-paragraph' stamp: 'MR 5/4/2006 13:43'!
handlecufi: token

	builder buildFirstLineIndentPercentage: (token arg)! !

!RTFParser methodsFor: 'handlers-paragraph' stamp: 'MR 5/4/2006 13:42'!
handleculi: token

	builder buildLeftIndentPercentage: (token arg)! !

!RTFParser methodsFor: 'handlers-paragraph' stamp: 'MR 5/4/2006 13:43'!
handlecuri: token

	builder buildRightIndentPercentage: (token arg)! !

!RTFParser methodsFor: 'handlers' stamp: 'tat 11/1/2006 15:09'!
handledatafield: token

	state destination block: [:string|] type: #ignore.
	self parseUntilBlockClose.! !

!RTFParser methodsFor: 'handlers-header' stamp: 'tat 11/1/2006 15:18'!
handledeff: token
	builder buildSetDefaultFont: token arg! !

!RTFParser methodsFor: 'handlers-pictures' stamp: 'tat 5/18/2007 18:53'!
handledibitmap: token
	"Windows device-independent bitmap (DIB)"
	"state context atProperty: #type put: #bmp"
	state context at: #type put: #bmp! !

!RTFParser methodsFor: 'handlers-charformat' stamp: 'MR 6/7/2006 18:09'!
handledn: token
		
	builder buildSetBaselineOffset: (token arg negated)! !

!RTFParser methodsFor: 'handlers-stylesheet' stamp: 'tat 5/6/2006 16:57'!
handleds: token
	self handles: token.
	state context isNil ifFalse: [state context type: #paragraph]! !

!RTFParser methodsFor: 'handlers-characters' stamp: 'tat 6/22/2006 22:53'!
handleemdash: token
	builder buildAddEmDash! !

!RTFParser methodsFor: 'handlers-pictures' stamp: 'tat 5/18/2007 18:54'!
handleemfblip: token
	"Enhanced metafile (EMF)"
	"state context atProperty: #type put: #emf"
	state context at: #type put: #emf! !

!RTFParser methodsFor: 'handlers-characters' stamp: 'tat 6/22/2006 22:53'!
handleemspace: token
	builder buildAddEmSpace! !

!RTFParser methodsFor: 'handlers-characters' stamp: 'tat 6/22/2006 22:53'!
handleendash: token
	builder buildAddEnDash! !

!RTFParser methodsFor: 'handlers-characters' stamp: 'tat 6/22/2006 22:53'!
handleenspace: token
	builder buildAddEnSpace! !

!RTFParser methodsFor: 'handlers-fonttable' stamp: 'tat 1/16/2007 11:59'!
handlef: token
	"check if we are defining or using a font"
	(state destination type = #fonttable)
		ifTrue: [ state context: (RTFFontInfo new). state context num: (token arg) ]
		ifFalse: [ builder buildSetFont: (token arg) ]! !

!RTFParser methodsFor: 'handlers-fonttable' stamp: 'tat 5/5/2006 01:36'!
handlefbidi: token
	self handlefnil: token! !

!RTFParser methodsFor: 'handlers-fonttable' stamp: 'tat 5/5/2006 01:40'!
handlefcharset: token
	state context charset: (token arg)! !

!RTFParser methodsFor: 'handlers-fonttable' stamp: 'tat 5/5/2006 01:35'!
handlefdecor: token
	self handlefnil: token! !

!RTFParser methodsFor: 'handlers-paragraph' stamp: 'kalin 5/3/2006 11:45'!
handlefi: token

	builder buildFirstLineIndent: (token arg)! !

!RTFParser methodsFor: 'handlers' stamp: 'tat 11/1/2006 15:09'!
handlefield: token

	|linkUri linkAlt|

	linkUri := nil.
	linkAlt := ''.

	state destination block:
		[:string | 	linkUri isNil ifTrue:
						[(string findString: 'HYPERLINK') > 0 ifTrue:
							[linkUri := self getAddress: string]]
						ifFalse:
							[linkAlt := linkAlt , string]
		] type: #field.

		self parseUntilBlockClose.

		linkAlt isNil not ifTrue: [builder buildAddURI: linkUri alternate: linkAlt].
! !

!RTFParser methodsFor: 'handlers-header' stamp: 'tat 5/5/2006 13:44'!
handlefiletbl: token
	self handlerSupressText! !

!RTFParser methodsFor: 'handlers' stamp: 'kalin 6/24/2006 17:17'!
handlefldinst: token
! !

!RTFParser methodsFor: 'handlers-fonttable' stamp: 'tat 5/5/2006 01:32'!
handlefmodern: token
	self handlefnil: token! !

!RTFParser methodsFor: 'handlers-fonttable' stamp: 'tat 5/5/2006 01:31'!
handlefnil: token
	state context family: (token word)! !

!RTFParser methodsFor: 'handlers-fonttable' stamp: 'tat 11/1/2006 15:10'!
handlefonttbl: token
	state destination block: [:string |
		(string endsWith: ';')
			ifTrue: [string = ';'
				"a single ; means end of font info"
				ifTrue: [ builder buildAddFontInfo: (state context) ]
				"this is a font name with a ; at the end"
				ifFalse: [
					state context name: (string allButLast).
					builder buildAddFontInfo: (state context)
				]
			]
			" this is a font name"
			ifFalse: [ state context name: string ]
	]
	type: #fonttable! !

!RTFParser methodsFor: 'handlers-skipping' stamp: 'tat 6/18/2007 00:47'!
handlefooter: token

	self skipUntilBlockClose.! !

!RTFParser methodsFor: 'handlers-general' stamp: 'tat 11/1/2006 17:24'!
handlefootnote: token
	self skipUntilBlockClose! !

!RTFParser methodsFor: 'handlers-fonttable' stamp: 'tat 5/5/2006 01:32'!
handlefroman: token
	self handlefnil: token! !

!RTFParser methodsFor: 'handlers-charformat' stamp: 'kalin 5/3/2006 12:12'!
handlefs: token
		
	builder buildSetFontSize: (token arg)! !

!RTFParser methodsFor: 'handlers-fonttable' stamp: 'tat 5/5/2006 01:32'!
handlefscript: token
	self handlefnil: token! !

!RTFParser methodsFor: 'handlers-fonttable' stamp: 'tat 5/5/2006 01:32'!
handlefswiss: token
	self handlefnil: token! !

!RTFParser methodsFor: 'handlers-fonttable' stamp: 'tat 5/5/2006 01:36'!
handleftech: token
	self handlefnil: token! !

!RTFParser methodsFor: 'handlers-skipping' stamp: 'kalin 7/9/2006 16:58'!
handleftnsepc: token

	self skipUntilBlockClose.! !

!RTFParser methodsFor: 'handlers-colortable' stamp: 'tat 5/5/2006 13:50'!
handlegreen: token
	state context green: (token arg)! !

!RTFParser methodsFor: 'handlers-skipping' stamp: 'kalin 7/9/2006 16:55'!
handleheader: token

	self skipUntilBlockClose.! !

!RTFParser methodsFor: 'handlers-charformat' stamp: 'tat 6/13/2007 23:50'!
handlehighlight: token
	"treat highlighting as changing the background color"
	self handlecb: token! !

!RTFParser methodsFor: 'handlers-charformat' stamp: 'tat 7/11/2006 00:38'!
handlei: token

	builder buildSetItalic: (token arg = 1)! !

!RTFParser methodsFor: 'handlers' stamp: 'tat 5/5/2006 00:30'!
handleinfo: token
	self handlerSupressText! !

!RTFParser methodsFor: 'handlers-pictures' stamp: 'tat 5/18/2007 18:10'!
handlejpegblip: token
	"state context atProperty: #type put: #jpeg"
	state context at: #type put: #jpeg! !

!RTFParser methodsFor: 'handlers-charformat' stamp: 'MR 6/7/2006 17:59'!
handlekerning: token
		
	builder buildSetKerning: (token arg)! !

!RTFParser methodsFor: 'handlers-characters' stamp: 'tat 6/22/2006 22:42'!
handleldblquote: token

	builder buildAddDoubleLeftQuote! !

!RTFParser methodsFor: 'handlers-paragraph' stamp: 'kalin 5/3/2006 11:47'!
handleli: token

	builder buildLeftIndent: (token arg)! !

!RTFParser methodsFor: 'handlers-paragraph' stamp: 'kalin 5/3/2006 11:47'!
handlelin: token

	builder buildLeftIndent: (token arg)! !

!RTFParser methodsFor: 'handlers-characters' stamp: 'MR 7/10/2007 08:31'!
handleline: token

	builder buildStartParagraph! !

!RTFParser methodsFor: 'handlers-characters' stamp: 'tat 6/22/2006 22:54'!
handlelquote: token
	builder buildAddLeftQuote! !

!RTFParser methodsFor: 'handlers-general' stamp: 'tat 7/11/2006 22:26'!
handlemac: token
	builder buildMacCharacterSet! !

!RTFParser methodsFor: 'handlers-pictures' stamp: 'tat 5/18/2007 18:54'!
handlemacpict: token
	"QuickDraw image (PICT)"
	"state context atProperty: #type put: #pict"
	state context at: #type put: #pict! !

!RTFParser methodsFor: 'handlers-characters' stamp: 'tat 6/22/2006 22:55'!
handlenbhyph: token
	builder buildAddNonBreakingHyphen! !

!RTFParser methodsFor: 'handlers-characters' stamp: 'tat 6/22/2006 22:55'!
handlenbsp: token
	builder buildAddNonBreakingSpace! !

!RTFParser methodsFor: 'handlers-pictures' stamp: 'tat 6/24/2006 00:38'!
handlenonshppict: token
	"ignore this group"
	self skipUntilBlockClose! !

!RTFParser methodsFor: 'handlers-characters' stamp: 'tat 6/22/2006 22:55'!
handleopthyph: token
	builder buildAddOptionalHyphen! !

!RTFParser methodsFor: 'handlers-paragraph' stamp: 'kalin 5/3/2006 11:25'!
handlepar: token
	builder buildStartParagraph! !

!RTFParser methodsFor: 'handlers-paragraph' stamp: 'tat 11/1/2006 15:40'!
handlepard: token
	builder buildResetParagraphSettings! !

!RTFParser methodsFor: 'handlers-pictures' stamp: 'MR 7/2/2006 19:24'!
handlepiccropb: token
	"Bottom cropping value in twips. A positive value crops toward the center of the picture; a negative value crops away from the center, adding a space border around the picture (the default value is 0). OPTIONAL"
	state context at: 'piccropb' put: token arg
! !

!RTFParser methodsFor: 'handlers-pictures' stamp: 'MR 7/2/2006 19:23'!
handlepiccropr: token
	"Right cropping value in twips. A positive value crops toward the center of the picture; a negative value crops away from the center, adding a space border around the picture (the default value is 0). OPTIONAL"
	state context at: 'piccropr' put: token arg
! !

!RTFParser methodsFor: 'handlers-pictures' stamp: 'MR 7/2/2006 19:17'!
handlepich: token
	"yExt field if the picture is a Windows metafile; picture height in pixels if the picture is a bitmap or from QuickDraw. The N argument is a long integer"
	state context at: 'pich' put: token arg
! !

!RTFParser methodsFor: 'handlers-pictures' stamp: 'MR 7/2/2006 19:18'!
handlepichgoal: token
	"Desired height of the picture in twips. The N argument is a long integer. OPTIONAL"
	state context at: 'pichgoal' put: token arg
! !

!RTFParser methodsFor: 'handlers-pictures' stamp: 'MR 7/2/2006 19:20'!
handlepicscalex: token
	"Horizontal scaling value. The N argument is a value representing a percentage (the default is 100 percent). OPTIONAL"
	state context at: 'picscalex' put: token arg
! !

!RTFParser methodsFor: 'handlers-pictures' stamp: 'MR 7/2/2006 19:20'!
handlepicscaley: token
	"Vertical scaling value. The N argument is a value representing a percentage (the default is 100 percent). OPTIONAL"
	state context at: 'picscaley' put: token arg
! !

!RTFParser methodsFor: 'handlers-pictures' stamp: 'tat 5/18/2007 18:45'!
handlepict: token
	| stream |
	stream _ ReadWriteStream with: ByteArray new.
	
	state destination block: [:string | 
		1 to: string size by: 2 do: [:index |
			stream
				nextPut: ((self digitValue: (string at: index)) bitShift: 4)
				+ (self digitValue: (string at: index+1))]]
		type: #pict.	
	state context: Dictionary new.
	stream reset.

	"state context atProperty: #type put: #none."
	state context at: #type put: #none.

	self parseUntilBlockClose.
	builder buildAddPicture: state context from: stream! !

!RTFParser methodsFor: 'handlers-pictures' stamp: 'MR 7/2/2006 19:17'!
handlepicw: token
	"xExt field if the picture is a Windows metafile; picture width in pixels if the picture is a bitmap or from QuickDraw. The N argument is a long integer"
	state context at: 'picw' put: token arg
! !

!RTFParser methodsFor: 'handlers-pictures' stamp: 'MR 7/2/2006 19:19'!
handlepicwgoal: token
	"Desired width of the picture in twips. The N argument is a long integer. OPTIONAL"
	state context at: 'picwgoal' put: token arg
! !

!RTFParser methodsFor: 'handlers-charformat' stamp: 'kalin 5/3/2006 11:58'!
handleplain: token

	builder buildResetCharFormat! !

!RTFParser methodsFor: 'handlers-pictures' stamp: 'tat 5/18/2007 18:51'!
handlepmmetafile: token
	"OS/2 bitmap"
	"state context atProperty: #type put: #pmm"
	state context at: #type put: #pmm! !

!RTFParser methodsFor: 'handlers-pictures' stamp: 'tat 6/23/2006 22:39'!
handlepngblip: token
	"state context atProperty: #type put: #png"
	state context at: #type put: #png! !

!RTFParser methodsFor: 'handlers-characters' stamp: 'tat 5/2/2006 01:46'!
handlepntxta: token
	"only skips the text that follows"
	((tokenizer lookAhead: 1) type = #string) 
		ifTrue: [tokenizer getToken].
! !

!RTFParser methodsFor: 'handlers' stamp: 'tat 5/2/2006 02:04'!
handlepntxtb: token
	self handlepntxta: token! !

!RTFParser methodsFor: 'handlers-paragraph' stamp: 'kalin 5/3/2006 11:29'!
handleqc: token

	builder buildParagraphAlignCenter! !

!RTFParser methodsFor: 'handlers-paragraph' stamp: 'kalin 5/3/2006 11:29'!
handleqj: token

		builder buildParagraphAlignJustified! !

!RTFParser methodsFor: 'handlers-paragraph' stamp: 'kalinm 5/4/2006 19:00'!
handleql: token

		builder buildParagraphAlignLeft! !

!RTFParser methodsFor: 'handlers-characters' stamp: 'tat 6/22/2006 22:53'!
handleqmspace: token
	builder buildAddQmSpace! !

!RTFParser methodsFor: 'handlers-paragraph' stamp: 'kalin 5/3/2006 11:30'!
handleqr: token
	
	builder buildParagraphAlignRight! !

!RTFParser methodsFor: 'handlers-general' stamp: 'tat 11/1/2006 15:11'!
handlerSupressText
	"suppress plain text output"
	state destination block: [:string | ] type: #ignore.
	self parseUntilBlockClose.! !

!RTFParser methodsFor: 'handlers-characters' stamp: 'tat 6/22/2006 22:42'!
handlerdblquote: token

	builder buildAddDoubleRightQuote! !

!RTFParser methodsFor: 'handlers-colortable' stamp: 'tat 5/5/2006 13:50'!
handlered: token
	state context red: (token arg)! !

!RTFParser methodsFor: 'handlers-paragraph' stamp: 'kalin 5/3/2006 11:47'!
handleri: token

	builder buildRightIndent: (token arg)! !

!RTFParser methodsFor: 'handlers-paragraph' stamp: 'kalin 5/3/2006 11:47'!
handlerin: token

	builder buildRightIndent: (token arg)! !

!RTFParser methodsFor: 'handlers-characters' stamp: 'tat 6/22/2006 22:54'!
handlerquote: token
	builder buildAddRightQuote! !

!RTFParser methodsFor: 'handlers-stylesheet' stamp: 'tat 11/1/2006 15:13'!
handles: token
	state destination type = #stylesheet
		ifFalse: [ builder buildApplyStylesheet: (token arg)]
		ifTrue: [		
			state context: (builder buildStartStylesheet).
			state context num: (token arg).
			state context type: #paragraph
		]! !

!RTFParser methodsFor: 'handlers-paragraph' stamp: 'MR 5/4/2006 13:26'!
handlesa: token
	"sa = space after"

	builder buildSpaceBelow: (token arg)! !

!RTFParser methodsFor: 'handlers-paragraph' stamp: 'MR 5/4/2006 13:27'!
handlesb: token
	"sb = space before"

	builder buildSpaceAbove: (token arg)! !

!RTFParser methodsFor: 'handlers-stylesheet' stamp: 'tat 5/6/2006 16:53'!
handlesbasedon: token
	state context basedon: (token arg)! !

!RTFParser methodsFor: 'handlers' stamp: 'kalin 5/3/2006 11:16'!
handlesectd: token
	
	builder buildResetSectionSettings! !

!RTFParser methodsFor: 'handlers-paragraph' stamp: 'tat 7/11/2006 22:11'!
handlesl: token

	builder buildLineSpacing: (token arg).

	"If the next command is not \slmult then we have to call it manually with default
	parameter 0"

	(tokenizer lookAhead: 1) word = #slmult ifFalse: [
		builder buildLineSpacingType: 0
	]
! !

!RTFParser methodsFor: 'handlers-paragraph' stamp: 'MR 5/6/2006 13:15'!
handleslmult: token

	builder buildLineSpacingType: (token arg)! !

!RTFParser methodsFor: 'handlers-stylesheet' stamp: 'tat 11/1/2006 15:11'!
handlestylesheet: token
	state destination block: [:string | (string endsWith: ';')
		ifTrue: [builder buildAddStylesheet: state context]
		ifFalse: [state context name: string]
	] type: #stylesheet.
	state context: (builder buildStartStylesheet)
! !

!RTFParser methodsFor: 'handlers-characters' stamp: 'kalin 5/3/2006 12:34'!
handletab: token

	builder buildAddTab! !

!RTFParser methodsFor: 'handlers-general' stamp: 'jmv 11/22/2011 15:23'!
handleu: token
	| string char |
	char _ token arg < 0
		ifTrue: [ RTFUnicode value: (65536 + token arg) or: nil ]
		ifFalse: [ RTFUnicode value: token arg or: nil ].

	"Do not output Unicode characters to a different destination, just suppress output"
	state destination type = #default ifFalse: [^self].

	"If we can represent char (i.e. it belongs in ISO 8859-15), add it, and skip next char
	(as it should be included only if the unicode char point is not recognized)"
	char ifNotNil: [
		string _ char asString.
		builder buildAddUnicodeContents: string.
		builder skipNextCharacters: 1 ]! !

!RTFParser methodsFor: 'handlers-characters' stamp: 'tat 1/16/2007 12:03'!
handleuc: token
	"this command states how many characters to skip after reading an unicode character"
	builder defaultSkipNextCharacters: token arg! !

!RTFParser methodsFor: 'handlers-characters' stamp: 'tat 6/21/2006 23:51'!
handleud: token
	"Do nothing. This handler exists only to state that the upcoming block of unicode text should not be ignored"! !

!RTFParser methodsFor: 'handlers-charformat' stamp: 'kalinm 5/4/2006 19:51'!
handleul: token

	builder buildSetUnderline: true! !

!RTFParser methodsFor: 'handlers-charformat' stamp: 'tat 6/13/2007 11:35'!
handleuld: token

	builder buildSetUnderline: true! !

!RTFParser methodsFor: 'handlers-charformat' stamp: 'tat 6/13/2007 11:35'!
handleuldash: token

	builder buildSetUnderline: true! !

!RTFParser methodsFor: 'handlers-charformat' stamp: 'tat 6/13/2007 11:35'!
handleuldashd: token

	builder buildSetUnderline: true! !

!RTFParser methodsFor: 'handlers-charformat' stamp: 'tat 6/13/2007 11:35'!
handleuldb: token

	builder buildSetUnderline: true! !

!RTFParser methodsFor: 'handlers-charformat' stamp: 'tat 6/13/2007 11:35'!
handleulhwave: token

	builder buildSetUnderline: true! !

!RTFParser methodsFor: 'handlers-charformat' stamp: 'tat 6/13/2007 11:35'!
handleulldash: token

	builder buildSetUnderline: true! !

!RTFParser methodsFor: 'handlers-charformat' stamp: 'tpr 9/6/2006 13:00'!
handleulnone: token

	builder buildSetUnderline: false! !

!RTFParser methodsFor: 'handlers-charformat' stamp: 'tat 6/13/2007 11:36'!
handleulth: token

	builder buildSetUnderline: true! !

!RTFParser methodsFor: 'handlers-charformat' stamp: 'tat 6/13/2007 11:36'!
handleulthd: token

	builder buildSetUnderline: true! !

!RTFParser methodsFor: 'handlers-charformat' stamp: 'tat 6/13/2007 11:36'!
handleulthdash: token

	builder buildSetUnderline: true! !

!RTFParser methodsFor: 'handlers-charformat' stamp: 'tat 6/13/2007 11:36'!
handleulthdashd: token

	builder buildSetUnderline: true! !

!RTFParser methodsFor: 'handlers-charformat' stamp: 'tat 6/13/2007 11:36'!
handleulthdashdd: token

	builder buildSetUnderline: true! !

!RTFParser methodsFor: 'handlers-charformat' stamp: 'tat 6/13/2007 11:36'!
handleulthldashd: token

	builder buildSetUnderline: true! !

!RTFParser methodsFor: 'handlers-charformat' stamp: 'tat 6/13/2007 11:36'!
handleululdbwave: token

	builder buildSetUnderline: true! !

!RTFParser methodsFor: 'handlers-charformat' stamp: 'tat 6/13/2007 11:36'!
handleulw: token

	builder buildSetUnderline: true! !

!RTFParser methodsFor: 'handlers-charformat' stamp: 'tat 6/13/2007 11:36'!
handleulwave: token

	builder buildSetUnderline: true! !

!RTFParser methodsFor: 'handlers-charformat' stamp: 'MR 6/7/2006 18:09'!
handleup: token
		
	builder buildSetBaselineOffset: (token arg)! !

!RTFParser methodsFor: 'handlers-characters' stamp: 'tat 6/21/2006 23:51'!
handleupr: token
	"{\upr{keyword ansi_text}{\*\ud{keyword Unicode_text}}}"

	"skip the first block of ansi text"

	self skipBlock

	"the Unicode block will be parsed by the \ud keyword"! !

!RTFParser methodsFor: 'handlers-pictures' stamp: 'tat 5/18/2007 18:54'!
handlewbitmap: token
	"Windows device-dependent bitmap (DDB)"
	"state context atProperty: #type put: #ddb"
	state context at: #type put: #ddb! !

!RTFParser methodsFor: 'handlers-pictures' stamp: 'tat 5/18/2007 18:51'!
handlewmetafile: token
	"Windows metafile"
	"state context atProperty: #type put: #wmf"
	state context at: #type put: #wmf! !

!RTFParser methodsFor: 'initialization' stamp: 'kalin 5/7/2006 19:25'!
initialize

	RTFToken initialize! !

!RTFParser methodsFor: 'parsing' stamp: 'tat 5/4/2006 18:52'!
parse
	self parseBlock.
! !

!RTFParser methodsFor: 'parsing' stamp: 'tat 5/4/2006 00:50'!
parse: string buildWith: b
	"parse the string with the specified builder"
	self parseWithTokenizer: (RTFTokenizer newFromString: string) buildWith: b! !

!RTFParser methodsFor: 'utilities' stamp: 'tat 8/13/2007 10:41'!
parseBlock
	| token |
	"parses a complete {..} block"

	"is there a block to parse?"
	token := (tokenizer lookAhead: 1).
	((token isNil not) and: [token type == #blockOpen]) ifFalse: [^self].
	
	"skip the { character"
	tokenizer getToken.

	"save the state"
	self saveState.

	"parse all words until the end of the block is reached"
	self parseUntilBlockClose.

	"skip the } character"
	tokenizer getToken.

	"restore the state"
	self restoreState! !

!RTFParser methodsFor: 'utilities' stamp: 'kalin 5/1/2006 14:08'!
parseCommand

	"parse a command, string or a block of commands
	disregarding their inner sctructure"

	|peeked|
	
	peeked := (tokenizer lookAhead: 1) type.
	
	(peeked == #blockOpen) 
		ifTrue: [self parseBlock].
	(peeked == #string) 
		ifTrue: [self addContents].
	(peeked == #keyword) 
		ifTrue: [self parseKeyWord].
! !

!RTFParser methodsFor: 'utilities' stamp: 'kalin 5/16/2006 12:04'!
parseKeyWord
	"assuming that the next RTF token is a keyword,
	 find appropriate keyword handler and execute it"

	| token message |

	token := tokenizer getToken.

	"build the message name for the RTF keyword"
	message := self createHandleMessage: token.

	(self respondsTo: message) ifTrue: [
		"Transcript show: 'Handling keyword: '; show: (token word); cr."
		self perform: message with: token
	] ifFalse: [
		"run a general handler"
		self handleAll: token
	]




	! !

!RTFParser methodsFor: 'utilities' stamp: 'tat 5/4/2006 23:09'!
parseUntilBlockClose
	"reads until a not parsed #blockClose token"

	[(tokenizer lookAhead: 1) type = #blockClose] whileFalse: 
		[self parseCommand]

! !

!RTFParser methodsFor: 'parsing' stamp: 'jmv 4/8/2011 11:29'!
parseWithTokenizer: t buildWith: b
	tokenizer := t.
	builder := b.
	state := RTFParserState new.
	"set default destination to builder's method to add contents"
	state destination block: [:string | builder buildAddContents: string] type: #default.
	self parse.
	builder finishBuild! !

!RTFParser methodsFor: 'utilities' stamp: 'tat 5/4/2006 23:59'!
restoreState
	"restores the state of the parser on exiting a block"
	state restoreState.
	builder restoreState! !

!RTFParser methodsFor: 'utilities' stamp: 'tat 5/4/2006 23:59'!
saveState
	"saves the state of the parser on entering a block"
	state saveState.
	builder saveState! !

!RTFParser methodsFor: 'utilities' stamp: 'tat 5/4/2006 23:11'!
skipBlock

	"skips a complete {..} block"

	"is there a block to skip?"
	((tokenizer lookAhead: 1) type = #blockOpen) ifFalse: [^self].
	
	"skip the { character"
	tokenizer getToken.

	"skip until }"
	self skipUntilBlockClose.

	"skip the } character"
	tokenizer getToken! !

!RTFParser methodsFor: 'utilities' stamp: 'kalin 4/30/2006 23:12'!
skipCommand

	"skips a command. skips the whole block if the next token
	is a #blockOpen"
	
	((tokenizer lookAhead: 1) type == #blockOpen) 
	   ifTrue: [self skipBlock]
	   ifFalse: [tokenizer getToken]
	
	! !

!RTFParser methodsFor: 'utilities' stamp: 'tat 5/4/2006 23:13'!
skipUntilBlockClose
	"skips until a not parsed #blockClose token"

	[(tokenizer lookAhead: 1) type = #blockClose] whileFalse: 
		[self skipCommand]

! !

!RTFParser class methodsFor: 'class initialization' stamp: 'tat 4/11/2007 14:28'!
initialize
	"RTFParser initialize"

	HandleMessages := Dictionary new: 11.
	HandleMessages
		at: '*' put: #handleStar: ;
		at: '|' put: #handleFormula: ;
		at: '~' put: #handlenbsp: ;
		at: '-' put: #handleopthyph: ;
		at: '_' put: #handlenbhyph: ;
		at: ':' put: #handlesubentry: ;
		at: '{' put: #handleLeftCurlyBracket: ;
		at: '}' put: #handleRightCurlyBracket: ;
		yourself.
! !

!RTFParserDestination methodsFor: 'accessing' stamp: 'tat 11/1/2006 15:03'!
block
	^block! !

!RTFParserDestination methodsFor: 'accessing' stamp: 'tat 11/1/2006 15:05'!
block: aBlock type: aSymbol
	block := aBlock.
	type := aSymbol! !

!RTFParserDestination methodsFor: 'accessing' stamp: 'tat 11/1/2006 15:03'!
type
	^type! !

!RTFParserDestination methodsFor: 'accessing' stamp: 'tat 11/1/2006 15:03'!
value: arg
	block ifNotNil: [^block value: arg]! !

!RTFParserState methodsFor: 'accessing' stamp: 'tat 5/4/2006 23:39'!
context
	^context! !

!RTFParserState methodsFor: 'accessing' stamp: 'tat 5/4/2006 23:39'!
context: anObject
	context := anObject! !

!RTFParserState methodsFor: 'accessing' stamp: 'tat 5/4/2006 23:39'!
destination
	^destination! !

!RTFParserState methodsFor: 'accessing' stamp: 'tat 5/4/2006 23:38'!
destination: aBlock
	destination := aBlock! !

!RTFParserState methodsFor: 'private' stamp: 'tat 5/4/2006 23:48'!
empty
	^stack size = 0! !

!RTFParserState methodsFor: 'initialization' stamp: 'tat 11/1/2006 15:06'!
initialize
	super initialize.
	destination := RTFParserDestination new.
	context := nil.
	stack := OrderedCollection new! !

!RTFParserState methodsFor: 'private' stamp: 'tat 5/5/2006 00:25'!
pop
	self empty ifTrue: [^nil] ifFalse: [^stack removeLast]! !

!RTFParserState methodsFor: 'private' stamp: 'tat 5/5/2006 00:25'!
push: anObject
	stack addLast: anObject! !

!RTFParserState methodsFor: 'stack' stamp: 'tat 5/4/2006 23:42'!
restoreState
	context := self pop.
	destination := self pop! !

!RTFParserState methodsFor: 'stack' stamp: 'tat 11/1/2006 15:00'!
saveState
	self push: (destination copy).
	self push: (context copy)! !

!RTFStylesheet methodsFor: 'accessing' stamp: 'tat 5/5/2006 14:50'!
additive
	additive := true! !

!RTFStylesheet methodsFor: 'accessing' stamp: 'tat 5/6/2006 15:53'!
basedon
	^basedon! !

!RTFStylesheet methodsFor: 'accessing' stamp: 'tat 5/6/2006 15:53'!
basedon: bo
	basedon := bo! !

!RTFStylesheet methodsFor: 'initializing' stamp: 'tat 5/6/2006 16:50'!
initialize
	additive := false.
	type := #paragraph.
	num := 0! !

!RTFStylesheet methodsFor: 'accessing' stamp: 'tat 5/5/2006 14:49'!
name
	^name! !

!RTFStylesheet methodsFor: 'accessing' stamp: 'tat 5/5/2006 14:49'!
name: n
	name := n! !

!RTFStylesheet methodsFor: 'accessing' stamp: 'tat 5/5/2006 15:02'!
num
	^num! !

!RTFStylesheet methodsFor: 'accessing' stamp: 'tat 5/5/2006 15:01'!
num: n
	num := n! !

!RTFStylesheet methodsFor: 'accessing' stamp: 'tat 5/5/2006 15:33'!
style
	^style! !

!RTFStylesheet methodsFor: 'accessing' stamp: 'tat 5/5/2006 15:32'!
style: s
	style := s! !

!RTFStylesheet methodsFor: 'accessing' stamp: 'tat 5/5/2006 14:49'!
type
	^type! !

!RTFStylesheet methodsFor: 'accessing' stamp: 'tat 5/5/2006 14:49'!
type: t
	type := t! !

!RTFSophieStylesheet methodsFor: 'applying' stamp: 'tat 3/11/2007 22:15'!
applyTo: builder
	"applies the stylesheet to the builder"
	basedon isNil ifFalse: [ builder buildApplyStylesheet: basedon].
	additive ifFalse: [ self resetStyle: builder ].
	style do: [:aBlock | aBlock value: builder state]
	! !

!RTFSophieStylesheet methodsFor: 'applying' stamp: 'tat 5/6/2006 17:05'!
getStyleFrom: builder
	type = #paragraph ifTrue: [^builder activeParagraphStyle].
	type = #character ifTrue: [^builder activeCharacterStyle]! !

!RTFSophieStylesheet methodsFor: 'applying' stamp: 'tat 5/6/2006 17:40'!
resetStyle: builder
	type = #paragraph ifTrue: [builder buildResetParagraphSettings. builder buildResetCharFormat].
	type = #character ifTrue: [builder buildResetCharFormat ]! !

!RTFTextBuilder methodsFor: 'private' stamp: 'jmv 3/30/2011 15:53'!
addToText: aString
	self addToText: aString specialAttributes: nil! !

!RTFTextBuilder methodsFor: 'private' stamp: 'jmv 4/11/2011 21:58'!
addToText: aString specialAttributes: nonFormattingAttributesOrNil
	"nonFormattingAttributesOrNil should only contains attributes that answer false to #isForFormatting"
	| attributes emphasis |

	attributes _ Array streamContents: [ :strm |
		fontFamilyName ifNotNil: [
			fontPointSize ifNotNil: [
				strm nextPut: (TextFontFamilyAndSize
					familyName: fontFamilyName pointSize: fontPointSize) ]].
		emphasis _ 0.
		bold ifTrue: [ emphasis _ emphasis + 1 ].
		italic ifTrue: [ emphasis _ emphasis + 2 ].
		underline ifTrue: [ emphasis _ emphasis + 4 ].
		emphasis > 0 ifTrue: [
			strm nextPut: (TextEmphasis new emphasisCode: emphasis) ].
		currentFgColor ifNotNil: [
			strm nextPut: (TextColor color: currentFgColor) ].
		nonFormattingAttributesOrNil ifNotNil: [ strm nextPutAll: nonFormattingAttributesOrNil ]].
	textStream nextPutAllString: aString withAttributes: attributes! !

!RTFTextBuilder methodsFor: 'building' stamp: 'jmv 11/22/2011 15:21'!
addUnicodeContents: string

	"Add Unicode strings to the content tree. Skip characters if prescribed by the \uc command (see doSkipNextCharacters)"	

	string size <= skipNextCharacters ifTrue: [
		self skipNextCharacters: skipNextCharacters - (string size). ^self].

	"For unicode characters do not use converter"
	self addToText: (string allButFirst: skipNextCharacters).

	self skipNextCharacters: 0! !

!RTFTextBuilder methodsFor: 'private' stamp: 'jmv 3/30/2011 16:36'!
attachCharacterStyleToParagraph
	"Ver si hace falta algo como lo que hace Sophie..."! !

!RTFTextBuilder methodsFor: 'building-characters' stamp: 'jmv 4/6/2011 10:39'!
buildAddBullet
	self addUnicodeContents: (RTFUnicode value: 16r2022 or: $°) asString! !

!RTFTextBuilder methodsFor: 'building-header' stamp: 'jmv 3/29/2011 22:26'!
buildAddColorDef: coldef
	"adds an entry to the color table"
	(coldef red isNil or: [coldef green isNil or: [coldef blue isNil]])
		ifTrue: [colorTable addLast: nil]
		ifFalse: [colorTable addLast: (Color r: (coldef red) / 255 g: (coldef green) / 255 b: (coldef blue) / 255)]! !

!RTFTextBuilder methodsFor: 'building' stamp: 'jmv 3/30/2011 15:42'!
buildAddContents: string
	| possibleString |
	possibleString _ self convertAndSkip: string.
	possibleString ifNil: [^self].

	self addToText: possibleString! !

!RTFTextBuilder methodsFor: 'building-characters' stamp: 'jmv 4/6/2011 10:39'!
buildAddDoubleLeftQuote
	self addUnicodeContents: (RTFUnicode value: 16r201C or: $") asString! !

!RTFTextBuilder methodsFor: 'building-characters' stamp: 'jmv 4/6/2011 10:39'!
buildAddDoubleRightQuote
	self addUnicodeContents: (RTFUnicode value: 16r201D or: $") asString! !

!RTFTextBuilder methodsFor: 'building-characters' stamp: 'jmv 4/6/2011 10:39'!
buildAddEmDash
	self addUnicodeContents: (RTFUnicode value: 16r2014 or: '--') asString! !

!RTFTextBuilder methodsFor: 'building-characters' stamp: 'jmv 4/6/2011 10:39'!
buildAddEmSpace
	self addUnicodeContents: (RTFUnicode value: 16r2003 or: '  ') asString! !

!RTFTextBuilder methodsFor: 'building-characters' stamp: 'jmv 4/6/2011 10:39'!
buildAddEnDash
	self addUnicodeContents: (RTFUnicode value: 16r2013 or: $-) asString! !

!RTFTextBuilder methodsFor: 'building-characters' stamp: 'jmv 4/6/2011 10:39'!
buildAddEnSpace
	self addUnicodeContents: (RTFUnicode value: 16r2002 or: ' ') asString! !

!RTFTextBuilder methodsFor: 'building-header' stamp: 'jmv 3/30/2011 16:38'!
buildAddFontInfo: fontInfo
	"add font information to the table"
	"fontTable atProperty: (fontInfo num) put: fontInfo"
	fontTable at: (fontInfo num) put: fontInfo! !

!RTFTextBuilder methodsFor: 'building-characters' stamp: 'jmv 4/6/2011 10:39'!
buildAddLeftQuote
	self addUnicodeContents: (RTFUnicode value: 16r2018 or: $') asString! !

!RTFTextBuilder methodsFor: 'building-characters' stamp: 'jmv 4/6/2011 10:39'!
buildAddNonBreakingHyphen
	self addUnicodeContents: (RTFUnicode value: 16r2011 or: '-') asString! !

!RTFTextBuilder methodsFor: 'building-characters' stamp: 'jmv 4/6/2011 10:39'!
buildAddNonBreakingSpace
	"This character is supported by ISO-8859-15"
	self addUnicodeContents: (RTFUnicode value: 16r00A0) asString! !

!RTFTextBuilder methodsFor: 'building-characters' stamp: 'jmv 4/6/2011 10:39'!
buildAddOptionalHyphen
	"This character is supported by ISO-8859-15"
	self addUnicodeContents: (RTFUnicode value: 16r00AD) asString! !

!RTFTextBuilder methodsFor: 'building-pictures' stamp: 'jmv 3/30/2011 17:18'!
buildAddPicture: picInfo from: aStream

	(ImageReadWriter formFromStream: aStream)
		ifNotNil: [ :form |
			self addToText: '*' specialAttributes: {TextAnchor new anchoredFormOrMorph: form} ]! !

!RTFTextBuilder methodsFor: 'building-characters' stamp: 'jmv 4/6/2011 10:39'!
buildAddQmSpace
	self addUnicodeContents: (RTFUnicode value: 16r2005 or: ' ') asString! !

!RTFTextBuilder methodsFor: 'building-characters' stamp: 'jmv 4/6/2011 10:39'!
buildAddRightQuote
	self addUnicodeContents: (RTFUnicode value: 16r2019 or: $') asString! !

!RTFTextBuilder methodsFor: 'building-header' stamp: 'jmv 3/30/2011 16:38'!
buildAddStylesheet: pp! !

!RTFTextBuilder methodsFor: 'building-characters' stamp: 'jmv 4/6/2011 10:39'!
buildAddTab
	"This character is supported by ISO-8859-15"
	self addUnicodeContents: (RTFUnicode value: 16r0009) asString! !

!RTFTextBuilder methodsFor: 'building' stamp: 'jmv 3/30/2011 15:55'!
buildAddURI: uriString alternate: string
	| possibleString |
	possibleString _ self convertAndSkip: string.
	possibleString ifNil: [^self].
	
	self addToText: possibleString specialAttributes: { TextURL new url: uriString }! !

!RTFTextBuilder methodsFor: 'building' stamp: 'jmv 3/30/2011 15:57'!
buildAddUnicodeContents: string

	self addToText: string! !

!RTFTextBuilder methodsFor: 'building-general' stamp: 'jmv 4/6/2011 10:44'!
buildAnsiCharacterSet
	self textConverter: RTFLatin1TextConverter new! !

!RTFTextBuilder methodsFor: 'building-general' stamp: 'jmv 3/30/2011 16:39'!
buildApplyStylesheet:pp! !

!RTFTextBuilder methodsFor: 'building-general' stamp: 'jmv 3/30/2011 16:39'!
buildCodePage: cp
	self textConverter: (CodePageConverterTable at: cp ifAbsent: [^nil]) new! !

!RTFTextBuilder methodsFor: 'building-paragraph' stamp: 'jmv 4/8/2011 15:06'!
buildFirstLineIndent: aNumber
	"Guardar en una ivar, como todo lo demas.
	meter en el stack tambien.
	Quizas cambiar el stack... meter self copy????? no se.
	
	Despues usar al construir el parastyle
	
	"
	firstIndent _ (aNumber / 20.0) rounded! !

!RTFTextBuilder methodsFor: 'building-paragraph' stamp: 'jmv 3/30/2011 16:27'!
buildFirstLineIndentPercentage: i! !

!RTFTextBuilder methodsFor: 'building-paragraph' stamp: 'jmv 4/8/2011 15:06'!
buildLeftIndent: aNumber

	leftIndent _ (aNumber / 20.0) rounded! !

!RTFTextBuilder methodsFor: 'building-paragraph' stamp: 'jmv 3/30/2011 16:28'!
buildLeftIndentPercentage: i! !

!RTFTextBuilder methodsFor: 'building-paragraph' stamp: 'jmv 3/30/2011 16:28'!
buildLineSpacing:pp! !

!RTFTextBuilder methodsFor: 'building-paragraph' stamp: 'jmv 3/30/2011 16:28'!
buildLineSpacingType:pp! !

!RTFTextBuilder methodsFor: 'building-general' stamp: 'jmv 3/30/2011 16:40'!
buildMacCharacterSet! !

!RTFTextBuilder methodsFor: 'building-paragraph' stamp: 'jmv 3/30/2011 16:28'!
buildParagraphAlignCenter
	align _ 2! !

!RTFTextBuilder methodsFor: 'building-paragraph' stamp: 'jmv 4/7/2011 09:06'!
buildParagraphAlignJustified
"Atencion. Este, y cualquier otro de parrafo, falla si la ultima linea NO termina en enter... atrapar el final, y hacer algo como #buildStartParagraph..."
align _ 3! !

!RTFTextBuilder methodsFor: 'building-paragraph' stamp: 'jmv 4/11/2011 09:58'!
buildParagraphAlignLeft

	align _ 0! !

!RTFTextBuilder methodsFor: 'building-paragraph' stamp: 'jmv 3/30/2011 16:28'!
buildParagraphAlignRight
align _ 1! !

!RTFTextBuilder methodsFor: 'building-charformat' stamp: 'jmv 3/30/2011 16:20'!
buildResetCharFormat! !

!RTFTextBuilder methodsFor: 'building-paragraph' stamp: 'jmv 4/11/2011 21:39'!
buildResetParagraphSettings
	"resets the paragraph settings to the default paragraph 
	settings"
	
	bold _ italic _ underline _ false.
	align _ 0. "left"
	
	firstIndent _ leftIndent _ spaceBefore _ spaceAfter _ 0.
	rightIndent _ nil! !

!RTFTextBuilder methodsFor: 'building-general' stamp: 'jmv 3/30/2011 16:40'!
buildResetSectionSettings! !

!RTFTextBuilder methodsFor: 'building-paragraph' stamp: 'jmv 4/8/2011 15:06'!
buildRightIndent: aNumber
	rightIndent _ (aNumber / 20.0) rounded! !

!RTFTextBuilder methodsFor: 'building-paragraph' stamp: 'jmv 3/30/2011 16:29'!
buildRightIndentPercentage: i! !

!RTFTextBuilder methodsFor: 'building-charformat' stamp: 'jmv 3/30/2011 16:19'!
buildSetBaselineOffset: s
	"sets the font size in half points"! !

!RTFTextBuilder methodsFor: 'building-charformat' stamp: 'jmv 3/30/2011 16:21'!
buildSetBkColor: index
	"Ignored in Sophie"! !

!RTFTextBuilder methodsFor: 'building-charformat' stamp: 'jmv 3/30/2011 16:19'!
buildSetBold: aBoolean
	bold _ aBoolean! !

!RTFTextBuilder methodsFor: 'building-charformat' stamp: 'jmv 3/30/2011 16:21'!
buildSetCaps
	"Ignored in Sophie"! !

!RTFTextBuilder methodsFor: 'building-header' stamp: 'jmv 3/30/2011 16:38'!
buildSetDefaultFont: pp! !

!RTFTextBuilder methodsFor: 'building-charformat' stamp: 'jmv 3/30/2011 16:19'!
buildSetFgColor: zindex
		"sets the current foreground color to
		color with the given index in the
		color table"
| index |
index _ zindex + 1.
currentFgColor _ colorTable at: index! !

!RTFTextBuilder methodsFor: 'building-charformat' stamp: 'jmv 3/31/2011 15:36'!
buildSetFont: i
	"en Sophie, pone el textConverter... ver!!"
	"Hasta donde entiendo, el font NO incluye emphasis ni pointSize, solo la familia, charset y code page"! !

!RTFTextBuilder methodsFor: 'building-charformat' stamp: 'jmv 4/12/2011 09:16'!
buildSetFontSize: s
	"sets the font size in half points"

	"Note font shoudl always be a baseFont, any emphasis should be done by emphasis attributes"
	fontPointSize _ (s / 2.0 / Text pointSizeConversionFactor) rounded.
	fontPointSize = 0
		ifTrue: [
			fontFamilyName _ nil.
			fontPointSize _ nil ]
		ifFalse: [ fontFamilyName _ 'DejaVu' ]! !

!RTFTextBuilder methodsFor: 'building-charformat' stamp: 'jmv 3/30/2011 16:20'!
buildSetItalic: aBoolean
	italic _ aBoolean! !

!RTFTextBuilder methodsFor: 'building-charformat' stamp: 'jmv 4/8/2011 09:43'!
buildSetKerning: popo! !

!RTFTextBuilder methodsFor: 'building-charformat' stamp: 'jmv 3/30/2011 16:20'!
buildSetUnderline: aBoolean
	underline _ aBoolean! !

!RTFTextBuilder methodsFor: 'building-paragraph' stamp: 'jmv 4/8/2011 15:06'!
buildSpaceAbove: aNumber
	spaceBefore _ (aNumber / 20.0) rounded! !

!RTFTextBuilder methodsFor: 'building-paragraph' stamp: 'jmv 4/8/2011 15:06'!
buildSpaceBelow: aNumber
	spaceAfter _ (aNumber / 20.0) rounded! !

!RTFTextBuilder methodsFor: 'building-paragraph' stamp: 'jmv 4/8/2011 11:36'!
buildStartParagraph

	self finishParagraph! !

!RTFTextBuilder methodsFor: 'building-header' stamp: 'jmv 3/30/2011 16:38'!
buildStartStylesheet
	"?????"
	^RTFStylesheet new! !

!RTFTextBuilder methodsFor: 'private' stamp: 'jmv 11/22/2011 15:26'!
convert: string startingAt: startIndex withConverter: converter

	| writeStream |
	converter ifNil: [^string copyFrom: startIndex to: string size].
	writeStream _ (String new: string size) writeStream.
	startIndex to: string size do: [:index |
		writeStream nextPut: (converter toSqueak: (string at: index))].
	^writeStream contents! !

!RTFTextBuilder methodsFor: 'building' stamp: 'jmv 11/22/2011 15:26'!
convertAndSkip: string

	"Convert non-Unicode string using the specified encoder.
	Skip characters if prescribed by the \uc command (see doSkipNextCharacters)."

	| convertedString |
	
	"Check if some characters need to be skipped and do the math"
	(string size <= skipNextCharacters and: [string size > 0])
		ifTrue: [
			self skipNextCharacters: skipNextCharacters - (string size).
			^nil].

	"convert the string using the specified encoder"
	convertedString := self
		convert: string
		startingAt: skipNextCharacters+1
		withConverter: self textConverter.

	"don't skip characters anymore"
	self skipNextCharacters: 0.

	^convertedString! !

!RTFTextBuilder methodsFor: 'accessing' stamp: 'jmv 3/29/2011 10:15'!
defaultSkipNextCharacters: anInteger
	defaultSkipNextCharacters := anInteger! !

!RTFTextBuilder methodsFor: 'building-general' stamp: 'jmv 4/11/2011 10:18'!
finishBuild
	"Don't do this. It looks like it would help, include the paragraphstyle of the last paragraph, but it will hurt when, for example, we are pasting one or a few words, not an entire paragraph."
"	self finishParagraph"! !

!RTFTextBuilder methodsFor: 'building-paragraph' stamp: 'jmv 3/13/2012 16:57'!
finishParagraph

	| attrs |
	"Don't include attributes for left alignment. No need to."
	attrs _ align = 0
		ifTrue: [ #() ]
		ifFalse: [ { TextAlignment new alignment: align } ].
	textStream
		nextPutAllString: String newLineString
		withAttributes: attrs! !

!RTFTextBuilder methodsFor: 'initialize' stamp: 'jmv 4/11/2011 21:39'!
initialize
	
"	state := RTFSophieContentTreeBuilderState new."

	"the active character style is the character style of the current rtf block
	being built. It is modified by methods from the buiding-charformat section and
	is used mainly by the buildAddContents method"

"	self buildResetCharFormat."
	textStream _ TextStream on: (Text string: (String new: 400)).

"	paragraphStarted := false."

	colorTable := OrderedCollection new.

	fontTable := Dictionary new.

	styleTable := Dictionary new.
	bold _ italic _ underline _ false.
	align _ 0. "left"
	firstIndent _ leftIndent _ spaceBefore _ spaceAfter _ 0.
	rightIndent _ nil.
	
	stateStack _ OrderedCollection new.

"	styleChangeList := nil."

"	self buildStartParagraph."

	"RTF spec says assume default value of \uc1"
	self defaultSkipNextCharacters: 1.

	"reset skip next characters counter to 0"
	self skipNextCharacters: 0.
! !

!RTFTextBuilder methodsFor: 'state' stamp: 'jmv 4/8/2011 15:07'!
restoreState

	spaceAfter _ stateStack removeLast.
	spaceBefore _ stateStack removeLast.
	rightIndent _ stateStack removeLast.
	leftIndent _ stateStack removeLast.
	firstIndent _ stateStack removeLast.
	align _ stateStack removeLast.
	underline _ stateStack removeLast.
	italic _ stateStack removeLast.
	bold _ stateStack removeLast.
	fontPointSize _ stateStack removeLast.
	fontFamilyName _ stateStack removeLast.
	currentFgColor _ stateStack removeLast.
	defaultSkipNextCharacters _ stateStack removeLast.
	textConverter _ stateStack removeLast! !

!RTFTextBuilder methodsFor: 'state' stamp: 'jmv 4/8/2011 15:08'!
saveState
	
	stateStack
		addLast: textConverter;
		addLast: defaultSkipNextCharacters;
		addLast: currentFgColor;
		addLast: fontFamilyName;
		addLast: fontPointSize;
		addLast: bold;
		addLast: italic;
		addLast: underline;
		addLast: align;
		addLast: firstIndent;
		addLast: leftIndent;
		addLast: rightIndent;
		addLast: spaceBefore;
		addLast: spaceAfter! !

!RTFTextBuilder methodsFor: 'accessing' stamp: 'jmv 3/29/2011 10:14'!
skipNextCharacters: value
	"sets a count to skip the next character added with simplyAddContents:"
	skipNextCharacters := value! !

!RTFTextBuilder methodsFor: 'accessing' stamp: 'jmv 3/30/2011 16:34'!
text
	"viene a ser equivalente a #contentTree, no?
	En ese caso, llamara #finishBuild, PERO SOLO UNA VEZ!!
	"
	"
	solo una vez ifTrue: [
		self finishBuild ].
	"
	^textStream contents! !

!RTFTextBuilder methodsFor: 'accessing' stamp: 'jmv 3/29/2011 10:16'!
textConverter
	^textConverter! !

!RTFTextBuilder methodsFor: 'accessing' stamp: 'jmv 3/29/2011 10:24'!
textConverter: aTextConverter
	textConverter := aTextConverter! !

!RTFTextBuilder methodsFor: 'private' stamp: 'jmv 4/6/2011 10:38'!
textConverterFromCharset: n
	"returns a new text converter from a given font charset"
	n = 77 ifTrue: [^RTFMacRomanUnicodeTextConverter new].
	n = 204 ifTrue: [^RTFCP1251UnicodeTextConverter new].
	n = 0 ifTrue: [^RTFCP1252UnicodeTextConverter new].
	^self textConverter! !

!RTFTextBuilder class methodsFor: 'instance creation' stamp: 'jmv 4/11/2011 18:10'!
currentClass
	^Smalltalk at: #RTFStyledTextBuilder ifAbsent: [ self ]! !

!RTFTextBuilder class methodsFor: 'class initialization' stamp: 'jmv 4/6/2011 10:37'!
initialize
	"
	self initialize
	"
	(CodePageConverterTable := Dictionary new)
	"atProperty: 10000 put: MacRomanUnicodeTextConverter;
	atProperty: 1250 put: CP1250UnicodeTextConverter;
	atProperty: 1251 put: CP1251UnicodeTextConverter;
	atProperty: 1252 put: CP1252UnicodeTextConverter"
	at: 10000 put: RTFMacRomanUnicodeTextConverter;
	at: 1250 put: RTFCP1250UnicodeTextConverter;
	at: 1251 put: RTFCP1251UnicodeTextConverter;
	at: 1252 put: RTFCP1252UnicodeTextConverter! !

!RTFTextConverter class methodsFor: 'instance creation' stamp: 'jmv 3/29/2011 08:43'!
default
	"add if neeced"
	^ "UTF8TextConverter new." nil! !

!RTFTextConverter class methodsFor: 'services' stamp: 'jmv 4/4/2011 16:28'!
digitValue: char

	| value |
	value _ char asciiValue.
	value <= $9 asciiValue 
		ifTrue: [^value - $0 asciiValue].
	value >= $A asciiValue 
		ifTrue: [value <= $Z asciiValue ifTrue: [^value - $A asciiValue + 10]].
	value >= $a asciiValue 
		ifTrue: [value <= $z asciiValue ifTrue: [^value - $a asciiValue + 10]].
! !

!RTFLatin1TextConverter methodsFor: 'conversion' stamp: 'mir 8/13/2006 17:36'!
toSqueak: aChar
	^aChar! !

!RTFMappingUnicodeTextConverter methodsFor: 'conversion' stamp: 'tat 8/2/2006 22:15'!
toSqueak: char
	^self subclassResponsibility! !

!RTFMappingUnicodeTextConverter methodsFor: 'conversion' stamp: 'jmv 4/6/2011 10:40'!
toSqueak: char withTable: table

	| value |
	value _ char asciiValue.
	value < 128 ifTrue: [^ char].
	value > 255 ifTrue: [^ char].
	^ RTFUnicode value: (table at: (value - 128 + 1)).
! !

!RTFCP1250UnicodeTextConverter methodsFor: 'conversion' stamp: 'tat 8/2/2006 22:49'!
toSqueak: char
	^self toSqueak: char withTable: #(8364 129 8218 131 8222 8230 8224 8225 136 8240 352 8249 346 356 381 377 144 8216 8217 8220 8221 8226 8211 8212 152 8482 353 8250 347 357 382 378 160 711 728 321 164 260 166 167 168 169 350 171 172 173 174 379 176 177 731 322 180 181 182 183 184 261 351 187 317 733 318 380 340 193 194 258 196 313 262 199 268 201 280 203 282 205 206 270 272 323 327 211 212 336 214 215 344 366 218 368 220 221 354 223 341 225 226 259 228 314 263 231 269 233 281 235 283 237 238 271 273 324 328 243 244 337 246 247 345 367 250 369 252 253 355 729)
! !

!RTFCP1251UnicodeTextConverter methodsFor: 'conversion' stamp: 'tat 8/2/2006 22:49'!
toSqueak: char
	^self toSqueak: char withTable: #(1026 1027 8218 1107 8222 8230 8224 8225 8364 8240 1033 8249 1034 1036 1035 1039 1106 8216 8217 8220 8221 8226 8211 8212 152 8482 1113 8250 1114 1116 1115 1119 160 1038 1118 1032 164 1168 166 167 1025 169 1028 171 172 173 174 1031 176 177 1030 1110 1169 181 182 183 1105 8470 1108 187 1112 1029 1109 1111 1040 1041 1042 1043 1044 1045 1046 1047 1048 1049 1050 1051 1052 1053 1054 1055 1056 1057 1058 1059 1060 1061 1062 1063 1064 1065 1066 1067 1068 1069 1070 1071 1072 1073 1074 1075 1076 1077 1078 1079 1080 1081 1082 1083 1084 1085 1086 1087 1088 1089 1090 1091 1092 1093 1094 1095 1096 1097 1098 1099 1100 1101 1102 1103)
! !

!RTFCP1252UnicodeTextConverter methodsFor: 'conversion' stamp: 'tat 8/2/2006 22:49'!
toSqueak: char
	^self toSqueak: char withTable: #(8364 129 8218 402 8222 8230 8224 8225 710 8240 352 8249 338 141 381 143 144 8216 8217 8220 8221 8226 8211 8212 732 8482 353 8250 339 157 382 376 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216 217 218 219 220 221 222 223 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 240 241 242 243 244 245 246 247 248 249 250 251 252 253 254 255)
! !

!RTFMacRomanUnicodeTextConverter methodsFor: 'conversion' stamp: 'tat 8/2/2006 22:13'!
toSqueak: char
	^self toSqueak: char withTable: #(
		196 197 199 201 209 214 220 225 224 226 228 227 229 231 233 232
		234 235 237 236 238 239 241 243 242 244 246 245 250 249 251 252
		8224 176 162 163 167 8226 182 223 174 169 8482 180 168 8800 198 216
		8734 177 8804 8805 165 181 8706 8721 8719 960 8747 170 186 937 230 248
		191 161 172 8730 402 8776 8710 171 187 8230 160 192 195 213 338 339
		8211 8212 8220 8221 8216 8217 247 9674 255 376 8260 8364 8249 8250 64257 64258
		8225 183 8218 8222 8240 194 202 193 203 200 205 206 207 204 211 212
		63743 210 218 219 217 305 710 732 175 728 729 730 184 733 731 711 256)! !

!RTFToken methodsFor: 'accessing' stamp: 'tat 7/11/2006 00:38'!
arg

    "returns the argument of this keyword token"

	self isKeyword ifFalse: [^nil].

	arg isNil not ifTrue: [^arg].

     (DefaultArgs includesKey: self word)
		ifTrue: [^ DefaultArgs at: self word].

	^0! !

!RTFToken methodsFor: 'converting' stamp: 'MR 5/5/2006 00:24'!
asString
	self isKeyword ifTrue: [(arg = nil) ifTrue: [^'keyword:', content] ifFalse: [^'keyword:', content, ' ', (arg asString)]].
	self isStringToken ifTrue: [^'string:', '!!',content,'!!'].
	^type asString.! !

!RTFToken methodsFor: 'deprecated' stamp: 'kalin 5/7/2006 19:14'!
getArg

	self isKeyword ifTrue: [^arg].
	^nil! !

!RTFToken methodsFor: 'deprecated' stamp: 'kalin 5/7/2006 19:14'!
getString

	self isString ifTrue: [^content].
	^nil! !

!RTFToken methodsFor: 'deprecated' stamp: 'MR 5/1/2006 16:23'!
getType
	self isBlockOpen ifTrue: [^#blockOpen].
	self isBlockClose ifTrue: [^#blockClose].
	self isKeyword ifTrue: [^#keyword].
	self isStringToken ifTrue: [^#string].
	^nil
! !

!RTFToken methodsFor: 'deprecated' stamp: 'kalin 5/7/2006 19:14'!
getWord
    
    "if this token is a command, returns the command's keyword.
     For example:   \kw0102  getWord -> kw"

	self isKeyword ifTrue: [^content].
	^nil! !

!RTFToken methodsFor: 'testing' stamp: 'MR 4/30/2006 23:10'!
isBlockClose
	^(type = #blockClose)! !

!RTFToken methodsFor: 'testing' stamp: 'MR 4/30/2006 23:10'!
isBlockOpen
	^(type = #blockOpen)! !

!RTFToken methodsFor: 'testing' stamp: 'MR 4/30/2006 23:10'!
isKeyword
	^(type = #keyword)! !

!RTFToken methodsFor: 'testing' stamp: 'MR 5/1/2006 16:23'!
isStringToken
	^(type = #string)! !

!RTFToken methodsFor: 'accessing' stamp: 'kalin 5/7/2006 19:15'!
string
    "returns the contents of this string token"

	self isStringToken ifTrue: [^content].
	^nil! !

!RTFToken methodsFor: 'private' stamp: 'MR 7/4/2006 10:50'!
string: aContent
	content := aContent.
	type := #string! !

!RTFToken methodsFor: 'accessing' stamp: 'MR 5/2/2006 13:48'!
type
	self isBlockOpen ifTrue: [^#blockOpen].
	self isBlockClose ifTrue: [^#blockClose].
	self isKeyword ifTrue: [^#keyword].
	self isStringToken ifTrue: [^#string].
	^nil
! !

!RTFToken methodsFor: 'private' stamp: 'MR 7/4/2006 10:50'!
type: aType
	type := aType.! !

!RTFToken methodsFor: 'accessing' stamp: 'kalin 5/7/2006 19:14'!
word
    
    "if this token is a command, returns the command's keyword.
     For example:   \kw0102  getWord -> kw"

	self isKeyword ifTrue: [^content].
	^nil! !

!RTFToken methodsFor: 'private' stamp: 'MR 4/30/2006 23:16'!
word: aWord
	self word: aWord withArg: nil! !

!RTFToken methodsFor: 'private' stamp: 'MR 7/4/2006 10:50'!
word: aWord withArg: anArg
	content := aWord.
	arg := anArg.
	type := #keyword! !

!RTFToken class methodsFor: 'instance creation' stamp: 'tat 5/4/2006 00:25'!
emptyToken
	^self new type: #empty; word: #empty! !

!RTFToken class methodsFor: 'class initialization' stamp: 'kalin 5/7/2006 19:23'!
initialize

	self initializeDefaultArgs.! !

!RTFToken class methodsFor: 'class initialization' stamp: 'tat 7/10/2006 23:31'!
initializeDefaultArgs

	DefaultArgs := Dictionary new.

	DefaultArgs at: #i put: 1.
	DefaultArgs at: #b put: 1.
! !

!RTFToken class methodsFor: 'instance creation' stamp: 'MR 7/4/2006 10:51'!
newBlockClose
	| instance |
	instance := self new.
	instance type: #blockClose.
	^instance! !

!RTFToken class methodsFor: 'instance creation' stamp: 'MR 7/4/2006 10:51'!
newBlockOpen
	| instance |
	instance := self new.
	instance type: #blockOpen.
	^instance! !

!RTFToken class methodsFor: 'instance creation' stamp: 'MR 4/30/2006 23:24'!
newKeyword: aWord
	^(self newKeyword: aWord withArg: nil)! !

!RTFToken class methodsFor: 'instance creation' stamp: 'MR 7/4/2006 10:51'!
newKeyword: aWord withArg: anArg
	| instance |
	instance := self new.
	instance word: aWord withArg: anArg.
	^instance! !

!RTFToken class methodsFor: 'instance creation' stamp: 'MR 7/4/2006 10:51'!
newString: aString
	| instance |
	instance := self new.
	instance string: aString.
	^instance! !

!RTFTokenizer methodsFor: 'private' stamp: 'MR 5/3/2006 13:26'!
addStringToken: aString to: aBuffer
	"puts the new token to the buffer only if it is not empty"
	aString ifNotEmpty: [aBuffer add: (RTFToken newString: aString)]! !

!RTFTokenizer methodsFor: 'private' stamp: 'mir 8/12/2006 14:24'!
chunkBuffer
	^chunkBuffer ifNil: [chunkBuffer := String new: 2048]! !

!RTFTokenizer methodsFor: 'private' stamp: 'mir 8/11/2006 20:54'!
controlSymbolSet
	^ControlSymbolSet! !

!RTFTokenizer methodsFor: 'private' stamp: 'mir 8/11/2006 20:53'!
endOfArgumentSet
	^EndOfArgumentSet! !

!RTFTokenizer methodsFor: 'private' stamp: 'mir 8/11/2006 21:01'!
endOfKeywordSet
	^EndOfKeywordSet! !

!RTFTokenizer methodsFor: 'tokenizing' stamp: 'MR 5/5/2006 14:30'!
getToken

	self readNext.
	^self peekLast! !

!RTFTokenizer methodsFor: 'tokenizing' stamp: 'kalin 5/6/2006 01:04'!
lookAhead: n     
     "peeks n tokens ahead where n is less or equal to 3"

	n = 0 ifTrue: [^self peekLast].
	n = 1 ifTrue: [^self peekNext].
	n = 2 ifTrue: [^self peekAfterNext].
	n = 3 ifTrue: [^self peekAfterAfter].
! !

!RTFTokenizer methodsFor: 'tokenizing' stamp: 'kalin 5/6/2006 01:05'!
moreTokens
    "are there any tokens left?"

	^next notNil
! !

!RTFTokenizer methodsFor: 'private' stamp: 'tat 8/13/2007 11:00'!
on: aStream

	"this message expects proper RTF as input"

	stream := aStream.
	last := nil.
	buffer := stream upTo: $\.
	(buffer = '{')
		ifTrue: [next := RTFToken newBlockOpen]
		ifFalse: [RTFException new signal: 'Not a valid RTF file'].
	self parseChunk.
	"buffer cannot be empty"
	afterNext := buffer removeFirst.
	buffer ifEmpty: [self parseChunk].
	afterAfter := buffer removeFirst.
	
	
	
! !

!RTFTokenizer methodsFor: 'private' stamp: 'mir 8/31/2006 12:27'!
parseChunk

	"reads a string between two backslashes into buffer"

	| index token word arg tokenFirst tokenStream |

	buffer := OrderedCollection new.
	stream atEnd ifTrue: [^buffer].

	tokenStream := self chunkBuffer writeStream.
	self readStreamUpToNoEscapeInto: tokenStream.
	token := tokenStream contents.

	"control words start with alphabetic character,
	control symbols are special character"

	index := 1.
	tokenFirst := token first.
	(self controlSymbolSet includes: tokenFirst)
		ifTrue: [
			buffer add: (RTFToken newKeyword: (tokenFirst asString)).
			index := index+1]
		ifFalse: [ "this is a keyword"
			index :=  token indexOfAnyOf: self endOfKeywordSet.
			(index = 0)
				ifTrue: [
					buffer add: (RTFToken newKeyword: token).
					^buffer]
				ifFalse: [
					word := token truncateTo: (index - 1).
					((token at: index) isDigit
						or: [(token at: index) = $-])
						ifTrue: [
							token := token copyFrom: index to: token size.
							index := token indexOfAnyOf: self endOfArgumentSet.
							(index = 0)
								"if no separator or brace found, then whole chunk is a number"
								ifTrue: [arg := token asNumber.
									buffer add: (RTFToken newKeyword: word withArg: arg).
									^buffer]
								ifFalse: [
									arg := (token truncateTo: (index - 1)) asNumber.
									buffer add: (RTFToken newKeyword: word withArg: arg).
									(token at: index) isSeparator
										ifTrue: [index := index+1]]]
						ifFalse: [
							buffer add: (RTFToken newKeyword: word).
							(token at: index) isSeparator ifTrue: [index := index+1].
						]
				].
		 ].

	self parseNoKeywordChunk: token startingAt: index.

	^buffer! !

!RTFTokenizer methodsFor: 'private' stamp: 'mir 8/12/2006 16:04'!
parseNoKeywordChunk: aString startingAt: index
	"reads a string without \ into buffer"
	"tokenizes on {, and then tokenizes chunks on }"

	RTFChunkScanner new scan: aString into: buffer startingAt: index

"	| chunkStream nextChar bufferStream charValue |

	chunkStream := ReadStream on: aString.
	bufferStream := WriteStream on: self chunkBuffer.
	[chunkStream atEnd] whileFalse: [
		nextChar := chunkStream next.
		charValue := nextChar asciiValue.
		charValue = 123 
			ifTrue: [
				self addStringToken: bufferStream contents.
				bufferStream reset.
				buffer add: (RTFToken newBlockOpen)]
			ifFalse: [
				charValue = 125
					ifTrue: [
						self addStringToken: bufferStream contents.
						bufferStream reset.
						buffer add: (RTFToken newBlockClose)]
					ifFalse: [
						(charValue = 10
							or: [charValue = 13])
							ifFalse: [bufferStream nextPut: nextChar]]]].
	self addStringToken: bufferStream contents
"! !

!RTFTokenizer methodsFor: 'accesing' stamp: 'MR 5/3/2006 10:51'!
peekAfterAfter
	^afterAfter! !

!RTFTokenizer methodsFor: 'accesing' stamp: 'MR 4/30/2006 23:41'!
peekAfterNext
	^afterNext! !

!RTFTokenizer methodsFor: 'accesing' stamp: 'MR 4/30/2006 23:40'!
peekLast
	^last! !

!RTFTokenizer methodsFor: 'accesing' stamp: 'MR 4/30/2006 23:40'!
peekNext
	^next! !

!RTFTokenizer methodsFor: 'accesing' stamp: 'MR 7/4/2006 10:54'!
readNext
	last := next.
	next := afterNext.
	afterNext := afterAfter.
	buffer ifEmpty: [self parseChunk].
	buffer ifEmpty: [afterAfter := nil] ifNotEmpty: [afterAfter := buffer removeFirst]
! !

!RTFTokenizer methodsFor: 'private' stamp: 'jmv 3/30/2011 14:39'!
readStreamUpToNoEscapeInto: tokenStream
	"reads from the stream until the \ at the end is not an escape character (i.e. not \\,\{ or \}"
	"assume that stream is not at end"
	| token peek index peekValue |

	token := (stream upTo: $\).
	index := 1.

	"convert newline (#10#13, #13#10) to \par
	 convert \'HH to \specialDDD"
	token ifNotEmpty: [
		peekValue := (token at: 1) asciiValue.
		peekValue = 10
			ifTrue: [
				index := index + 1.
				token size > 1
					ifTrue: [
						peekValue := (token at: 2) asciiValue.
						peekValue = 13
							ifTrue: [index := index + 1]].
				tokenStream
					nextPutAll: 'par ';
					nextPutAll: (token copyFrom: index to: token size)]
			ifFalse: [
				peekValue = 13
					ifTrue: [
						index := index + 1.
						token size > 1
							ifTrue: [
								peekValue := (token at: 2) asciiValue.
								peekValue = 10
									ifTrue: [index := index + 1]].
						tokenStream
							nextPutAll: 'par ';
							nextPutAll: (token copyFrom: index to: token size)]
		 			ifFalse: ["a non-ascii characters"
						peekValue = 39 "$' asciiValue"
							ifTrue: [
								tokenStream
									nextPutAll: 'SomeCodePageHexValue';
									nextPutAll: (Integer readFrom: ((token copyFrom: 2 to: 3) asUppercase) readStream base: 16) asString;
									nextPutAll: ' ';
									nextPutAll: (token copyFrom: 4 to: token size)]
							ifFalse: [tokenStream nextPutAll: token]]]].

	"if token is empty, than we have \\ and read next token"
	"similar if first character is { or }"
	peek := stream peek.
"	tokenStream nextPutAll: token."
	peek ifNil: [^self].

	peek = $\
		ifTrue: [		
			tokenStream nextPut: stream next.
			self readStreamUpToNoEscapeInto: tokenStream]
! !

!RTFTokenizer methodsFor: 'accesing' stamp: 'tat 5/4/2006 00:46'!
tokenClass
	"the token class used by this tokenizer"
	^RTFToken! !

!RTFTokenizer class methodsFor: 'class initialization' stamp: 'tat 4/11/2007 15:10'!
initialize
	"RTFTokenizer initialize"

	EndOfKeywordSet := CharacterSet new.
	(0 to: ($A asInteger - 1)) do:
		[:v | EndOfKeywordSet add: (Character value: v)].
	(($Z asInteger + 1) to: ($a asInteger - 1)) do:
		[:v | EndOfKeywordSet add: (Character value: v)]. 
	(($z asInteger + 1) to: 255) do:
		[:v | EndOfKeywordSet add: (Character value: v)].

	" \'hh is handled separately. it is converted to \sophieUtfHexDDD"
	ControlSymbolSet _ CharacterSet newFrom: '|~-_:*{}'.

	EndOfArgumentSet := CharacterSet newFrom: (Character allCharacters reject: [ :c | c isDigit or: [ #($- $.) includes: c ]]).
	EndOfArgumentSet add: $.
! !

!RTFTokenizer class methodsFor: 'instance creation' stamp: 'mir 8/11/2006 20:57'!
newFromString: aString
	^self on: aString readStream! !

!RTFTokenizer class methodsFor: 'instance creation' stamp: 'mir 8/11/2006 20:57'!
on: aStream
	^self new on: aStream! !

!RTFUnicode class methodsFor: 'compatibility' stamp: 'jmv 3/30/2011 14:55'!
value: unicodeCodePoint
	^self value: unicodeCodePoint or: $?! !

!RTFUnicode class methodsFor: 'compatibility' stamp: 'jmv 3/30/2011 15:27'!
value: unicodeCodePoint or: aCharacter
	"In Cuis, #unicodeCodePoint: handles those Unicode characters that are present in the ISO8859-15 char set"
	^(Character unicodeCodePoint: unicodeCodePoint) ifNil: [ aCharacter ]! !
RTFChunkScanner initialize!
RTFParser initialize!
RTFTextBuilder initialize!
RTFToken initialize!
RTFTokenizer initialize!
