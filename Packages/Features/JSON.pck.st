'From Cuis 5.0 [latest update: #4415] on 22 October 2020 at 9:27:24 pm'!
'Description '!
!provides: 'JSON' 1 19!
SystemOrganization addCategory: 'JSON'!


!classDefinition: #JsonObject category: 'JSON'!
OrderedDictionary subclass: #JsonObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'JSON'!
!classDefinition: 'JsonObject class' category: 'JSON'!
JsonObject class
	instanceVariableNames: ''!

!classDefinition: #JsonSyntaxError category: 'JSON'!
Error subclass: #JsonSyntaxError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'JSON'!
!classDefinition: 'JsonSyntaxError class' category: 'JSON'!
JsonSyntaxError class
	instanceVariableNames: ''!

!classDefinition: #Json category: 'JSON'!
Object subclass: #Json
	instanceVariableNames: 'stream ctorMap'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'JSON'!
!classDefinition: 'Json class' category: 'JSON'!
Json class
	instanceVariableNames: 'escapeArray'!


!JsonSyntaxError commentStamp: '<historical>' prior: 0!
Class Json signals instances of me when an input stream contains invalid JSON input.!

!Json commentStamp: '<historical>' prior: 0!
This class reads and writes JSON format data - strings, numbers, boolean, nil, arrays and dictionaries. See http://www.json.org . It has been extended with syntax for invoking a prearranged list of constructors on read objects.!

!JsonObject methodsFor: 'accessing' stamp: 'rh 2/27/2010 00:03'!
name
"override 'cause Object defines this"
	^self at: 'name'! !

!JsonObject methodsFor: 'accessing' stamp: 'rh 2/19/2010 21:34'!
at: aKey

	"make it work more like javascript objects"
	^ self at: aKey ifAbsent: [nil]! !

!JsonObject methodsFor: 'accessing' stamp: 'rh 2/27/2010 00:04'!
value
"override 'cause Object defines this"
	^self at: 'value'! !

!JsonObject methodsFor: 'as yet unclassified' stamp: 'rh 2/19/2010 21:33'!
doesNotUnderstand: aMessage
	| key |
	key := aMessage selector.
	key isUnary ifTrue: [^ self at: key ifAbsent: [nil]].
	^ (key isKeyword and: [(key occurrencesOf: $:) = 1])
		ifTrue: [key := key allButLast asSymbol.
				self at: key put: aMessage arguments first]
		ifFalse: [super doesNotUnderstand: aMessage]
				
	! !

!JsonObject class methodsFor: 'as yet unclassified' stamp: 'Igor.Stasenko 1/9/2010 16:51'!
fromAssociations: collectionOfAssociations

	| result |
	result := self new.
	
	collectionOfAssociations do: [:each |
		result at: each key put: each value ].
	^ result! !

!Json methodsFor: 'private' stamp: 'Igor.Stasenko 4/7/2010 01:19'!
consume: aString returning: anObject
	aString do: [:c | stream next == c ifFalse: [JsonSyntaxError signal: 'Expected ''', aString, ''''] ].
	^ anObject! !

!Json methodsFor: 'private' stamp: 'jmv 8/28/2013 12:15'!
interpretStringEscapeTo: aStream
	| c |
	c _ stream next.
	c == $b ifTrue: [
		aStream nextPut: Character backspace.
		^ self ].
	c == $n ifTrue: [
		aStream nextPut: Character lf.
		^ self ].
	c == $f ifTrue: [
		aStream nextPut: Character newPage.
		^ self ].
	c == $r ifTrue: [
		aStream nextPut: Character cr.
		^ self ].
	c == $t ifTrue: [
		aStream nextPut: Character tab.
		^ self ].
	c == $u ifTrue: [
		self unescapeUnicodeTo: aStream.
		^ self ].
	aStream nextPut: c! !

!Json methodsFor: 'private' stamp: 'Igor.Stasenko 4/7/2010 01:39'!
readArray
	| a |
	a := OrderedCollection new.

	self skipWhitespace.
	(stream peekFor: $]) ifTrue: [ ^ #() ].	

	[
		a add: self readAny.
		self skipWhitespace.
		(stream peekFor: $]) ifTrue: [ ^ a asArray].
		(stream peekFor: $, ) ifFalse: [JsonSyntaxError signal: 'Missing comma'].
	] repeat.
! !

!Json methodsFor: 'private' stamp: 'Igor.Stasenko 4/7/2010 01:20'!
readConstructor
	| s c v ctor |
	s := WriteStream on: ''.
	[
		c := stream peek.
		c ifNil: [JsonSyntaxError signal: 'Premature EOF reading constructor name'].
		((c == $.) or: [c isLetter])
			ifTrue: [s nextPut: c. stream next]
			ifFalse: [
				v := self readAny.
				s := s contents.
				ctor := ctorMap ifNotNil: [ctor := ctorMap at: s ifAbsent: [nil]].
				ctor ifNil: [JsonSyntaxError signal: 'Unknown ctor ', s].
				^ ctor constructFromJson: v]
	] repeat.
! !

!Json methodsFor: 'private' stamp: 'Igor.Stasenko 4/7/2010 01:43'!
readDictionary
	| m k v needComma |
	m := JsonObject new.
	needComma := false.
	[
		self skipWhitespace.
		(stream peekFor: $}) ifTrue: [ ^ m].
		needComma
			ifTrue: [ (stream peekFor: $,) ifFalse: [JsonSyntaxError signal: 'Missing comma'].
					self skipWhitespace]
			ifFalse: [needComma := true].
"		k := self readAny. "
		(stream peekFor: $") ifFalse: [JsonSyntaxError signal: 'Key in dictionary must be string'].
		k := self readString.
		self skipWhitespace.
		(stream peekFor: $:) ifFalse: [JsonSyntaxError signal: 'Missing colon'].
		v := self readAny.
		m at: k put: v.
	] repeat.
! !

!Json methodsFor: 'private' stamp: 'jmv 11/17/2016 11:35:09'!
readNumber
	| acc c |
	acc := WriteStream on: ''.
	[
		c := stream peek.
		(c isNil not and: [Json numbersMayContain: c]) ifFalse: [
			[^ acc contents asNumber] on: Error do: [JsonSyntaxError signal: 'Invalid number']].
		acc nextPut: c.
		stream next.
	] repeat.

	"Allow Nan, Infinity and -Infinity.
	Breaks on non-PositionableStreams, like StdIOReadStream stdin"
	"
	^Number readFrom: stream
	"! !

!Json methodsFor: 'private' stamp: 'jmv 8/28/2013 12:18'!
readString
	| s c |
	s _ WriteStream on: ''.
	[
	c _ stream next.
	c == $\
		ifTrue: [ self interpretStringEscapeTo: s ]
		ifFalse: [
			c == $" ifTrue: [ ^ s contents ].
			s nextPut: c ]] repeat! !

!Json methodsFor: 'private' stamp: 'Igor.Stasenko 4/7/2010 01:22'!
skipComment
	stream peek == $/ ifTrue: [
		stream next.
		stream peek == $/
			ifTrue: [self skipToEndOfLine]
			ifFalse: [stream peek == $*
						ifTrue: [stream next. self skipCommentBody]
						ifFalse: [JsonSyntaxError signal: 'Invalid comment syntax']]]
! !

!Json methodsFor: 'private' stamp: 'Igor.Stasenko 4/7/2010 01:22'!
skipCommentBody
	[
		[stream next == $*] whileFalse.
		stream peek == $/
	] whileFalse.
	stream next. "skip that last slash"
	self skipWhitespace.! !

!Json methodsFor: 'private' stamp: 'Igor.Stasenko 4/7/2010 01:24'!
skipToEndOfLine
	| cr lf |
	cr := Character cr. lf := Character lf.
	[ | c | (c := stream peek) == cr or: [ c == lf]] whileFalse: [stream next].
	self skipWhitespace! !

!Json methodsFor: 'private' stamp: 'Igor.Stasenko 4/7/2010 01:24'!
skipWhitespace
	[stream peek isSeparator] whileTrue: [stream next].
	self skipComment.! !

!Json methodsFor: 'private' stamp: 'jmv 8/28/2013 12:17'!
unescapeUnicodeTo: aStream
	| string codePoint |
	string _ (String with: stream next with: stream next with: stream next with: stream next) asUppercase.
	codePoint _ Integer readFrom: string readStream base: 16.
	String addUnicodeCodePoint: codePoint to: aStream hex: true! !

!Json methodsFor: 'accessing' stamp: 'tonyg 11/29/2005 11:40'!
ctorMap
	^ ctorMap! !

!Json methodsFor: 'accessing' stamp: 'dkb 12/25/2009 02:52'!
ctorMap: m
	ctorMap := m! !

!Json methodsFor: 'accessing' stamp: 'tonyg 8/16/2005 20:20'!
stream
	"Answer the value of stream"

	^ stream! !

!Json methodsFor: 'accessing' stamp: 'jmv 11/17/2016 11:09:38'!
stream: aStream
	"Set the value of stream"
	"aStream must be on a String.
	This means that it should contain only Characters (i.e. ISO 8859-15)"

	stream := aStream.
	
	"Do not reset the stream"
	"
	stream reset
	"! !

!Json methodsFor: 'parsing' stamp: 'jmv 8/8/2018 13:21:08'!
readAny
	"This is the main entry point for the JSON parser. See also readFrom: on the class side."
	| c |
	self skipWhitespace.
	c := stream peek.
	c == ${ ifTrue: [stream next. ^ self readDictionary].
	c == $[ ifTrue: [stream next. ^ self readArray].
	c == $" ifTrue: [stream next. ^ self readString].
	c == $t ifTrue: [^ self consume: 'true' returning: true].
	c == $f ifTrue: [^ self consume: 'false' returning: false].
	c == $n ifTrue: [^ self consume: 'null' returning: nil].
	c == $@ ifTrue: [stream next. ^ self readConstructor].
	c == $- ifTrue: [ 	"Negative number or '-Infinity' - > Float negativeInfinity "
		stream next.
		^ self readAny negated ].
	(Json numbersMayContain: c) ifTrue: [^ self readNumber].
	c == $N ifTrue: [^ self consume: 'NaN' returning: Float nan].
	c == $I ifTrue: [^ self consume: 'Infinity' returning: Float infinity].
	JsonSyntaxError signal: 'Unknown Json input'! !

!Json methodsFor: 'parsing' stamp: 'jmv 1/18/2017 16:52:22'!
readFrom: aStream
	"aStream must be on a String.
	This means that it should contain only Characters (i.e. ISO 8859-15).
	To handle a ByteArray with utf-8, please do 'String fromUtf8: aByteArray' or similar"
	aStream atEnd ifTrue: [ ^ JsonObject new ].
	self stream: aStream.
	^ self readAny! !

!Json class methodsFor: 'as yet unclassified' stamp: 'ul 11/9/2010 04:07'!
escapeForCharacter: c
	
	| asciiValue |
	(asciiValue := c asciiValue) < 128
		ifTrue: [ ^escapeArray at: asciiValue + 1 ].
	^nil! !

!Json class methodsFor: 'as yet unclassified' stamp: 'ul 11/4/2008 06:23'!
initialize
	"Json initialize."
	
	escapeArray := Array new: 128.
	(0 to: 31), #(127) do: [ :each |
		escapeArray at: each + 1 put: '\u', (each printStringHex padded: #left to: 4 with: $0) ].
	{
		$" -> '\"'.
		$\ -> '\\'.
		Character backspace -> '\b'.
		Character lf -> '\n'.
		Character newPage -> '\f'.
		Character cr -> '\r'.
		Character tab -> '\t'.
	} do: [ :each |
		escapeArray at: each key asciiValue + 1 put: each value ].
! !

!Json class methodsFor: 'as yet unclassified' stamp: 'DSG 9/10/2013 17:26'!
mimeType
	^ 'application/json'! !

!Json class methodsFor: 'as yet unclassified' stamp: 'dkb 12/25/2009 02:51'!
newWithConstructors: aCollection
	| m |
	m := Dictionary new.
	aCollection do: [:each |
		(each isKindOf: Association)
			ifTrue: [m add: each]
			ifFalse: [m at: each name asString put: each]].
	^ self new ctorMap: m; yourself.! !

!Json class methodsFor: 'as yet unclassified' stamp: 'tonyg 8/17/2005 09:09'!
numbersMayContain: aChar
	^ aChar isDigit or: [#($- $+ $. $e $E) includes: aChar]! !

!Json class methodsFor: 'as yet unclassified' stamp: 'tonyg 11/29/2005 17:45'!
readFrom: aStream
	^ self new readFrom: aStream.! !

!Json class methodsFor: 'as yet unclassified' stamp: 'tonyg 8/17/2005 07:45'!
render: anObject
	| s |
	s := WriteStream on: String new.
	anObject jsonWriteOn: s.
	^ s contents.! !

!Json class methodsFor: 'as yet unclassified' stamp: 'tonyg 11/30/2005 16:32'!
render: anObject withConstructor: aConstructorName on: aStream
	aStream nextPutAll: '@', aConstructorName.
	anObject jsonWriteOn: aStream.
! !

!Json class methodsFor: 'as yet unclassified' stamp: 'dkb 12/25/2009 02:52'!
renderInstanceVariables: aCollection of: anObject on: aStream
	| map |
	map := Dictionary new.
	aCollection do: [:ivarName | map at: ivarName put: (anObject instVarNamed: ivarName)].
	self render: map withConstructor: anObject class name asString on: aStream! !

!Object methodsFor: '*JSON' stamp: 'Igor.Stasenko 1/9/2010 16:18'!
asJsonString

	^ String streamContents: [:str |
		self jsonWriteOn: str ]! !

!False methodsFor: '*JSON-writing' stamp: 'tonyg 8/17/2005 00:43'!
jsonWriteOn: aStream
	aStream nextPutAll: 'false'! !

!True methodsFor: '*JSON-writing' stamp: 'tonyg 8/17/2005 00:44'!
jsonWriteOn: aStream
	aStream nextPutAll: 'true'! !

!UndefinedObject methodsFor: '*JSON-writing' stamp: 'tonyg 8/17/2005 00:45'!
jsonWriteOn: aWriteStream 
	aWriteStream nextPutAll: 'null'! !

!Number methodsFor: '*JSON-writing' stamp: 'ul 11/4/2008 04:46'!
jsonWriteOn: aWriteStream 

	self printOn: aWriteStream base: 10! !

!Integer methodsFor: '*JSON-writing' stamp: 'Igor.Stasenko 12/27/2009 08:05'!
jsonWriteOn: aWriteStream
	^ self printOn: aWriteStream base: 10
! !

!Association methodsFor: '*JSON-writing' stamp: 'jmv 6/2/2016 09:35'!
jsonWriteOn: aStream
	self key asString jsonWriteOn: aStream.
	aStream nextPut: $:; space.
	self value jsonWriteOn: aStream.! !

!Collection methodsFor: '*JSON' stamp: 'jmv 5/17/2016 13:11'!
jsonObjectWriteOn: aStream
	"Write as 'JSON Object' format
	This is a list of (name:value) pairs, and is analogous to Smalltalk Dictionaries where names (keys) are strings.
	Assume the receiver is either:
	- a Dictionary
	- a collection of Association objects (or some sort of object that on #jsonWriteOn: print like an Association)"

	| needComma |
	needComma := false.
	aStream nextPut: ${.
	self associationsDo: [ :assoc |
		needComma
			ifTrue: [ aStream nextPut: $, ; space]
			ifFalse: [ needComma := true ].
		assoc jsonWriteOn: aStream ].
	aStream nextPut: $}! !

!Collection methodsFor: '*JSON' stamp: 'jmv 7/11/2018 17:31:14'!
jsonWriteElement: element on: aStream
	"Allow for specific subclasses (FloatArray, using 32 bit Floats!!) to use specific way to write its elements"

	element jsonWriteOn: aStream! !

!Collection methodsFor: '*JSON' stamp: 'jmv 7/11/2018 17:31:50'!
jsonWriteOn: aStream
	"By default, use array braces "
	| useNewLines |
	aStream nextPut: $[.

	useNewLines _ nil.
	self do: [ :each |
		useNewLines ifNil: [
			useNewLines _ each isLiteral not ].
		self jsonWriteElement: each on: aStream.
	] separatedBy: [
		aStream nextPut: $, .
		useNewLines ifTrue: [ aStream newLine ]].

	aStream nextPut: $]! !

!String methodsFor: '*JSON-writing' stamp: 'ul 11/9/2010 04:07'!
jsonWriteOn: aStream

	aStream nextPut: $".
	self do: [ :ch |
		(Json escapeForCharacter: ch)
			ifNil: [ aStream nextPut: ch ]
			ifNotNil: [ :replacement |
				aStream nextPutAll: replacement ] ].
	aStream nextPut: $".
! !

!FloatArray methodsFor: '*JSON' stamp: 'jmv 7/11/2018 18:14:09'!
jsonWriteElement: element on: aStream
	"FloatArray elements are answered as 64 bit Float, but are really 32 bit Float.
	When writing to JSON (or printing), print them as 32 bit Float."

	element printAsIEEE32BitPrecisionFloatOn: aStream base: 10! !

!Text methodsFor: '*JSON-printing' stamp: 'cwp 10/26/2006 22:25'!
jsonWriteOn: aStream
	self string jsonWriteOn: aStream! !

!Dictionary methodsFor: '*JSON-writing' stamp: 'jmv 5/17/2016 12:47'!
jsonWriteOn: aStream
	"Dictionaries are written as 'JSON Objects'
	Use the general implementation for that."

	self jsonObjectWriteOn: aStream! !

!Array2D methodsFor: '*JSON' stamp: 'jmv 7/11/2018 17:33:20'!
jsonWriteOn: aStream
	"By default, use array braces "
	aStream nextPut: $[.
	1 to: height
		do: [ :i |
			aStream nextPut: $[.
				1 to: width
					do: [ :j | elements jsonWriteElement: (self i: i j: j) on: aStream ]
					separatedBy: [aStream nextPut: $, ; space].
			aStream nextPut: $] ]
		separatedBy: [aStream nextPut: $, ; newLine ].
	aStream nextPut: $]! !

!WriteStream methodsFor: '*JSON-printing' stamp: 'cwp 10/25/2006 12:27'!
jsonPrint: anObject
	anObject jsonWriteOn: self! !

!FileEntry methodsFor: '*json' stamp: 'pb 5/25/2016 01:32'!
jsonContents
	"
	'noesta.json' asFileEntry jsonContents
	"
	^self readStreamDo: [ :stream |
		Json readFrom: stream ]! !
Json initialize!
