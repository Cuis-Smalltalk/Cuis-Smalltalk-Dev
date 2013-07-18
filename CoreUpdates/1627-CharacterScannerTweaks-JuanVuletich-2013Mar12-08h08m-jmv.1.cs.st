'From Cuis 4.1 of 12 December 2012 [latest update: #1626] on 12 March 2013 at 8:13:19 am'!

!CharacterScanner commentStamp: '<historical>' prior: 0!
My instances hold the state associated with scanning text. My subclasses scan characters for specified purposes, such as computing a CharacterBlock or placing characters into Forms.

My instances are usually transient, lasting only during the scanning operation, and therefore don't need a TextModel, but just a Text with the current contents.!


!Character methodsFor: 'accessing' stamp: 'jmv 3/5/2013 21:57'!
asciiValue
	"Answer the value of the receiver that represents its ISO 8859-15 encoding.
	This is the same as ASCII for the first 128 characters."

	^value! !


!CharacterScanner methodsFor: 'scanning' stamp: 'jmv 3/12/2013 08:10'!
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
	| nextDestX char |
	<primitive: 103>
	lastIndex _ startIndex.
	[ lastIndex <= stopIndex ]
		whileTrue: [
			char _ sourceString at: lastIndex.

			"stops are only defined for the first 256 characters.
			For Unicode Code Points above 255, please answer nil!!"
			char iso8859s15Code ifNotNil: [ :code |
				(stops at: code + 1) ifNotNil: [ :stop | ^stop ]].

			"Note: The following is querying the font about the width
			since the primitive may have failed due to a non-trivial
			mapping of characters to glyphs or a non-existing xTable."
			nextDestX _ destX + (font widthOf: char).
			nextDestX > rightX ifTrue: [ ^stops at: CharacterScanner crossedXCode ].
			destX _ nextDestX + kernDelta.
			lastIndex _ lastIndex + 1 ].
	lastIndex _ stopIndex.
	^stops at: CharacterScanner endOfRunCode! !


!CharacterTest methodsFor: 'UTF-8 conversion' stamp: 'jmv 3/5/2013 22:07'!
testSomeLatinCharsToUtf8
	"
	CharacterTest new testSomeLatinCharsToUtf8
	"
	| characters bytes |
	characters _ 'áåæñÆ¥ü' readStream.
	bytes _ ByteArray streamContents: [ :strm |
		[ characters atEnd ] whileFalse: [
			Character
				evaluate: [ :byte | strm nextPut: byte ]
				withUtf8BytesOfUnicodeCodePoint: characters next unicodeCodePoint ]].
	self assert: bytes hex = 'C3A1C3A5C3A6C3B1C386C2A5C3BC'! !

!methodRemoval: Character #asciiCode!
Character removeSelector: #asciiCode!
