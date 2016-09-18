'From Cuis 4.2 of 25 July 2013 [latest update: #2929] on 5 September 2016 at 9:16:32 pm'!

!String class methodsFor: 'primitives' stamp: 'jmv 9/5/2016 21:15:47'!
indexOfByte: anInteger inString: aString startingAt: start

	| stringSize |
	<primitive: 'primitiveIndexOfAsciiInString' module: 'MiscPrimitivePlugin'>

	self var: #aCharacter declareC: 'int anInteger'.
	self var: #aString declareC: 'unsigned char *aString'.

	stringSize _ aString size.
	start to: stringSize do: [:pos |
		(aString at: pos) asciiValue = anInteger ifTrue: [^ pos]].

	^ 0
! !


!DateAndTime methodsFor: 'squeak protocol' stamp: 'jmv 9/5/2016 17:11:13'!
printOn: aStream withLeadingSpace: printLeadingSpaceToo includeOffset: aBoolean
	"Print as per ISO 8601 section 5.4.2
	If printLeadingSpaceToo is false, prints either:
		'YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for negative years)
	If printLeadingSpaceToo is true, prints either:
		' YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for negative years)
	"

	self printYMDOn: aStream withLeadingSpace: printLeadingSpaceToo.
	aStream nextPut: $T.
	self printHMSOn: aStream.
	self nanoSecond ~= 0 ifTrue:
		[ | z ps |
		ps := self nanoSecond printString padded: #left to: 9 with: $0.
		z := ps findLast: [ :c | c numericValue > $0 numericValue ].
		(z > 0) ifTrue: [aStream nextPut: $.].
		ps from: 1 to: z do: [ :c | aStream nextPut: c ] ].
	aBoolean ifTrue: [
		aStream
			nextPut: (offset positive ifTrue: [$+] ifFalse: [$-]);
			nextPutAll: (offset hours abs asString padded: #left to: 2 with: $0);
			nextPut: $:;
			nextPutAll: (offset minutes abs asString padded: #left to: 2 with: $0).
		"Note: We should check the ISO to see if the seconds part is supported by the standard
		(usually not an issue. offsets usually don't include seconds)"
		offset seconds = 0 ifFalse: [
			aStream
				nextPut: $:;
				nextPutAll: (offset seconds abs truncated asString) ]]! !


!Duration methodsFor: 'squeak protocol' stamp: 'jmv 9/5/2016 17:11:17'!
printOn: aStream
	"Format as per ANSI 5.8.2.16: [-]D:HH:MM:SS[.S]" 	| d h m s n |
	d _ self days abs.
	h _ self hours abs.
	m _ self minutes abs.
 	s _ self seconds abs truncated.
	n _ self nanoSeconds abs. 	self negative ifTrue: [ aStream nextPut: $- ].
	d printOn: aStream. aStream nextPut: $:.
	h < 10 ifTrue: [ aStream nextPut: $0. ].
	h printOn: aStream. aStream nextPut: $:.
	m < 10 ifTrue: [ aStream nextPut: $0. ].
	m printOn: aStream. aStream nextPut: $:.
	s < 10 ifTrue: [ aStream nextPut: $0. ].
	s printOn: aStream.
	n = 0 ifFalse:
		[ | z ps |
		aStream nextPut: $..
		ps _ n printString padded: #left to: 9 with: $0. 
		z _ ps findLast: [ :c | c numericValue > $0 numericValue ].
		ps from: 1 to: z do: [ :c | aStream nextPut: c ] ].
! !


!String methodsFor: 'accessing' stamp: 'jmv 9/5/2016 17:14:38'!
byteAt: index
	<primitive: 60>
	^(self at: index) numericValue! !

!String methodsFor: 'accessing' stamp: 'jmv 9/5/2016 21:15:58'!
indexOf: aCharacter

	(aCharacter class == Character) ifFalse: [^ 0].
	^ String indexOfByte: aCharacter numericValue inString: self startingAt: 1! !

!String methodsFor: 'accessing' stamp: 'jmv 9/5/2016 21:16:01'!
indexOf: aCharacter  startingAt: start

	(aCharacter class == Character) ifFalse: [^ 0].
	^ String indexOfByte: aCharacter numericValue inString: self startingAt: start! !

!String methodsFor: 'accessing' stamp: 'jmv 9/5/2016 21:16:04'!
indexOf: aCharacter  startingAt: start  ifAbsent: aBlock
	| ans |
	(aCharacter class == Character) ifFalse: [ ^ aBlock value ].
	ans _ String indexOfByte: aCharacter numericValue inString: self  startingAt: start.
	^ans = 0
		ifTrue: [ aBlock value ]
		ifFalse: [ ans ]! !

!String methodsFor: 'comparing' stamp: 'jmv 9/5/2016 17:13:56'!
alike: aString 
	"Answer some indication of how alike the receiver is to the argument,  0 is no match, twice aString size is best score.  Case is ignored."

	| i j k minSize bonus |
	minSize _ (j _ self size) min: (k _ aString size).
	bonus _ (j - k) abs < 2 ifTrue: [ 1 ] ifFalse: [ 0 ].
	i _ 1.
	[(i <= minSize) and: [((super at: i) bitAnd: 16rDF)  = ((aString at: i) numericValue bitAnd: 16rDF)]]
		whileTrue: [ i _ i + 1 ].
	[(j > 0) and: [(k > 0) and:
		[((super at: j) bitAnd: 16rDF) = ((aString at: k) numericValue bitAnd: 16rDF)]]]
			whileTrue: [ j _ j - 1.  k _ k - 1. ].
	^ i - 1 + self size - j + bonus. ! !

!String methodsFor: 'comparing' stamp: 'jmv 9/5/2016 21:14:24'!
crc16
	"Compute a 16 bit cyclic redundancy check."

	| crc |
	crc := 0.
	self do: [:c |
		crc := (crc bitShift: -8) bitXor: (
		 #(	16r0000	16rC0C1	16rC181	16r0140	16rC301	16r03C0	16r0280	16rC241
			16rC601	16r06C0	16r0780	16rC741	16r0500	16rC5C1	16rC481	16r0440
			16rCC01	16r0CC0	16r0D80	16rCD41	16r0F00	16rCFC1	16rCE81	16r0E40
			16r0A00	16rCAC1	16rCB81	16r0B40	16rC901	16r09C0	16r0880	16rC841
			16rD801	16r18C0	16r1980	16rD941	16r1B00	16rDBC1	16rDA81	16r1A40
			16r1E00	16rDEC1	16rDF81	16r1F40	16rDD01	16r1DC0	16r1C80	16rDC41
			16r1400	16rD4C1	16rD581	16r1540	16rD701	16r17C0	16r1680	16rD641
			16rD201	16r12C0	16r1380	16rD341	16r1100	16rD1C1	16rD081	16r1040
			16rF001	16r30C0	16r3180	16rF141	16r3300	16rF3C1	16rF281	16r3240
			16r3600	16rF6C1	16rF781	16r3740	16rF501	16r35C0	16r3480	16rF441
			16r3C00	16rFCC1	16rFD81	16r3D40	16rFF01	16r3FC0	16r3E80	16rFE41
			16rFA01	16r3AC0	16r3B80	16rFB41	16r3900	16rF9C1	16rF881	16r3840
			16r2800	16rE8C1	16rE981	16r2940	16rEB01	16r2BC0	16r2A80	16rEA41
			16rEE01	16r2EC0	16r2F80	16rEF41	16r2D00	16rEDC1	16rEC81	16r2C40
			16rE401	16r24C0	16r2580	16rE541	16r2700	16rE7C1	16rE681	16r2640
			16r2200	16rE2C1	16rE381	16r2340	16rE101	16r21C0	16r2080	16rE041
			16rA001	16r60C0	16r6180	16rA141	16r6300	16rA3C1	16rA281	16r6240
			16r6600	16rA6C1	16rA781	16r6740	16rA501	16r65C0	16r6480	16rA441
			16r6C00	16rACC1	16rAD81	16r6D40	16rAF01	16r6FC0	16r6E80	16rAE41
			16rAA01	16r6AC0	16r6B80	16rAB41	16r6900	16rA9C1	16rA881	16r6840
			16r7800	16rB8C1	16rB981	16r7940	16rBB01	16r7BC0	16r7A80	16rBA41
			16rBE01	16r7EC0	16r7F80	16rBF41	16r7D00	16rBDC1	16rBC81	16r7C40
			16rB401	16r74C0	16r7580	16rB541	16r7700	16rB7C1	16rB681	16r7640
			16r7200	16rB2C1	16rB381	16r7340	16rB101	16r71C0	16r7080	16rB041
			16r5000	16r90C1	16r9181	16r5140	16r9301	16r53C0	16r5280	16r9241
			16r9601	16r56C0	16r5780	16r9741	16r5500	16r95C1	16r9481	16r5440
			16r9C01	16r5CC0	16r5D80	16r9D41	16r5F00	16r9FC1	16r9E81	16r5E40
			16r5A00	16r9AC1	16r9B81	16r5B40	16r9901	16r59C0	16r5880	16r9841
			16r8801	16r48C0	16r4980	16r8941	16r4B00	16r8BC1	16r8A81	16r4A40
			16r4E00	16r8EC1	16r8F81	16r4F40	16r8D01	16r4DC0	16r4C80	16r8C41
			16r4400	16r84C1	16r8581	16r4540	16r8701	16r47C0	16r4680	16r8641
			16r8201	16r42C0	16r4380	16r8341	16r4100	16r81C1	16r8081	16r4040)
			 at: ((crc bitXor: c numericValue) bitAnd: 16rFF) + 1) ].
	^crc! !

!String methodsFor: 'system primitives' stamp: 'jmv 9/5/2016 17:14:46'!
findSubstring: key in: body startingAt: start matchTable: matchTable
	"Answer the index in the string body at which the substring key first occurs, at or beyond start.  The match is determined using matchTable, which can be used to effect, eg, case-insensitive matches.  If no match is found, zero will be returned.

	The algorithm below is not optimum -- it is intended to be translated to C which will go so fast that it wont matter."
	| index |
	<primitive: 'primitiveFindSubstring' module: 'MiscPrimitivePlugin'>
	self var: #key declareC: 'unsigned char *key'.
	self var: #body declareC: 'unsigned char *body'.
	self var: #matchTable declareC: 'unsigned char *matchTable'.

	key size = 0 ifTrue: [^ 0].
	start to: body size - key size + 1 do:
		[:startIndex |
		index _ 1.
			[(matchTable at: (body at: startIndex+index-1) numericValue + 1)
				= (matchTable at: (key at: index) numericValue + 1)]
				whileTrue:
				[index = key size ifTrue: [^ startIndex].
				index _ index+1]].
	^ 0
"
' ' findSubstring: 'abc' in: 'abcdefabcd' startingAt: 1 matchTable: CaseSensitiveOrder 1
' ' findSubstring: 'abc' in: 'abcdefabcd' startingAt: 2 matchTable: CaseSensitiveOrder 7
' ' findSubstring: 'abc' in: 'abcdefabcd' startingAt: 8 matchTable: CaseSensitiveOrder 0
' ' findSubstring: 'abc' in: 'abcdefABcd' startingAt: 2 matchTable: CaseSensitiveOrder 0
' ' findSubstring: 'abc' in: 'abcdefABcd' startingAt: 2 matchTable: CaseInsensitiveOrder 7
"! !


!String class methodsFor: 'initialization' stamp: 'jmv 9/5/2016 17:16:45'!
initialize
	"
	String initialize
	"

	| order newOrder lowercase |

	"Case insensitive compare sorts null, space, digits, letters, all the rest..."
	newOrder _ Array new: 256.
	order _ -1.
	newOrder at: 0+1 put:  (order _ order+1).
	32 to: 64 do: [ :c |
		newOrder at: c + 1 put: (order _ order+1)].
	#(92 94 124 126) do: [ :c | "\^|~"
		newOrder at: c + 1 put: (order _ order+1)].
	#(oplus otimes odot dot circle bullet) do: [:each|
		newOrder at: (Character perform: each) numericValue + 1 put: (order _ order+1)].
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |
		order _ order+1.
		newOrder at: upperAndLowercase first numericValue + 1 put: order.
		upperAndLowercase size > 1 ifTrue: [
			newOrder at: upperAndLowercase second numericValue + 1 put: order ]].
	1 to: newOrder size do: [ :i |
		(newOrder at: i) ifNil: [
			newOrder at: i put: (order _ order+1)]].
	CaseInsensitiveOrder _ newOrder asByteArray.
	
	"Case sensitive compare sorts null, space, digits, letters, all the rest..."
	newOrder _ Array new: 256.
	order _ -1.
	newOrder at: 0+1 put:  (order _ order+1).
	32 to: 64 do: [ :c |
		newOrder at: c + 1 put: (order _ order+1)].
	#(92 94 124 126) do: [ :c | "\^|~"
		newOrder at: c + 1 put: (order _ order+1)].
	#(oplus otimes odot dot circle bullet) do: [:each|
		newOrder at: (Character perform: each) numericValue + 1 put: (order _ order+1)].
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |
		upperAndLowercase size > 1 ifTrue: [
			newOrder at: upperAndLowercase first numericValue + 1 put: (order _ order+1) ]].
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |
		lowercase _ upperAndLowercase size = 1
			ifTrue: [ upperAndLowercase first ]
			ifFalse: [ upperAndLowercase second ].
		newOrder at: lowercase numericValue + 1 put: (order _ order+1) ].
	1 to: newOrder size do: [ :i |
		(newOrder at: i) ifNil: [
			newOrder at: i put: (order _ order+1)]].
	order = 255 ifFalse: [self error: 'order problem'].
	CaseSensitiveOrder _ newOrder asByteArray.

	"a table for translating to lower case"
	LowercasingTable _ String withAll: (Character allCharacters collect: [:c | c asLowercase]).

	"a table for translating to upper case"
	UppercasingTable _ String withAll: (Character allCharacters collect: [:c | c asUppercase]).

	"a table for testing tokenish (for fast numArgs)"
	Tokenish _ String withAll: (Character allCharacters collect:
									[:c | c tokenish ifTrue: [c] ifFalse: [$~]]).

	"CR and LF--characters that terminate a line"
	CSLineEnders _ CharacterSet new.
	CSLineEnders add: Character cr.
	CSLineEnders add: Character lf.

 	"separators and non-separators"
	CSSeparators _ CharacterSet separators.
	CSNonSeparators _ CSSeparators complement! !

!String class methodsFor: 'primitives' stamp: 'jmv 9/5/2016 17:15:52'!
findFirstInString: aString  inSet: inclusionMap  startingAt: start
	| i stringSize |
	<primitive: 'primitiveFindFirstInString' module: 'MiscPrimitivePlugin'>
	self var: #aString declareC: 'unsigned char *aString'.
	self var: #inclusionMap  declareC: 'char *inclusionMap'.

	inclusionMap size ~= 256 ifTrue: [ ^0 ].

	i _ start.
	stringSize _ aString size.
	[ i <= stringSize and: [ (inclusionMap at: (aString at: i) numericValue+1) = 0 ] ] whileTrue: [ 
		i _ i + 1 ].

	i > stringSize ifTrue: [ ^0 ].
	^i! !

!String class methodsFor: 'primitives' stamp: 'jmv 9/5/2016 17:16:54'!
stringHash: aString initialHash: speciesHash

	| stringSize hash low |
	<primitive: 'primitiveStringHash' module: 'MiscPrimitivePlugin'>

	self var: #aHash declareC: 'int speciesHash'.
	self var: #aString declareC: 'unsigned char *aString'.

	stringSize _ aString size.
	hash _ speciesHash bitAnd: 16rFFFFFFF.
	1 to: stringSize do: [:pos |
		hash _ hash + (aString at: pos) numericValue.
		"Begin hashMultiply"
		low _ hash bitAnd: 16383.
		hash _ (16r260D * low + ((16r260D * (hash bitShift: -14) + (16r0065 * low) bitAnd: 16383) * 16384)) bitAnd: 16r0FFFFFFF.
	].
	^ hash! !

!String class methodsFor: 'primitives' stamp: 'jmv 9/5/2016 17:16:57'!
translate: aString from: start  to: stop  table: table
	"translate the characters in the string by the given table, in place"
	<primitive: 'primitiveTranslateStringWithTable' module: 'MiscPrimitivePlugin'>
	self var: #table  declareC: 'unsigned char *table'.
	self var: #aString  declareC: 'unsigned char *aString'.

	start to: stop do: [ :i |
		aString at: i put: (table at: (aString at: i) numericValue+1) ]! !

!String class methodsFor: 'character collation' stamp: 'jmv 9/5/2016 17:15:31'!
does: aCharacter caseInsensitiveCollateBefore: otherCharacter
	"
	String does: $a caseInsensitiveCollateBefore: $b
	String does: $a caseInsensitiveCollateBefore: $B
	String does: $A caseInsensitiveCollateBefore: $b
	"
	^ (CaseInsensitiveOrder at: aCharacter numericValue + 1) < (CaseInsensitiveOrder at: otherCharacter numericValue + 1)! !

!String class methodsFor: 'character collation' stamp: 'jmv 9/5/2016 17:15:39'!
does: aCharacter collateAfter: otherCharacter
	^ (CaseSensitiveOrder at: aCharacter numericValue + 1) > (CaseSensitiveOrder at: otherCharacter numericValue + 1)! !

!String class methodsFor: 'character collation' stamp: 'jmv 9/5/2016 17:15:44'!
does: aCharacter collateBefore: otherCharacter
	^ (CaseSensitiveOrder at: aCharacter numericValue + 1) < (CaseSensitiveOrder at: otherCharacter numericValue + 1)! !


!Symbol class methodsFor: 'access' stamp: 'jmv 9/5/2016 17:17:06'!
selectorsContaining: aString
	"Answer a list of selectors that contain aString within them. Case-insensitive.  Does return symbols that begin with a capital letter."

	| size selectorList ascii |

	selectorList _ OrderedCollection new.
	(size _ aString size) = 0 ifTrue: [^selectorList].

	aString size = 1 ifTrue:
		[
			ascii _ aString first numericValue.
			ascii < 128 ifTrue: [selectorList add: (OneCharacterSymbols at: ascii+1)]
		].

	aString first isValidInIdentifiers ifFalse:
		[
			aString size = 2 ifTrue: 
				[Symbol hasInterned: aString ifTrue:
					[:s | selectorList add: s]].
			^selectorList
		].

	selectorList _ selectorList copyFrom: 2 to: selectorList size.

	self allSymbolTablesDo: [:each |
		each size >= size ifTrue:
			[(each findSubstring: aString in: each startingAt: 1 
				matchTable: CaseInsensitiveOrder) > 0
						ifTrue: [selectorList add: each]]].

	^selectorList reject: [:each | "reject non-selectors, but keep ones that begin with an uppercase"
		each numArgs < 0 and: [each asString withFirstCharacterDownshifted numArgs < 0]].

"Symbol selectorsContaining: 'scon'"! !

!Symbol class methodsFor: 'instance creation' stamp: 'jmv 9/5/2016 17:17:00'!
internCharacter: aCharacter

	OneCharacterSymbols ifNil: [^self intern: aCharacter asString].

	^OneCharacterSymbols at: aCharacter numericValue + 1
! !


!ByteArray methodsFor: 'accessing' stamp: 'jmv 9/5/2016 21:15:52'!
indexOf: anInteger startingAt: start

	(anInteger isInteger and: [
		anInteger >= 0 and: [
		anInteger <= 255 ] ]) ifFalse: [ ^0 ].
	^String indexOfByte: anInteger inString: self startingAt: start! !


!StandardFileStream methodsFor: 'read, write, position' stamp: 'jmv 9/5/2016 21:14:06'!
crc16
	"Copied from String>>crc16"
	
	| buffer crc |
	self reset.
	buffer _ String new: 2000.
	crc _ 0.
	[ self atEnd ] whileFalse: [
		buffer _ self nextInto: buffer.
		buffer do: [:c |
			crc _ (crc bitShift: -8) bitXor: (
			 #(	16r0000	16rC0C1	16rC181	16r0140	16rC301	16r03C0	16r0280	16rC241
				16rC601	16r06C0	16r0780	16rC741	16r0500	16rC5C1	16rC481	16r0440
				16rCC01	16r0CC0	16r0D80	16rCD41	16r0F00	16rCFC1	16rCE81	16r0E40
				16r0A00	16rCAC1	16rCB81	16r0B40	16rC901	16r09C0	16r0880	16rC841
				16rD801	16r18C0	16r1980	16rD941	16r1B00	16rDBC1	16rDA81	16r1A40
				16r1E00	16rDEC1	16rDF81	16r1F40	16rDD01	16r1DC0	16r1C80	16rDC41
				16r1400	16rD4C1	16rD581	16r1540	16rD701	16r17C0	16r1680	16rD641
				16rD201	16r12C0	16r1380	16rD341	16r1100	16rD1C1	16rD081	16r1040
				16rF001	16r30C0	16r3180	16rF141	16r3300	16rF3C1	16rF281	16r3240
				16r3600	16rF6C1	16rF781	16r3740	16rF501	16r35C0	16r3480	16rF441
				16r3C00	16rFCC1	16rFD81	16r3D40	16rFF01	16r3FC0	16r3E80	16rFE41
				16rFA01	16r3AC0	16r3B80	16rFB41	16r3900	16rF9C1	16rF881	16r3840
				16r2800	16rE8C1	16rE981	16r2940	16rEB01	16r2BC0	16r2A80	16rEA41
				16rEE01	16r2EC0	16r2F80	16rEF41	16r2D00	16rEDC1	16rEC81	16r2C40
				16rE401	16r24C0	16r2580	16rE541	16r2700	16rE7C1	16rE681	16r2640
				16r2200	16rE2C1	16rE381	16r2340	16rE101	16r21C0	16r2080	16rE041
				16rA001	16r60C0	16r6180	16rA141	16r6300	16rA3C1	16rA281	16r6240
				16r6600	16rA6C1	16rA781	16r6740	16rA501	16r65C0	16r6480	16rA441
				16r6C00	16rACC1	16rAD81	16r6D40	16rAF01	16r6FC0	16r6E80	16rAE41
				16rAA01	16r6AC0	16r6B80	16rAB41	16r6900	16rA9C1	16rA881	16r6840
				16r7800	16rB8C1	16rB981	16r7940	16rBB01	16r7BC0	16r7A80	16rBA41
				16rBE01	16r7EC0	16r7F80	16rBF41	16r7D00	16rBDC1	16rBC81	16r7C40
				16rB401	16r74C0	16r7580	16rB541	16r7700	16rB7C1	16rB681	16r7640
				16r7200	16rB2C1	16rB381	16r7340	16rB101	16r71C0	16r7080	16rB041
				16r5000	16r90C1	16r9181	16r5140	16r9301	16r53C0	16r5280	16r9241
				16r9601	16r56C0	16r5780	16r9741	16r5500	16r95C1	16r9481	16r5440
				16r9C01	16r5CC0	16r5D80	16r9D41	16r5F00	16r9FC1	16r9E81	16r5E40
				16r5A00	16r9AC1	16r9B81	16r5B40	16r9901	16r59C0	16r5880	16r9841
				16r8801	16r48C0	16r4980	16r8941	16r4B00	16r8BC1	16r8A81	16r4A40
				16r4E00	16r8EC1	16r8F81	16r4F40	16r8D01	16r4DC0	16r4C80	16r8C41
				16r4400	16r84C1	16r8581	16r4540	16r8701	16r47C0	16r4680	16r8641
				16r8201	16r42C0	16r4380	16r8341	16r4100	16r81C1	16r8081	16r4040)
			 at: ((crc bitXor: c numericValue) bitAnd: 16rFF) + 1) 
		].
	].
	^crc! !

!StandardFileStream methodsFor: 'read, write, position' stamp: 'jmv 9/5/2016 17:13:27'!
padToEndWith: aChar
	"On the Mac, files do not truncate.  One can delete the old file and write a new one, but sometime deletion fails (file still open? file stale?).  This is a sad compromise.  Just let the file be the same length but pad it with a harmless character."

	| pad |
	self atEnd ifTrue: [^ self].
	pad := self isBinary 
		ifTrue: [aChar asCharacter numericValue]	"ok for char or number"
		ifFalse: [aChar asCharacter].
	self nextPutAll: (self collectionSpecies new: ((self size - self position) min: 20000) 
							withAll: pad).! !


!Scanner class methodsFor: 'testing' stamp: 'jmv 9/5/2016 17:12:58'!
isLiteralSymbol: aSymbol 
	"Test whether a symbol can be stored as # followed by its characters.  
	Symbols created internally with asSymbol may not have this property, 
	e.g. '3' asSymbol."
	| i ascii type |
	i := aSymbol size.
	i = 0 ifTrue: [^ false].
	ascii := (aSymbol at: 1) numericValue.
	"TypeTable should have been origined at 0 rather than 1 ..."
	ascii = 0 ifTrue: [^ false].
	type := self typeTable at: ascii ifAbsent: [#xLetter].
	type == #xBinary ifTrue:
		[^i <= 2
		  and: [i = 1 or: [(self typeTable at: (aSymbol at: 2) numericValue ifAbsent: []) == #xBinary]]].
	(type == #xColon or: [type == #verticalBar]) ifTrue:
		[^i = 1].
	(type == #xLetter or: [type == #xUnderscore]) ifTrue: [
		[i > 1] whileTrue: [
			ascii := (aSymbol at: i) numericValue.
			ascii = 0 ifTrue: [^false].
			type := self typeTable at: ascii ifAbsent: [#xLetter].
			((type == #xLetter or: [type == #xUnderscore])or: [type == #xDigit or: [type == #xColon]]) ifFalse: [
				^false].
			i := i - 1].
		^true].
	^false! !

!Scanner class methodsFor: 'cached class state' stamp: 'jmv 9/5/2016 17:12:50'!
initTypeTable
	| newTable |
	newTable := Array new: 256 withAll: #xIllegal. "default"
	newTable atAll: #(9 10 12 13 32 ) put: #xDelimiter. "tab lf ff cr space"

	1 to: 255
		do: [:index |
			(Character numericValue: index) isValidInIdentifiers
				ifTrue: [
					"Digits and $_ are overwritten later"
					newTable at: index put: #xLetter ]].

	newTable atAll: ($0 numericValue to: $9 numericValue) put: #xDigit.
	newTable atAll:  '!!%&+-*/\±×÷¬­¯,<=>”•–—˜™š›œ«»?@~€‚ƒŽ‘’“žŸ°·' asByteArray put: #xBinary.

	newTable at: Scanner doItCharacterValue put: #doIt.
	newTable at: $" numericValue put: #xDoubleQuote.
	newTable at: $# numericValue put: #xLitQuote.
	newTable at: $$ numericValue put: #xDollar.
	newTable at: $' numericValue put: #xSingleQuote.
	newTable at: $: numericValue put: #xColon.
	newTable at: $( numericValue put: #leftParenthesis.
	newTable at: $) numericValue put: #rightParenthesis.
	newTable at: $. numericValue put: #period.
	newTable at: $; numericValue put: #semicolon.
	newTable at: $[ numericValue put: #leftBracket.
	newTable at: $] numericValue put: #rightBracket.
	newTable at: ${ numericValue put: #leftBrace.
	newTable at: $} numericValue put: #rightBrace.
	newTable at: $^ numericValue put: #upArrow.
	newTable at: $_ numericValue put: #xUnderscore.
	newTable at: $| numericValue put: #verticalBar.
	TypeTable := newTable "bon voyage!!"

	"
	Scanner initTypeTable
	"! !


!StrikeFont methodsFor: 'accessing' stamp: 'jmv 9/5/2016 17:13:46'!
widthOf: aCharacter 
	"Answer the width of the argument as a character in the receiver."

	| ascii |
	ascii _ characterToGlyphMap
		ifNil: [ aCharacter numericValue ]
		ifNotNil: [ characterToGlyphMap at: aCharacter numericValue + 1 ].
	(ascii >= minAscii and: [ ascii <= maxAscii ]) ifFalse: [ ascii _ maxAscii + 1 ].
	^ (xTable at: ascii + 2) - (xTable at: ascii + 1) + self baseKern! !

!StrikeFont methodsFor: 'testing' stamp: 'jmv 9/5/2016 17:13:35'!
checkCharacter: character 
	"Answer a Character that is within the ascii range of the receiver--either 
	character or the last character in the receiver."

	| ascii |  
	ascii _ character numericValue.
	((ascii < minAscii) or: [ascii > maxAscii])
			ifTrue: [^maxAscii asCharacter]
			ifFalse:	[^character]
! !

!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 9/5/2016 17:13:39'!
glyphAt: character 
	"Answer a Form copied out of the glyphs for the argument, character."
	| ascii leftX rightX |
	ascii _ character numericValue.
	(ascii between: minAscii and: maxAscii) ifFalse: [ascii _ maxAscii + 1].
	leftX _ xTable at: ascii + 1.
	rightX _ xTable at: ascii + 2.
	^ glyphs copy: (leftX @ 0 corner: rightX @ self height)! !

!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 9/5/2016 17:13:41'!
glyphAt: character put: characterForm
	"Copy characterForm over the glyph for the argument, character."
	| ascii leftX rightX widthDif newGlyphs |
	ascii _ character numericValue.
	ascii < minAscii ifTrue: [^ self error: 'Cant store characters below min ascii'].
	ascii > maxAscii ifTrue:
		[(self confirm:
'This font does not accomodate ascii values higher than ' , maxAscii printString , '.
Do you wish to extend it permanently to handle values up to ' , ascii printString)
			ifTrue: [self extendMaxAsciiTo: ascii]
			ifFalse: [^ self error: 'No change made']].
	leftX _ xTable at: ascii + 1.
	rightX _ xTable at: ascii + 2.
	widthDif _ characterForm width - (rightX - leftX).
	widthDif ~= 0 ifTrue:
		["Make new glyphs with more or less space for this char"
		newGlyphs _ Form extent: (glyphs width + widthDif) @ glyphs height depth: glyphs depth.
		newGlyphs copy: (0@0 corner: leftX@glyphs height)
			from: 0@0 in: glyphs rule: Form over.
		newGlyphs copy: ((rightX+widthDif)@0 corner: newGlyphs width@glyphs height)
			from: rightX@0 in: glyphs rule: Form over.
		glyphs _ newGlyphs.
		"adjust further entries on xTable"
		xTable _ xTable copy.
		ascii+2 to: xTable size
			do: [:i | xTable at: i put: (xTable at: i) + widthDif]].
	glyphs copy: (leftX @ 0 extent: characterForm extent)
		from: 0@0 in: characterForm rule: Form over
"
| f |  f _ AbstractFont default.
f glyphAt: $  put: (Form extent: (f widthOf: $ )+10@f height)
"! !


!MenuMorph methodsFor: 'events' stamp: 'jmv 9/5/2016 21:13:50'!
keyStroke: aKeyboardEvent 
	| matchString char asc selectable |
	char := aKeyboardEvent keyCharacter.
	asc := char numericValue.
	aKeyboardEvent isReturnKey
		ifTrue: [
			selectedItem ifNotNil: [
					selectedItem hasSubMenu 
						ifTrue: [
							aKeyboardEvent hand newMouseFocus: selectedItem subMenu.
							^aKeyboardEvent hand newKeyboardFocus: selectedItem subMenu]
						ifFalse: [
							"self delete."
							^selectedItem invokeWithEvent: aKeyboardEvent]].
			(selectable := self items) size = 1 
				ifTrue: [^selectable first invokeWithEvent: aKeyboardEvent].
			^self].
	asc = 27 
		ifTrue: [
			"escape key"
			self
				valueOfProperty: #matchString
				ifPresentDo: [ :str | 
					str isEmpty 
						ifFalse: [
							"If filtered, first ESC removes filter"
							self setProperty: #matchString toValue: String new.
							self selectItem: nil.
							^self displayFiltered: aKeyboardEvent]].
			"If a stand-alone menu, just delete it"
			popUpOwner ifNil: [^self delete].
			"If a sub-menu, then deselect, and return focus to outer menu"
			self selectItem: nil.
			aKeyboardEvent hand newMouseFocus: popUpOwner owner.
			^aKeyboardEvent hand newKeyboardFocus: popUpOwner owner].
	(asc = 28 or: [asc = 29]) 
		ifTrue: [
			"left or right arrow key"
			(selectedItem notNil and: [selectedItem hasSubMenu]) 
				ifTrue: [
					aKeyboardEvent hand newMouseFocus: selectedItem subMenu.
					selectedItem subMenu moveSelectionDown: 1 event: aKeyboardEvent.
					^aKeyboardEvent hand newKeyboardFocus: selectedItem subMenu]].
	asc = 30 ifTrue: [^self moveSelectionDown: -1 event: aKeyboardEvent].	"up arrow key"
	asc = 31 ifTrue: [^self moveSelectionDown: 1 event: aKeyboardEvent].	"down arrow key"
	asc = 11 ifTrue: [^self moveSelectionDown: -5 event: aKeyboardEvent].	"page up key"
	asc = 12 ifTrue: [^self moveSelectionDown: 5 event: aKeyboardEvent].	"page down key"
	matchString := self valueOfProperty: #matchString ifAbsent: [String new].
	matchString := char = Character backspace 
				ifTrue: [
					matchString isEmpty ifTrue: [matchString] ifFalse: [matchString allButLast]]
				ifFalse: [matchString copyWith: aKeyboardEvent keyCharacter].
	self setProperty: #matchString toValue: matchString.
	self displayFiltered: aKeyboardEvent! !


!Theme methodsFor: 'keyboard shortcuts' stamp: 'jmv 9/5/2016 17:17:16'!
keyStroke: aKeyboardEvent morph: aMorph
	aKeyboardEvent controlKeyPressed ifTrue: [^false].
	aKeyboardEvent commandAltKeyPressed ifFalse: [^false].

	aKeyboardEvent keyValue = $w numericValue
		ifTrue: [aMorph owningWindow ifNotNil: [ :w | w delete. ^true ]].
"	aKeyboardEvent keyValue = $f numericValue
		ifTrue: [aMorph owningWindow ifNotNil: [ :w | w fullScreen. ^true ]].
"
	^ false! !

!methodRemoval: String class #indexOfAscii:inString:startingAt:!
String class removeSelector: #indexOfAscii:inString:startingAt:!
String initialize!
