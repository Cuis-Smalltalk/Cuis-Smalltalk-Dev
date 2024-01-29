'From Cuis6.3 [latest update: #6171] on 8 January 2024 at 1:19:40 pm'!

!SmallInteger methodsFor: 'printing' stamp: 'jmv 1/8/2024 12:27:13'!
printStringBase: b nDigits: n
	"Return a string representation of this number in base b with n digits (left padded with 0).
	Should be invoked with: 0 <= self < (b raisedToInteger: n)."
	
	| integer next result |
	result := String new: n.
	integer := self.
	n to: 1 by: -1 do: [:i |
		next := integer // b.
		result at: i put: (Character digitValue: (integer - (next * b))).
		integer := next].
	^result! !


!Character methodsFor: 'accessing' stamp: 'jmv 1/8/2024 12:29:22'!
asciiValue
	"Answer the value of the receiver that represents its ANSI encoding.
	This is the same as ISO 8859-15 for the first 128 characters.
	Answer nil if out of range."

	| n |
	n := self codePoint.
	^ n < 128 ifTrue: [ n ].! !

!Character methodsFor: 'accessing' stamp: 'jmv 1/8/2024 12:35:36'!
digitValue
	"Answer 0-9 if the receiver is $0-$9, 10-35 if it is $A-$Z, and < 0 
	otherwise. This is used to parse literal numbers of radix 2-36.
	$0 numericValue = 48
	$9 numericValue = 57
	$A numericValue = 65
	$Z numericValue = 90 
	$7 digitValue = 7 
	"

	| nv |
	nv := self codePoint.
	(nv between: 48 and: 57)
		ifTrue: [ ^ nv - 48 ].
	(nv between: 65 and: 90)
		ifTrue: [ ^ nv - 55 ].
	^ -1! !

!Character methodsFor: 'comparing' stamp: 'jmv 1/8/2024 12:35:49'!
hash
	"Hash is reimplemented because = is implemented."

	^self codePoint hash! !

!Character methodsFor: 'testing' stamp: 'jmv 1/8/2024 12:37:08'!
isDigit
	"Answer whether the receiver is a digit."

	^ self codePoint between: 48 and: 57! !

!Character methodsFor: 'testing' stamp: 'jmv 1/8/2024 12:37:17'!
isLineSeparator
	"Answer whether the receiver is a line separator character:
	line feed, cr, or form feed."

	^ #(10 13 12) statePointsTo: self codePoint! !

!Character methodsFor: 'testing' stamp: 'jmv 1/8/2024 12:37:20'!
isNullCharacter
	"Answer whether the receiver is the NULL character."

	^ self codePoint = 0! !

!Character methodsFor: 'testing' stamp: 'jmv 1/8/2024 12:37:25'!
isSeparator
	"Answer whether the receiver is one of the separator characters
	space, tab, lf, cr, or form feed."
	"Also see #separators"

	^ #(32 9 10 13 12) statePointsTo: self codePoint! !

!Character methodsFor: 'printing' stamp: 'jmv 1/8/2024 12:37:00'!
hex
	self codePoint < 256 ifTrue: [
		^ String
			with: ('0123456789ABCDEF' at: self codePoint//16+1)
			with: ('0123456789ABCDEF' at: self codePoint\\16+1) ].
	^self codePoint printStringBase: 16 length: 6 padded: true! !

!Character methodsFor: 'printing' stamp: 'jmv 1/8/2024 12:37:41'!
printOn: aStream 
	aStream nextPut: $$.
	aStream nextPut: self.
	self codePoint <= 32 ifTrue: [
		aStream nextPut: $".
		aStream nextPutAll: self hex.
		aStream nextPut: $" ].! !

!Character methodsFor: 'converting' stamp: 'jmv 1/8/2024 12:28:18'!
asLowercase
self flag: #unicodeNeedsImprovement.
	self codePoint > 255 ifTrue: [ ^self ].
	^ Character lowercaseMappingTable at: self codePoint + 1.! !

!Character methodsFor: 'converting' stamp: 'jmv 1/8/2024 12:28:52'!
asUnaccented
	"
	$A asUnaccented
	$Á asUnaccented
	(0 to: 255) collect: [ :i | (Character codePoint: i) asUnaccented ]
	(0 to: 255) collect: [ :i | (Character codePoint: i) asUnaccented  asLowercase]
	"
self flag: #unicodeNeedsImprovement.
	self codePoint > 255 ifTrue: [ ^self ].
	^ Character
		codePoint: (self class unaccentedTable at: self codePoint + 1)! !

!Character methodsFor: 'converting' stamp: 'jmv 1/8/2024 12:29:10'!
asUppercase
	"If the receiver is lowercase, answer its matching uppercase Character."
self flag: #unicodeNeedsImprovement.
	self codePoint > 255 ifTrue: [ ^self ].
	^ Character uppercaseMappingTable at: self codePoint + 1.! !

!Character methodsFor: 'converting' stamp: 'jmv 1/8/2024 12:38:10'!
to: other
	"Answer with a collection in ascii order -- $a to: $z"
	^ (self codePoint to: other codePoint) collect: [ :cp |
		Character codePoint: cp ]! !


!Character class methodsFor: 'class initialization' stamp: 'jmv 1/8/2024 12:58:23'!
uppercaseLowercaseAndUnaccentedLetters
	"Answer all uppercase and lowercase letters in standard collation order.
	For accented characters, include the non-accented ones.
	Note that $ß is 'LATIN SMALL LETTER SHARP S' and ISO 8859-15 doesn't have an uppercase version.
	Note: for $ÿ, 'Latin Small Letter Y with diaeresis' the uppercase version is not Latin 1"
self flag: #unicodeNeedsImprovement.
	^#(
		'Aa' 'ÀàAa' 'ÁáAa' 'ÂâAa' 'ÃãAa' 'ÄäAa' 'ÅåAa'
		'Bb' 'Cc' 'Çç' 'Dd'
			'Ee' 'ÈèEe' 'ÉéEe' 'ÊêEe' 'ËëEe'
		'Ff' 'Gg' 'Hh'
			'Ii' 'ÌìIi' 'ÍíIi' 'ÎîIi' 'ÏïIi'
		'Jj' 'Kk' 'Ll' 'Mm' 'Nn' 'Ññ'
			'Oo' 'ÒòOo' 'ÓóOo' 'ÔôOo' 'ÕõOo' 'ÖöOo'
		'Pp' 'Qq' 'Rr'
			'Ss'
		'Tt'
			'Uu' 'ÙùUu' 'ÚúUu' 'ÛûUu' 'ÜüUu'
		'Vv' 'Ww' 'Xx'
			'Yy' 'ÝýYy' 'ÿ'
			'Zz'
		'Ææ' 'Ðð' 'Øø' 'Þþ' 'ß'
	).! !

!Character class methodsFor: 'static state' stamp: 'jmv 1/8/2024 12:30:23'!
basicLookupTables
	| lowercaseMappingTable uppercaseMappingTable unaccentedTable |
	lowercaseMappingTable := Array new: 256.
	uppercaseMappingTable := Array new: 256.
	unaccentedTable := ByteArray new: 256.
	0
		to: 255
		do: [ :idx | | char |
			"Default to an identity mapping with a false truth mapping"
			char := self codePoint: idx.
			lowercaseMappingTable
				at: idx + 1 put: char.
			uppercaseMappingTable
				at: idx + 1 put: char.
			unaccentedTable at: idx + 1 put: idx].
	"Now override as needed"
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :group | | uppercase lowercase |
		group size > 1
			ifTrue: [ | lowercaseChar uppercaseChar |
				uppercase := group first codePoint.
				lowercase := group second codePoint.
				lowercaseChar := self codePoint: lowercase.
				uppercaseChar := self codePoint: uppercase.
				lowercaseMappingTable
					at: lowercase + 1 put: lowercaseChar;
					at: uppercase + 1 put: lowercaseChar.
				uppercaseMappingTable
					at: lowercase + 1 put: uppercaseChar;
					at: uppercase + 1 put: uppercaseChar.
				group size > 2
					ifTrue: [|unaccentedUppercase unaccentedLowercase|
						unaccentedUppercase := group third codePoint.
						unaccentedLowercase := group fourth codePoint.
						unaccentedTable
							at: uppercase+1 put: unaccentedUppercase;
							at: lowercase+1 put: unaccentedLowercase]]
			ifFalse: [ | lowercaseChar |
				lowercase := group first codePoint.
				lowercaseChar := self codePoint: lowercase.
				lowercaseMappingTable
					at: lowercase + 1 put: lowercaseChar.
				uppercaseMappingTable
					at: lowercase + 1 put: lowercaseChar ]].
	^{ lowercaseMappingTable. uppercaseMappingTable. unaccentedTable}.! !

!Character class methodsFor: 'static state' stamp: 'jmv 1/8/2024 12:58:45'!
lowercaseMappingTable
	^`Character basicLookupTables at: 1`! !

!Character class methodsFor: 'static state' stamp: 'jmv 1/8/2024 12:58:48'!
unaccentedTable
	^`Character basicLookupTables at: 3`! !

!Character class methodsFor: 'static state' stamp: 'jmv 1/8/2024 12:58:51'!
uppercaseMappingTable
	^`Character basicLookupTables at: 2`! !


!InputSensor methodsFor: 'keyboard' stamp: 'jmv 1/8/2024 12:38:20'!
kbdTest
	"
	Sensor kbdTest
	"
	"This test routine will print the unmodified character, its keycode,
	and the OR of all its modifier bits, until the character x is typed

Also useful:
	Sensor test
	"
	| char |
	char := nil.
	[ char = $x ] whileFalse: [
		[ self keyboardPressed ] whileFalse.
		char := self keyboard.
		Display fill: `5@5 extent: 400@50` fillColor: `Color white`.
		(String streamContents: [ :s |
			s
				nextPut: $-;
				nextPut: char;
				nextPut: $-;
				space;
				print: char codePoint;
				space.
			self isMouseButton3Pressed ifTrue: [ s nextPutAll: ' mouseButton3/blue/tertiary/halo ' ].
			self isMouseButton2Pressed ifTrue: [ s nextPutAll: ' mouseButton2/yellow/secondary/menu ' ].
			self isMouseButton1Pressed ifTrue: [ s nextPutAll: ' mouseButton1/red/primary ' ].
			self shiftPressed ifTrue: [ s nextPutAll: ' shift ' ].
			self controlKeyPressed ifTrue: [ s nextPutAll: ' control/ctrl ' ].
			(self primMouseButtons anyMask: 32) ifTrue: [ s nextPutAll: ' macOption ' ].
			self commandAltKeyPressed ifTrue: [ s nextPutAll: ' macCommand/winAlt ' ].
			Time now printOn: s
			])
		displayAt: 10 @ 10 ].! !


!EventSensor methodsFor: 'initialization' stamp: 'jmv 1/8/2024 12:38:44'!
initialize
	"Run the I/O process"
	mouseButtons := 0.
	mousePosition := `0@0`.
	self setInterruptKey: (interruptKey ifNil: [$. codePoint bitOr: 16r0800 ]). 	"cmd-."
	self flushAllButDandDEvents.
	inputSemaphore := Semaphore new.
	hasInputSemaphore := false.

	self primSetInputSemaphore: (Smalltalk registerExternalObject: inputSemaphore).
	self installInterruptWatcher.
	self installEventTickler.
	self flushAllButDandDEvents.

	"Attempt to discover whether the input semaphore is actually being signaled."
	hasInputSemaphore := false.
	inputSemaphore initSignals! !


!String methodsFor: 'accessing' stamp: 'jmv 1/8/2024 12:39:05'!
at: index put: aCharacter 
	"Primitive. Store the Character in the field of the receiver indicated by
	the index. Fail if the index is not an Integer or is out of bounds, or if
	the argument is not a Character. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 64>
	aCharacter isCharacter
		ifTrue: [	

			aCharacter class == UnicodeCodePoint ifTrue: [
				aCharacter codePoint > 255 ifTrue: [self error: 'String only store Latin-1 Characters'].
				^self at: index put: (Character codePoint: aCharacter codePoint) ].

			aCharacter codePoint > 255 ifTrue: [
				self error: 'String only store Latin-1 Characters'].
			index isInteger
				ifTrue: [self errorSubscriptBounds: index]
				ifFalse: [self errorNonIntegerIndex]]
		ifFalse: [self error: 'Strings only store Characters']! !

!String methodsFor: 'comparing' stamp: 'jmv 1/8/2024 13:02:09'!
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
			 at: ((crc bitXor: c codePoint) bitAnd: 16rFF) + 1) ].
	^crc! !


!String class methodsFor: 'character collation' stamp: 'jmv 1/8/2024 13:04:20'!
does: aCharacter caseInsensitiveCollateBefore: otherCharacter
	"
	String does: $a caseInsensitiveCollateBefore: $b
	String does: $a caseInsensitiveCollateBefore: $B
	String does: $A caseInsensitiveCollateBefore: $b
	"
	"Only for Latin-1"
	self flag: #unicodeNeedsImprovement.
	^ (self caseInsensitiveOrder at: aCharacter codePoint + 1) <
		(self caseInsensitiveOrder at: otherCharacter codePoint + 1)! !

!String class methodsFor: 'character collation' stamp: 'jmv 1/8/2024 13:04:06'!
does: aCharacter collateAfter: otherCharacter
	"Only for Latin-1"
	self flag: #unicodeNeedsImprovement.
	^ (self caseSensitiveOrder at: aCharacter codePoint + 1) >
		(self caseSensitiveOrder at: otherCharacter codePoint + 1)! !

!String class methodsFor: 'character collation' stamp: 'jmv 1/8/2024 13:03:55'!
does: aCharacter collateBefore: otherCharacter
	"Only for Latin-1"
	self flag: #unicodeNeedsImprovement.
	^ (self caseSensitiveOrder at: aCharacter codePoint + 1) <
		(self caseSensitiveOrder at: otherCharacter codePoint + 1)! !

!String class methodsFor: 'static state' stamp: 'jmv 1/8/2024 12:39:19'!
basicCaseInsensitiveOrder
	"Case insensitive compare sorts null, space, digits, letters, all the rest..."
	| newOrder order |
	newOrder := Array new: 256.
	order := -1.
	newOrder at: 0+1 put:  (order := order+1).
	32 to: 64 do: [ :c |
		newOrder at: c + 1 put: (order := order+1)].
	#(92 94 124 126 183 215 247) do: [ :c | "\^|~·÷×"
		newOrder at: c + 1 put: (order := order+1)].
	16r90 to: 16r9F do: [:c| "special math operators"
		newOrder at: c + 1 put: (order := order+1)].
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |
		order := order+1.
		newOrder at: upperAndLowercase first codePoint + 1 put: order.
		upperAndLowercase size > 1 ifTrue: [
			newOrder at: upperAndLowercase second codePoint + 1 put: order ]].
	1 to: newOrder size do: [ :i |
		(newOrder at: i) ifNil: [
			newOrder at: i put: (order := order+1)]].
	^newOrder asByteArray.! !

!String class methodsFor: 'static state' stamp: 'jmv 1/8/2024 12:59:55'!
basicCaseSensitiveOrder
	"Case sensitive compare sorts null, space, digits, letters, all the rest..."
	| newOrder order lowercase |
	newOrder := Array new: 256.
	order := -1.
	newOrder at: 0+1 put:  (order := order+1).
	32 to: 64 do: [ :c |
		newOrder at: c + 1 put: (order := order+1)].
	#(92 94 124 126 183 215 247) do: [ :c | "\^|~·÷×"
		newOrder at: c + 1 put: (order := order+1)].
	16r90 to: 16r9F do: [:c| "special math operators"
		newOrder at: c + 1 put: (order := order+1)].
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |
		upperAndLowercase size > 1 ifTrue: [
			newOrder at: upperAndLowercase first codePoint + 1 put: (order := order+1) ]].
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |
		lowercase := upperAndLowercase size = 1
			ifTrue: [ upperAndLowercase first ]
			ifFalse: [ upperAndLowercase second ].
		newOrder at: lowercase codePoint + 1 put: (order := order+1) ].
	1 to: newOrder size do: [ :i |
		(newOrder at: i) ifNil: [
			newOrder at: i put: (order := order+1)]].
	order = 255 ifFalse: [self error: 'order problem'].
	^newOrder asByteArray.! !

!String class methodsFor: 'static state' stamp: 'jmv 1/8/2024 12:59:01'!
caseInsensitiveOrder
	^`String basicCaseInsensitiveOrder`! !

!String class methodsFor: 'static state' stamp: 'jmv 1/8/2024 13:00:08'!
caseSensitiveOrder
	^`String basicCaseSensitiveOrder`! !


!UnicodeString methodsFor: 'paragraph support' stamp: 'jmv 1/8/2024 13:05:35'!
encompassLine: aCodePointInterval
	"Return an interval that includes anInterval, and that comprises one or several whole paragraphs in the receiver.
	Answer starts at the position following a newLine (or eventually 1) and ends before a newLine (or eventually at self size)
	See also encompassParagraph:"
	| argumentLeftByte argumentRightByte leftByte rightCrByte left nullPosition right |
	argumentLeftByte := self byteIndexAt: aCodePointInterval first.
	argumentRightByte := self byteIndexAt: aCodePointInterval last.

	"Find byte position and code point position following last newLine before us."
	leftByte := (bytes lastIndexOf: `Character newLineCharacter codePoint` startingAt: argumentLeftByte - 1 ifAbsent: [0]) + 1.
	left := self codePointIndexAt: leftByte.

	"Find byte position and code point position for next newLine. Don't include it."
	right := count.
	rightCrByte := bytes indexOf: `Character newLineCharacter codePoint` startingAt: (argumentRightByte max: argumentLeftByte).
	rightCrByte = 0 ifFalse: [
		right := (self codePointIndexAt: rightCrByte)-1].

	"But stop before if any nulls. Don't include them!!"
	nullPosition := bytes indexOf: 0 startingAt: (argumentRightByte max: argumentLeftByte).
	nullPosition = 0 ifFalse: [
		right := right min: ((self codePointIndexAt: nullPosition)-1) ].

	^left to: right! !

!UnicodeString methodsFor: 'paragraph support' stamp: 'jmv 1/8/2024 13:05:41'!
encompassParagraph: aCodePointInterval
	"Return an interval that includes anInterval, and that comprises one or several whole paragraphs in the receiver.
	Answer starts at the position following a newLine (or eventually 1) and ends at a newLine (or eventually at self size).
	Look also for null characters. Never include null characters in the answer.
	See also #encompassLine:"
	| argumentLeftByte argumentRightByte leftByte rightCrByte left nullPosition right |
	argumentLeftByte := self byteIndexAt: aCodePointInterval first.
	argumentRightByte := self byteIndexAt: aCodePointInterval last.

	"Find byte position and code point position following last newLine before us."
	leftByte := (bytes lastIndexOf: `Character newLineCharacter codePoint` startingAt: argumentLeftByte - 1 ifAbsent: [0]) + 1.
	left := self codePointIndexAt: leftByte.

	"Find byte position and code point position for next newLine. Include it."
	right := count.
	rightCrByte := bytes indexOf: `Character newLineCharacter codePoint` startingAt: (argumentRightByte max: argumentLeftByte).
	rightCrByte = 0 ifFalse: [
		right := self codePointIndexAt: rightCrByte].

	"But stop before if any nulls. Don't include them!!"
	nullPosition := bytes indexOf: 0 startingAt: (argumentRightByte max: argumentLeftByte).
	nullPosition = 0 ifFalse: [
		right := right min: ((self codePointIndexAt: nullPosition)-1) ].

	^left to: right! !


!RWBinaryOrTextStream methodsFor: 'reading' stamp: 'jmv 1/8/2024 13:14:56'!
next

	| byte |
	^ isBinary 
			ifTrue: [byte := super next.
				 byte ifNil: [nil] ifNotNil: [byte codePoint]]
			ifFalse: [super next].
! !


!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 1/8/2024 13:06:28'!
padToEndIfCantTruncate
	"Only makes sense for file streams with existing content.
	On file systems that don't support truncating this is needed.
	If truncating is supported, try that first"

	"On the Mac, files do not truncate.  One can delete the old file and write a new one, but sometime deletion fails (file still open? file stale?).  This is a sad compromise.  Just let the file be the same length but pad it with a harmless character."

	| pad |
	self atEnd ifTrue: [^ self].
	self truncate.
	self atEnd ifTrue: [^ self].
	pad := Character space codePoint.
	self nextPutAll: (ByteArray new: ((self size - self position) min: 20000) 
							withAll: pad)! !


!DataStream methodsFor: 'write and read' stamp: 'jmv 1/8/2024 13:06:36'!
writeCharacter: aCharacter
	"PRIVATE -- Write a Character."

	byteStream nextUint32Put: aCharacter codePoint bigEndian: true! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 1/8/2024 13:07:26'!
writeStringOld: aString
	"PRIVATE -- Write the contents of a String."

	| length |
	aString size < 16384 
		ifTrue: [
			(length := aString size) < 192
				ifTrue: [byteStream nextPut: length]
				ifFalse: 
					[byteStream nextPut: (length // 256 + 192).
					byteStream nextPut: (length \\ 256)].
			aString do: [:char | byteStream nextPut: (char codePoint min: 255)]]
		ifFalse: [self writeByteArray: aString].	"takes more space"! !


!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 1/8/2024 13:08:38'!
xIllegal
	"An illegal character was encountered"
	self notify: 'Illegal character (char code ' , hereChar printString, ' ', hereChar hex , ')' at: mark! !


!Scanner class methodsFor: 'cached class state' stamp: 'jmv 1/8/2024 13:08:03'!
initTypeTable
	| newTable |
	newTable := Array new: 127 withAll: #xIllegal. "default"
	newTable atAll: #(9 10 12 13 32 ) put: #xDelimiter. "tab lf ff cr space"
	newTable atAll: '!!%&+-*/\,<=>?@~' asByteArray put: #xBinary.
	1 to: 127 do: [:index |
		(Character codePoint: index) isLetter ifTrue: [
			newTable at: index put: #xLetter ]].
	newTable atAll: ($0 codePoint to: $9 codePoint) put: #xDigit.
	newTable at: self doItCharacterValue put: #doIt.
	newTable at: $" codePoint put: #xDoubleQuote.
	newTable at: $# codePoint put: #xLitQuote.
	newTable at: $$ codePoint put: #xDollar.
	newTable at: $' codePoint put: #xSingleQuote.
	newTable at: $` codePoint put: #xBacktick.
	newTable at: $: codePoint put: #xColon.
	newTable at: $( codePoint put: #leftParenthesis.
	newTable at: $) codePoint put: #rightParenthesis.
	newTable at: $. codePoint put: #period.
	newTable at: $; codePoint put: #semicolon.
	newTable at: $[ codePoint put: #leftBracket.
	newTable at: $] codePoint put: #rightBracket.
	newTable at: ${ codePoint put: #leftBrace.
	newTable at: $} codePoint put: #rightBrace.
	newTable at: $^ codePoint put: #upArrow.
	newTable at: $_ codePoint put: #xUnderscore.
	newTable at: $| codePoint put: #verticalBar.
	newTable at: Character smalltalkLeftArrow codePoint put: #leftArrow.
	newTable at: Character smalltalkUpArrow codePoint put: #upArrow.
	TypeTable := newTable "bon voyage!!"

	"
	Scanner initTypeTable
	"! !


!Editor class methodsFor: 'class initialization' stamp: 'jmv 1/8/2024 13:09:38'!
basicInitialize
	"
	Editor initialize
	"
	self withAllSubclassesDo: [ :c | c initializeShortcuts; initializeCmdShortcuts ]! !

!Editor class methodsFor: 'class initialization' stamp: 'jmv 1/8/2024 13:09:43'!
initialize
	"
	Editor initialize
	"
	self withAllSubclassesDo: [ :c | c basicInitialize ]! !

!Editor class methodsFor: 'class initialization' stamp: 'jmv 1/8/2024 13:09:31'!
initializeCmdShortcuts
	"Initialize the (unshifted) command-key (or alt-key if not on Mac) shortcut table.
	If you want to add a new shortcut for an specific editor, for example SmalltalkEditor, you should
	define the message #smalltalkEditorCmdShortcutsSpec in a class of your category and it will
	be dynamically send"

	"NOTE: if you don't know what your keyboard generates, use Sensor test"

	"
	Editor initialize
	"

	cmdShortcuts := Array new: 256 withAll: #noop:.

	self putIntoCmdShortcuts: self allCmdShortcutsSpec
! !

!Editor class methodsFor: 'class initialization' stamp: 'jmv 1/8/2024 13:09:15'!
putIntoCmdShortcuts: shortcutsSpec

	shortcutsSpec do: [ :ary | | i previous |
		i := ary first codePoint + 1.
		previous := cmdShortcuts at: i.
		previous = #noop:
			ifTrue: [
				cmdShortcuts at: i put: ary second ]
			ifFalse: [ 
				('Editor shortcut: ', ary first printString, ' already taken for: ', previous, 
				'. Override request for: ', ary second, '  ignored') print ]].! !


!CharacterScanner class methodsFor: 'class initialization' stamp: 'jmv 1/8/2024 13:10:22'!
initialize
	"
	CharacterScanner initialize
	"
	| stopConditions |
	stopConditions := Array new: 258.
	stopConditions atAllPut: nil.
	stopConditions at: Character space codePoint + 1 put: nil.
	stopConditions at: Character tab codePoint + 1 put: #tab.

	"This line makes lf be shown as a newLine"
	stopConditions at: Character lf codePoint + 1 put: #doNewLine.

	"This line makes cr be shown as a newLine"
	stopConditions at: Character cr codePoint + 1 put: #doNewLine.

	stopConditions at: CharacterScanner endOfRunCode put: #endOfRun.
	stopConditions at: CharacterScanner crossedXCode put: #crossedX.
	DefaultStopConditions := stopConditions.

	StopConditionsWithSpace := DefaultStopConditions copy.
	StopConditionsWithSpace at: Character space codePoint + 1 put: #space.

	StopConditionsWithPaddedSpace := DefaultStopConditions copy.
	StopConditionsWithPaddedSpace at: Character space codePoint + 1 put: #paddedSpace.
	
	! !

!methodRemoval: RemoteString #checkSum: stamp: 'jmv 1/8/2024 13:10:05'!
RemoteString removeSelector: #checkSum:!
!methodRemoval: String #unescapePercents stamp: 'jmv 1/8/2024 13:11:51'!
String removeSelector: #unescapePercents!
!methodRemoval: UnicodeCodePoint #numericValue stamp: 'jmv 1/8/2024 13:15:08'!
UnicodeCodePoint removeSelector: #numericValue!
!methodRemoval: Character #numericValue stamp: 'jmv 1/8/2024 13:15:08'!
Character removeSelector: #numericValue!
Editor initialize!
CharacterScanner initialize!
