'From Cuis 6.0 [latest update: #5202] on 26 May 2022 at 4:35:15 pm'!

!PositionableStream methodsFor: 'positioning' stamp: 'jmv 5/26/2022 15:12:26'!
skip
	"Skips one object in the receiver's future sequence values."

	self skip: 1! !

!PositionableStream methodsFor: 'positioning' stamp: 'jmv 5/26/2022 15:11:56'!
skipBack
	"Skips back one object in the receiver's future sequence values."

	self skip: -1.! !


!DummyStream methodsFor: 'positioning' stamp: 'jmv 5/26/2022 14:38:09'!
skip
	"Do nothing."! !

!DummyStream methodsFor: 'positioning' stamp: 'jmv 5/26/2022 14:51:58'!
skipBack
	"Do nothing."! !


!TextModel methodsFor: 'undoable commands' stamp: 'jmv 5/26/2022 15:13:45'!
undoAndEvaluate: aTwoArgBlock
	| modelUpdated newCursorPos |
	modelUpdated _ false.
	undoRedoCommands position > 0 ifTrue: [
		undoRedoCommands skipBack.
		newCursorPos _ undoRedoCommands peek undoOn: self.
		modelUpdated _ true ].
	aTwoArgBlock value: modelUpdated value: newCursorPos.
	lastEditTimeStamp _ nil! !


!Number class methodsFor: 'instance creation' stamp: 'jmv 5/26/2022 15:15:32'!
readRemainderOf: integerPart from: aStream base: base withSign: sign
	"Read optional fractional part and exponent, and return the final result"
	| value fraction fracpos peekChar exp scale convertToFloat |

	convertToFloat := false.
	value := integerPart.
	(aStream peekFor: $.)
		ifTrue: [		 "<integer>.<fraction>"
			(aStream atEnd not and: [ aStream peek digitValue between: 0 and: base - 1 ])
				ifTrue:  [
					fracpos := aStream position.
					fraction := Integer readFrom: aStream base: base.
					fraction := fraction / (base raisedToInteger: aStream position - fracpos).
					value := value + fraction.
					convertToFloat := true ]
				ifFalse: [
					"oops - just <integer>."
					aStream skipBack.
					"un-gobble the period"
					^ value * sign"Number readFrom: '3r-22.2'"]].
	peekChar := aStream peek.
	('deqp' includes: peekChar)
		ifTrue: [			"<number>(e|d|q)<exponent>>" "<number>(p)<binaryExponent>>"
			aStream next.
			(aStream atEnd not and: [ (aStream peek digitValue between: 0 and: 9) or: [ '+-' includes: aStream peek ]])
				ifTrue:  [
					exp := Integer readFrom: aStream.
					scale := (peekChar = $p ifTrue: [2] ifFalse: [base]) raisedToInteger: exp.
					value := value * scale ]
				ifFalse: [
					"oops - just <number>."
					aStream skipBack.]].
	^convertToFloat
		ifTrue: [
			(value = 0.0 and: [ sign = -1 ])
				ifTrue: [ Float negativeZero ]
				ifFalse: [ (value * sign) asFloat ]]
		ifFalse: [ value * sign ]! !


!Integer class methodsFor: 'instance creation' stamp: 'jmv 5/26/2022 15:13:51'!
readBaseOneFrom: aStream
	"Unary numeral system
	Supports positive and negative integers.
	See http://en.wikipedia.org/wiki/Unary_numeral_system
	Non-standard features of this system include:
		- The value of a digit does not depend on its position. Thus, one can easily argue that unary is not a positional system at all.
		- Introducing a radix point in this system will not enable representation of non-integer values.
		- The single numeral represents the value 1, not the value 0=b-1.
		- The value 0 cannot be represented (or is implicitly represented by an empty digit string).
	Examples (and equivalent decimal notation)
		1r1 = 10r1
		1r11 = 10r2
		1r111 = 10r3
		1r = 10r0
	"

	| digit value neg |
	neg _ aStream peekFor: $-.
	neg ifFalse: [aStream peekFor: $+].
	value _ 0.
	[ aStream atEnd ]
		whileFalse: [ 
			digit _ aStream next digitValue.
			digit = 1
				ifTrue: [value _ value + digit]
				ifFalse: [
					aStream skipBack.
					neg ifTrue: [^ value negated].
					^ value]].
	neg ifTrue: [ ^ value negated ].
	^ value! !

!Integer class methodsFor: 'instance creation' stamp: 'jmv 5/26/2022 15:13:54'!
readFrom: aStream base: base
	"Answer an instance of one of my concrete subclasses. Initial minus sign
	accepted, and bases > 10 use letters A-Z. Embedded radix specifiers not
	allowed--use Number readFrom: for that. Raises an error if
	there are no digits."

	| digit value neg cc atLeastOneDigitRead |
	
	(aStream atEnd) ifTrue: [ self error: 'At least one digit expected here' ].
	
	neg _ aStream peekFor: $-.
	neg ifFalse: [aStream peekFor: $+].
	value _ 0.
	atLeastOneDigitRead _ false.
	[ aStream atEnd ]
		whileFalse: [
			cc _ aStream next.
			digit _ cc digitValue.
			(digit < 0 or: [digit >= base])
				ifTrue: [
					aStream skipBack.
					atLeastOneDigitRead ifFalse: [self error: 'At least one digit expected here'].
					^neg
						ifTrue: [value negated]
						ifFalse: [value]].
			value _ value * base + digit.
			atLeastOneDigitRead _ true ].
	neg ifTrue: [^ value negated].
	^ value! !


!Date class methodsFor: 'squeak protocol' stamp: 'jmv 5/26/2022 16:33:54'!
readFrom: aStream 
	"Read a Date from the stream in any of the forms:
		<day> <month> <year>		(15 April 1982; 15-APR-82; 15.4.82; 15APR82)  
		<month> <day> <year>		(April 15, 1982; 4/15/82)
		<year>-<month>-<day>		(1982-04-15) (ISO8601)"
	| day month year firstAsNumber firstAsNumberSign |
	firstAsNumberSign := aStream peek = $- ifTrue: [-1] ifFalse: [1].
	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	aStream peek isDigit ifTrue: [
		firstAsNumber := (Integer readFrom: aStream) * firstAsNumberSign.
		(firstAsNumber < 0 or: [firstAsNumber > 31])
			ifTrue: [year := firstAsNumber]].

	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	aStream peek isLetter
		ifTrue: ["MonthName-DD-YY or DD-MonthName-YY or YY-MonthName-DD"
			month _ String streamContents: [ :strm |
				[ aStream peek isLetter ] whileTrue: [ strm nextPut: aStream next ]].
			[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
			firstAsNumber
				ifNil: ["MonthName DD YY"
					day := Integer readFrom: aStream]
				ifNotNil: [
					year ifNil: ["DD MonthName YY"
						day := firstAsNumber]]]
		ifFalse: ["MM-DD-YY or DD-MM-YY or YY-MM-DD"
			year 
				ifNil: ["MM-DD-YY or DD-MM-YY"
					firstAsNumber > 12
						ifTrue: ["DD-MM-YY"
							day := firstAsNumber.
							month := Month nameOfMonth: (Integer readFrom: aStream)]
						ifFalse: ["MM-DD-YY"
							month := Month nameOfMonth: firstAsNumber.
							day := Integer readFrom: aStream]]
				ifNotNil: ["YY-MM-DD"
					month := Month nameOfMonth: (Integer readFrom: aStream)]].

	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	year
		ifNil: [year := Integer readFrom: aStream]
		ifNotNil: [day := Integer readFrom: aStream].
	(year < 100 and: [year >= 0])  ifTrue: [
		year _  year < 69 
			ifTrue: [2000 + year]
			ifFalse: [1900 + year]].

	^ self year: year month: month day: day! !


!Month class methodsFor: 'squeak protocol' stamp: 'jmv 5/26/2022 16:33:59'!
readFrom: aStream 
	"Read a Month from the stream in any of the forms:  
		<month> <year>			(April 1982; APR-82; 4.82; APR82)  
		<month> <year>			(April, 1982; 4/82)
		<year>-<month>			(1982-04) (ISO8601)"
	"
	Month readFrom: 'July 1998' readStream
	"
	| month year firstAsNumber firstAsNumberSign |
	firstAsNumberSign := aStream peek = $- ifTrue: [-1] ifFalse: [1].
	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	aStream peek isDigit ifTrue: [
		firstAsNumber := (Integer readFrom: aStream) * firstAsNumberSign.
		(firstAsNumber < 0 or: [firstAsNumber > 12])
			ifTrue: [year := firstAsNumber]].

	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	aStream peek isLetter
		ifTrue: ["MonthName-YY or YY-MonthName"
			month _ String streamContents: [ :strm |
				[ aStream peek isLetter ] whileTrue: [ strm nextPut: aStream next ]].
			[aStream peek isAlphaNumeric] whileFalse: [aStream skip]]
		ifFalse: ["MM-YY or YY-MM"
			month _ year 
				ifNil: ["MM-YY"
					Month nameOfMonth: firstAsNumber ]
				ifNotNil: ["YY-MM"
					Month nameOfMonth: (Integer readFrom: aStream)]].

	year ifNil: [
		[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
		year := Integer readFrom: aStream].
	(year < 100 and: [year >= 0])  ifTrue: [
		year _ year < 69 
			ifTrue: [2000 + year]
			ifFalse: [1900 + year]].

	^ self month: month year: year! !


!Week class methodsFor: 'squeak protocol' stamp: 'jmv 5/26/2022 15:07:01'!
readFrom: aStream 
	"Read a Week from the stream in any of the forms:  
		<year>-W<week>			(2009-W01) (ISO8601)"
	| weekNumber yearNumber firstAsNumber firstAsNumberSign |
	firstAsNumberSign := aStream peek = $- ifTrue: [-1] ifFalse: [1].
	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	aStream peek isDigit ifTrue: [
		firstAsNumber := (Integer readFrom: aStream) * firstAsNumberSign.
		(firstAsNumber < 0 or: [firstAsNumber > 12])
			ifTrue: [yearNumber := firstAsNumber]].

	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	aStream next = $W ifFalse: [
		self error: 'Invalid Format' ].

	weekNumber _ Integer readFrom: aStream.
	weekNumber < 1 ifTrue: [ self error: 'Invalid Format' ].
	(yearNumber < 100 and: [yearNumber >= 0])  ifTrue: [
		yearNumber _ yearNumber < 69 
			ifTrue: [2000 + yearNumber]
			ifFalse: [1900 + yearNumber]].

	^ self yearNumber: yearNumber weekNumber: weekNumber! !


!Year class methodsFor: 'squeak protocol' stamp: 'jmv 5/26/2022 15:07:07'!
readFrom: aStream 

	| year sign |
	sign := aStream peek = $- ifTrue: [-1] ifFalse: [1].
	[aStream peek isDigit] whileFalse: [aStream skip].
	year := (Integer readFrom: aStream) * sign.
	^ self yearNumber: year! !


!InstructionPrinter methodsFor: 'printing' stamp: 'jmv 5/26/2022 15:13:56'!
print: instruction 
	"Append to the receiver a description of the bytecode, instruction." 

	| code |
	stream tab: self indent.
	printPC ifTrue: [stream print: oldPC; space].
	stream tab: (innerIndents at: oldPC).
	stream nextPut: $<.
	oldPC to: scanner pc - 1 do: 
		[:i | 
		code := (method at: i) printStringBase: 16.
		stream nextPut: 
			(code size < 2
				ifTrue: [$0]
				ifFalse: [code at: 1]).
		stream nextPut: code last; space].
	stream skipBack.
	stream nextPut: $>.
	stream space.
	stream nextPutAll: instruction.
	stream newLine.
	oldPC := scanner pc.
	"(InstructionPrinter compiledMethodAt: #print:) symbolic."
! !


!RelativeInstructionPrinter methodsFor: 'printing' stamp: 'jmv 5/26/2022 15:13:58'!
print: instruction 
	"Append to the receiver a description of the bytecode, instruction." 

	| code |
	stream tab: self indent.
	labelling
		ifTrue: [stream print: oldPC - method initialPC; space]
		ifFalse: [stream tab].
	stream tab: (innerIndents at: oldPC).
	self printCode ifTrue: [
		stream nextPut: $<.
		 oldPC to: scanner pc - 1 do: [ :i |
			code := (method at: i) printStringBase: 16.
			stream
				nextPut: (code size < 2 ifTrue: [$0] ifFalse: [code at: 1]);
				nextPut: code last;
				space].
		 stream skipBack; nextPut: $>; space].
	stream nextPutAll: instruction.
	stream newLine.
	labelling ifFalse: [
		(labels at: scanner pc + 1) ~~ false ifTrue:
			[stream nextPutAll: (labels at: scanner pc + 1); nextPut: $:; newLine]].
	oldPC := scanner pc! !


!Message methodsFor: 'printing' stamp: 'jmv 5/26/2022 15:14:00'!
printOn: stream

	args isEmpty ifTrue: [^ stream nextPutAll: selector].
	args with: selector keywords do: [:arg :word |
		stream nextPutAll: word.
		stream nextPutAll: ' ('.
		arg printOn: stream.
		stream nextPutAll: ') '.
	].
	stream skipBack.
! !


!Pragma methodsFor: 'printing' stamp: 'jmv 5/26/2022 15:14:02'!
printOn: aStream
	aStream nextPut: $<.
	self keyword precedence = 1
		ifTrue: [ aStream nextPutAll: self keyword ]
		ifFalse: [
			self keyword keywords with: self arguments do: [ :key :arg |
				aStream nextPutAll: key; space; print: arg; space ].
			aStream skipBack ].
	aStream nextPut: $>.! !


!Collection methodsFor: 'printing' stamp: 'jmv 5/26/2022 15:14:05'!
printElementsOn: aStream
	aStream nextPut: $(.
	self do: [:element | aStream print: element; space].
	self isEmpty ifFalse: [aStream skipBack].
	aStream nextPut: $)! !


!SequenceableCollection methodsFor: 'converting' stamp: 'jmv 5/26/2022 16:34:04'!
printStringWithNewline
	"Convert to a string with returns between items.  Elements are usually strings.
	 Useful for labels for PopUpMenus.
	#('something' 'there') asStringWithNewline
	"
	
	^String streamContents: [ :labelStream |
		self do: [ :each |
			each isString
				ifTrue: [ labelStream nextPutAll: each; newLine ]
				ifFalse: [
					each printOn: labelStream.
					labelStream newLine ]].
		self size > 0 ifTrue: [ labelStream skipBack ]]! !


!ArrayedCollection class methodsFor: 'instance creation' stamp: 'jmv 5/26/2022 15:14:09'!
newFromStream: s
	"Only meant for my subclasses that are raw bits and word-like.  For quick unpack form the disk."
	| len |

	self isPointers | self isWords not ifTrue: [^ super newFromStream: s].
		"super may cause an error, but will not be called."

	s next = 16r80 ifTrue:
		["A compressed format.  Could copy what BitMap does, or use a 
		special sound compression format.  Callers normally compress their own way."
		^ self error: 'not implemented'].
	s skipBack.
	len _ s nextSignedInt32BigEndian: true.
	^ s nextWordsInto: (self basicNew: len)! !


!Float32Array methodsFor: 'printing' stamp: 'jmv 5/26/2022 15:14:11'!
printElementsOn: aStream
	"FloatArray elements are answered as 64 bit Float, but are really 32 bit Float.
	When printing, print them as 32 bit Float."
	aStream nextPut: $(.
	self do: [ :element |
		element printAsIEEE32BitPrecisionFloatOn: aStream base: 10.
		aStream space].
	self isEmpty ifFalse: [aStream skipBack].
	aStream nextPut: $)! !


!WeakSet methodsFor: 'public' stamp: 'jmv 5/26/2022 15:14:13'!
printElementsOn: aStream
	| oldPos |
	aStream nextPut: $(.
	oldPos _ aStream position.
	self do: [:element | aStream print: element; space].
	aStream position > oldPos ifTrue: [aStream skipBack "remove the extra space"].
	aStream nextPut: $)! !


!Base64MimeConverter methodsFor: 'conversion' stamp: 'jmv 5/26/2022 15:14:16'!
mimeEncode
	"Do conversion reading from dataStream writing to mimeStream"
	"Convert from data to 6 bit characters."

	| phase1 phase2 byte nib lineLength |
	ToCharTable ifNil: [ self class initializeTables ].
	phase1 := phase2 := false.
	lineLength := 0.
	[dataStream atEnd] whileFalse: [
		(multiLine and:[lineLength >= 70]) ifTrue: [ mimeStream newLine.  lineLength := 0. ].
		data := byte := dataStream next.
		nib := (data bitAnd: 16rFC) bitShift: -2.
		mimeStream nextPut: (ToCharTable at: nib+1).
		(byte := dataStream next) ifNil: [byte := 0. phase1 := true].
		data := ((data bitAnd: 3) bitShift: 8) + byte.
		nib := (data bitAnd: 16r3F0) bitShift: -4.
		mimeStream nextPut: (ToCharTable at: nib+1).
		(byte := dataStream next) ifNil: [byte := 0. phase2 := true].
		data := ((data bitAnd: 16rF) bitShift: 8) + (byte).
		nib := (data bitAnd: 16rFC0) bitShift: -6.
		mimeStream nextPut: (ToCharTable at: nib+1).
		nib := (data bitAnd: 16r3F).
		mimeStream nextPut: (ToCharTable at: nib+1).

		lineLength := lineLength + 4.].
	phase1 ifTrue: [
		mimeStream skip: -2; nextPut: $=; nextPut: $=.
		^ mimeStream].
	phase2 ifTrue: [
		mimeStream skipBack; nextPut: $=.
		^ mimeStream]! !


!Stream methodsFor: 'nonhomogeneous accessing' stamp: 'jmv 5/26/2022 15:14:19'!
nextNumber
	"Answer a number from the (text) stream."

	|element|
	[(element := self next) isNil or: [element isDigit or: [element = $- or: [element = $. or: [element = $)]]]]] whileFalse.
	element ifNil: [^nil].
	self skipBack.
	element = $) ifTrue: [^nil].
	^Number readFrom: self! !


!PositionableStream methodsFor: 'accessing' stamp: 'jmv 5/26/2022 15:14:30'!
back
	"Go back one element and return it."
	self position = 0 ifTrue: [ self error: 'CantGoBack' ].
	self skipBack.
	^ self peek! !

!PositionableStream methodsFor: 'accessing' stamp: 'jmv 5/26/2022 15:53:03'!
peekBack
	"Return the element at the previous position, without changing position."

	self position = 0 ifTrue: [ self error: 'CantGoBack' ].
	self skipBack.
	^self next! !

!PositionableStream methodsFor: 'positioning' stamp: 'jmv 5/26/2022 15:11:51'!
skip: n 
	"Skips the next amount objects in the receiver's future sequence values."

	self position: (self position + n min: self contents size).! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'jmv 5/23/2022 17:47:39'!
nextChunk
	"Answer the contents of the receiver, up to the next terminator character. Doubled terminators indicate an embedded terminator character."
	| terminator ch done |
	terminator _ $!!.
	^self collectionSpecies streamContents: [ :strm |
		self skipSeparators.
		done _ false.
		[done not and: [(ch _ self next) ~~ nil]] whileTrue: [
			done _ false.
			(ch = terminator) ifTrue: [
				self peek = terminator ifTrue: [
					self next.  "skip doubled terminator"
				] ifFalse: [
					done _ true  "terminator is not doubled; we're done!!"
				].
			].
			done ifFalse: [
				strm nextPut: ch ].
		]
	]! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'jmv 5/21/2022 10:57:33'!
skipSeparators
	| prevPosition |
	[self atEnd]
		whileFalse: [
			prevPosition _ self position.
			self next isSeparator ifFalse: [
				self position: prevPosition.
				^ self ]]! !


!WriteStream methodsFor: 'character writing' stamp: 'jmv 5/26/2022 15:14:44'!
ensureNoSpace
	"If there is not one on the end, remove it."

	(position > 0 and: [(collection at: position) = Character space]) 
		ifTrue: [self skipBack].! !


!StandardFileStream methodsFor: 'access' stamp: 'jmv 5/26/2022 15:14:52'!
peekFor: item 
	"Answer false and do not advance if the next element is not equal to item, or if this stream is at the end.  If the next element is equal to item, then advance over it and return true"
	| next |
	"self atEnd ifTrue: [^ false]. -- SFStream will give nil"
	(next _ self next) ifNil: [^ false].
	item = next ifTrue: [^ true].
	self skipBack.
	^ false! !


!ReferenceStream methodsFor: 'writing' stamp: 'jmv 5/26/2022 15:15:36'!
beginInstance: aClass size: anInteger
	"This is for use by storeDataOn: methods.  Cf. Object>>storeDataOn:."
	"Addition of 1 seems to make extra work, since readInstance has to compensate.  Here for historical reasons dating back to Kent Beck's original implementation in late 1988.
	In ReferenceStream, class is just 5 bytes for shared symbol.
	SmartRefStream puts out the names and number of class's instances variables for checking.
6/10/97 16:09 tk: See if we can put on a short header. Type = 16. "

	| short ref |
	short _ true.	"All tests for object header that can be written in 4 bytes"
	anInteger <= 254 ifFalse: [short _ false].	"one byte size"
	ref _ references at: aClass name ifAbsent: [short _ false. nil].
	ref isInteger ifFalse: [short _ false].
	short ifTrue: [short _ (ref < 65536) & (ref > 0) "& (ref ~= self vacantRef)"].  "vacantRef is big"
	short ifTrue: [
		byteStream skipBack.
		short _ byteStream next = 9.
		byteStream skip: 0].	"ugly workaround"
	short 
		ifTrue: ["passed all the tests!!"
			byteStream skipBack; nextPut: 16; "type = short header"
				nextPut: anInteger + 1;	"size is short"
				nextUnsignedInt16Put: ref bigEndian: true ]
		ifFalse: [
			"default to normal longer object header"
			byteStream nextUnsignedInt32Put: anInteger + 1 bigEndian: true.
			self nextPut: aClass name].! !


!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 5/26/2022 15:15:15'!
xDigit
	"Form a number."

	tokenType := #number.
	(aheadChar = DoItCharacter and: [source atEnd
			and:  [source skipBack. source next ~= DoItCharacter]])
		ifTrue: [source skipBack "Read off the end last time"]
		ifFalse: [source skipBack; skipBack].
	token := [Number readFrom: source] ifError: [:err :rcvr | self offEnd: err].
	self step; step! !


!DifferenceFinder methodsFor: 'private' stamp: 'jmv 5/26/2022 15:15:18'!
linesIn: aString
	"
	LongestCommonSequenceFinder linesIn: 'x y'
	"
	^Array streamContents: [:strm | | cr read |
		cr := '
'.
		read := aString readStream.
		[read atEnd] whileFalse: [| line |
			line := read nextLine.
			read skipBack.
			read peek = cr last ifTrue: [line := line , cr].
			read skip.
			strm nextPut: line]]
! !


!PopUpMenu methodsFor: 'basic control sequence' stamp: 'jmv 5/26/2022 15:15:24'!
startUpSegmented: segmentHeight withCaption: captionOrNil at: location allowKeyboard: aBoolean
	"This menu is too big to fit comfortably on the screen.
	Break it up into smaller chunks, and manage the relative indices.
	Inspired by a special-case solution by Reinier van Loon.  The boolean parameter indicates whether the menu should be given keyboard focus (if in morphic)"

"
(PopUpMenu labels: (String streamContents: [:s | 1 to: 100 do: [:i | s print: i; newLine]. s skipBack])
		lines: (5 to: 100 by: 5)) startUpWithCaption: 'Give it a whirl...'.
"
	| nLines nLinesPer allLabels from to subset subLines index |
	allLabels := labelString lines.
	nLines _ allLabels size.
	lineArray ifNil: [lineArray _ Array new].
	nLinesPer _ segmentHeight // (PreferenceNG at: #standardMenuFont) lineSpacing - 5.
	from := 1.
	[ true ] whileTrue: [
		to := (from + nLinesPer) min: nLines.
		subset := (allLabels copyFrom: from to: to) asOrderedCollection.
		subset add: (to = nLines ifTrue: ['start over...'] ifFalse: ['more...'])
			before: subset first.
		subLines _ lineArray select: [:n | n >= from] thenCollect: [:n | n - (from-1) + 1].
		subLines _ (Array with: 1) , subLines.
		index := (PopUpMenu labels: subset printStringWithNewline lines: subLines)
					startUpWithCaption: captionOrNil at: location allowKeyboard: aBoolean.
		index = 1
			ifTrue: [from := to + 1.
					from > nLines ifTrue: [ from := 1 ]]
			ifFalse: [index = 0 ifTrue: [^ 0].
					^ from + index - 2]]! !


!PopUpMenu class methodsFor: 'instance creation' stamp: 'jmv 5/26/2022 16:34:10'!
labelArray: labelArray lines: lineArray icons: icons
	"Answer an instance of me whose items are in labelArray, with lines 
	drawn after each item indexed by anArray. 2/1/96 sw"

	labelArray isEmpty ifTrue: [self error: 'Menu must not be zero size'].
	^ self
		labels: (String streamContents: [ :stream |
			labelArray do: [:each | stream nextPutAll: each; newLine].
			stream skipBack "remove last newline"])
		lines: lineArray
		icons: icons

"Example:
	(PopUpMenu labelArray: #('frog' 'and' 'toad') lines: #() icons: #()) startUpWithCaption: 'Please pick one.'
"! !

