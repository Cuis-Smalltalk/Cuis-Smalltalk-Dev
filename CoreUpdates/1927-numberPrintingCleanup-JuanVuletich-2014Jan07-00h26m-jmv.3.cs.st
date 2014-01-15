'From Cuis 4.2 of 25 July 2013 [latest update: #1926] on 7 January 2014 at 1:12:14.587442 am'!

!SequenceableCollection methodsFor: 'converting' stamp: 'jmv 1/7/2014 00:31'!
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
		self size > 0 ifTrue: [ labelStream skip: -1 ]]! !


!Integer methodsFor: 'printing' stamp: 'jmv 1/7/2014 00:49'!
printOn: aStream thousandSeparator: aString includePlusSign: aBoolean
	"
	String streamContents: [ :strm | 123456789 printOn: strm thousandSeparator: ',' includePlusSign: false ]
	String streamContents: [ :strm | -123456789 printOn: strm thousandSeparator: ',' includePlusSign: false ]
	"
	| digits |
	digits _ self abs printString.
	self sign = -1
		ifTrue: [ aStream nextPut: $- ]
		ifFalse: [
			aBoolean ifTrue: [ aStream nextPut: $+ ]].
	1 to: digits size do: [ :i |
		aStream nextPut: (digits at: i).
		(i < digits size and: [ i - digits size \\ 3 = 0 ]) ifTrue: [
			aStream nextPutAll: aString ]]! !

!Integer methodsFor: 'printing' stamp: 'jmv 1/7/2014 00:46'!
printStringAsBytes
	"Answer a terse, easily-readable representation of this Integer reprsenting a number of bytes.  Useful for file-browsers.
	123 printStringAsBytes
	(12*1024) printStringAsBytes
	1024 printStringAsBytes
	(1024*1024) printStringAsBytes
	(1024*1024*1024) printStringAsBytes
	(1024*1024*1024*1024) printStringAsBytes
	(30 factorial) printStringAsBytes
	"
	| suffixes |
	suffixes _ { 'K'"ilobytes". 'M'"egabytes". 'G'"igabytes". 'T'"erabytes". 'P'"etabytes". 'E'"xabytes". 'Z'"ettabytes". 'Y'"ottabytes"}.
	suffixes size to: 1 by: -1 do: [ :index |  | units |
		units _ 1024 raisedTo: index.
		self >= units ifTrue: [
			^(String streamContents: [ :strm |
				(self / units) printOn: strm fractionDigits: 2.
				strm nextPutAll: (suffixes at: index)
				])]].
	^ self asString! !

!Integer methodsFor: 'printing' stamp: 'jmv 1/7/2014 00:45'!
printStringWords
	"
	SmallInteger maxVal printStringWords
	"
	| mils minus three num answer milCount |
	self = 0 ifTrue: [^'zero'].
	mils _ #('' ' thousand' ' million' ' billion' ' trillion' ' quadrillion' ' quintillion' ' sextillion' ' septillion' ' octillion' ' nonillion' ' decillion' ' undecillion' ' duodecillion' ' tredecillion' ' quattuordecillion' ' quindecillion' ' sexdecillion' ' septendecillion' ' octodecillion' ' novemdecillion' ' vigintillion').
	num _ self.
	minus _ ''.
	self < 0 ifTrue: [
		minus _ 'negative '.
		num _ num negated.
	].
	answer _ String new.
	milCount _ 1.
	[num > 0] whileTrue: [
		three _ (num \\ 1000) threeDigitName.
		num _ num // 1000.
		three isEmpty ifFalse: [
			answer isEmpty ifFalse: [
				answer _ ', ',answer
			].
			answer _ three,(mils at: milCount),answer.
		].
		milCount _ milCount + 1.
	].
	^minus,answer! !


!FileList methodsFor: 'private' stamp: 'jmv 1/7/2014 00:36'!
readContentsHex: brevity
	"retrieve the contents from the external file unless it is too long.
	  Don't create a file here.  Check if exists."
	| f size data hexData |

	f _ directory oldFileOrNoneNamed: self fullName. 
	f ifNil: [^ 'For some reason, this file cannot be read'].
	((size _ f size)) > 5000 & brevity
		ifTrue: [ data _ f next: 10000. f close. brevityState := #briefHex ]
		ifFalse: [ data _ f contentsOfEntireFile. brevityState := #fullHex ].

	hexData _ String streamContents: [ :s |
		0 to: data size-1 by: 16 do: [ :loc |
			loc printOn: s base: 16 length: 8 padded: true.
			s
				space;
				nextPut: $(.
			loc printOn: s base: 10 length: 10 padded: true.
			s
				nextPut: $);
				space;
				tab.
			loc+1 to: (loc+16 min: data size) do: [ :i | s nextPutAll: (data at: i) hex; space ].
			s newLine ]].

	^ acceptedContentsCache _ ((size > 5000) & brevity
		ifTrue: ['File ''{1}'' is {2} bytes long.
You may use the ''get'' command to read the entire file.

Here are the first 5000 characters...
------------------------------------------
{3}
------------------------------------------
... end of the first 5000 characters.' format: {fileName. size. hexData}]
		ifFalse: [hexData])! !


!WordArray class methodsFor: 'as yet unclassified' stamp: 'jmv 1/7/2014 00:55'!
bobsTest
	| wa s1 s2 wa2 answer rawData |
"
WordArray bobsTest
"
	answer _ OrderedCollection new.
	wa _ WordArray with: 16r01020304 with: 16r05060708.
	{false. true} do: [ :pad |
		0 to: 3 do: [ :skip |
			s1 _ ReadWriteStream on: ByteArray new.

			s1 next: skip put: 0.		"start at varying positions"
			wa writeOn: s1.
			pad ifTrue: [s1 next: 4-skip put: 0].	"force length to be multiple of 4"

			rawData _ s1 contents.
			s2 _ ReadWriteStream with: rawData.
			s2 reset.
			s2 skip: skip.			"get to beginning of object"
			wa2 _ WordArray newFromStream: s2.
			answer add: {
				rawData size. 
				skip. 
				wa2 = wa. 
				wa2 asArray collect: [ :each | each printStringBase: 16]
			}
		].
	].
	^answer explore! !


!DirectoryEntry methodsFor: 'access' stamp: 'jmv 1/7/2014 00:46'!
fileSizeString
	"Answer my file size as an easy-to-read String."
	^ self fileSize printStringAsBytes! !


!InstructionPrinter methodsFor: 'printing' stamp: 'jmv 1/7/2014 00:55'!
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
	stream skip: -1.
	stream nextPut: $>.
	stream space.
	stream nextPutAll: instruction.
	stream newLine.
	oldPC := scanner pc.
	"(InstructionPrinter compiledMethodAt: #print:) symbolic."
! !


!RelativeInstructionPrinter methodsFor: 'printing' stamp: 'jmv 1/7/2014 00:55'!
print: instruction 
	"Append to the receiver a description of the bytecode, instruction." 

	stream tab: self indent.
	labelling
		ifTrue: [stream print: oldPC - method initialPC; space]
		ifFalse: [stream tab].
	stream tab: (innerIndents at: oldPC).
	self printCode ifTrue:
		[stream nextPut: $<.
		 oldPC to: scanner pc - 1 do: 
			[:i | | code |
			code := (method at: i) printStringBase: 16.
			stream
				nextPut: (code size < 2 ifTrue: [$0] ifFalse: [code at: 1]);
				nextPut: code last;
				space].
		 stream skip: -1; nextPut: $>; space].
	stream nextPutAll: instruction.
	stream newLine.
	labelling ifFalse:
		[(labels at: scanner pc + 1) ~~ false ifTrue:
			[stream nextPutAll: (labels at: scanner pc + 1); nextPut: $:; newLine]].
	oldPC := scanner pc! !


!Decompiler methodsFor: 'private' stamp: 'jmv 1/7/2014 00:55'!
interpretNextInstructionFor: client

	| code varNames |

"Change false here will trace all state in Transcript."
true ifTrue: [^ super interpretNextInstructionFor: client].

	varNames := self class allInstVarNames.
	code := (self method at: pc) printStringBase: 16.
	Transcript newLine; newLine; print: pc; space;
		nextPutAll: '<' , code, '>'.
	8 to: varNames size do:
		[:i | i <= 10 ifTrue: [Transcript newLine]
				ifFalse: [Transcript space; space].
		Transcript nextPutAll: (varNames at: i);
				nextPutAll: ': '; print: (self instVarAt: i)].
	Transcript endEntry.
	^ super interpretNextInstructionFor: client! !


!Integer methodsFor: 'printing' stamp: 'jmv 1/7/2014 00:29'!
asStringWithCommas
	"
	123456789 asStringWithCommas
	-123456789 asStringWithCommas
	"
	^String streamContents: [ :strm |
		self printOn: strm thousandSeparator: ',' includePlusSign: false ]! !

!Integer methodsFor: 'printing' stamp: 'jmv 1/7/2014 00:30'!
asStringWithCommasSigned
	"
	123456789 asStringWithCommasSigned
	-123456789 asStringWithCommasSigned
	"
	^String streamContents: [ :strm |
		self printOn: strm thousandSeparator: ',' includePlusSign: true ]! !

!Integer methodsFor: 'printing' stamp: 'jmv 1/7/2014 00:43'!
hex
	"Print the receiver as hex, prefixed with 16r.  DO NOT CHANGE THIS!!  The Cog VMMaker depends on this.
	 Consider using any of
		printStringBase: 16
		printStringBase: 16 length: 8 padded: true
		storeStringBase: 16
		storeStringBase: 16 length: 11 padded: true"
	^self storeStringBase: 16! !

!Integer methodsFor: 'printing' stamp: 'jmv 1/7/2014 01:04'!
storeOn: aStream base: base
	"Print a representation of the receiver on the stream
	<aStream> in base <base> where
	2 <= <baseInteger> <= 16. If <base> is other than 10
	it is written first separated by $r followed by the number
	like for example: 16rFCE2"

	| integer |
	integer := self negative
		ifTrue: [ aStream nextPut: $-. self negated ]
		ifFalse: [ self ].
	base = 10 ifFalse: [ aStream nextPutAll: base printString; nextPut: $r ].
	integer printOn: aStream base: base! !


!PopUpMenu methodsFor: 'basic control sequence' stamp: 'jmv 1/7/2014 00:31'!
startUpSegmented: segmentHeight withCaption: captionOrNil at: location allowKeyboard: aBoolean
	"This menu is too big to fit comfortably on the screen.
	Break it up into smaller chunks, and manage the relative indices.
	Inspired by a special-case solution by Reinier van Loon.  The boolean parameter indicates whether the menu should be given keyboard focus (if in morphic)"

"
(PopUpMenu labels: (String streamContents: [:s | 1 to: 100 do: [:i | s print: i; newLine]. s skip: -1])
		lines: (5 to: 100 by: 5)) startUpWithCaption: 'Give it a whirl...'.
"
	| nLines nLinesPer allLabels from to subset subLines index |
	allLabels := labelString lines.
	nLines _ allLabels size.
	lineArray ifNil: [lineArray _ Array new].
	nLinesPer _ segmentHeight // Preferences standardMenuFont height - 5.
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

!methodRemoval: Integer class #readFrom:radix:!
Integer class removeSelector: #readFrom:radix:!

!Integer class reorganize!
('instance creation' basicNew byte1:byte2:byte3:byte4: initializedInstance new new:neg: readBaseOneFrom: readFrom: readFrom:base:)
('prime numbers' largePrimesUpTo: largePrimesUpTo:do: primesUpTo: primesUpTo:do: verbosePrimesUpTo: verbosePrimesUpTo:do:)
('class initialization' initialize initializeLowBitPerByteTable)
!

!methodRemoval: Integer #asBytesDescription!
Integer removeSelector: #asBytesDescription!
!methodRemoval: Integer #asStringThousandSeparator:includePlusSign:!
Integer removeSelector: #asStringThousandSeparator:includePlusSign:!
!methodRemoval: Integer #asWords!
Integer removeSelector: #asWords!
!methodRemoval: Integer #bytesDescription!
Integer removeSelector: #bytesDescription!
!methodRemoval: Integer #destinationBuffer:!
Integer removeSelector: #destinationBuffer:!
!methodRemoval: Integer #digitBuffer:!
Integer removeSelector: #digitBuffer:!
!methodRemoval: Integer #printOn:base:showRadix:!
Integer removeSelector: #printOn:base:showRadix:!
!methodRemoval: Integer #printStringRadix:!
Integer removeSelector: #printStringRadix:!
!methodRemoval: Integer #printWords!
Integer removeSelector: #printWords!
!methodRemoval: Integer #radix:!
Integer removeSelector: #radix:!
!methodRemoval: SequenceableCollection #asStringWithNewline!
SequenceableCollection removeSelector: #asStringWithNewline!
