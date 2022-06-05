'From Cuis 6.0 [latest update: #5213] on 1 June 2022 at 11:12:10 am'!

!ChangeSorter methodsFor: 'code pane' stamp: 'jmv 6/1/2022 10:45:26'!
acceptedStringOrText
	"return the source code that shows in the bottom pane"

	| sel class changeType answer |
	self changed: #clearUserEdits.
	currentClassName ifNil: [^ myChangeSet preambleString ifNil: ['']].
	class _ self selectedClassOrMetaClass.
	(sel _ currentSelector)
		ifNotNil: [
			changeType _ (myChangeSet atSelector: (sel _ sel asSymbol) class: class).
			changeType == #remove
				ifTrue: [^'Method has been removed (see versions)'].
			changeType == #addedThenRemoved
				ifTrue: [^'Added then removed (see versions)'].
			changeType == #movedToOtherPackage
				ifTrue: [^'Method was moved to some other package'].
			class ifNil: [^'Method was added, but cannot be found!!'].
			(class includesSelector: sel)
				ifFalse: [^'Method was added, but cannot be found!!'].
			answer _  (class sourceCodeAt: sel).
			(#(prettyPrint prettyLineDiffs prettyWordDiffs) includes: self contentsSymbol) ifTrue: [
				answer _ (class compilerClass new
						format: answer
						in: class 
						notifying: nil)].
			self showingAnyKindOfDiffs
				ifTrue: [ answer _ (self diffFromPriorSourceFor: answer) ].
			^ answer ]
		ifNil: [
			^ String streamContents: [ :strm |
				(myChangeSet classChangeAt: currentClassName) do: [ :each |
					each == #remove ifTrue: [strm nextPutAll: 'Entire class was removed.'; newLine].
					each == #addedThenRemoved ifTrue: [strm nextPutAll: 'Class was added then removed.'; newLine].
					each == #rename ifTrue: [strm nextPutAll: 'Class name was changed.'; newLine].
					each == #add ifTrue: [strm nextPutAll: 'Class definition was added.'; newLine].
					each == #change ifTrue: [strm nextPutAll: 'Class definition was changed.'; newLine].
					each == #reorganize ifTrue: [strm nextPutAll: 'Class organization was changed.'; newLine].
					each == #comment ifTrue: [strm nextPutAll: 'New class comment.'; newLine].
					each == #movedToOtherPackage ifTrue: [strm nextPutAll: 'Class was moved to some other package.'; newLine].
				]]].! !


!Behavior methodsFor: 'accessing method dictionary' stamp: 'jmv 6/1/2022 10:48:05'!
firstPrecodeCommentFor:  selector
	"If there is a comment in the source code at the given selector that preceeds the body of the method, return it here, else return nil"

	| parser source tree |
	"Behavior firstPrecodeCommentFor: #firstPrecodeCommentFor:"
	(MessageSet isPseudoSelector: selector)
		ifTrue: [
			"Not really a selector"
			^ nil].
	source _ self sourceCodeAt: selector asSymbol ifAbsent: [^ nil].
	parser _ self parserClass new.
	tree _ 
		parser
			parse: source readStream
			class: self
			noPattern: false
			context: nil
			notifying: nil
			ifFail: [^ nil].
	^ (tree comment ifNil: [^ nil]) first! !


!ClassDescription methodsFor: 'instance variables' stamp: 'jmv 6/1/2022 10:46:20'!
browseClassVarRefs 
	"Put up a menu offering all class variable names; if the user chooses one, open up a message-list browser on all methods that refer to the selected class variable"

	| lines labelStream allVars index owningClasses |
	lines _ OrderedCollection new.
	allVars _ OrderedCollection new.
	owningClasses _ OrderedCollection new.
	labelStream _ String writeStream.
	self withAllSuperclasses reverseDo: [ :class | | vars |
		vars _ class classVarNames asArray sort.
		vars do: [ :var |
			labelStream position = 0 ifFalse: [labelStream newLine].
			labelStream nextPutAll: var.
			allVars add: var.
			owningClasses add: class].
		vars isEmpty ifFalse: [ lines add: allVars size ]].
	labelStream contents isEmpty ifTrue: [^Smalltalk beep]. "handle nil superclass better"
	index _ (PopUpMenu labels: labelStream contents lines: lines) startUpMenu.
	index = 0 ifTrue: [^ self].
	Smalltalk browseAllCallsOn:
		((owningClasses at: index) classPool associationAt: (allVars at: index))! !

!ClassDescription methodsFor: 'instance variables' stamp: 'jmv 6/1/2022 10:47:21'!
chooseClassVarName 
	"Present the user with a list of class variable names and answer the one selected, or nil if none"

	| lines labelStream  allVars index |
	lines _ OrderedCollection new.
	allVars _ OrderedCollection new.
	labelStream _ String writeStream.
	self withAllSuperclasses reverseDo: [ :class | | vars |
		vars _ class classVarNames asArray sort.
		vars do: [ :var |
			labelStream position = 0 ifFalse: [labelStream newLine].
			labelStream nextPutAll: var.
			allVars add: var].
		vars isEmpty ifFalse: [lines add: allVars size]].
	labelStream contents isEmpty ifTrue: [^Smalltalk beep]. "handle nil superclass better"
	index _ (PopUpMenu labels: labelStream contents lines: lines) startUpMenu.
	index = 0 ifTrue: [^ nil].
	^ allVars at: index! !

!ClassDescription methodsFor: 'instance variables' stamp: 'jmv 6/1/2022 10:47:34'!
chooseInstVarThenDo: aBlock
	"Put up a menu of all the instance variables in the receiver, and when
the user chooses one, evaluate aBlock with the chosen variable as its
parameter.  If the list is 6 or larger, then offer an alphabetical
formulation as an alternative. triggered by a 'show alphabetically' item
at the top of the list."

	| lines labelStream allVars index count offerAlpha |
	(count _ self allInstVarNames size) = 0 ifTrue: 
		[ ^ self inform: 'There are no\instance variables.' withNewLines ].

	allVars _ OrderedCollection new.
	lines _ OrderedCollection new.
	labelStream _ String writeStream.

	(offerAlpha _ count > 5)
		ifTrue: [
			lines add: 1.
			allVars add: 'show alphabetically'.
			labelStream nextPutAll: allVars first].
	self withAllSuperclasses reverseDo: [ :class | | vars |
		vars _ class instVarNames.
		vars do: [ :var |
			labelStream position = 0 ifFalse: [labelStream newLine].
			labelStream nextPutAll: var.
			allVars add: var].
		vars isEmpty ifFalse: [lines add: allVars size]].
	(lines notEmpty and: [ lines last = allVars size ]) ifTrue: [
		lines removeLast ].  "dispense with inelegant line beneath last item"
	index _ (PopUpMenu labels: labelStream contents lines: lines)
startUpWithCaption: 'Instance variables in
', self name.
	index = 0 ifTrue: [^ self].
	(index = 1 and: [offerAlpha]) ifTrue: [
		^ self chooseInstVarAlphabeticallyThenDo: aBlock].
	aBlock value: (allVars at: index)! !


!DateAndTime class methodsFor: 'squeak protocol' stamp: 'jmv 5/22/2022 20:21:04'!
fromString: aString


	^ self readFrom: aString readStream.! !


!Duration class methodsFor: 'squeak protocol' stamp: 'jmv 5/22/2022 20:21:16'!
fromString: aString
	"
	Extended to accept non-ANSI formats, such as:
		Duration fromString: '1:50:00'
		Duration fromString: '5:30.5'
		Duration fromString: '1:6:50:00'
		Duration fromString: '3.5h'
		Duration fromString: '2.5min'
		Duration fromString: '1.5s'
		Duration fromString: '200s'
		Duration fromString: '200ms'
	"
	| colons s |

	"If aString includes at least one $:, complete ANSI format"
	colons _ aString occurrencesOf: $:.
	colons > 0 ifTrue: [
		s _ aString.
		[colons < 3] whileTrue: [
			s _ '00:', s.
			colons _ colons + 1 ].
		^ self readFrom: s readStream ].

	"'3.5h' means 3.5 hours"
	(aString endsWith: 'h') ifTrue: [
		^self hours: aString asNumber ].

	"'3.5min' means 3.5 minutes"
	(aString endsWith: 'min') ifTrue: [
		^self minutes: aString asNumber ].

	"'3ms' means 3 milliseconds"
	(aString endsWith: 'ms') ifTrue: [
		^self milliSeconds: aString asNumber ].

	"'3.5s' means 3.5 seconds"
	(aString endsWith: 's') ifTrue: [
		^self seconds: aString asNumber ].
	
	^nil! !

!Duration class methodsFor: 'squeak protocol' stamp: 'jmv 5/22/2022 20:58:59'!
readFrom: aStream
	"Formatted as per ANSI 5.8.2.16: [-]D:HH:MM:SS[.S]"

	| sign days hours minutes seconds nanos nanosBuffer |
	sign := (aStream peekFor: $-) ifTrue: [-1] ifFalse: [1].
	days := (aStream upTo: $:) findPositiveInteger * sign.
	hours := (aStream upTo: $:) findPositiveInteger * sign.
	minutes := (aStream upTo: $:) findPositiveInteger * sign.
	seconds := (aStream upTo: $.) findPositiveInteger * sign.
	nanosBuffer := '000000000' copy.
	nanos := nanosBuffer writeStream.
	[aStream atEnd not and: [aStream peek isDigit]]
		whileTrue: [nanos nextPut: aStream next].
		
	^ self 
		days: days 
		hours: hours 
		minutes: minutes 
		seconds: seconds 
		nanoSeconds: (nanosBuffer findPositiveInteger * sign)

	"
		Duration readFrom: '0:00:00:00' readStream
		Duration readFrom: '0:00:00:00.000000001' readStream
		Duration readFrom: '0:00:00:00.9' readStream
		Duration readFrom: '0:00:00:00.99' readStream
		Duration readFrom: '0:00:00:00.99999999' readStream
		Duration readFrom: '0:00:00:00.999999999' readStream
		Duration readFrom: '0:00:00:00.100000000' readStream
		Duration readFrom: '0:00:00:00.001 ' readStream
		Duration readFrom: '0:00:00:00.1' readStream
		Duration readFrom: '0:00:00:01 ' readStream
		Duration readFrom: '0:12:45:45' readStream
		Duration readFrom: '1:00:00:00' readStream
		Duration readFrom: '365:00:00:00' readStream
		Duration readFrom: '-7:09:12:06.10' readStream
		Duration readFrom: '+0:01:02:3' readStream
 	"! !


!Time class methodsFor: 'smalltalk-80' stamp: 'jmv 5/22/2022 20:59:06'!
readFrom: aStream
	"Read a Time from the stream in the form:
		<hour>:<minute>:<second> <am/pm>

	<minute>, <second> or <am/pm> may be omitted.  e.g. 1:59:30 pm; 8AM; 15:30"

	| hour minute second ampm nanos nanosBuffer |
	hour := Integer readFrom: aStream.
	minute := 0.
	second := 0.
	nanosBuffer := '000000000' copy.
	nanos := nanosBuffer writeStream.
	(aStream peekFor: $:) 
		ifTrue: [
			minute := Integer readFrom: aStream.
			(aStream peekFor: $:) 
				ifTrue: [
					second := Integer readFrom: aStream].
					(aStream peekFor: $.)
						ifTrue: [
							[aStream atEnd not and: [aStream peek isDigit]]
								whileTrue: [nanos nextPut: aStream next]]].
	aStream skipSeparators.
	(aStream atEnd not and: [aStream peek isLetter]) ifTrue: 
		[ampm := aStream next asLowercase.
	
	(ampm = $p and: [hour < 12]) ifTrue: [hour := hour + 12].
		(ampm = $a and: [hour = 12]) ifTrue: [hour := 0].
	
	(aStream peekFor: $m) ifFalse: [aStream peekFor: $M ]].
	^ self 
		hour: hour 
		minute: minute 
		second: second 
		nanoSecond: nanosBuffer asNumber

	"Time readFrom: '2:23:09 pm' readStream"! !

!Time class methodsFor: 'squeak protocol' stamp: 'jmv 5/22/2022 20:21:28'!
fromString: aString
	^ self readFrom: aString readStream! !


!Date methodsFor: 'printing' stamp: 'jmv 6/1/2022 10:52:59'!
printFormat: formatArray 
	"Answer a String describing the receiver using the format denoted by the 
	argument, formatArray."

	^ String streamContents: [ :strm |
		self printOn: strm format: formatArray].! !


!RelativeInstructionPrinter methodsFor: 'printing' stamp: 'jmv 6/1/2022 10:58:52'!
printInstructionsOn: aStream
	"Append to the stream, aStream, a description of each bytecode in the instruction stream."
	
	| label |
	labelling := true.
	labels := Array new: method size + 1 withAll: false.
	super printInstructionsOn: String writeStream.
	label := 0.
	labels withIndexDo:
		[:bool :index|
		bool ifTrue: [labels at: index put: 'L', (label := label + 1) printString]].
	labelling := false.
	super printInstructionsOn: aStream! !

!RelativeInstructionPrinter methodsFor: 'printing' stamp: 'jmv 6/1/2022 10:58:56'!
printInstructionsOn: aStream do: aBlock
	"Append to the stream, aStream, a description of each bytecode in the instruction stream.
	  Evaluate aBlock with the receiver, the scanner and the stream after each instruction."
	
	| label |
	labelling := true.
	labels := Array new: method size withAll: false.
	super printInstructionsOn: String writeStream do: [:ig :no :re|].
	label := 0.
	labels withIndexDo:
		[:bool :index|
		bool ifTrue: [labels at: index put: 'L', (label := label + 1) printString]].
	labelling := false.
	super printInstructionsOn: aStream do: aBlock! !


!ContextPart methodsFor: 'debugger access' stamp: 'jmv 6/1/2022 10:59:12'!
tempsAndValuesLimitedTo: sizeLimit indent: indent
	"Return a string of the temporary variabls and their current values"

	| tempNames title |
	^ String streamContents: [ :strm |
		tempNames _ self tempNames.
		1 to: self size do: [ :index |
			title _ tempNames size = self size ifTrue: [tempNames at: index]  ifFalse: [ 'argOrTemp', index printString ].
			indent timesRepeat: [strm tab].
			strm nextPutAll: title; nextPut: $:; space; tab.
			strm nextPutAll: 
				((self tempAt: index) printStringLimitedTo: (sizeLimit -3 -title size max: 1)).
			strm newLine ]].! !


!Decompiler methodsFor: 'instruction decoding' stamp: 'jmv 5/22/2022 20:22:33'!
case: dist
	"statements = keyStmts #CascadeFlag keyValueBlock ... keyStmts"

	| nextCase thenJump stmtStream elements b node cases otherBlock myExits |
	nextCase := pc + dist.

	"Now add #CascadeFlag & keyValueBlock to statements"
	statements addLast: stack removeLast.
	stack addLast: #CaseFlag. "set for next pop"
	statements addLast: (self blockForCaseTo: nextCase).

	stack last == #CaseFlag
		ifTrue: "Last case"
			["ensure jump is within block (in case thenExpr returns weirdly I guess)"
			stack removeLast. "get rid of #CaseFlag"
			stmtStream := (self popTo: stack removeLast) readStream.
			
			elements := OrderedCollection new.
			b := OrderedCollection new.
			[stmtStream atEnd] whileFalse:
				[(node := stmtStream next) == #CascadeFlag
					ifTrue:
						[elements addLast: (constructor
							codeMessage: (constructor codeBlock: b returns: false)
							selector: (constructor codeSelector: #-> code: #macro)
							arguments: (Array with: stmtStream next)).
						 b := OrderedCollection new]
					ifFalse: [b addLast: node]].
			b size > 0 ifTrue: [self error: 'Bad cases'].
			cases := constructor codeBrace: elements.
			
			"try find the end of the case"
			myExits := caseExits removeLast: elements size.
			myExits := myExits reject: [ :e | e isNil or: [ e < 0 or: [ e > method endPC ] ] ].
			thenJump := myExits isEmpty
							ifTrue: [ nextCase ]
							ifFalse: [ myExits max ].
			
			otherBlock := self blockTo: thenJump.
			stack addLast:
				(constructor
					codeMessage: stack removeLast
					selector: (constructor codeSelector: #caseOf:otherwise: code: #macro)
					arguments: (Array with: cases with: otherBlock))].! !


!SequenceableCollection class methodsFor: 'stream creation' stamp: 'jmv 6/1/2022 10:57:20'!
streamContents: blockWithArg estimatedSize: estimatedSize

	| stream originalContents |
	
	stream _ (self ofSize: estimatedSize) writeStream.
	blockWithArg value: stream.
	originalContents _ stream originalContents.
	
	^stream position = originalContents size
		ifTrue: [ originalContents asStreamResult ]
		ifFalse: [ stream contents ]! !


!String methodsFor: 'copying' stamp: 'jmv 6/1/2022 10:59:22'!
join: aCollection 
	"'*' join: #('WWWWW' 'W  EW' 'zzzz')
		->  'WWWWW*W  EW*zzzz' "

	^String streamContents: [ :strm |
		aCollection
			do: [ :elem | strm nextPutAll: elem asString]
			separatedBy: [strm nextPutAll: self]].! !

!String methodsFor: 'converting' stamp: 'jmv 6/1/2022 10:59:18'!
asHex

	^String streamContents: [ :strm |
		self do: [ :ch | strm nextPutAll: ch hex ]].! !

!String methodsFor: 'converting' stamp: 'jmv 6/1/2022 11:00:06'!
asUnHtml
	"Strip out all Html stuff (commands in angle brackets <>) and convert
the characters &<> back to their real value.  Leave actual cr and tab as
they were in text."
	| in out char rest did |
	in _ self readStream.
	out _ String writeStream.
	[ in atEnd ] whileFalse: [
		in peek = $<
			ifTrue: [in unCommand] 	"Absorb <...><...>"
			ifFalse: [(char _ in next) = $&
				ifTrue: [
					rest _ in upTo: $;.
					did _ out position.
					rest = 'lt' ifTrue: [out nextPut: $<].
					rest = 'gt' ifTrue: [out nextPut: $>].
					rest = 'amp' ifTrue: [out nextPut: $&].
					rest = 'deg' ifTrue: [out nextPut: $¡].
					rest = 'quot' ifTrue: [out nextPut: $"].
					rest first = $# ifTrue: [ out nextPut: (Character numericValue: rest findPositiveInteger) ].
					did = out position ifTrue: [
						out nextPut: $&; nextPutAll: rest.
						"self error: 'unknown encoded HTML char'."
						"Please add it to this method"]]
				ifFalse: [out nextPut: char]].
		].
	^ out contents! !

!String methodsFor: 'converting' stamp: 'jmv 6/1/2022 11:00:50'!
keywords
	"Answer an array of the keywords that compose the receiver."
	| kwd char keywords isAllValidInIdentifiers |
	keywords _ Array streamContents: [ :kwds |
		kwd _ String writeStream.
		isAllValidInIdentifiers _ true.
		1
			to: self size
			do: [ :i |
				char _ self at: i.
				kwd nextPut: char.
				char = $: & isAllValidInIdentifiers
					ifTrue: [
						kwds nextPut: kwd contents.
						kwd reset.
						isAllValidInIdentifiers _ true ]
					ifFalse: [
						char isValidInIdentifiers ifFalse: [ isAllValidInIdentifiers _ false ]]].
		kwd isEmpty ifFalse: [ kwds nextPut: kwd contents ]].
	
	^ keywords.! !

!String methodsFor: 'converting' stamp: 'jmv 6/1/2022 11:10:20'!
squeezedTo: n
	"
Examples:
	Do nothing:
		'This one is a rather long phrase' squeezedTo: 32

	1-remove blanks (result can be shorter than asked):
		'This one is a rather long phrase' squeezedTo: 30

	2-remove necessary trailing vowels
		'This one is a rather long phrase' squeezedTo: 24

	3-truncate as needed (and add ellipsis)
		'This one is a rather long phrase' squeezedTo: 15

	4-avoid ellipsis
		'This one is a rather long phrase' squeezedTo: 5
	"
	| vowelCount read write i char allowedVowels str desiredSize postFix j |
	str := self.
	desiredSize := n.
	str size <= n ifTrue: [^str].
	str := str asCamelCase.
	str size <= n ifTrue: [^str].
	postFix := ''.
	desiredSize := n - postFix size.
	vowelCount := str
		inject: 0
		into: [:prev :each | each isVowel ifTrue: [prev + 1] ifFalse: [prev]].
	str size - vowelCount <= desiredSize
		ifTrue: [allowedVowels := vowelCount - (str size - desiredSize)]
		ifFalse: [
			allowedVowels := 0.
			postFix := '...'.
			n - postFix size < 5 ifTrue: [postFix := ''].
			desiredSize := n - postFix size].

	read := str readStream.
	write := String writeStream.
	i := 0.
	j := 0.
	[read atEnd not and: [j < desiredSize]] whileTrue: [
		char := read next.
		(char isVowel not or: [i < allowedVowels]) ifTrue: [
			char isVowel ifTrue: [i := i + 1].
			write nextPut: char.
			j := j + 1]].
	str := write contents , postFix.
	^ str! !

!String methodsFor: 'private' stamp: 'jmv 6/1/2022 11:10:35'!
getEnclosedExpressionFrom: aStream 
	"private - get the expression enclosed between '{' and 
	'}' and remove all the characters from the stream"
	| result currentChar |
	result := String writeStream.

	[aStream atEnd 
		or: [(currentChar := aStream next) = $}]]
		whileFalse: [result nextPut: currentChar].

	^ result contents withBlanksTrimmed! !

!String methodsFor: 'paragraph support' stamp: 'jmv 5/22/2022 20:22:59'!
indentationIfBlank: aBlock
	"Answer the number of leading tabs in the receiver.  If there are
	 no visible characters, pass the number of tabs to aBlock and return its value."
	| reader leadingTabs lastSeparator tab ch |
	tab _ Character tab.
	reader _ self readStream.
	leadingTabs _ 0.
	[ reader atEnd not and: [ (ch _ reader next) == tab ]] whileTrue: [
		leadingTabs _ leadingTabs + 1 ].
	lastSeparator _ leadingTabs + 1.
	[ reader atEnd not and: [
		ch isSeparator and: [ ch isLineSeparator not ]]] whileTrue: [
			lastSeparator _ lastSeparator + 1.
			ch _ reader next ].
	lastSeparator = self size | (ch notNil and: [ch isLineSeparator]) ifTrue: [
		^ aBlock value: leadingTabs ].
	^ leadingTabs! !

!String methodsFor: 'formatting' stamp: 'jmv 6/1/2022 11:10:39'!
format: aCollection 
	"format the receiver with aCollection  
	 
	simplest example:  
	'foo {1} bar' format: {Date today}.
	 
	complete example:  
	'\{ \} \\ foo {1} bar {2}' format: {12. 'string'}.  
	"
	| result stream |
	result := String writeStream.
	stream := self readStream.

	[stream atEnd]
		whileFalse: [| currentChar | 
			currentChar := stream next.
			currentChar = ${
				ifTrue: [| expression | 
					expression := self getEnclosedExpressionFrom: stream.
					result
						nextPutAll: (self evaluateExpression: expression parameters: aCollection)]
				ifFalse: [
					currentChar = $\
						ifTrue: [stream atEnd
								ifFalse: [result nextPut: stream next]]
						ifFalse: [result nextPut: currentChar]]].

	^ result contents! !


!Symbol methodsFor: 'system primitives' stamp: 'jmv 6/1/2022 11:01:33'!
numArgs: n
	"Answer a string that can be used as a selector with n arguments.
	 TODO: need to be extended to support shrinking and for selectors like #+ 
	
	Note: Unrelated, but remember that we inherit #numArgs " 

	| numArgs offs |
	(numArgs _ self numArgs) >= n ifTrue: [ ^self ].	
	^ (String streamContents: [ :strm |
		strm nextPutAll: self.
		(numArgs = 0) ifTrue: [ strm nextPutAll: ':'. offs _ 0 ] ifFalse: [offs _ 1].
		2 to: n - numArgs + offs do: [ :i | strm nextPutAll: 'with:' ]]) asSymbol! !


!CompiledMethod methodsFor: 'printing' stamp: 'jmv 6/1/2022 11:06:21'!
abstractSymbolic
	"Answer a String that contains a list of all the byte codes in a method with a
	 short description of each, using relative addresses and not including code bytes."

	^ String streamContents: [ :strm |
		self longPrintRelativeOn: strm indent: 0 ].! !

!CompiledMethod methodsFor: 'printing' stamp: 'jmv 6/1/2022 11:06:26'!
symbolic
	"Answer a String that contains a list of all the byte codes in a method 
	with a short description of each."

	^ String streamContents: [ :strm |
		self longPrintOn: strm ].! !


!Base64MimeConverter class methodsFor: 'services' stamp: 'jmv 5/22/2022 20:28:33'!
mimeDecode: aString
	^ ByteArray streamContents: [ :out |
		self mimeDecode: aString to: out]! !

!Base64MimeConverter class methodsFor: 'services' stamp: 'jmv 5/22/2022 20:28:53'!
mimeDecode: aString to: outStream
	self new
		mimeStream: aString readStream;
		dataStream: outStream;
		mimeDecodeToByteArray! !

!Base64MimeConverter class methodsFor: 'services' stamp: 'jmv 5/22/2022 20:29:50'!
mimeEncode: aByteArray to: outStream
	self new
		dataStream: aByteArray readStream;
		mimeStream: outStream;
		multiLine: true;
		mimeEncode! !

!Base64MimeConverter class methodsFor: 'additional services' stamp: 'jmv 5/22/2022 20:23:18'!
decodeInteger: mimeString
	| bytes sum |
	"Decode the MIME string into an integer of any length"

	bytes _ (Base64MimeConverter mimeDecodeToBytes: 
				mimeString readStream) contents.
	sum _ 0.
	bytes reverseDo: [:by | sum _ sum * 256 + by].
	^ sum! !


!PositionableStream methodsFor: 'accessing' stamp: 'jmv 5/22/2022 21:13:39'!
upToAny: aCollection 
	"Answer a subcollection from the current access position to the 
	occurrence (if any, but not inclusive) of any objects in the given collection in the receiver. If 
	any of these is not in the collection, answer the entire rest of the receiver."
	| element prevPos |
	^self collectionSpecies streamContents: [ :strm |
		[self atEnd or: [
			prevPos _ self position.
			element _ self next.
			aCollection includes: element ]]
				whileFalse: [strm nextPut: element].
		(aCollection includes: element)
			ifTrue: [self position: prevPos]].! !

!PositionableStream methodsFor: 'positioning' stamp: 'jmv 5/22/2022 20:33:08'!
match: subCollection
	"Set the access position of the receiver to be past the next occurrence of the subCollection. Answer whether subCollection is found.  No wildcards, and case does matter."

	| pattern startMatch |
	pattern _ subCollection readStream.
	startMatch _ nil.
	[pattern atEnd] whileFalse: 
		[self atEnd ifTrue: [^ false].
		(self next) = (pattern next) 
			ifTrue: [pattern position = 1 ifTrue: [startMatch _ self position]]
			ifFalse: [pattern position: 0.
					startMatch ifNotNil: [
						self position: startMatch.
						startMatch _ nil]]].
	^ true

! !


!Compiler methodsFor: 'private' stamp: 'jmv 5/22/2022 20:36:30'!
from: textOrStream class: aClass context: aContext notifying: req

	sourceStream _ (textOrStream is: #Stream)
		ifTrue: [ textOrStream ]
		ifFalse: [ textOrStream asPlainString readStream ].
	class _ aClass.
	context _ aContext.
	requestor _ req.! !


!ParseNode methodsFor: 'printing' stamp: 'jmv 6/1/2022 11:06:49'!
nodePrintOn: aStrm indent: nn
	| var aaStrm myLine |
	"Show just the sub nodes and the code."

	aaStrm := aStrm ifNil: [String writeStream].
	nn timesRepeat: [aaStrm tab].
	aaStrm nextPutAll: self class name; space.
	myLine := self printString withBlanksCondensed.
	myLine := myLine copyFrom: 1 to: (myLine size min: 70).
	aaStrm nextPutAll: myLine; newLine.
	1 to: self class instSize do: [:ii | 
		var := self instVarAt: ii.
		(var respondsTo: #asReturnNode) ifTrue: [var nodePrintOn: aaStrm indent: nn+1]].
	1 to: self class instSize do: [:ii | 
		var := self instVarAt: ii.
		(var isCollection and: [var isSequenceable]) ifTrue: [
			var do: [ :aNode | 
				(aNode respondsTo: #asReturnNode) ifTrue: [
					aNode nodePrintOn: aaStrm indent: nn+1]]]].
	^ aaStrm! !

!ParseNode methodsFor: 'private' stamp: 'jmv 6/1/2022 11:06:31'!
nextWordFrom: aStream setCharacter: aBlock

	| outStream char |
	outStream _ String writeStream.
	[ (aStream peekFor: Character space) or: [ aStream peekFor: Character tab ]] whileTrue.
	[ aStream atEnd or: [
		char _ aStream next.
		char isSeparator ]]
			whileFalse: [ outStream nextPut: char ].
	aBlock value: char.
	^ outStream contents! !


!BlockNode methodsFor: 'printing' stamp: 'jmv 6/1/2022 11:07:32'!
printTemporaries: tempSequence on: aStream doPrior: aBlock
	"Print any in-scope temporaries.  If there are any evaluate aBlock
	 prior to printing.  Answer whether any temporaries were printed."
	| tempStream seen |
	tempSequence ifNil:
		[^false].
	tempStream := String writeStream.
	"This is for the decompiler which canmot work out which optimized block a particular temp is
	 local to and hence may produce diplicates as in
		expr ifTrue: [| aTemp | ...] ifFalse: [| aTemp | ...]"
	seen := Set new.
	tempSequence do:
		[:tempNode |
		tempNode isIndirectTempVector
			ifTrue:
				[tempNode remoteTemps do:
					[:tempVariableNode|
					 (tempVariableNode scope >= 0
					  and: [(seen includes: tempNode key) not]) ifTrue:
						[tempStream space; nextPutAll: (seen add: tempVariableNode key)]]]
			ifFalse:
				[(tempNode scope >= -1
				  and: ["This is for the decompiler which may create a block arg when converting
						a while into a to:do: but won't remove it form temporaries"
					   tempNode isBlockArg not
				  and: [(seen includes: tempNode key) not]]) ifTrue:
					[tempStream space; nextPutAll: (seen add: tempNode key)]]].
	tempStream position = 0 ifTrue:
		[^false].
	aBlock value.
	aStream nextPut: $|; nextPutAll: tempStream contents; space; nextPut: $|.
	^true! !


!TextEditor methodsFor: 'editing keys' stamp: 'jmv 5/22/2022 20:43:07'!
inOutdent: aKeyboardEvent delta: delta
	"Add/remove a tab at the front of every line occupied by the selection. 
	Derived from work by Larry Tesler back in December 1985.  Now triggered by Cmd-L and Cmd-R.  2/29/96 sw"
	"This is a user command, and generates undo"

	| realStart realStop lines startLine stopLine start stop adjustStart indentation size numLines inStream newString outStream |

	"Operate on entire lines, but remember the real selection for re-highlighting later"
	realStart _ self startIndex.
	realStop _ self stopIndex - 1.

	"Special case: the text cursor on a line of its own, including weird case at end of paragraph"
	(realStart > realStop and: [
				realStart < 2 or: [(self privateCurrentString at: realStart - 1) isLineSeparator ]])
		ifTrue: [
			delta < 0
				ifTrue: [
					morph flash]
				ifFalse: [
					self replaceSelectionWith: Character tab asSymbol.
					self deselectAndPlaceCursorAt: realStart + 1].
			^true].

	lines _ textComposition lines.
	startLine _ textComposition lineIndexFor: realStart.
	stopLine _ textComposition lineIndexFor: (realStart max: realStop).
	start _ (lines at: startLine) first.
	stop _ (lines at: stopLine) last.
	
	"Pin the start of highlighting unless the selection starts a line"
	adjustStart _ realStart > start.

	"Find the indentation of the least-indented non-blank line; never outdent more"
	indentation _ (startLine to: stopLine) inject: 1000 into: [ :previousValue :each |
		previousValue min: (textComposition indentationOfLineIndex: each ifBlank: [ :tabs | 1000 ])].			

	size _  stop + 1 - start.
	numLines _ stopLine + 1 - startLine.
	inStream _ self privateCurrentString readStreamFrom: start to: stop.

	newString _ self stringClassToUse new: size + ((numLines * delta) max: 0).
	outStream _ ReadWriteStream on: newString.

	"This subroutine does the actual work"
	self indent: delta fromStream: inStream toStream: outStream.

	"Adjust the range that will be highlighted later"
	adjustStart ifTrue: [realStart _ (realStart + delta) max: start].
	realStop _ realStop + outStream position - size.

	"Prepare for another iteration"
	indentation _ indentation + delta.
	size _ outStream position.
	inStream _ outStream setFrom: 1 to: size.

	outStream
		ifNil: [ 	"tried to outdent but some line(s) were already left flush"
			morph flash]
		ifNotNil: [
			self selectInvisiblyFrom: start to: stop.
			size = newString size ifFalse: [newString _ outStream contents].
			self replaceSelectionWith: newString].
	self selectFrom: realStart to: realStop. 	"highlight only the original range"
	^ true! !


!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 5/22/2022 20:43:16'!
explainNumber: string 
	"Is string a Number?"

	| strm c |
	(c _ string at: 1) isDigit ifFalse: [(c = $- and: [string size > 1 and: [(string at: 2) isDigit]])
			ifFalse: [^nil]].
	strm _ string readStream.
	c _ Number readFrom: strm.
	strm atEnd ifFalse: [ ^nil ].
	c printString = string
		ifTrue: [ ^ string , ' is a ' , c class name ]
		ifFalse: [ ^ string , ' (= ' , c printString , ') is a ' , c class name ]! !


!Transcript class methodsFor: 'class initialization' stamp: 'jmv 6/1/2022 11:10:09'!
initialize
	"
	self initialize
	"
	showOnDisplay _ true.
	bounds _ 20@20 extent: 300@500.
	logToFile _ false.
	logToStdout _ false.
	entries _ Array new: self maxEntries.
	unfinishedEntry _ String writeStream.
	lastDisplayPosition _ 0.
	accessSemaphore _ Semaphore forMutualExclusion.
	self clear! !


!Utilities class methodsFor: 'vm statistics' stamp: 'jmv 5/22/2022 20:43:25'!
vmStatisticsShortString
	"Convenience item for access to recent statistics only"
	"
	(TextModel new contents: Utilities vmStatisticsShortString) openLabel: 'VM Recent Statistics'
	"

	^ self vmStatisticsReportString readStream upToAll: 'Since'; crLfNextLine; upToEnd
! !


!PluggableListMorph methodsFor: 'menus' stamp: 'jmv 6/1/2022 11:07:36'!
copyListToClipboard
	"Copy my items to the clipboard as a multi-line string"

	Clipboard storeObject: (String streamContents: [ :strm |
		list
			do: [:ea | strm nextPutAll: ea asString]
			separatedBy: [ strm newLine ]]).! !


!ChangeSet methodsFor: 'class changes' stamp: 'jmv 6/1/2022 11:07:41'!
fatDefForClass: class

	| newDef oldDef oldStrm newStrm outStrm oldVars newVars addedVars |
	newDef _ class definition.
	oldDef _ (self changeRecorderFor: class) priorDefinition.
	oldDef ifNil: [^ newDef].
	oldDef = newDef ifTrue: [^ newDef].

	oldStrm _ oldDef readStream.
	newStrm _ newDef readStream.
	outStrm _ String writeStream.

	"Merge inst vars from old and new defs..."
	oldStrm upToAll: 'instanceVariableNames'; upTo: $'.
	outStrm 
		nextPutAll: (newStrm upToAll: 'instanceVariableNames'); 
		nextPutAll: 'instanceVariableNames:'.
	newStrm peek = $: ifTrue: [newStrm next].	"may or may not be there, but already written"
	outStrm
		nextPutAll: (newStrm upTo: $'); nextPut: $'.
	oldVars _ (oldStrm upTo: $') findTokens: Character separators.
	newVars _ (newStrm upTo: $') findTokens: Character separators.
	addedVars _ oldVars asSet addAll: newVars; removeAll: oldVars; asOrderedCollection.
	oldVars , addedVars do: [:var | outStrm nextPutAll: var; space].
	outStrm nextPut: $'.

	class isMeta ifFalse: [
		"Merge class vars from old and new defs..."
		oldStrm upToAll: 'classVariableNames:'; upTo: $'.
		outStrm nextPutAll: (newStrm upToAll: 'classVariableNames:'); nextPutAll: 'classVariableNames:';
			nextPutAll: (newStrm upTo: $'); nextPut: $'.
		oldVars _ (oldStrm upTo: $') findTokens: Character separators.
		newVars _ (newStrm upTo: $') findTokens: Character separators.
		addedVars _ oldVars asSet addAll: newVars; removeAll: oldVars; asOrderedCollection.
		oldVars , addedVars do: [:var | outStrm nextPutAll: var; space].
		outStrm nextPut: $'].

	outStrm nextPutAll: newStrm upToEnd.
	^ outStrm contents
! !


!DebuggerMethodMap methodsFor: 'accessing' stamp: 'jmv 6/1/2022 11:07:45'!
tempsAndValuesForContext: aContext
	"Return a string of the temporary variabls and their current values"

	^ String streamContents: [ :strm |
		(self tempNamesForContext: aContext) withIndexDo: [ :title :index |
			strm nextPutAll: title; nextPut: $:; space; tab.
			aContext print: (self namedTempAt: index in: aContext) on: strm.
			strm newLine]].! !


!ReferencesExplorerWrapper methodsFor: 'printing' stamp: 'jmv 6/1/2022 11:07:49'!
pathStringFrom: parentObject to: childObject count: anInteger

	| answer referenceDescription |
	answer _ String writeStream.
	referenceDescription _ self referenceDescriptionFrom: parentObject to: childObject count: anInteger.
	self print: parentObject on: answer.
	answer
		nextPutAll: ' ';
		nextPutAll: referenceDescription;
		nextPutAll: ' --> '.
	self print: childObject on: answer.
	^answer contents! !


!WeightExplorerWrapper methodsFor: 'printing' stamp: 'jmv 6/1/2022 11:07:54'!
weightStringFrom: parentObject to: childObject count: anInteger

	| answer |
	answer _ String writeStream.
	self print: parentObject on: answer.
	^answer contents! !


!ClosureTraceNode methodsFor: 'printing' stamp: 'jmv 6/1/2022 11:09:55'!
dumpString

	^String streamContents: [ :strm |
		self dumpOn: strm ].! !


!SHParserST80 methodsFor: 'scan' stamp: 'jmv 5/23/2022 12:46:55'!
scanNumber
	| start c nc base |
	start := sourcePosition.
	self skipDigits.
	c := self currentChar.
	('rx' includes: c)
		ifTrue: [
			base := Integer readFrom: ((source copyFrom: start to: sourcePosition - 1) readStream).
			self peekChar = $- ifTrue: [self nextChar].
			self skipBigDigits: base.
			c := self currentChar.
			c = $. 
				ifTrue: [
					(self isBigDigit: self nextChar base: base) 
						ifFalse: [sourcePosition := sourcePosition - 1]
						ifTrue: [self skipBigDigits: base]].
			c := self currentChar.
			('deqp'includes: c)
				ifTrue: [
					((nc := self nextChar) isDigit or: [nc = $- and:[self peekChar isDigit]]) 
						ifFalse: [sourcePosition := sourcePosition - 1]
						ifTrue: [self skipDigits]].
			c = $s 
				ifTrue: [
					self nextChar isDigit 
						ifFalse: [sourcePosition := sourcePosition - 1]
						ifTrue: [self skipDigits]].
			currentToken := source copyFrom: start to: sourcePosition - 1.
			^currentTokenSourcePosition := start].
	c = $s 
		ifTrue: [
			self nextChar isDigit 
				ifFalse: [sourcePosition := sourcePosition - 1]
				ifTrue: [self skipDigits.].
			currentToken := source copyFrom: start to: sourcePosition - 1.
			^currentTokenSourcePosition := start].
	c = $. 
		ifTrue: [
			self nextChar isDigit 
				ifFalse: [
					sourcePosition := sourcePosition - 1.
					currentToken := source copyFrom: start to: sourcePosition - 1.
					^currentTokenSourcePosition := start]
				ifTrue: [self skipDigits]].
	c := self currentChar.
	('deqp' includes: c)
		ifTrue: [
			((nc := self nextChar) isDigit or: [nc = $-  and:[self peekChar isDigit]]) 
				ifFalse: [sourcePosition := sourcePosition - 1]
				ifTrue: [self skipDigits]].
	c = $s 
		ifTrue: [
			self nextChar isDigit 
				ifFalse: [sourcePosition := sourcePosition - 1]
				ifTrue: [self skipDigits]].
	currentToken := source copyFrom: start to: sourcePosition - 1.
	^currentTokenSourcePosition := start! !


!DifferenceFinder class methodsFor: 'compatibility' stamp: 'jmv 6/1/2022 11:09:23'!
wordsDisplayPatchFrom: srcString to: dstString
	| finder answer src1 dst1 changedCount |
	finder _ self base: srcString case: dstString.
	finder compareLines; compute.
	answer _ String new asText.
	src1 _ String writeStream.
	dst1 _ String writeStream.
	changedCount _ 0.
	finder differences sort first do: [:item :condition |
		condition caseOf: {
			[ #unchanged ] -> [
				changedCount > 0 ifTrue: [
					"If the sequence of changed lines is large, comparing words gets too slow and less useful"
					changedCount > 30 ifTrue: [
						^nil ].
					"Compare the just ended sequence of changed lines"
					finder base: src1 contents case: dst1 contents.
					finder compareWords; compute: true.
					finder differences ifNil: [ ^nil ].
					answer _ answer append:  finder differences anyOne asText.
					src1 resetToStart.
					dst1 resetToStart.
					changedCount _ 0.
				].
				"This line hasn't changed. Just add it to the result in plain text."
				answer append: item ].
			[ #removed ] -> [
				"A removed line belongs in the source"
				src1 nextPutAll: item.
				changedCount _ changedCount + 1 ].
			[ #inserted ] -> [
				"An added line belongs in the destination"
				dst1 nextPutAll: item.
				changedCount _ changedCount + 1  ].
			}.
		].
	"If the sequence of changed lines is large, comparing words gets too slow and less useful"
	changedCount > 30 ifTrue: [
		^nil ].
	finder base: src1 contents case: dst1 contents.
	finder compareWords; compute: true.
	finder differences ifNil: [ ^nil ].
	answer _ answer append: finder differences anyOne asText.

	^answer! !
