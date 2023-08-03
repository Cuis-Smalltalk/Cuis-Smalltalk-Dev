'From Cuis 6.0 [latest update: #5898] on 6 July 2023 at 11:46:42 am'!

!Object methodsFor: 'message handling' stamp: 'jmv 7/6/2023 11:38:18'!
perform: selector withArguments: argArray inSuperclass: lookupClass
	"NOTE:  This is just like perform:withArguments:, except that
	the message lookup process begins, not with the receivers's class,
	but with the supplied superclass instead.  It will fail if lookupClass
	cannot be found among the receiver's superclasses.
	Primitive. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 100 error: errorCode>
	(selector isSymbol)
		ifFalse: [^ self error: 'selector argument must be a Symbol'].
	(selector numArgs = argArray size)
		ifFalse: [^ self error: 'incorrect number of arguments'].
	(self class == lookupClass or: [self class inheritsFrom: lookupClass])
		ifFalse: [^ self error: 'lookupClass is not in my inheritance chain'].
	self error: errorCode.! !


!CodeProvider methodsFor: 'annotation' stamp: 'jmv 7/6/2023 11:39:57'!
annotationForMessageCategory: aCategory ofClass: aClass
	"Provide a line of content for an annotation pane, given that the receiver is pointing at a System Category (i.e. a group of classes)."
	^ String streamContents: [ :strm |
		strm
			nextPutAll: 'Message Category: ';
			nextPutAll: aCategory.
		(Preferences at: #messageCategoryAnnotations) do: [ :each |
			strm nextPutAll: self annotationSeparator.
			each caseOf: {
				[#messagesCount] -> [
					strm
						print: (aClass messagesCountInCategory: aCategory);
						nextPutAll: ' total messages' ].
				[#messageSendsCount] -> [
					strm
						print: (aClass messageSendsCountInCategory: aCategory);
						nextPutAll: ' total message sends' ].
			}]].! !

!CodeProvider methodsFor: 'annotation' stamp: 'jmv 7/6/2023 11:39:47'!
annotationForSystemCategory: aCategory
	"Provide a line of content for an annotation pane, given that the receiver is pointing at a System Category (i.e. a group of classes)."

	^ String streamContents: [ :strm |
		strm
			nextPutAll: 'System Category: ';
			nextPutAll: aCategory.
		(Preferences at: #systemCategoryAnnotations) do: [ :each |
			strm nextPutAll: self annotationSeparator.
			each caseOf: {
				[#classCount] -> [
					strm
						print: (SystemOrganization fullListAtCategoryNamed: aCategory) size;
						nextPutAll: ' classes' ].
				[#instanceMethodsCount] -> [
					strm
						print: (SystemOrganization instanceMethodCountOf: aCategory);
						nextPutAll: ' instance methods' ].
				[#classMethodsCount] -> [
					strm
						print: (SystemOrganization classMethodCountOf: aCategory);
						nextPutAll: ' class methods' ].
				[#linesOfCode] -> [
					strm
						print: (SystemOrganization linesOfCodeOf: aCategory);
						nextPutAll: ' total lines of code' ].
				[#messageSendsCount] -> [
					strm
						print: (SystemOrganization messageSendsCountOf: aCategory);
						nextPutAll: ' total message sends' ].
			}]].! !


!Browser methodsFor: 'annotation' stamp: 'jmv 7/6/2023 11:40:38'!
annotation
	"Provide a line of content for an annotation pane, representing information about the method associated with the selected class and selector in the receiver."

	|  aSelector aClass |
	(aClass := self selectedClassOrMetaClass)
		ifNil: [
			self selectedSystemCategory ifNotNil: [ :sysCat |
				^self annotationForSystemCategory: sysCat ].
			^ ''].
	self editSelection == #editComment
		ifTrue: [^ self annotationForSelector: #Comment ofClass: aClass].
	self isEditingExistingClass 
		ifTrue: [^ self annotationForSelector: #Definition ofClass: aClass].
	(aSelector := self selectedMessageName)
		ifNil: [
			self selectedMessageCategoryName ifNotNil: [ :messageCategory |
				^self annotationForMessageCategory: messageCategory ofClass: aClass ].
			^ ''].
	^ self annotationForSelector: aSelector ofClass: aClass.! !


!CodePackageList methodsFor: 'accessing' stamp: 'jmv 7/6/2023 11:40:22'!
summary

	| count |
	selection ifNil: [ ^'' ].
	^ String streamContents: [ :strm |
		| methods  linesOfCode |
		strm
			nextPutAll: 'Package: ';
			nextPutAll: selection packageName;
			nextPutAll: ' -- ';
			nextPutAll: selection sourceSystem.
		strm nextPutAll: ' -- Number of system categories '.
		selection systemCategories size printOn: strm.
		strm nextPutAll: '.'.
		strm nextPutAll: ' -- Number of classes: '.
		count := 0.
		selection classesDo: [ :cls | count := count + 1 ].
		count printOn: strm.
		strm nextPutAll: '. Number of extension methods: '.
		selection extensionMethods size printOn: strm.
		strm nextPutAll: '. Total number of methods: '.
		methods := selection methods size.
		methods printOn: strm.
		strm nextPutAll: '. Total lines of code: '.
		linesOfCode := selection linesOfCode.
		linesOfCode printOn: strm.
		strm nextPutAll: ' ('.
		linesOfCode / (methods asFloat max: 1.0) printOn: strm fractionDigits: 2.
		strm nextPutAll: ' per method)'.
		strm nextPutAll: '. Total message sends: '.
		linesOfCode := selection messageSendsCount.
		linesOfCode printOn: strm ]! !


!SmallInteger methodsFor: 'printing' stamp: 'jmv 7/6/2023 11:28:41'!
printString
	"Highly optimized version for base 10
	and that we know it is a SmallInteger."
	
	| integer next result len |
	self = 0 ifTrue: [^'0'].
	self < 0 ifTrue: [^'-', self negated printString].
	len := self decimalDigitLength.
	result := ByteArray new: len.
	integer := self.
	len to: 1 by: -1 do: [:i |
		next := integer // 10.
		result at: i put: 48 + (integer - (next * 10)).
		integer := next ].
	^result asString! !


!CharacterSequence methodsFor: 'converting' stamp: 'jmv 7/6/2023 11:36:41'!
asIdentifier: shouldBeCapitalized
	"Return a legal identifier, with first character in upper case if shouldBeCapitalized is true, else lower case.  This will always return a legal identifier, even for an empty string"

	| aString |
	aString := self select: [ :el | el isValidInIdentifiers ].
	(aString isEmpty or: [aString first isValidStartOfIdentifiers not])
		ifTrue:	 [aString := 'a', aString].
	^ shouldBeCapitalized ifTrue: [ aString capitalized ] ifFalse: [ aString uncapitalized ]

"
'234Fred987' asIdentifier: false
'235Fred987' asIdentifier: true
'' asIdentifier: true
'()87234' asIdentifier: false
'())z>=PPve889  U >' asIdentifier: false
"! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 7/6/2023 11:37:54'!
asValidInstanceVariableName
	| answer |
	answer := self asIdentifier: false.
	(Scanner pseudoVariableNames includes: answer)
		ifTrue: [ answer := answer , 'x' ].
	^ answer

"
'234znak 43 ) 2' asValidInstanceVariableName
'234 xx\ Uml /ler42342380-4' asValidInstanceVariableName
'super' asValidInstanceVariableName
"! !

!CharacterSequence methodsFor: 'converting' stamp: 'di 1/16/98 16:33'!
contractTo: smallSize
	"return myself or a copy shortened by ellipsis to smallSize"
	| leftSize |
	self size <= smallSize
		ifTrue: [^ self].  "short enough"
	smallSize < 5
		ifTrue: [^ self copyFrom: 1 to: smallSize].    "First N characters"
	leftSize _ smallSize-2//2.
	^ self copyReplaceFrom: leftSize+1		"First N/2 ... last N/2"
		to: self size - (smallSize - leftSize - 3)
		with: '...'
"
	'A clear but rather long-winded summary' contractTo: 18
"! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 7/6/2023 11:38:07'!
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
	write := self species writeStream.
	i := 0.
	j := 0.
	[read atEnd not and: [j < desiredSize]] whileTrue: [
		char := read next.
		(char isVowel not or: [i < allowedVowels]) ifTrue: [
			char isVowel ifTrue: [i := i + 1].
			write nextPut: char.
			j := j + 1]].
	str := write contents , postFix.
	^ str.! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 7/6/2023 11:37:39'!
withDescriptiveLineEndings
	"
	('Hello', String newLineString, 'World') withDescriptiveLineEndings
	"
	^ self withLineEndings: '[enter]'.! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 7/6/2023 11:37:47'!
withoutTrailingBlanks
	"Return a copy of the receiver from which trailing blanks have been trimmed."

	| last |
	
	last := self lastNonSeparator.
	last = 0 ifTrue: [^ ''].  "no non-separator character"
	last = self size ifTrue: [ ^self ].
	
	^ self copyFrom: 1 to: last

	"
	' abc  d   ' withoutTrailingBlanks
	"! !


!Symbol methodsFor: 'evaluating' stamp: 'jmv 7/6/2023 11:35:38'!
value: anObject 
	self numArgs = 0 ifFalse: [
		^ self error: 'incorrect number of arguments'		].
	^anObject perform: self! !


!UnicodeString class methodsFor: 'unicode demo' stamp: 'jmv 7/6/2023 11:38:24'!
demoAncientEgyptian
	"
	UnicodeString demoAncientEgyptian.
	"
	| utf8 fontFamily |
	utf8 :=  #[240 147 128 128 240 147 133 184 240 147 137 128 240 147 144 174 240 147 128 144 240 147 128 145 240 147 128 146 32 240 147 131 176 240 147 131 177 240 147 131 178 240 147 131 179 240 147 131 180 240 147 131 181 240 147 131 182 32 240 147 130 128 32 240 147 133 184 240 147 133 185 240 147 133 186 240 147 133 187 32 240 147 133 189 32 240 147 133 190 240 147 133 191 32 240 147 137 160 240 147 137 161 240 147 137 162 240 147 137 163 240 147 137 164 32 240 147 134 178 240 147 129 183 240 147 143 164 240 147 142 159 240 147 128 128 240 147 129 144 240 147 143 165 240 147 131 128 240 147 136 150 240 147 140 177 240 147 133 147 240 147 142 155 240 147 133 177 240 147 128 148 240 147 136 150 240 147 140 177 240 147 133 147 240 147 142 155 240 147 135 139 240 147 135 139 240 147 143 143 240 147 129 144 240 147 144 141 240 147 130 139 240 147 139 180 240 147 130 157 240 147 142 155 240 147 139 169 240 147 137 148 240 147 138 170 240 147 143 155 240 147 139 180 240 147 144 160 240 147 132 191 240 147 130 139 240 147 143 143 240 147 140 151 240 147 128 129 240 147 140 183 240 147 130 157 240 147 143 143 240 147 143 173 240 147 143 155 240 147 135 190 240 147 143 143 240 147 133 147 240 147 133 177 240 147 128 128 240 147 129 144 240 147 143 170 240 147 131 128 240 147 140 162 240 147 140 162 240 147 136 150 240 147 136 150 240 147 143 155].

	fontFamily := FontFamily readIfNeeded: DirectoryEntry smalltalkImageDirectory / 'TrueTypeFonts'/ 'NotoEgyptianHieroglyphs'.
	(Text string: (UnicodeString fromUtf8Bytes: utf8) attribute: (TextFontFamilyAndSize familyName: fontFamily familyName pointSize: 60))
		editLabel: 'Ancient Egyptian Hieroglyphs!!'.! !


!UnicodeSymbol methodsFor: 'converting' stamp: 'jmv 7/6/2023 11:34:24'!
numArgs: n
	"Answer a string that can be used as a selector with n arguments.
	 TODO: need to be extended to support shrinking and for selectors like #+ 
	
	Note: Unrelated, but remember that we inherit #numArgs " 

	| numArgs offs |
	(numArgs := self numArgs) >= n ifTrue: [ ^self ].	
	^ (UnicodeString streamContents: [ :strm |
		strm nextPutAll: self.
		(numArgs = 0) ifTrue: [ strm nextPutAll: ':'. offs := 0 ] ifFalse: [offs := 1].
		2 to: n - numArgs + offs do: [ :i | strm nextPutAll: 'with:' ]]) asSymbol.! !

!UnicodeSymbol methodsFor: 'evaluating' stamp: 'jmv 7/6/2023 11:34:32'!
value: anObject 
	self numArgs = 0 ifFalse: [
		^ self error: 'incorrect number of arguments'		].
	^anObject perform: self.! !


!UnicodeSymbol class methodsFor: 'testing' stamp: 'jmv 7/6/2023 11:32:49'!
testSelectorPrefix

	^ 'test'.! !


!SystemDictionary methodsFor: 'startup' stamp: 'jmv 7/6/2023 11:41:30'!
displayCommandLineUsageOn: aStream
	"Display command line usage on standard out."

	[ aStream
		nextPutAll: 'Command line options:'; newLine;
		nextPutAll: '  -h Display this help message and quit'; newLine;
		nextPutAll: '  -v Display the system information, containing the version and quit'; newLine;
		nextPutAll: '  -q Just quit'; newLine;
		nextPutAll: '  -e Do not ignore exceptions, but open a Debugger'; newLine;
		nextPutAll: '  -r <feature> Require <feature>'; newLine;
		nextPutAll: '  -d <code> Evaluate <code>'; newLine;
		nextPutAll: '  -l <codeFile> Load e.g. file in <codeFile>'; newLine;
		nextPutAll: '  -s <scriptFile> Evaluate code in <scriptFile>'; newLine;
		nextPutAll: '  -- end of options; stop processing options'; newLine;
		newLine;
		nextPutAll: 'Arguments for scripts, code files, packages (and <code>):'; newLine;
		nextPutAll: '  are available in ''Smalltalk startUpScriptArguments'''; newLine;
		newLine;
		nextPutAll: 'Multiple options:'; newLine;
		nextPutAll: '  any option can be passed multiple times'; newLine ]
	on: UnhandledError
	do: [ :exception | exception return ].! !

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 7/6/2023 11:41:24'!
displaySystemInformationStringOn: aStream
	"Display the system information string on standard output."

	[ aStream
		nextPutAll: 'System information:';		newLine;
		nextPutAll: self systemInformationString;		newLine ]
	on: UnhandledError
	do: [ :exception | exception return ].! !

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 7/6/2023 11:41:16'!
processCommandLineOption: optionCharacter optionArgument: optionArgument raiseExceptions: aBoolean
	"
	A possible example (duplicated single quotes: '' should be double quotes, but not allowed in a Smalltalk comment):
Squeak.app/Contents/MacOS/Squeak Cuis-Smalltalk-Dev/Cuis6.0-????.image -h
Squeak.app/Contents/MacOS/Squeak Cuis-Smalltalk-Dev/Cuis6.0-????.image -v
Squeak.app/Contents/MacOS/Squeak Cuis-Smalltalk-Dev/Cuis6.0-????.image -q
Squeak.app/Contents/MacOS/Squeak Cuis-Smalltalk-Dev/Cuis6.0-????.image -e -d''0 zope''
Squeak.app/Contents/MacOS/Squeak Cuis-Smalltalk-Dev/Cuis6.0-????.image -r YAXO -rJSON -d ''Smalltalk startUpScriptArguments print'' xx YY 3492 -d'' 'here comes another' print'' -d ''{'The answer is: '. Smalltalk startUpScriptArguments first asNumber + Smalltalk startUpScriptArguments second asNumber} print'' 3 4 -- -d'' 'not this' print' ''

Squeak.exe Cuis4.2-2211x.image -r RequiredFeature1 -rRequiredFeature2 -d ''Transcript show: 'popo1'; newLine'' -d''Transcript show: 'popo2'; newLine'' -s smalltalkScript.st param1ToScript param2ToSCript ''parametro 3 to script ->>>--// 3''
	"
	"You can use 'Smalltalk startUpScriptArguments' inside the startUp script:"


	| exceptionToIgnoreOrNil entry |
	exceptionToIgnoreOrNil := aBoolean ifFalse: [UnhandledError] ifTrue: [nil].
	optionCharacter caseOf: {
		[ $h ] -> [
			self displayCommandLineUsageOn: StdIOWriteStream stdout.
			self quit ].

		[ $v ] -> [ 
			self displaySystemInformationStringOn: StdIOWriteStream stdout.
			self quit ].

		[ $q ] -> [ self quit ].

		[ $r ] -> [		"as in 		-rRequiredFeature2        or         -r RequiredFeature2"
			{ 'Feature require: '. optionArgument } print.
			[ Feature require: optionArgument ] on: exceptionToIgnoreOrNil do: [:ex | ex return] ].

		[ $d ] -> [		"as in 		-d ''Transcript show: 'popo1'; newLine'' -d''Transcript show: 'popo2'; newLine''        (duplicated singleQuotes should read doubleQuote)"
			{ 'Compiler evaluate: '. optionArgument } print.
			[ Compiler evaluate: optionArgument ] on: exceptionToIgnoreOrNil do: [:ex | ex return] ].

		[$l ] -> ["file in the file"
			{ 'File in: '. optionArgument} print.
			[(CodeFile newFromFile: optionArgument asFileEntry) fileIn] on: exceptionToIgnoreOrNil do: [:ex | ex return]].

		[ $s ] -> [		"as in 		-s smalltalkScript.st paramAlScript1 paramAlSCript2 ''parameter to script script ->>>--// 3'' 			(duplicated singleQuotes should read doubleQuote)"
			{ 'Compiler evaluate contents of file named: '. optionArgument. ' arguments: '. Smalltalk startUpScriptArguments } print.
			entry := optionArgument asFileEntry.
			entry exists ifTrue: [
				entry readStreamDo: [ :stream |
					[ Compiler evaluate: stream contentsOfEntireFile ] on: exceptionToIgnoreOrNil do: [:ex | ex return]]]].
	}
	otherwise: [].! !


!SystemDictionary class methodsFor: 'copyright' stamp: 'jmv 7/6/2023 11:40:49'!
copyright
	"The Cuis Smalltalk copyright.
	Parts are copyright of many contributors to Squeak and Cuis projects."

	^
'Portions of Cuis are:
Copyright (c) Xerox Corp. 1981, 1982.
Copyright (c) Apple Computer, Inc. 1985-1996.
Copyright (c) Contributors to Squeak project. 1997-2023.
Copyright (c) Contributors to Cuis Smalltalk project. 2009-2023.'.! !


!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 7/6/2023 11:36:21'!
next: anInteger putAll: aStringOrBytes startingAt: startIndex
	"Argument can be String, UnicodeString or ByteArray.
	Write to this file. Both kinds of Strings are UTF-8 encoded.
	Optimized only for ByteArray, the common use."

	(aStringOrBytes is: #ByteArray) ifFalse: [
		(startIndex = 1 and: [anInteger = aStringOrBytes size])ifTrue: [
			^self nextPutAll: aStringOrBytes].
		^self nextPutAll: (aStringOrBytes copyFrom: startIndex to: startIndex+anInteger-1) ].

	rwmode ifFalse: [^ self error: 'Cannot write a read-only file'].
	collection ifNotNil: [ 
		position < readLimit ifTrue: [ self flushReadBuffer ] ].
	self primWrite: fileID from: aStringOrBytes startingAt: startIndex count: anInteger.
	lastWrittenElement := aStringOrBytes at: startIndex+anInteger-1.! !

!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 7/6/2023 11:36:10'!
nextPutAll: aStringOrBytes
	"Argument can be String, UnicodeString or ByteArray.
	Write to this file. Both kinds of Strings are UTF-8 encoded."

	| bytes |
	aStringOrBytes isEmpty ifTrue: [ ^self ].
	bytes := aStringOrBytes.
	(aStringOrBytes is: #ByteArray) ifFalse: [
		bytes := aStringOrBytes asUtf8Bytes].
	rwmode ifFalse: [^ self error: 'Cannot write a read-only file'].
	collection ifNotNil: [ 
		position < readLimit ifTrue: [ self flushReadBuffer ] ].
	self primWrite: fileID from: bytes startingAt: 1 count: bytes basicSize.
	lastWrittenElement := aStringOrBytes last.! !


!DataStream methodsFor: 'write and read' stamp: 'jmv 7/6/2023 11:33:58'!
next
	"Answer the next object in the stream."
	| type selector anObject isARefType pos internalObject |

	type := byteStream next.
	type ifNil: [pos := byteStream position.	"absolute!!!!"
		byteStream close.	"clean up"
		byteStream position = 0 
			ifTrue: [self error: 'The file did not exist in this directory'] 
			ifFalse: [self error: 'Unexpected end of object file'].
		pos.	"so can see it in debugger"
		^ nil].
	type = 0 ifTrue: [pos := byteStream position.	"absolute!!!!"
		byteStream close.	"clean up"
		self error: 'Expected start of object, but found 0'.
		^ nil].
	isARefType := self noteCurrentReference: type.
	selector := #(readNil readTrue readFalse readInteger	"<-4"
			readStringOld readSymbol readByteArray		"<-7"
			readArray readInstance readReference readBitmap	"<-11"
			readClass readUser readFloat errorRead readShortInst 	"<-16"
			readString readWordArray readWordArrayForSegment 	"<-19"
			readWordLike readMethod 
			readCharacter readUnicodeString readUnicodeSymbol "<-24") at: type.
	selector = 0 ifTrue: [pos := byteStream position.	"absolute!!!!"
			byteStream close. 
			self error: 'file is more recent than this system'. ^ nil].
	anObject := self perform: selector. "A method that recursively
		calls next (readArray, readInstance, objectAt:) must save &
		restore the current reference position."
	isARefType ifTrue: [self beginReference: anObject].

		"After reading the externalObject, internalize it.
		 #readReference is a special case. Either:
		   (1) We actually have to read the object, recursively calling
			   next, which internalizes the object.
		   (2) We just read a reference to an object already read and
			   thus already interalized.
		 Either way, we must not re-internalize the object here."
	selector == #readReference ifTrue: [^ anObject].
	internalObject := anObject comeFullyUpOnReload: self.
	^ self maybeBeginReference: internalObject.! !


!Process methodsFor: 'debugging' stamp: 'jmv 7/6/2023 11:35:25'!
debugContext: context title: title
	"Open debugger on self with context shown on top"

	self isTerminated ifTrue: [^ self error: 'can not debug a terminated process'].
	Debugger openProcess: self context: context label: title! !


!Scanner class methodsFor: 'cached class state' stamp: 'jmv 7/6/2023 11:33:34'!
initTypeTable
	| newTable |
	newTable := Array new: 127 withAll: #xIllegal. "default"
	newTable atAll: #(9 10 12 13 32 ) put: #xDelimiter. "tab lf ff cr space"
	newTable atAll: '!!%&+-*/\,<=>?@~' asByteArray put: #xBinary.
	1 to: 127 do: [:index |
		(Character numericValue: index) isLetter ifTrue: [
			newTable at: index put: #xLetter ]].
	newTable atAll: ($0 numericValue to: $9 numericValue) put: #xDigit.
	newTable at: self doItCharacterValue put: #doIt.
	newTable at: $" numericValue put: #xDoubleQuote.
	newTable at: $# numericValue put: #xLitQuote.
	newTable at: $$ numericValue put: #xDollar.
	newTable at: $' numericValue put: #xSingleQuote.
	newTable at: $` numericValue put: #xBacktick.
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
	newTable at: Character smalltalkLeftArrow numericValue put: #leftArrow.
	newTable at: Character smalltalkUpArrow numericValue put: #upArrow.
	TypeTable := newTable "bon voyage!!"

	"
	Scanner initTypeTable
	"! !


!MessageNode methodsFor: 'macro transformations' stamp: 'jmv 7/6/2023 11:35:12'!
transformIfFalseIfTrue: encoder
	(Preferences at: #allowNonBooleanReceiversOfBooleanMessages) ifTrue: [
		(receiver == NodeTrue) | (receiver == NodeTrue) ifFalse: [
			^false ]].
	^(self checkBlock: (arguments at: 1) as: 'False arg' from: encoder maxArgs: 0)
	   and: [(self checkBlock: (arguments at: 2) as: 'True arg' from: encoder maxArgs: 0)
	   and: [selector := SelectorNode new key: #ifTrue:ifFalse: code: #macro.
			arguments swap: 1 with: 2.
			arguments do: [:arg| arg noteOptimizedIn: self].
			true]].! !

!MessageNode methodsFor: 'macro transformations' stamp: 'jmv 7/6/2023 11:35:03'!
transformIfTrueIfFalse: encoder
	(Preferences at: #allowNonBooleanReceiversOfBooleanMessages) ifTrue: [
		(receiver == NodeTrue) | (receiver == NodeTrue) ifFalse: [
			^false ]].
	^(self checkBlock: (arguments at: 1) as: 'True arg' from: encoder maxArgs: 0)
	   and: [(self checkBlock: (arguments at: 2) as: 'False arg' from: encoder maxArgs: 0)
	   and: [arguments do: [:arg| arg noteOptimizedIn: self].
			true]].! !


!MessageNode class methodsFor: 'class initialization' stamp: 'jmv 7/6/2023 11:33:49'!
initClassCachedState
	"
	MessageNode initialize
	"
	MacroSelectors := 
		#(	ifTrue: ifFalse: ifTrue:ifFalse: ifFalse:ifTrue:
			and: or:
			whileFalse: whileTrue: whileFalse whileTrue
			to:do: to:by:do:
			caseOf: caseOf:otherwise:
			ifNil: ifNotNil:  ifNil:ifNotNil: ifNotNil:ifNil:
			repeat ).
	MacroTransformers := 
		#(	transformIfTrue: transformIfFalse: transformIfTrueIfFalse: transformIfFalseIfTrue:
			transformAnd: transformOr:
			transformWhile: transformWhile: transformWhile: transformWhile:
			transformToDo: transformToDo:
			transformCase: transformCase:
			transformIfNil: transformIfNil:  transformIfNilIfNotNil: transformIfNotNilIfNil:
			transformRepeat: ).
	MacroEmitters := 
		#(	emitCodeForIf:encoder:value: emitCodeForIf:encoder:value:
			emitCodeForIf:encoder:value: emitCodeForIf:encoder:value:
			emitCodeForIf:encoder:value: emitCodeForIf:encoder:value:
			emitCodeForWhile:encoder:value: emitCodeForWhile:encoder:value:
			emitCodeForWhile:encoder:value: emitCodeForWhile:encoder:value:
			emitCodeForToDo:encoder:value: emitCodeForToDo:encoder:value:
			emitCodeForCase:encoder:value: emitCodeForCase:encoder:value:
			emitCodeForIfNil:encoder:value: emitCodeForIfNil:encoder:value:
			emitCodeForIf:encoder:value: emitCodeForIf:encoder:value:
			emitCodeForRepeat:encoder:value:).
	MacroSizers := 
		#(	sizeCodeForIf:value: sizeCodeForIf:value: sizeCodeForIf:value: sizeCodeForIf:value:
			sizeCodeForIf:value: sizeCodeForIf:value:
			sizeCodeForWhile:value: sizeCodeForWhile:value: sizeCodeForWhile:value: sizeCodeForWhile:value:
			sizeCodeForToDo:value: sizeCodeForToDo:value:
			sizeCodeForCase:value: sizeCodeForCase:value:
			sizeCodeForIfNil:value: sizeCodeForIfNil:value: sizeCodeForIf:value: sizeCodeForIf:value:
			sizeCodeForRepeat:value:).
	MacroPrinters := 
		#(	printIfOn:indent: printIfOn:indent: printIfOn:indent: printIfOn:indent:
			printIfOn:indent: printIfOn:indent:
			printWhileOn:indent: printWhileOn:indent: printWhileOn:indent: printWhileOn:indent:
			printToDoOn:indent: printToDoOn:indent:
			printCaseOn:indent: printCaseOn:indent:
			printIfNil:indent: printIfNil:indent: printIfNilNotNil:indent: printIfNilNotNil:indent:
			printRepeatOn:indent:).
	Preferences
		name: #allowNonBooleanReceiversOfBooleanMessages
		description: 'If set, do not optimize boolean selectors, allowing, for example, new Boolean classes.'
		category: #programming type: Boolean value: false! !


!TextEditor methodsFor: 'editing keys' stamp: 'jmv 7/6/2023 11:34:55'!
enclose: aKeyboardEvent
	"Insert or remove bracket characters around the current selection."
	"This is a user command, and generates undo"

	| left right startIndex stopIndex oldSelection which |
	startIndex := self startIndex.
	stopIndex := self stopIndex.
	oldSelection := self selection.
	which := '([<{"''`' indexOf: aKeyboardEvent keyCharacter ifAbsent: [ ^true ].
	left := '([<{"''`' at: which.
	right := ')]>}"''`' at: which.
	((startIndex > 1 and: [stopIndex <= model textSize])
			and: [ (model actualContents at: startIndex-1) = left
				and: [(model actualContents at: stopIndex) = right]])
		ifTrue: [
			"already enclosed; strip off brackets"
			self selectFrom: startIndex-1 to: stopIndex.
			self replaceSelectionWith: oldSelection]
		ifFalse: [
			"not enclosed; enclose by matching brackets"
			self replaceSelectionWith:
				(Text string: (UnicodeString with: left) attributes: emphasisHere),
				oldSelection,
				(Text string: (UnicodeString with: right) attributes: emphasisHere).
			self selectFrom: startIndex+1 to: stopIndex].
	^ true.! !

!TextEditor methodsFor: 'typing support' stamp: 'jmv 7/6/2023 11:34:40'!
dispatchOn: aKeyboardEvent
	"Carry out the action associated with this character, if any."

	| codePoint asciiValue c |
	self clearParens.
  	codePoint := aKeyboardEvent keyValue.
	
	codePoint > 255 ifTrue: [
		^self normalCharacter: aKeyboardEvent ].

	asciiValue := codePoint.
	"Control keys are handled by #shortcuts even if they have any modifiers"
	(self shouldHandleUsingCmdShortcuts: aKeyboardEvent) ifTrue: [
		^self perform: (self cmdShortcuts at: asciiValue + 1) with: aKeyboardEvent ].

	c := aKeyboardEvent keyCharacter.
	(')]}' includes: c)
		ifTrue: [ self blinkPrevParen: c ].

	^ self perform: (self shortcuts at: asciiValue + 1) with: aKeyboardEvent.! !


!FileIOAccessor methodsFor: 'file stream creation' stamp: 'jmv 7/6/2023 11:31:40'!
privateWriteableFile: aFileEntry
	"Open the file with the given name in this directory for writing."

	| pathName |
	pathName := aFileEntry pathName.
	^ (UniFileStream new open: pathName forWrite: true)
		ifNil: [
			"Failed to open the file"
			(FileWriteError fileName: pathName)
				signal: ('File [', pathName, '] open for write failed' ) ].! !


!FeatureRequirement methodsFor: 'testing' stamp: 'jmv 7/6/2023 11:33:00'!
isAlreadySatisfied
	"Answer true if requirement is satisfied by some installed package, or by base system."

	(Feature baseSystemFeature satisfies: self)
		ifTrue: [ ^true ].

	"VectorGraphics is now part of the base Cuis image, and therefore already satisfied."
	name = 'VectorGraphics' ifTrue: [ ^true ].

	^CodePackage installedPackages anySatisfy: [ :package |
		package provides satisfies: self ].! !


!Clipboard methodsFor: 'accessing' stamp: 'jmv 7/6/2023 11:31:49'!
retrieveMorph
	"Answer a Morph or nil."
	| answer |
	
	answer := self retrieveObject.
	
	"If clipboard contents is already a Text or String, answer it."
	(answer is: #Morph) ifTrue: [
		^ answer ].

	"If clipboard contains a Form, prepare a Morph including it."
	(answer is: #Form) ifTrue: [
		^ ImageMorph new image: answer ].

	"If clipboard contents is already a Text or String, answer it."
	(answer isString or: [ answer is: #Text ]) ifTrue: [
		^ (TextModelMorph withText: answer asText)
			embeddedInMorphicWindowLabeled: 'Text editor' ].
	
	"No useful contents"
	^nil.! !


!TrueTypeFont methodsFor: 'printing' stamp: 'jmv 7/6/2023 11:32:26'!
printOn: aStream
	aStream
		nextPutAll: 'TrueTypeFont(''';
		nextPutAll: self familyName;
		nextPutAll: ''' ';
		space;
		print: self pointSize;
		nextPut: $).! !


!FontFamily class methodsFor: 'ui' stamp: 'jmv 7/6/2023 11:29:15'!
promptUserAndSetDefault
	"Present a menu of available font families, and if one is chosen, change to it.
	FontFamily promptUserAndSetDefault
	"
	| fontFamily |
	self promptUserFolders ifNotNil: [ :selectedNameOrDirectory |
		fontFamily := self readIfNeeded: selectedNameOrDirectory.
		fontFamily includesAscii
			ifTrue: [PreferenceSet setDefaultFont: fontFamily familyName ]
			ifFalse: [self inform: 'Selected font does not include ASCII characters. Can not be set as default.' ]]! !


!HandMorph methodsFor: 'private events' stamp: 'jmv 7/6/2023 11:35:45'!
generateKeyboardEvent: evtBuf
	"Generate the appropriate mouse event for the given raw event buffer"
	| buttons modifiers type keyValue pressType stamp mouseScrollDirection |
	stamp := evtBuf second.
	stamp = 0 ifTrue: [ stamp := Time millisecondClockValue ]. "VMs report events using #millisecondClockValue"
	(evtBuf sixth <= 0 or: [ (keyValue := evtBuf sixth) isNil ]) ifTrue: [ keyValue := Character macRomanToLatin1: evtBuf third ].

	modifiers := evtBuf fifth.
	pressType := evtBuf fourth.
	pressType = EventSensor eventKeyDown ifTrue: [
		type := #keyDown.
		lastKeyDownValue := keyValue ].
	pressType = EventSensor eventKeyUp ifTrue: [
		(keyValue = 9 and: [(modifiers anyMask: 1) and: [Smalltalk platformName = 'unix']])
			ifTrue: [
				"Linux VMs don't generate shift-tab keystroke. Turn #keyUp into #keystroke"
				pressType := EventSensor eventKeyChar ]
			ifFalse: [type := #keyUp ]].
	pressType = EventSensor eventKeyChar ifTrue: [
		type := #keystroke.
		"If Control key pressed, and the VM answers a code below 27,
		 it means it did the translation, convert it back to regular character:
		We want to handle the meaning of ctrl ourselves."
		(modifiers anyMask: 2) ifTrue: [ 											"Control key pressed"
			keyValue < 27 ifTrue: [ 										"But we don't want to do it for Home/End/PgUp/PgDn, just for alphabetic keys"
				lastKeyDownValue = keyValue ifFalse: [   									"If equal, real Home/End/PgUp/PgDn in Windows => don't translate"
					(keyValue + 64 = lastKeyDownValue or: [ "If Equal, Ctrl-alphabetic in Windows => do translate"
						lastKeyDownValue < 47 ]) ifTrue: [ 			"Not on windows. If less (not sure about the bound, but do not translate 48: tab on Mac), alphabetic on Mac => do translate"
						keyValue := (modifiers anyMask: 1)
							ifTrue: [ keyValue + 64 ]
							ifFalse: [ keyValue + 96   						"shift not pressed: conver to lowercase letter" ]]]].
			"On Windows, ctrl-backSpace is reported as ctrl-forwardDelete. But keyDown is ok, so we can know and fix."
			(keyValue = 127 and: [ lastKeyDownValue = 8 ])
				ifTrue: [ keyValue := 8 ].
			"Act as if command/alt was pressed for some usual Windows ctrl-key combinations"
			(self shouldControlEmulateAltFor: keyValue) ifTrue: [ modifiers := modifiers bitOr: 8 ]].
		(modifiers anyMask: 8) ifTrue: [ 									"CmdAlt key pressed (or Control key pressed, and #shouldControlEmulateAltFor: just answered true)"
			(modifiers anyMask: 1) ifTrue: [								"Shift pressed"
				| i |
				"It seems that for ctrl-shifted keys and cmd-shifted keys, the VM incorrectly reports the UNSHIFTED character.
				Correct this, at least for common cmd-shortcuts, and for the US keyboard... Sigh...
				(This has only been observed on Mac VMs, but seems harmless if proper shifted character is reported (as in Linux), as this wil be NOP)
				(On Windows, the situation is even worse: ctrl-{ is not even reported as a keystroke event. Only keyDown and keyUp.)"
				"#($' $, $. $9 $0 $[ $]) -> #($'' $< $> $( $) ${) $}"
				i := #[39 44 46 57 48 91 93 ] indexOf: keyValue.
				i > 0 ifTrue: [
					keyValue := #[34 60 62 40 41 123 125] at: i ]]]].
	buttons := modifiers bitShift: 3.
	"Linux and Windows VM send keyboard ctrl-upArrow and ctrl-downArrow when the user tries to scroll using the mouse wheel
	Mac VM sends cmd-option-ctrl-shift-upArrow and cmd-option-ctrl-shift-downArrow for trackpad vertical scroll gestures,
		and cmd-option-ctrl-shift-leftArrow and cmd-option-ctrl-shift-rightArrow for horizontal scroll gestures.
	This way of reporting scroll events by the VM also enables scrolling using the keyboard (actually, we can't tell if user gesture was on Mouse, Trackpad or Keyboard).
	But ctrl-shift and cmdAlt-shift are needed used for selecting while moving by word, line, etc.
	Additionally, #ctrlArrowsScrollHorizontally allows chosing between keyboard horizontal scroll and moving word by word in text editors."
	mouseScrollDirection := nil.
	"Ctrl for Keyboard or Mouse wheel gestures. All modifiers for Trackpad gestures."
	(buttons = InputSensor controlKey or: [buttons = InputSensor cmdAltOptionCtrlShiftModifierKeys]) ifTrue: [
		keyValue = 30
			ifTrue: [mouseScrollDirection := #up]
		ifFalse: [keyValue = 31
			ifTrue: [mouseScrollDirection := #down]]].
	"Ctrl for Keyboard or Mouse wheel gestures, only if preference is set. All modifiers for Trackpad gestures."
	((buttons = InputSensor controlKey and: [Preferences at: #ctrlArrowsScrollHorizontally]) or: [buttons = InputSensor cmdAltOptionCtrlShiftModifierKeys]) ifTrue: [
		keyValue = 28
			ifTrue: [mouseScrollDirection := #left]
		ifFalse: [keyValue = 29
			ifTrue: [mouseScrollDirection := #right]]].
	mouseScrollDirection ifNotNil: [
		^ MouseScrollEvent new
			setType: #mouseScroll
			position: self morphPosition
			direction: mouseScrollDirection
			buttons: buttons
			hand: self
			stamp: stamp ].
	^ KeyboardEvent new
		setType: type
		buttons: buttons
		position: self morphPosition
		keyValue: keyValue
		hand: self
		stamp: stamp.! !


!StringRequestMorph methodsFor: 'initialization' stamp: 'jmv 7/6/2023 11:40:57'!
addTextPane
	| result |
	result := (TextModelMorph
		textProvider: self
		textGetter: #response
		textSetter: #response:
		selectionGetter: #selectionInterval) emptyTextDisplayMessage: 'Enter response'.
	self
		valueOfProperty: #emptyTextDisplayMessage
		ifPresentDo: [ :msg |
			result emptyTextDisplayMessage: msg ].
	result
		hasUnacceptedEdits: true;
		acceptOnCR: true;
		escAction: [ self cancel ].
	self addMorph: result layoutSpec: LayoutSpec useAll.! !


!ImageMorph class methodsFor: 'cached state access' stamp: 'jmv 7/6/2023 11:33:09'!
defaultForm
	"
	On an Inspector on the Form, do 
		self writeBMPfileNamed: 'DefaultForm.bmp'
		self writeJPEGfileNamed: 'DefaultForm.jpg'
	(Base64MimeConverter mimeEncode: ( 'DefaultForm.jpg' asFileEntry binaryContents readStream)) upToEnd edit
	Select all, copy, paste in the String literal in this method.
	ImageMorph defaultForm display
	"

	DefaultForm ifNotNil: [ ^DefaultForm ].
	DefaultForm := Form fromBinaryStream:
		'/9j/4AAQSkZJRgABAQAAAQABAAD/2wBDAAgGBgcGBQgHBwcJCQgKDBQNDAsLDBkSEw8UHRof
Hh0aHBwgJC4nICIsIxwcKDcpLDAxNDQ0Hyc5PTgyPC4zNDL/2wBDAQkJCQwLDBgNDRgyIRwh
MjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjL/wAAR
CAA5AFUDASIAAhEBAxEB/8QAHwAAAQUBAQEBAQEAAAAAAAAAAAECAwQFBgcICQoL/8QAtRAA
AgEDAwIEAwUFBAQAAAF9AQIDAAQRBRIhMUEGE1FhByJxFDKBkaEII0KxwRVS0fAkM2JyggkK
FhcYGRolJicoKSo0NTY3ODk6Q0RFRkdISUpTVFVWV1hZWmNkZWZnaGlqc3R1dnd4eXqDhIWG
h4iJipKTlJWWl5iZmqKjpKWmp6ipqrKztLW2t7i5usLDxMXGx8jJytLT1NXW19jZ2uHi4+Tl
5ufo6erx8vP09fb3+Pn6/8QAHwEAAwEBAQEBAQEBAQAAAAAAAAECAwQFBgcICQoL/8QAtREA
AgECBAQDBAcFBAQAAQJ3AAECAxEEBSExBhJBUQdhcRMiMoEIFEKRobHBCSMzUvAVYnLRChYk
NOEl8RcYGRomJygpKjU2Nzg5OkNERUZHSElKU1RVVldYWVpjZGVmZ2hpanN0dXZ3eHl6goOE
hYaHiImKkpOUlZaXmJmaoqOkpaanqKmqsrO0tba3uLm6wsPExcbHyMnK0tPU1dbX2Nna4uPk
5ebn6Onq8vP09fb3+Pn6/9oADAMBAAIRAxEAPwD3+iisbxP4ks/C+jvfXWHkJ2W9uGw08nZR
/MnsAT2oA5nxP8UbXwx4oOlXGntJbQxpJc3AnCugfONsePmHT+IHrgHu1fjL4UN2sBOoKu8I
8rWrBUyCQT39OACeRxjJHjHinxJHe64us67KjX0iCOMRR4WNAScADnALHlsnnrgYFe80qC5s
540jUSur7HPJVmOc5+tAH1PYX9pqljDfWNxHcWsy7o5Y2yrD/PFTySJFG0kjqiICzMxwAB1J
NfMfhrXNf0SF5bDWLq1WchzEu1ozwMkI4YKWIJJAB55NP1C9vtXZm1XULq/LNvK3MpaMN6rH
9xT/ALqigD3Sb4ieEYJmifX7QspwdhLD8CAQauaX4w8Oa1P5Gna1ZT3H/PETASHpyEOCRyOc
Yr55AAGAMCmyRpKu10DDryKAPqOivA/D/j/XvDgSIP8A2nYr/wAu11IQ6jGAEk5I7cEEcY4z
ket+GvGmj+KXnisZJY7mEBnt7hNj7f7w5IYZ4yCefqMgHQ0UUUAFfNviK/vr/wAWaodVdjfQ
zvEUIOIow3yIvGNuCD75z3r6SrjfF/w503xXcC8W4m06/wABZLi3APmqOgdTwSOx644zjigD
wyW3gnZGmhjkKHKF1B2n1GelSV0XjD4fa34VsItQt72PUbVFP2t/s/liHnhiN5IXHBbJx1Ix
05Lzb2QYS3ij/wBt5Nwx7ADn9KAJ5ZkhVS+fmZUAHUknFSVXhtdriWaQzSjozAAL/ujt/P3q
xQAUUUUAFTWmoXOkX9vqdmC1zaOJUUHG/HVeo4YZH41DRQB9NWV5b6jYW99aSCW2uYlmikAI
DIwyDz6giiub+Gs3nfD3SP8AWYiR4Bv64jkZBj2wvHtiigDq6KKKAEZVdSrAFSMEEcEV8za7
p58O6/qWkRxNLFZXBjTY2SsTKskY55JCOoPfIPWvpiSRIo2kkdURAWZmOAAOpJrwNLT/AITn
4n30cMhW3vLqSSSeEZAt4QsQdW5HzhEIJ4+fPtQBzMU0cy7o3DD9R9R2p9dV8Thoenavp9to
8US/YrFxciAbgQSvlKTzlwFfrz8wJzkV3WifC/w7aaRE2sWCXl+yB7medj8pxkqMHAVeQB+J
JJJIB43UYuIWnaASp5q8lM/Nj1x6V6RqNx8IJHuIlwjsxzcada3LKDnPyPGpQj6ZHavMtZm0
eXU7i10aO/1KziAKPdRKjh+cjdwOOMHCn2JGaALNM0qz1DVtWGj6apubmYZikYfLHyQTIQOF
X169RycZghvLZtL8pbC4i1G2QCa5F/5sHqTIpT5SFzwGHIHOM16J8MtF1+28Sw6lb2clrpNz
ETdyyIqi4XafL2g/NncwO4DBAOSeKAPWtH0yDRdGstMts+TaQrCpPVtoxk+56n3NFXaKACii
igDmvGPgy18a2dtZ3t/eW1tC5kZLYqPMOMDO5WHHUcflVTRPhtoWg2d7DbyX00t5ataS3U9w
TL5ZGCFIACnvkDORXYUUAef6b8HfDGmzxyK+ozrHL5qxzXPycNuC4AGQD/LnNd9JGksbRyIr
o4KsrDIIPUEU6igDn/8AhBfCO3b/AMItopXOQDYREA8dBt46CiTwL4SleNn8M6QfLBCr9jj2
9AOVxg8DjPTtXQUUAZ+maFo+ieb/AGTpVjYedjzPslukW/GcZ2gZxk9fU1oUUUAFFFFAH//Z'
				base64Decoded readStream.
	^DefaultForm.! !


!PluggableListMorph methodsFor: 'initialization' stamp: 'jmv 7/6/2023 11:35:52'!
initForKeystrokes
	lastKeystrokeTime := 0.
	lastKeystrokes := ''.! !


!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 7/6/2023 11:41:06'!
createTextPaneAcceptOnCR: acceptBoolean
	"create the textPane"
	| result |
	self flag: #todo.
	"Integrate this method with the Theme system. --cbr"
	result := (TextModelMorph
		textProvider: self
		textGetter: #response
		textSetter: #response:
		selectionGetter: #selectionInterval) emptyTextDisplayMessage: 'Enter response'.
	self
		valueOfProperty: #emptyTextDisplayMessage
		ifPresentDo: [ :msg |
			result emptyTextDisplayMessage: msg ].
	result
		hasUnacceptedEdits: true;
		acceptOnCR: acceptBoolean;
		escAction: [ self cancelClicked ];
		morphExtent: `18 @ 5` * self sizeUnit.
	self
		addMorph: result
		position: `1 @ 2` * self sizeUnit.
	^ result.! !


!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 7/6/2023 11:30:04'!
preferencesMenuOptions

	^`{
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		10.
				#label 			-> 		'Focus follows mouse'.
				#selector 			-> 		#enableFocusFollowsMouse.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'At all times, make the active window and widget the one on which the mouse is located.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		20.
				#label 			-> 		'Click to focus'.
				#selector 			-> 		#disableFocusFollowsMouse.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'At all times, make the active window and widget the one where the mouse was clicked.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		40.
				#label 			-> 		'Size of GUI elements...'.
				#object 			-> 		Theme.
				#selector 			-> 		#changeFontSizes.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'use larger or smaller text and widgets'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		50.
				#label			->		'Set System Font...'.
				#object 			-> 		FontFamily.
				#selector 			-> 		#promptUserAndSetDefault.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'change the current system font family.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		60.
				#label			->		'Load all TrueType Fonts'.
				#object 			-> 		TrueTypeFontFamily.
				#selector 			-> 		#readAdditionalFonts.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'Load additional TrueType fonts included with Cuis.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		70.
				#label 			-> 		'Icons...'.
				#object 			-> 		Theme.
				#selector 			-> 		#changeIcons.
				#icon 			-> 		#worldIcon.
				#balloonText 	-> 		'show more or less icons.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		80.
				#label 			-> 		'Themes...'.
				#object 			-> 		Theme.
				#selector 			-> 		#changeTheme.
				#icon 			-> 		#appearanceIcon.
				#balloonText 	-> 		'switch to another theme.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'Show taskbar'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#showTaskbar.
				#icon 			-> 		#expandIcon.
				#balloonText 	-> 		'show the taskbar'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		20.
				#label 			-> 		'Hide taskbar'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#hideTaskbar.
				#icon 			-> 		#collapseIcon.
				#balloonText 	-> 		'hide the taskbar'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		10.
				#label 			-> 		'Full screen on'.
				#selector 			-> 		#fullScreenOn.
				#icon 			-> 		#viewFullscreenIcon.
				#balloonText 	-> 		'puts you in full-screen mode, if not already there.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		20.
				#label 			-> 		'Full screen off'.
				#selector 			-> 		#fullScreenOff.
				#icon 			-> 		#exitFullscreenIcon.
				#balloonText 	-> 		'if in full-screen mode, takes you out of it.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		10.
				#label 			-> 		'Set Code Author...'.
				#object 			-> 		Utilities.
				#selector 			-> 		#setAuthor.
				#icon 			-> 		#usersIcon.
				#balloonText 	-> 		'supply initials to be used to identify the author of code and other content.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		20.
				#label 			-> 		'All preferences...'.
				#object 			-> 		PreferenceSet.
				#selector 			-> 		#openPreferencesInspector.
				#icon 			-> 		#preferencesIcon.
				#balloonText 	-> 		'view and change various options.'
			} asDictionary.
		}`.! !

!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 7/6/2023 11:30:14'!
worldMenuOptions

	^`
		TheWorldMenu worldMainMenuOptions,
		TheWorldMenu additionalMenuOptions,
		TheWorldMenu preferencesMenuOptions,
		TheWorldMenu windowsMenuOptions,
		TheWorldMenu helpMenuOptions,
		TheWorldMenu changesMenuOptions,
		TheWorldMenu debugMenuOptions`.! !


!VectorCanvas methodsFor: 'initialization' stamp: 'jmv 7/6/2023 11:39:22'!
setForm: aForm subPixelAntiAliasing: aBoolean
	"nil means use default kind of anti aliasing"
	"
	self runningWorld setCanvas
	"
	aForm depth = 32 ifFalse: [ self error: 'VectorCanvas can only work on 32 bit target' ].
	form := aForm.
	engine := VectorEngine onForm: form subPixelAntiAliasing: aBoolean.
	engine canvas: self.
	auxBitBltEngine := BitBltCanvasEngine toForm: form.
	self newClipRect: nil.! !


!VectorCanvas class methodsFor: 'class initialization' stamp: 'jmv 7/6/2023 11:39:31'!
initialize
	VectorEngineWithPlugin isPluginAvailable ifFalse: [
		Feature require: 'VectorEngineInSmalltalk' ].
	UISupervisor whenUIinSafeState: [
		(VectorEngineWithPlugin isPluginAvailable
			ifTrue: [
				"VectorEnginePlugin is still slow to make fills of rectangles (world background, window background)
				For instance, these methods
				#blendFillOnlyWPOTAt:antiAliasAlphaByte:
				#blendFillOnlyWPOTLeft:top:right:bottom:targetBits:morphIds:edgeCounts:alphaMask:
				could keep computed values from previous pixel, and reuse them most times.
				"
				MorphicCanvas activeSubclass: HybridCanvas
				"MorphicCanvas activeSubclass: VectorCanvas"
				]
			ifFalse: [
				'VectorEnginePlugin (VM plugin) is not available. HybridCanvas will be activated.' print.
				MorphicCanvas activeSubclass: HybridCanvas ]) ].! !


!HybridCanvas methodsFor: 'private' stamp: 'jmv 7/6/2023 11:38:56'!
setForm: aForm subPixelAntiAliasing: aBooleanOrNil
	"nil means use default kind of anti aliasing"

	form := aForm.
	engine := BitBltCanvasEngine toForm: form.
	"Init BitBlt so that the first call to the 'primitiveDisplayString' primitive will not fail"
	engine sourceX: 0; width: 0.
	vectorCanvas := VectorCanvas onForm: form.
	vectorCanvas attachedCanvas: self.
	morphIDsForm := Form extent: form extent depth: 32 bits: (vectorCanvas engine instVarNamed: 'morphIds').
	drawingOnMorphIDs := false.
	self newClipRect: nil.! !


!Theme class methodsFor: 'user interface' stamp: 'jmv 7/6/2023 11:39:40'!
changeTheme

	| menu |
	menu := MenuMorph new.
	menu
		addTitle: 'Choose a theme';
		addStayUpIcons.
	(Theme allSubclasses sorted: [ :a :b | a name < b name ]) do: [ :themeClass |
		menu add: themeClass name target: themeClass action: #beCurrent ].
	(FeatureRequirement name: 'Theme-Themes') isAlreadySatisfied ifFalse: [
		menu addLine.
		menu add: '* Load Additional Themes *' target: self action: #loadAdditionalThemes ].
	menu popUpInWorld: self runningWorld.! !


!TTFontDescription methodsFor: 'private-initialization' stamp: 'jmv 7/6/2023 11:39:13'!
glyphsByUtf8Bytes: glyphsByUTF8 unknownGlyph: theUnknownGlyph scaleArrowGlyphs: arrowGlyphs

	| mGlyph |
	
	mGlyph := glyphsByUTF8 at: $M asciiValue + 1.
	letterMTopSideBearing := (mGlyph ifNil: [theUnknownGlyph]) topSideBearing.

	contourDataIndexesByUtf8 := (IntegerArray new: 256) as: OrderedCollection.
	contourDataForUtf8 := Float32Array streamContents: [ :stream |
		theUnknownGlyph addGlyphDataTo: stream tx: nil.		"First data is for unknownGlyph"	
		self fullName, ' - Processing glyph data' 
			displayProgressAt: 100@100
			from: 1 
			to: glyphsByUTF8 size 
			during: [ :bar |
				self appendGlyphDataTo: stream glyphsArray: glyphsByUTF8 indexes: contourDataIndexesByUtf8
					base: 0 bar: bar unknown: theUnknownGlyph scaleArrowGlyphs: arrowGlyphs ]].
	contourDataIndexesByUtf8 := contourDataIndexesByUtf8 as: IntegerArray.! !

!TTFontDescription methodsFor: 'emphasis' stamp: 'jmv 7/6/2023 11:32:36'!
emphasis
	"See senders of emphasis:"
	| emphasis |
	familyName = fullName ifTrue: [ ^ 0 ].
	emphasis := 0.
	('*Bold*' match: subfamilyName) ifTrue: [ emphasis := emphasis bitOr: 1 ].
	('*Oblique*' match: subfamilyName) ifTrue: [ emphasis := emphasis bitOr: 2 ].
	('*Italic*' match: subfamilyName) ifTrue: [ emphasis := emphasis bitOr: 2 ].
	^ emphasis.! !


!TTFontDescription class methodsFor: 'instance creation' stamp: 'jmv 7/6/2023 11:31:59'!
readTrueTypeFontsIn: aDirectoryEntry
	"
	TTFontDescription read: DirectoryEntry smalltalkImageDirectory / 'TrueTypeFonts'
	"
	| readFamilyNames |
	Descriptions ifNil: [ Descriptions := Dictionary new ].
	readFamilyNames := Set new.
	aDirectoryEntry
		allFilesDo: [ :file | | ttf |
			ttf := TTFontReader readTTFFrom: file binaryContents.
			ttf folderName: file parent name.
			(Descriptions at: ttf familyName ifAbsentPut: [ Dictionary new]) at: ttf emphasis put: ttf.
			readFamilyNames add: ttf familyName ]
		matches: [ :file | '*.ttf' match: file name ].
	aDirectoryEntry
		allFilesDo: [ :file | | ttc |
			ttc := TTFontReader readTTCFrom: file binaryContents.
			ttc do: [ :eachTtf |
				eachTtf folderName: file parent name.
				(Descriptions at: eachTtf familyName ifAbsentPut: [ Dictionary new]) at: eachTtf emphasis put: eachTtf.
				readFamilyNames add: eachTtf familyName ]]
		matches: [ :file | '*.ttc' match: file name ].
	^readFamilyNames.! !


!TTFontReader methodsFor: 'processing' stamp: 'jmv 7/6/2023 11:33:23'!
readFrom: fontData fromOffset: offset

	| headerEntry maxProfileEntry nameEntry indexLocEntry charMapEntry glyphEntry horzHeaderEntry horzMetricsEntry kerningEntry glyphOffset characterMap numHMetrics indexToLocFormat |

	"Search the tables required to build the font"
	(headerEntry := self getTableDirEntry: 'head' from: fontData offset: offset) == nil ifTrue: [
		^self error:'This font does not have a header table'].
	(maxProfileEntry := self getTableDirEntry: 'maxp' from: fontData offset: offset) == nil ifTrue: [
		^self error:'This font does not have a maximum profile table'].
	(nameEntry := self getTableDirEntry: 'name' from: fontData offset: offset) == nil ifTrue: [
		^self error:'This font does not have a name table'].
	(indexLocEntry := self getTableDirEntry: 'loca' from: fontData offset: offset) == nil ifTrue: [
		^self error:'This font does not have a relocation table'].
	(charMapEntry := self getTableDirEntry: 'cmap' from: fontData offset: offset) == nil ifTrue: [
		^self error:'This font does not have a character map table'].
	(glyphEntry := self getTableDirEntry: 'glyf' from: fontData offset: offset) == nil ifTrue: [
		^self error:'This font does not have a glyph table'].
	(horzHeaderEntry := self getTableDirEntry: 'hhea' from: fontData offset: offset) == nil ifTrue: [
		^self error:'This font does not have a horizontal header table'].
	(horzMetricsEntry := self getTableDirEntry: 'hmtx' from: fontData offset: offset) == nil ifTrue: [
		^self error:'This font does not have a horizontal metrics table'].
	(kerningEntry := self getTableDirEntry: 'kern' from: fontData offset: offset) == nil ifTrue: [
		"Transcript cr; show:'This font does not have a kerning table';endEntry" ].


	"Process the data"
	indexToLocFormat := self processFontHeaderTable: headerEntry.
	self processMaximumProfileTable: maxProfileEntry.
	self processNamingTable: nameEntry.
	glyphOffset := self processIndexToLocationTable: indexLocEntry format: indexToLocFormat.
	characterMap := self processCharacterMappingTable: charMapEntry.
	characterMap isNil
		ifTrue: [^self error:'This font has no Unicode character mappings'].
	self processGlyphDataTable: glyphEntry offsets: glyphOffset.
	numHMetrics := self processHorizontalHeaderTable: horzHeaderEntry.
	self processHorizontalMetricsTable: horzMetricsEntry length: numHMetrics.
	kerningEntry isNil 
		ifTrue: [kernPairs := #()]
		ifFalse: [self processKerningTable: kerningEntry].
	self mapAndStoreGlyphsUsing: characterMap.
	"jmv kernPairs refer to glyph number (in the .ttf file) while fontDescription glyphs are indexed by Unicode code point...
	Correct this before making any use of the kernPairs data."
	false ifTrue: [
		fontDescription setKernPairs: kernPairs ].! !

"VectorCanvas initialize"!
