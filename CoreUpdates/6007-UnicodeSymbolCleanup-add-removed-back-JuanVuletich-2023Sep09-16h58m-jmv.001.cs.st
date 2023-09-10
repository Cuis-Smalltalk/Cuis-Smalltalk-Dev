'From Cuis 6.0 [latest update: #6003] on 9 September 2023 at 4:59:16 pm'!

!Object methodsFor: 'testing' stamp: 'jmv 12/7/2022 09:16:18'!
isUnicodeString
	"Overridden to return true in UnicodeString, natch"
	^ false! !


!TextModel methodsFor: 'testing' stamp: 'SSC 11/12/2022 17:59:53'!
isDirty

	^undoRedoCommands position isZero not.! !


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


!ClassDescription methodsFor: 'accessing method dictionary' stamp: 'jmv 1/3/2023 15:06:34'!
selectorsInCategory: aSymbol 
	"Answer a list of the method selectors of the receiver that are in category named aName"
	
	| aColl |
	aColl _ Set withAll: (aSymbol == ClassOrganizer allCategory
			ifTrue: [self organization allMethodSelectors]
			ifFalse: [self organization listAtCategoryNamed: aSymbol]).
	^ aColl asArray sort! !

!ClassDescription methodsFor: 'source code management' stamp: 'jmv 1/2/2023 17:12:53'!
messageSendsCount
"
Object messageSendsCount
"
	"Number of message sends in code
	If asked to a class, also include its metaclass (i.e. the class side).
	If asked to a metaclass (the class side), don't include the class (the instance side)."

	| lines |
	lines _ 0.
	self selectorsDo: [ :sel |
		lines _ lines + (self compiledMethodAt: sel) messageSendsCount ].
	^self isMeta
		ifTrue: [ lines]
		ifFalse: [ lines + self class messageSendsCount].
"
(SystemOrganization categories select: [:c | 'Kernel*' match: c]) sum: [:c |
	(SystemOrganization superclassOrderIn: c) sum: [:cl | cl messageSendsCount]]
"
"
Smalltalk allClasses sum: [:cl | cl messageSendsCount]
"! !

!ClassDescription methodsFor: 'source code management' stamp: 'jmv 1/3/2023 15:11:56'!
messageSendsCountInCategory: aCategory
	"Number of message sends in code for methods in a method category.
	Object messageSendsCountInCategory: 'accessing'
	"

	| lines |
	lines _ 0.
	(self selectorsInCategory: aCategory) do: [ :sel |
		lines _ lines + (self compiledMethodAt: sel) messageSendsCount ].
	^lines! !

!ClassDescription methodsFor: 'source code management' stamp: 'jmv 1/3/2023 15:13:45'!
messagesCountInCategory: aCategory
	"Number of methods in a method category.
	Object messagesCountInCategory: 'accessing'
	"

	| lines |
	lines _ 0.
	(self selectorsInCategory: aCategory) do: [ :sel |
		lines _ lines + 1 ].
	^lines! !


!SystemOrganizer methodsFor: 'source code management' stamp: 'jmv 1/2/2023 17:10:46'!
messageSendsCountOf: category
"
SystemOrganization messageSendsCountOf: #'System-Files'

See also #linesOfCodeOf:
"
	"An approximate measure of lines of.
	Includes comments, but excludes blank lines."
	^ (self allClassesAt: category) sum: [ :cl | cl messageSendsCount ] ifEmpty: 0.! !


!Character methodsFor: 'testing' stamp: 'jmv 11/28/2022 10:12:17'!
isDiacriticalMark
	"No diacritical marks are instances of Character.
	See other implementors."

	^ false! !

!Character methodsFor: 'testing' stamp: 'jmv 12/1/2022 11:53:24'!
isNullCharacter
	"Answer whether the receiver is the NULL character."

	^ self numericValue = 0! !

!Character methodsFor: 'testing' stamp: 'jmv 11/28/2022 10:32:22'!
isSymbolLike
	^self asUnicodeCodePoint isSymbolLike! !


!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 11/28/2022 10:19:15'!
isDiacriticalMark
	"
	(UnicodeCodePoint codePoint: 16r0041) isDiacriticalMark
	(UnicodeCodePoint codePoint: 16r20D7) isDiacriticalMark
	"
	| i |
	i _ value + 1.
	DiacriticalMarksTable bitSize < i ifTrue: [ ^false ].
	^ DiacriticalMarksTable bitBooleanAt: i.! !

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 12/1/2022 11:53:35'!
isNullCharacter
	"Answer whether the receiver is the NULL character."

	^ value = 0! !

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 11/28/2022 10:30:07'!
isSymbolLike
	"
	(UnicodeCodePoint codePoint: 16r0041) isSymbolLike
	(UnicodeCodePoint codePoint: 16r002B) isSymbolLike
	(UnicodeCodePoint codePoint: 16r03F6) isSymbolLike
	(UnicodeCodePoint codePoint: 16r2140) isSymbolLike
	"
	| i |
	i _ value + 1.
	SymbolsLikeTable bitSize < i ifTrue: [ ^false ].
	^ SymbolsLikeTable bitBooleanAt: i.! !


!UnicodeCodePoint class methodsFor: 'constants' stamp: 'len 12/1/2022 12:23:44'!
namedCharactersMap
	"Dictionary of named characters that we can enter in an editor using \name."

	^ NamedCharactersMap! !

!UnicodeCodePoint class methodsFor: 'services' stamp: 'jmv 12/11/2022 19:05:09'!
canonicalCompositionOf: numericCodePoint and: combiningCodePoint

	| dict |
	dict _ CompositionMapping at: numericCodePoint
		ifAbsent: [^nil ].
	^dict at: combiningCodePoint ifAbsent: nil! !

!UnicodeCodePoint class methodsFor: 'services' stamp: 'jmv 12/11/2022 19:06:32'!
canonicalDecomposition: numericCodePoint on: aStream

	| decomposition |
	decomposition _ DecompositionMapping at: numericCodePoint
		ifAbsent: [^aStream nextPutCodePoint: numericCodePoint].
	decomposition do: [ :each |
		self canonicalDecomposition: each on: aStream ]! !

!UnicodeCodePoint class methodsFor: 'services' stamp: 'jmv 12/11/2022 19:14:16'!
combiningClass: numericCodePoint
	(self isDiacriticalMark: numericCodePoint) ifFalse: [ ^0 ].
	^CanonicalCombiningClasses at: numericCodePoint ifAbsent: [ 	0 ]! !

!UnicodeCodePoint class methodsFor: 'services' stamp: 'jmv 12/2/2022 14:13:46'!
isDiacriticalMark: numericCodePoint
	"
	(UnicodeCodePoint isDiacriticalMark: 16r0041) 
	(UnicodeCodePoint codePoint: 16r0041) isDiacriticalMark
	(UnicodeCodePoint isDiacriticalMark: 16r20D7)
	(UnicodeCodePoint codePoint: 16r20D7) isDiacriticalMark
	"
	| i |
	i _ numericCodePoint + 1.
	DiacriticalMarksTable bitSize < i ifTrue: [ ^false ].
	^ DiacriticalMarksTable bitBooleanAt: i.! !


!SequenceableCollection methodsFor: 'enumerating' stamp: 'jmv 1/13/2023 14:27:48'!
findFirst: aBlock startingAt: firstIndex
	"Return the index of my first element for which aBlock evaluates as true.
	Start at firstIndex"

	| s index |
	s _ self size.
	index _ firstIndex.
	[
		index <= s ] whileTrue: [
			(aBlock value: (self at: index)) ifTrue: [^index ].
			index _ index + 1.
			].
	^ 0! !


!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:13:10'!
asUnicodeString
	"Answer an instance of the UTF-8 encoded String class"

	| utf8Bytes |
	utf8Bytes _ self asUtf8Bytes.
	^UnicodeString fromUtf8Bytes: utf8Bytes codePointCount: self size.! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:15:21'!
asUnicodeStringOrText
	"Answer is either an UnicodeString or a Text including an UnicodeString"

	^self asUnicodeString! !

!CharacterSequence methodsFor: 'Unicode normalization' stamp: 'jmv 12/11/2022 19:05:00'!
canonicalComposition
	^self! !

!CharacterSequence methodsFor: 'Unicode normalization' stamp: 'jmv 12/11/2022 19:05:14'!
canonicalDecomposition
	^self! !


!String methodsFor: 'testing' stamp: 'jmv 12/11/2022 19:05:30'!
isInCanonicalCompositionForm

	^true! !


!String class methodsFor: 'instance creation' stamp: 'jmv 10/26/2022 15:43:16'!
fromUtf8Bytes: aByteArray trimLastNull: doTrimLastNullChar
	"Convert the given string from UTF-8 to the internal encoding: ISO Latin 9 (ISO 8859-15).
	Discard any code points that can not be represented."

	^String streamContents: [ :strm | | s byteIndex n |
		s _ aByteArray size.
		byteIndex _ 1.
		[  byteIndex <= s ] whileTrue: [
			n _ Character utf8BytesAndCodePointAt: byteIndex in: aByteArray
				into: nil
				into: [ :codePoint |
					(Character codePoint: codePoint) ifNotNil: [ :char | strm nextPut: char ]].
			byteIndex _ byteIndex + n
		].
		(doTrimLastNullChar and: [ aByteArray last = 0 ]) ifTrue: [
			strm position: strm position - 1 ]].! !


!UnicodeString methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:14:46'!
asUnicodeString
	"Convert me to an UnicodeString"
	
	^ self! !

!UnicodeString methodsFor: 'testing' stamp: 'jmv 12/11/2022 19:13:20'!
isInCanonicalCompositionForm
	"Answer if we are in NFC"

	self isAscii ifTrue: [ ^true ].

	"Lazy initialization."
	hash isNil ifTrue: [
		self findCanonicalCompositionAndHash ].

	"See findCanonicalCompositionAndHash"
	^hash isNumber! !

!UnicodeString methodsFor: 'testing' stamp: 'jmv 12/7/2022 09:16:22'!
isUnicodeString

	^true! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 12/11/2022 19:11:42'!
basicCanonicalComposition
	"
	Usually prefer #canonicalComposition or #canonicalDecomposition
	https://unicode.org/reports/tr15/#Norm_Forms

	See examples at #canonicalComposition.
	(UnicodeString fromCodePoints: #(119128 119141)) basicCanonicalComposition asCodePoints
	(UnicodeString fromCodePoints: #(119135 119151)) basicCanonicalComposition asCodePoints
	(UnicodeString fromCodePoints: #(119128 119141 119151)) basicCanonicalComposition asCodePoints
	"
	| byteIndex stopByteIndex n combining cls |
	byteIndex _ 1.
	stopByteIndex _ bytes size.
	combining _ OrderedCollection new.
	^UnicodeString streamContents: [ :strm |
		[ byteIndex <= stopByteIndex ] whileTrue: [
			n _ Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
				cls _ UnicodeCodePoint combiningClass: codePoint.
				cls = 0 ifTrue: [
					self basicCompose: combining on: strm.
					combining _ OrderedCollection new ].
				combining add: codePoint ].
			byteIndex _ byteIndex + n ].
		self basicCompose: combining on: strm ].! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 12/11/2022 19:11:54'!
basicCanonicalDecomposition
	"
	Usually prefer #canonicalComposition or #canonicalDecomposition
	https://unicode.org/reports/tr15/#Norm_Forms

	See examples at #canonicalDecomposition.
	(UnicodeCodePoint codePoint: 119135) asString basicCanonicalDecomposition asCodePoints
	(UnicodeCodePoint codePoint: 119137) asString basicCanonicalDecomposition asCodePoints
	"
	| byteIndex stopByteIndex n |
	byteIndex _ 1.
	stopByteIndex _ bytes size.
	^UnicodeString streamContents: [ :strm |
		[ byteIndex <= stopByteIndex ] whileTrue: [
			n _ Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
				UnicodeCodePoint canonicalDecomposition: codePoint on: strm ].
			byteIndex _ byteIndex + n ]].! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 12/11/2022 19:12:10'!
basicCanonicallyOrdered
	"
	Usually prefer #canonicalComposition or #canonicalDecomposition
	https://unicode.org/reports/tr15/#Norm_Forms

	(UnicodeString fromCodePoints: #(16r61  16r0300 16r0315 16r031B)) basicCanonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromCodePoints: #(16r61  16r0300 16r031B 16r0315)) basicCanonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromCodePoints: #(16r61  16r0315 16r031B 16r0300)) basicCanonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromCodePoints: #(16r61  16r0315 16r0300 16r031B)) basicCanonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromCodePoints: #(16r61  16r031B 16r0300 16r0315)) basicCanonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromCodePoints: #(16r61  16r031B 16r0315 16r0300)) basicCanonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	"
	| byteIndex stopByteIndex n combining cls element |
	byteIndex _ 1.
	stopByteIndex _ bytes size.
	combining _ OrderedCollection new.
	^UnicodeString streamContents: [ :strm |
		[ byteIndex <= stopByteIndex ] whileTrue: [
			n _ Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
				cls _ UnicodeCodePoint combiningClass: codePoint.
				cls = 0
					ifTrue: [
						combining notEmpty ifTrue: [
							combining sort: [ :a :b | a first < b first or: [ a first = b first and: [a second < b second]] ].
							combining do: [ :e | strm nextPutCodePoint: e third ]].
						strm nextPutCodePoint: codePoint.
						combining _ OrderedCollection new ]
					ifFalse: [
						element _ { cls. combining size+1. codePoint }.
						combining add: element ]].
			byteIndex _ byteIndex + n ].
		combining notEmpty ifTrue: [
			combining sort: [ :a :b | a first < b first or: [ a first = b first and: [a second < b second]] ].
			combining do: [ :e | strm nextPutCodePoint: e third ] ]].! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 12/11/2022 19:07:00'!
basicCompose: combiningCodePoints on: aStream
	| continue composition i base |
	combiningCodePoints notEmpty ifTrue: [
		continue _ combiningCodePoints size > 1.
		[continue] whileTrue: [
			continue _ false.
			base _ combiningCodePoints first.
			i _ 2.
			composition _ nil.
			[composition isNil and: [i <= combiningCodePoints size ]] whileTrue: [
				composition _ UnicodeCodePoint canonicalCompositionOf: base and: (combiningCodePoints at: i).
				composition ifNotNil: [
					combiningCodePoints at: 1 put: composition.
					combiningCodePoints removeAt: i.
					continue _ combiningCodePoints size > 1 ].
				i _ i + 1 ]].
		combiningCodePoints do: [ :e | aStream nextPutCodePoint: e ]].! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 2/23/2023 13:41:47'!
canonicalComposition
	"
	https://unicode.org/reports/tr15/#Norm_Forms
	Answer a String in the NFC form 'Normalization Form Composed'.

	The canonical composition is cached. Therefor it doesn't need to be computed every time it is needed.
	This makes it actually cheaper than #canonicalDecomposition!!

	UnicodeString fromUtf8Bytes: #[195 162].
	(UnicodeString fromUtf8Bytes: #[195 162]) asUtf8Bytes
	(UnicodeString fromUtf8Bytes: #[195 162]) canonicalDecomposition asUtf8Bytes
	(UnicodeString fromUtf8Bytes: #[195 162]) canonicalComposition asUtf8Bytes

	UnicodeString fromUtf8Bytes: #[97 204 130].
	(UnicodeString fromUtf8Bytes: #[97 204 130]) asUtf8Bytes.
	(UnicodeString fromUtf8Bytes: #[97 204 130]) canonicalDecomposition asUtf8Bytes
	(UnicodeString fromUtf8Bytes: #[97 204 130]) canonicalComposition asUtf8Bytes
	"

	"Lazy initialization."
	hash isNil ifTrue: [
		self findCanonicalCompositionAndHash ].

	^hash isNumber
		ifTrue: [
			"If 'hash' is a number it means we are already in canonicalComposition form (NFC)."
			self ]
		ifFalse: [
			"If 'hash' is not a number, it actually contains our #canonicalComposition.
			Answer a copy, because it could later be mutated."
			hash copy ].! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 12/11/2022 19:12:51'!
canonicalDecomposition
	"
	https://unicode.org/reports/tr15/#Norm_Forms
	Answer a String in the NFD form 'Normalization Form Decomposed'.

	(UnicodeCodePoint codePoint: 16r1EBF) asString
	(UnicodeCodePoint codePoint: 16r1EBF) asString asUtf8Bytes
	(UnicodeCodePoint codePoint: 16r1EBF) asString basicCanonicalDecomposition asUtf8Bytes
	(UnicodeCodePoint codePoint: 16r1EBF) asString asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeCodePoint codePoint: 16r1EBF) asString basicCanonicalDecomposition asCodePoints asArray collect: [ :e | e hex ]

	(UnicodeString fromUtf8Bytes: #[195 160 204 155 204 149]).
	(UnicodeString fromUtf8Bytes: #[195 160 204 155 204 149]) asUtf8Bytes.
	(UnicodeString fromUtf8Bytes: #[195 160 204 155 204 149]) basicCanonicalDecomposition asUtf8Bytes.
	(UnicodeString fromUtf8Bytes: #[195 160 204 155 204 149]) asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromUtf8Bytes: #[195 160 204 155 204 149]) basicCanonicalDecomposition asCodePoints asArray collect: [ :e | e hex ]

	(UnicodeCodePoint codePoint: 119137) asString basicCanonicalDecomposition asCodePoints
	(UnicodeCodePoint codePoint: 119135) asString basicCanonicalDecomposition asCodePoints
	"
	^self basicCanonicalDecomposition basicCanonicallyOrdered! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 12/11/2022 19:13:48'!
findCanonicalCompositionAndHash
	"Find our canonical composition (NFC). If we are found to be in NFC, store our hash.
	Otherwise, store our NFC. Queries of #hash will be forwarded to it."

	| canonicalComposition |
	"Ascii is already normalized to NFC."
	self isAscii ifTrue: [
		^self computeHash ].

	"Find our canonical composition"
	canonicalComposition _ self basicCanonicalDecomposition basicCanonicallyOrdered basicCanonicalComposition.
	(String is: bytes equalTo: canonicalComposition bytes)
		ifTrue: [
			"If we were already in NFC, compute and store our hash"
			self computeHash ]
		ifFalse: [
			"Otherwise, point to our NFC, and ensure it has its hash, for us to query when needed."
			hash _ canonicalComposition.
			canonicalComposition computeHash ].! !


!UnicodeString class methodsFor: 'services' stamp: 'jmv 12/6/2022 17:36:43'!
codePointsFromUtf8: aByteArray
	"Convert the given string from UTF-8 to UTF-32"

	^WordArray streamContents: [ :strm | | s byteIndex n |
		s _ aByteArray size.
		byteIndex _ 1.
		[  byteIndex <= s ] whileTrue: [
			n _ Character utf8BytesAndCodePointAt: byteIndex in: aByteArray
				into: nil
				into: [ :codePoint |
					strm nextPut: codePoint ].
			byteIndex _ byteIndex + n
		]].! !

!UnicodeString class methodsFor: 'services' stamp: 'jmv 12/6/2022 17:28:26'!
utf8FromCodePoints: aWordArray
	"Convert the given string from UTF-32 to UTF-8"

	^ByteArray streamContents: [ :strm |
		aWordArray do: [ :codePoint |
			Character
				evaluate: [ :byte | strm nextPut: byte ]
				withUtf8BytesOfUnicodeCodePoint: codePoint ]]! !


!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 1/2/2023 17:14:55'!
messageSendsCount
	"Number of message sends in code.
	A measure of the complexity of code. Alternative to #linesOfCode"

	| count |
	count _ 0.
	self decompile nodesDo: [ :node | node isMessageNode ifTrue: [count _ count + 1] ].
	^count! !


!Text methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:17:55'!
asUnicodeStringOrText
	"Answer is either an UnicodeString or a Text including an UnicodeString"

	string isUnicodeString ifTrue: [ ^self ].
	^Text string: string asUnicodeString runs: runs! !


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

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 7/6/2023 11:52:28'!
lastCommandLineArguments
	"To make all command line arguments after end of options '--' available to scripts applications"

	^startUpArguments copyAfter: '--'.! !

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


!Stream methodsFor: 'nonhomogeneous accessing' stamp: 'jmv 12/7/2022 12:31:35'!
nextUnicodeString
	"Read a string from the receiver. The first byte is the length of the string, unless it is greater than 192, in which case the first four bytes encode the length.  I expect to be in ascii mode when called (caller puts back to binary)."

	| bytes length |

	"read the length in binary mode"
	self binary.
	length _ self next.		"first byte."
	length >= 192 ifTrue: [length _ length - 192.
		1 to: 3 do: [:ii | length _ length * 256 + self next]].
	bytes _ ByteArray new: length.
	self nextInto: bytes.
	^UnicodeString fromUtf8Bytes: bytes! !

!Stream methodsFor: 'nonhomogeneous accessing' stamp: 'jmv 1/6/2023 17:31:25'!
nextUnicodeStringPut: s
	"Append the string, s, to the receiver.  Only used by DataStream.  Max size of 64*256*256*256."

	| bytes length |
	bytes _ s bytes.
	(length _ bytes size) < 192
		ifTrue: [self nextPut: length]
		ifFalse: 
			[self nextPut: (length digitAt: 4)+192.
			self nextPut: (length digitAt: 3).
			self nextPut: (length digitAt: 2).
			self nextPut: (length digitAt: 1)].
	self nextPutAll: bytes.
	^s! !


!UniFileStream methodsFor: 'reading modes' stamp: 'jmv 12/7/2022 09:44:14'!
useUnicodeString
	"Only for reading.
	For writing accept bytes, Characters, CodePoints or ByteArray, String, UnicodeString,
	according to protocol."
	readAdaptor _ UnicodeReadAdaptor new stream: self.! !


!DataStream methodsFor: 'write and read' stamp: 'jmv 1/6/2023 17:22:04'!
readUnicodeString

	| str |
	"byteStream ascii."
	str _ byteStream nextUnicodeString.
	byteStream binary.
	^ str
! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 12/7/2022 12:19:17'!
readUnicodeSymbol
    "PRIVATE -- Read the contents of a Symbol."

    ^ self readUnicodeString asSymbol! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 12/7/2022 12:29:57'!
writeUnicodeString: aString
	"PRIVATE -- Write the contents of a String."

	byteStream nextUnicodeStringPut: aString.! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 12/7/2022 12:17:38'!
writeUnicodeSymbol: aSymbol
    "PRIVATE -- Write the contents of a Symbol."

    self writeString: aSymbol! !


!Scanner class methodsFor: 'testing' stamp: 'jmv 4/3/2023 12:36:08'!
typeTable: aTypeTable at: aCharacterOrCodePoint

	"Prebuilt tables for the ASCII range"
	aCharacterOrCodePoint asciiValue ifNotNil: [ :ascii |
		^aTypeTable at: ascii ].

	"For CodePoints not in the ASCII range"
	aCharacterOrCodePoint isDiacriticalMark ifTrue: [ ^#xDiacritical ].
	aCharacterOrCodePoint isLetter ifTrue: [ ^#xLetter ].
	aCharacterOrCodePoint isSymbolLike ifTrue: [ ^#xBinary ].
	aCharacterOrCodePoint = $­ ifTrue: [ ^#xBinary ].
	aCharacterOrCodePoint isSeparator ifTrue: [ ^#xDelimiter ].
	"Many should actually be #xIllegal"
	^#xLetter! !


!CodePackage methodsFor: 'source code management' stamp: 'jmv 1/2/2023 17:15:17'!
messageSendsCount
	"See comment at CompiledMethod >> #messageSendsCount"

	^self methods inject: 0 into: [ :sum :each |
		sum + each compiledMethod messageSendsCount ].! !


!Clipboard methodsFor: 'private' stamp: 'jmv 12/7/2022 09:05:55'!
retrieveIdOrStringFromOS
	"Use a specific content type if ExtendedClipboard is active.
	Otherwise, use regular clipboard primitives"
	| utf8Bytes |
	self extendedClipboardInterface ifNotNil: [ :interface |
		interface canStore ifTrue: [
			"Answer nil if no id was stored"
			^ interface retrieveId ]].

	utf8Bytes _ self primitiveClipboardString.

	"Clipboard primitives answer an empty string if there is no string in OS clipboard."
	utf8Bytes isEmpty ifTrue: [ ^nil ].

	"Abuse the fact that the VM provides an instance of String (although it contains UTF-8 bytes)"
	utf8Bytes _ utf8Bytes fixLineEndings.

	"Clipboard primitive answers an instance of String containing the UTF-8 bytes.
	If not pure ASCII, contents are invalid!! Convert it ASAP into a reasonable ByteArray."
	utf8Bytes class format = ByteArray format  												"True unless image is in 32bit V3 format"
		ifTrue: [ ByteArray adoptInstance: utf8Bytes ]
		ifFalse: [ utf8Bytes _ ByteArray withAll: utf8Bytes ].
	^ UnicodeString fromUtf8Bytes: utf8Bytes.! !


!Sample10PythagorasTree methodsFor: 'as yet unclassified' stamp: 'MM 11/3/2022 16:11:16'!
drawTreeOn: aCanvas p1: p1 p2: p2 depth: depth

	| dx dy p3 p4 p5 color |
	
	depth = depthLimit ifTrue: [ ^ self].
	
	color _ self color.
	colors ifNotNil: [
		color _ colors at: (depth mod: colors size) + 1].
	
	dx := p2 x - (p1 x).
	dy := p1 y - (p2 y).
	
	p3 := (p2 x - dy) @ (p2 y - dx).
	p4 := (p1 x - dy) @ (p1 y - dx).
	p5 := (p4 x + (angle * (dx - dy))) @ (p4 y - (angle * (dx + dy))).
	
	aCanvas fillColor: color do: [
		aCanvas moveTo: p1;
			lineTo: p2;
			lineTo: p3;
			lineTo: p4;
			lineTo: p1].
			
	"aCanvas fillColor: color do: [
		aCanvas moveTo:  p3;
			lineTo: p4;
			lineTo: p5;
			lineTo: p3]."
			
	self drawTreeOn: aCanvas p1: p4 p2: p5 depth: depth + 1.
	self drawTreeOn: aCanvas p1: p5 p2: p3 depth: depth + 1.! !

!Sample10PythagorasTree methodsFor: 'as yet unclassified' stamp: 'MM 11/3/2022 16:10:25'!
withMultipleColors
	colors _ (0 to: 360 count: 15) collect: [ :k | Color hue: k saturation: 0.8 brightness: 0.8]! !


!VectorCanvas methodsFor: 'accessing' stamp: 'jmv 12/19/2022 11:36:46'!
attachedCanvas: aCanvas
	^attachedCanvas _ aCanvas! !


!HybridCanvas methodsFor: 'private' stamp: 'jmv 12/19/2022 11:40:59'!
currentMorph: aMorph drawingMorphStack: morphs currentTransformation: tx transformations: txs cti: i
	"For syncing to our VectorCanvas"
	currentMorph _ aMorph.
	drawingMorphStack _ morphs.
	currentTransformation _ tx.
	transformations _ txs.
	cti _ i.! !


!Theme class methodsFor: 'user interface' stamp: 'jmv 12/29/2022 13:03:07'!
loadAdditionalThemes
	"Load additional themes and reopen menu."
	Feature require: #'Theme-Themes'.
	self changeTheme! !


!FormGlyph methodsFor: 'accessing' stamp: 'jmv 12/5/2022 19:47:24'!
deltaXToCenter
	^deltaXToCenter! !

!FormGlyph methodsFor: 'accessing' stamp: 'jmv 12/5/2022 19:44:16'!
deltaXToCenter: aNumber
	deltaXToCenter _ aNumber! !


!TTContourConstruction methodsFor: 'enumerating' stamp: 'jmv 11/24/2022 11:55:35'!
initialMoveToAndPathCommands: anAffineTransformationOrNil
	"TrueType reasonably assumes Y coordinates increase upwards."

	| initialMoveToCommand pathCommand firstPoint prevEndPoint endPoints controlPoints cp ep |
	endPoints _ OrderedCollection new.
	controlPoints _ OrderedCollection new.
	self
		startPointDo: [ :firstBezierStart |
			firstPoint _ firstBezierStart.
			anAffineTransformationOrNil notNil ifTrue: [
				firstPoint _ anAffineTransformationOrNil transform: firstPoint ].
			prevEndPoint _ firstPoint ]
		quadraticBeziersDo: [ :controlPoint :endPoint |
			cp _ controlPoint.
			ep _ endPoint.
			anAffineTransformationOrNil notNil ifTrue: [
				cp _ anAffineTransformationOrNil transform: cp.
				ep _ anAffineTransformationOrNil transform: ep ].
			controlPoints add: cp - prevEndPoint.
			endPoints add: ep - prevEndPoint.
			prevEndPoint _ ep ].
	initialMoveToCommand _ PathMoveToCommand new toRelativePoint: firstPoint.
	pathCommand _ PathQuadraticCurveToCommand new toRelativePoints: endPoints controlPoints: controlPoints.
	^{initialMoveToCommand. pathCommand}! !

!TTContourConstruction methodsFor: 'enumerating' stamp: 'jmv 11/24/2022 12:33:31'!
startPointDo: initialMoveBlock quadraticBeziersDo: bezierBlock
	"Iterate over Bezier curves of the receiver.
	initialMoveBlock has a single argument: the startPoint of the first bezier
	bezierBlock has two arguments: controlPoint and endPoint.
	First StartPoint may be points first, or (if this is a control point), the mean between first and last.
		(this assumes that last point is also a control point, and a tacit first/end point was ommited. TTF can do this).
	Other StartPoints are assumed to be previous endPoint.
	Contour parts may either be straight line segments or quadratic bezier curves.
	The decision is made upon the type flags in controlPointFlags as follows:
	a) Two subsequent #isEndPoint points define a straight segment
	b) An #isEndPoint point followed by an #isControlPoint point followed 
		by an #isEndPoint point defines a quadratic bezier segment
	c)Two subsequent #isControlPoint points have an implicitely defined 
		#isEndPoint point at half the distance between them."

	| pointsSize firstPointIsControlPoint lastPointIsControlPoint firstBezierStart i nextI lastBezierEnd thisPoint thisPointIsControlPoint nextPoint nextPointIsControlPoint thisBezierControlPoint thisBezierEnd |
	pointsSize _ points size.
	firstPointIsControlPoint _ controlPointFlags bitBooleanAt: 1.
	firstPointIsControlPoint
		ifFalse: [
			firstBezierStart _ points first.
			i _ 2 ]
		ifTrue: [
			lastPointIsControlPoint _ controlPointFlags bitBooleanAt: pointsSize.
			firstBezierStart _ lastPointIsControlPoint
				ifTrue: [points first + points last / 2.0]
				ifFalse: [points last].
			i _ 1 ].
	initialMoveBlock value: firstBezierStart.
	lastBezierEnd _ firstBezierStart.
	[ i <= pointsSize ] whileTrue: [
		thisPoint _ points at: i.
		thisPointIsControlPoint _ controlPointFlags bitBooleanAt: i.
		thisPointIsControlPoint
			ifTrue: [
				thisBezierControlPoint _ thisPoint.
				nextI _ i = pointsSize ifFalse: [ i+1 ] ifTrue: [ 1 ].
				nextPoint _ points at: nextI.
				nextPointIsControlPoint _ controlPointFlags bitBooleanAt: nextI.
				nextPointIsControlPoint
					ifFalse: [ "Quadratic Bezier with explicit control and end points."
						thisBezierEnd _ nextPoint.
						i _ i + 2 ]
					ifTrue: [ "Quadratic Bezier with implicit end point."
						thisBezierEnd _ thisPoint + nextPoint / 2.0.
						i _ i + 1 ].
				]
			ifFalse: ["Line segment requested. Consider it as a bezier with an appropriate control point."
				thisBezierControlPoint _ thisPoint + lastBezierEnd / 2.0.
				thisBezierEnd _ thisPoint.
				i _ i + 1 ].
		"Avoid null beziers. This, besides being being useless, is dangerous.
		Rasterization code uses curve length for computing hop, and this case would result in a division by zero."
		thisBezierEnd = lastBezierEnd ifFalse: [
			bezierBlock value: 	thisBezierControlPoint value: 	thisBezierEnd.
			lastBezierEnd _ thisBezierEnd ].
		].
	"Close with a line if needed. TrueType contours must be closed shapes, because of fill."
	lastBezierEnd = firstBezierStart ifFalse: [
		bezierBlock value: lastBezierEnd + firstBezierStart / 2.0 value:  firstBezierStart ].! !


!TTFontDescription methodsFor: 'private-initialization' stamp: 'jmv 12/14/2022 13:21:19'!
appendGlyphDataTo: stream glyphsArray: glyphsArray indexes: indexes base: baseIndex	 bar: aProgressBarOrNil unknown: theUnknownGlyph scaleArrowGlyphs: arrowGlyphs

		glyphsArray withIndexDo: [ :glyphOrArrayOrNumberNil :index |
			aProgressBarOrNil ifNotNil: [ :bar | bar value: index ].
			glyphOrArrayOrNumberNil ifNotNil: [
				glyphOrArrayOrNumberNil == theUnknownGlyph
					ifTrue: [ indexes at: baseIndex + index put: 1 ]
					ifFalse: [
						glyphOrArrayOrNumberNil isArray
							ifTrue: [
								| nextBaseIndex |
								nextBaseIndex := indexes size - 128.
								indexes addAll: (IntegerArray new: 64).
								indexes at: baseIndex + index put: nextBaseIndex negated.
								self appendGlyphDataTo: stream glyphsArray: glyphOrArrayOrNumberNil indexes: indexes
									base: nextBaseIndex bar: nil unknown: theUnknownGlyph scaleArrowGlyphs: arrowGlyphs ]
							ifFalse: [
								glyphOrArrayOrNumberNil isNumber
									ifTrue: [ | glyph tx |
										glyph _ arrowGlyphs at: glyphOrArrayOrNumberNil.
										tx _ AffineTransformation withScale: letterMTopSideBearing asFloat / arrowGlyphs second topSideBearing.
										indexes at: baseIndex + index put: stream position+1.
										glyph addGlyphDataTo: stream tx: tx
										]
									ifFalse: [
										indexes at: baseIndex + index put: stream position+1.
										glyphOrArrayOrNumberNil addGlyphDataTo: stream tx: nil ]]]]].! !

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

