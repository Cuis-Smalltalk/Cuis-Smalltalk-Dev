'From Cuis 6.0 [latest update: #6003] on 9 September 2023 at 4:55:43 pm'!

!Object methodsFor: 'testing' stamp: 'jmv 9/9/2023 16:44:43'!
xisUnicodeString
	"Overridden to return true in UnicodeString, natch"
	^ false! !


!TextModel methodsFor: 'testing' stamp: 'jmv 9/9/2023 16:49:44'!
xisDirty

	^undoRedoCommands position isZero not.! !


!CodeProvider methodsFor: 'annotation' stamp: 'jmv 9/9/2023 16:34:02'!
xannotationForMessageCategory: aCategory ofClass: aClass
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
						print: (aClass xmessagesCountInCategory: aCategory);
						nextPutAll: ' total messages' ].
				[#messageSendsCount] -> [
					strm
						print: (aClass xmessageSendsCountInCategory: aCategory);
						nextPutAll: ' total message sends' ].
			}]].! !


!ClassDescription methodsFor: 'accessing method dictionary' stamp: 'jmv 9/9/2023 16:36:49'!
xselectorsInCategory: aSymbol 
	"Answer a list of the method selectors of the receiver that are in category named aName"
	
	| aColl |
	aColl := Set withAll: (aSymbol == ClassOrganizer allCategory
			ifTrue: [self organization allMethodSelectors]
			ifFalse: [self organization listAtCategoryNamed: aSymbol]).
	^ aColl asArray sort! !

!ClassDescription methodsFor: 'source code management' stamp: 'jmv 9/9/2023 16:35:54'!
xmessageSendsCount
"
Object messageSendsCount
"
	"Number of message sends in code
	If asked to a class, also include its metaclass (i.e. the class side).
	If asked to a metaclass (the class side), don't include the class (the instance side)."

	| lines |
	lines := 0.
	self selectorsDo: [ :sel |
		lines := lines + (self compiledMethodAt: sel) xmessageSendsCount ].
	^self isMeta
		ifTrue: [ lines]
		ifFalse: [ lines + self class xmessageSendsCount].
"
(SystemOrganization categories select: [:c | 'Kernel*' match: c]) sum: [:c |
	(SystemOrganization superclassOrderIn: c) sum: [:cl | cl messageSendsCount]]
"
"
Smalltalk allClasses sum: [:cl | cl messageSendsCount]
"! !

!ClassDescription methodsFor: 'source code management' stamp: 'jmv 9/9/2023 16:36:54'!
xmessageSendsCountInCategory: aCategory
	"Number of message sends in code for methods in a method category.
	Object messageSendsCountInCategory: 'accessing'
	"

	| lines |
	lines := 0.
	(self xselectorsInCategory: aCategory) do: [ :sel |
		lines := lines + (self compiledMethodAt: sel) xmessageSendsCount ].
	^lines! !

!ClassDescription methodsFor: 'source code management' stamp: 'jmv 9/9/2023 16:36:58'!
xmessagesCountInCategory: aCategory
	"Number of methods in a method category.
	Object messagesCountInCategory: 'accessing'
	"

	| lines |
	lines := 0.
	(self xselectorsInCategory: aCategory) do: [ :sel |
		lines := lines + 1 ].
	^lines! !


!SystemOrganizer methodsFor: 'source code management' stamp: 'jmv 9/9/2023 16:36:02'!
xmessageSendsCountOf: category
"
SystemOrganization messageSendsCountOf: #'System-Files'

See also #linesOfCodeOf:
"
	"An approximate measure of lines of.
	Includes comments, but excludes blank lines."
	^ (self allClassesAt: category) sum: [ :cl | cl xmessageSendsCount ] ifEmpty: 0.! !


!Character methodsFor: 'testing' stamp: 'jmv 9/9/2023 16:48:27'!
xisDiacriticalMark
	"No diacritical marks are instances of Character.
	See other implementors."

	^ false! !

!Character methodsFor: 'testing' stamp: 'jmv 9/9/2023 16:47:19'!
xisNullCharacter
	"Answer whether the receiver is the NULL character."

	^ self numericValue = 0! !

!Character methodsFor: 'testing' stamp: 'jmv 9/9/2023 16:48:16'!
xisSymbolLike
	^self asUnicodeCodePoint xisSymbolLike! !


!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 9/9/2023 16:48:29'!
xisDiacriticalMark
	"
	(UnicodeCodePoint codePoint: 16r0041) isDiacriticalMark
	(UnicodeCodePoint codePoint: 16r20D7) isDiacriticalMark
	"
	| i |
	i := value + 1.
	DiacriticalMarksTable bitSize < i ifTrue: [ ^false ].
	^ DiacriticalMarksTable bitBooleanAt: i.! !

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 9/9/2023 16:47:21'!
xisNullCharacter
	"Answer whether the receiver is the NULL character."

	^ value = 0! !

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 9/9/2023 16:47:59'!
xisSymbolLike
	"
	(UnicodeCodePoint codePoint: 16r0041) isSymbolLike
	(UnicodeCodePoint codePoint: 16r002B) isSymbolLike
	(UnicodeCodePoint codePoint: 16r03F6) isSymbolLike
	(UnicodeCodePoint codePoint: 16r2140) isSymbolLike
	"
	| i |
	i := value + 1.
	SymbolsLikeTable bitSize < i ifTrue: [ ^false ].
	^ SymbolsLikeTable bitBooleanAt: i.! !


!UnicodeCodePoint class methodsFor: 'constants' stamp: 'jmv 9/9/2023 16:46:52'!
xnamedCharactersMap
	"Dictionary of named characters that we can enter in an editor using \name."

	^ NamedCharactersMap! !

!UnicodeCodePoint class methodsFor: 'services' stamp: 'jmv 9/9/2023 16:39:21'!
xcanonicalCompositionOf: numericCodePoint and: combiningCodePoint

	| dict |
	dict := CompositionMapping at: numericCodePoint
		ifAbsent: [^nil ].
	^dict at: combiningCodePoint ifAbsent: nil! !

!UnicodeCodePoint class methodsFor: 'services' stamp: 'jmv 9/9/2023 16:39:11'!
xcanonicalDecomposition: numericCodePoint on: aStream

	| decomposition |
	decomposition := DecompositionMapping at: numericCodePoint
		ifAbsent: [^aStream nextPutCodePoint: numericCodePoint].
	decomposition do: [ :each |
		self xcanonicalDecomposition: each on: aStream ]! !

!UnicodeCodePoint class methodsFor: 'services' stamp: 'jmv 9/9/2023 16:46:46'!
xcombiningClass: numericCodePoint
	(self xisDiacriticalMark: numericCodePoint) ifFalse: [ ^0 ].
	^CanonicalCombiningClasses at: numericCodePoint ifAbsent: [ 	0 ]! !

!UnicodeCodePoint class methodsFor: 'services' stamp: 'jmv 9/9/2023 16:46:42'!
xisDiacriticalMark: numericCodePoint
	"
	(UnicodeCodePoint isDiacriticalMark: 16r0041) 
	(UnicodeCodePoint codePoint: 16r0041) isDiacriticalMark
	(UnicodeCodePoint isDiacriticalMark: 16r20D7)
	(UnicodeCodePoint codePoint: 16r20D7) isDiacriticalMark
	"
	| i |
	i := numericCodePoint + 1.
	DiacriticalMarksTable bitSize < i ifTrue: [ ^false ].
	^ DiacriticalMarksTable bitBooleanAt: i.! !


!SequenceableCollection methodsFor: 'enumerating' stamp: 'jmv 9/9/2023 16:31:04'!
xfindFirst: aBlock startingAt: firstIndex
	"Return the index of my first element for which aBlock evaluates as true.
	Start at firstIndex"

	| s index |
	s := self size.
	index := firstIndex.
	[
		index <= s ] whileTrue: [
			(aBlock value: (self at: index)) ifTrue: [^index ].
			index := index + 1.
			].
	^ 0! !


!CharacterSequence methodsFor: 'converting' stamp: 'jmv 9/9/2023 16:42:02'!
xasUnicodeString
	"Answer an instance of the UTF-8 encoded String class"

	| utf8Bytes |
	utf8Bytes := self asUtf8Bytes.
	^UnicodeString fromUtf8Bytes: utf8Bytes codePointCount: self size.! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 9/9/2023 16:42:12'!
xasUnicodeStringOrText
	"Answer is either an UnicodeString or a Text including an UnicodeString"

	^self xasUnicodeString! !

!CharacterSequence methodsFor: 'Unicode normalization' stamp: 'jmv 9/9/2023 16:43:54'!
xcanonicalComposition
	^self! !

!CharacterSequence methodsFor: 'Unicode normalization' stamp: 'jmv 9/9/2023 16:43:41'!
xcanonicalDecomposition
	^self! !


!String methodsFor: 'testing' stamp: 'jmv 9/9/2023 16:44:17'!
xisInCanonicalCompositionForm

	^true! !


!String class methodsFor: 'instance creation' stamp: 'jmv 9/9/2023 16:50:06'!
xfromUtf8Bytes: aByteArray trimLastNull: doTrimLastNullChar
	"Convert the given string from UTF-8 to the internal encoding: ISO Latin 9 (ISO 8859-15).
	Discard any code points that can not be represented."

	^String streamContents: [ :strm | | s byteIndex n |
		s := aByteArray size.
		byteIndex := 1.
		[  byteIndex <= s ] whileTrue: [
			n := Character utf8BytesAndCodePointAt: byteIndex in: aByteArray
				into: nil
				into: [ :codePoint |
					(Character codePoint: codePoint) ifNotNil: [ :char | strm nextPut: char ]].
			byteIndex := byteIndex + n
		].
		(doTrimLastNullChar and: [ aByteArray last = 0 ]) ifTrue: [
			strm position: strm position - 1 ]].! !


!UnicodeString methodsFor: 'converting' stamp: 'jmv 9/9/2023 16:42:04'!
xasUnicodeString
	"Convert me to an UnicodeString"
	
	^ self! !

!UnicodeString methodsFor: 'testing' stamp: 'jmv 9/9/2023 16:44:19'!
xisInCanonicalCompositionForm
	"Answer if we are in NFC"

	self isAscii ifTrue: [ ^true ].

	"Lazy initialization."
	hash isNil ifTrue: [
		self xfindCanonicalCompositionAndHash ].

	"See findCanonicalCompositionAndHash"
	^hash isNumber! !

!UnicodeString methodsFor: 'testing' stamp: 'jmv 9/9/2023 16:44:45'!
xisUnicodeString

	^true! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 9/9/2023 16:46:34'!
xbasicCanonicalComposition
	"
	Usually prefer #canonicalComposition or #canonicalDecomposition
	https://unicode.org/reports/tr15/#Norm_Forms

	See examples at #canonicalComposition.
	(UnicodeString fromCodePoints: #(119128 119141)) basicCanonicalComposition asCodePoints
	(UnicodeString fromCodePoints: #(119135 119151)) basicCanonicalComposition asCodePoints
	(UnicodeString fromCodePoints: #(119128 119141 119151)) basicCanonicalComposition asCodePoints
	"
	| byteIndex stopByteIndex n combining cls |
	byteIndex := 1.
	stopByteIndex := bytes size.
	combining := OrderedCollection new.
	^UnicodeString streamContents: [ :strm |
		[ byteIndex <= stopByteIndex ] whileTrue: [
			n := Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
				cls := UnicodeCodePoint xcombiningClass: codePoint.
				cls = 0 ifTrue: [
					self xbasicCompose: combining on: strm.
					combining := OrderedCollection new ].
				combining add: codePoint ].
			byteIndex := byteIndex + n ].
		self xbasicCompose: combining on: strm ].! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 9/9/2023 16:41:36'!
xbasicCanonicalDecomposition
	"
	Usually prefer #canonicalComposition or #canonicalDecomposition
	https://unicode.org/reports/tr15/#Norm_Forms

	See examples at #canonicalDecomposition.
	(UnicodeCodePoint codePoint: 119135) asString basicCanonicalDecomposition asCodePoints
	(UnicodeCodePoint codePoint: 119137) asString basicCanonicalDecomposition asCodePoints
	"
	| byteIndex stopByteIndex n |
	byteIndex := 1.
	stopByteIndex := bytes size.
	^UnicodeString streamContents: [ :strm |
		[ byteIndex <= stopByteIndex ] whileTrue: [
			n := Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
				UnicodeCodePoint xcanonicalDecomposition: codePoint on: strm ].
			byteIndex := byteIndex + n ]].! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 9/9/2023 16:46:38'!
xbasicCanonicallyOrdered
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
	byteIndex := 1.
	stopByteIndex := bytes size.
	combining := OrderedCollection new.
	^UnicodeString streamContents: [ :strm |
		[ byteIndex <= stopByteIndex ] whileTrue: [
			n := Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
				cls := UnicodeCodePoint xcombiningClass: codePoint.
				cls = 0
					ifTrue: [
						combining notEmpty ifTrue: [
							combining sort: [ :a :b | a first < b first or: [ a first = b first and: [a second < b second]] ].
							combining do: [ :e | strm nextPutCodePoint: e third ]].
						strm nextPutCodePoint: codePoint.
						combining := OrderedCollection new ]
					ifFalse: [
						element := { cls. combining size+1. codePoint }.
						combining add: element ]].
			byteIndex := byteIndex + n ].
		combining notEmpty ifTrue: [
			combining sort: [ :a :b | a first < b first or: [ a first = b first and: [a second < b second]] ].
			combining do: [ :e | strm nextPutCodePoint: e third ] ]].! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 9/9/2023 16:43:12'!
xbasicCompose: combiningCodePoints on: aStream
	| continue composition i base |
	combiningCodePoints notEmpty ifTrue: [
		continue := combiningCodePoints size > 1.
		[continue] whileTrue: [
			continue := false.
			base := combiningCodePoints first.
			i := 2.
			composition := nil.
			[composition isNil and: [i <= combiningCodePoints size ]] whileTrue: [
				composition := UnicodeCodePoint xcanonicalCompositionOf: base and: (combiningCodePoints at: i).
				composition ifNotNil: [
					combiningCodePoints at: 1 put: composition.
					combiningCodePoints removeAt: i.
					continue := combiningCodePoints size > 1 ].
				i := i + 1 ]].
		combiningCodePoints do: [ :e | aStream nextPutCodePoint: e ]].! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 9/9/2023 16:43:56'!
xcanonicalComposition
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
		self xfindCanonicalCompositionAndHash ].

	^hash isNumber
		ifTrue: [
			"If 'hash' is a number it means we are already in canonicalComposition form (NFC)."
			self ]
		ifFalse: [
			"If 'hash' is not a number, it actually contains our #canonicalComposition.
			Answer a copy, because it could later be mutated."
			hash copy ].! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 9/9/2023 16:43:43'!
xcanonicalDecomposition
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
	^self xbasicCanonicalDecomposition xbasicCanonicallyOrdered! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 9/9/2023 16:43:23'!
xfindCanonicalCompositionAndHash
	"Find our canonical composition (NFC). If we are found to be in NFC, store our hash.
	Otherwise, store our NFC. Queries of #hash will be forwarded to it."

	| canonicalComposition |
	"Ascii is already normalized to NFC."
	self isAscii ifTrue: [
		^self computeHash ].

	"Find our canonical composition"
	canonicalComposition := self xbasicCanonicalDecomposition xbasicCanonicallyOrdered xbasicCanonicalComposition.
	(String is: bytes equalTo: canonicalComposition bytes)
		ifTrue: [
			"If we were already in NFC, compute and store our hash"
			self computeHash ]
		ifFalse: [
			"Otherwise, point to our NFC, and ensure it has its hash, for us to query when needed."
			hash := canonicalComposition.
			canonicalComposition computeHash ].! !


!UnicodeString class methodsFor: 'services' stamp: 'jmv 9/9/2023 16:39:53'!
xcodePointsFromUtf8: aByteArray
	"Convert the given string from UTF-8 to UTF-32"

	^WordArray streamContents: [ :strm | | s byteIndex n |
		s := aByteArray size.
		byteIndex := 1.
		[  byteIndex <= s ] whileTrue: [
			n := Character utf8BytesAndCodePointAt: byteIndex in: aByteArray
				into: nil
				into: [ :codePoint |
					strm nextPut: codePoint ].
			byteIndex := byteIndex + n
		]].! !

!UnicodeString class methodsFor: 'services' stamp: 'jmv 9/9/2023 16:40:03'!
xutf8FromCodePoints: aWordArray
	"Convert the given string from UTF-32 to UTF-8"

	^ByteArray streamContents: [ :strm |
		aWordArray do: [ :codePoint |
			Character
				evaluate: [ :byte | strm nextPut: byte ]
				withUtf8BytesOfUnicodeCodePoint: codePoint ]]! !


!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 9/9/2023 16:35:00'!
xmessageSendsCount
	"Number of message sends in code.
	A measure of the complexity of code. Alternative to #linesOfCode"

	| count |
	count := 0.
	self decompile nodesDo: [ :node | node isMessageNode ifTrue: [count := count + 1] ].
	^count! !


!Text methodsFor: 'converting' stamp: 'jmv 9/9/2023 16:46:16'!
xasUnicodeStringOrText
	"Answer is either an UnicodeString or a Text including an UnicodeString"

	string xisUnicodeString ifTrue: [ ^self ].
	^Text string: string xasUnicodeString runs: runs! !


!SystemDictionary methodsFor: 'startup' stamp: 'jmv 9/9/2023 16:31:23'!
xdisplayCommandLineUsageOn: aStream
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

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 9/9/2023 16:31:44'!
xdisplaySystemInformationStringOn: aStream
	"Display the system information string on standard output."

	[ aStream
		nextPutAll: 'System information:';		newLine;
		nextPutAll: self systemInformationString;		newLine ]
	on: UnhandledError
	do: [ :exception | exception return ].! !

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 9/9/2023 16:31:33'!
xlastCommandLineArguments
	"To make all command line arguments after end of options '--' available to scripts applications"

	^startUpArguments copyAfter: '--'.! !

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 9/9/2023 16:32:01'!
xprocessCommandLineOption: optionCharacter optionArgument: optionArgument raiseExceptions: aBoolean
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
			self xdisplayCommandLineUsageOn: StdIOWriteStream stdout.
			self quit ].

		[ $v ] -> [ 
			self xdisplaySystemInformationStringOn: StdIOWriteStream stdout.
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


!Stream methodsFor: 'nonhomogeneous accessing' stamp: 'jmv 9/9/2023 16:39:29'!
xnextUnicodeString
	"Read a string from the receiver. The first byte is the length of the string, unless it is greater than 192, in which case the first four bytes encode the length.  I expect to be in ascii mode when called (caller puts back to binary)."

	| bytes length |

	"read the length in binary mode"
	self binary.
	length := self next.		"first byte."
	length >= 192 ifTrue: [length := length - 192.
		1 to: 3 do: [:ii | length := length * 256 + self next]].
	bytes := ByteArray new: length.
	self nextInto: bytes.
	^UnicodeString fromUtf8Bytes: bytes! !

!Stream methodsFor: 'nonhomogeneous accessing' stamp: 'jmv 9/9/2023 16:39:39'!
xnextUnicodeStringPut: s
	"Append the string, s, to the receiver.  Only used by DataStream.  Max size of 64*256*256*256."

	| bytes length |
	bytes := s bytes.
	(length := bytes size) < 192
		ifTrue: [self nextPut: length]
		ifFalse: 
			[self nextPut: (length digitAt: 4)+192.
			self nextPut: (length digitAt: 3).
			self nextPut: (length digitAt: 2).
			self nextPut: (length digitAt: 1)].
	self nextPutAll: bytes.
	^s! !


!UniFileStream methodsFor: 'reading modes' stamp: 'jmv 9/9/2023 16:41:09'!
xuseUnicodeString
	"Only for reading.
	For writing accept bytes, Characters, CodePoints or ByteArray, String, UnicodeString,
	according to protocol."
	readAdaptor := UnicodeReadAdaptor new stream: self.! !


!DataStream methodsFor: 'write and read' stamp: 'jmv 9/9/2023 16:40:24'!
xreadUnicodeString

	| str |
	"byteStream ascii."
	str := byteStream xnextUnicodeString.
	byteStream binary.
	^ str
! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 9/9/2023 16:40:34'!
xreadUnicodeSymbol
    "PRIVATE -- Read the contents of a Symbol."

    ^ self xreadUnicodeString asSymbol! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 9/9/2023 16:40:48'!
xwriteUnicodeString: aString
	"PRIVATE -- Write the contents of a String."

	byteStream xnextUnicodeStringPut: aString.! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 9/9/2023 16:40:40'!
xwriteUnicodeSymbol: aSymbol
    "PRIVATE -- Write the contents of a Symbol."

    self writeString: aSymbol! !


!Scanner class methodsFor: 'testing' stamp: 'jmv 9/9/2023 16:48:34'!
xtypeTable: aTypeTable at: aCharacterOrCodePoint

	"Prebuilt tables for the ASCII range"
	aCharacterOrCodePoint asciiValue ifNotNil: [ :ascii |
		^aTypeTable at: ascii ].

	"For CodePoints not in the ASCII range"
	aCharacterOrCodePoint xisDiacriticalMark ifTrue: [ ^#xDiacritical ].
	aCharacterOrCodePoint isLetter ifTrue: [ ^#xLetter ].
	aCharacterOrCodePoint xisSymbolLike ifTrue: [ ^#xBinary ].
	aCharacterOrCodePoint = $­ ifTrue: [ ^#xBinary ].
	aCharacterOrCodePoint isSeparator ifTrue: [ ^#xDelimiter ].
	"Many should actually be #xIllegal"
	^#xLetter! !


!CodePackage methodsFor: 'source code management' stamp: 'jmv 9/9/2023 16:36:09'!
xmessageSendsCount
	"See comment at CompiledMethod >> #messageSendsCount"

	^self methods inject: 0 into: [ :sum :each |
		sum + each compiledMethod xmessageSendsCount ].! !


!Clipboard methodsFor: 'private' stamp: 'jmv 9/9/2023 16:49:57'!
xretrieveIdOrStringFromOS
	"Use a specific content type if ExtendedClipboard is active.
	Otherwise, use regular clipboard primitives"
	| utf8Bytes |
	self extendedClipboardInterface ifNotNil: [ :interface |
		interface canStore ifTrue: [
			"Answer nil if no id was stored"
			^ interface retrieveId ]].

	utf8Bytes := self primitiveClipboardString.

	"Clipboard primitives answer an empty string if there is no string in OS clipboard."
	utf8Bytes isEmpty ifTrue: [ ^nil ].

	"Abuse the fact that the VM provides an instance of String (although it contains UTF-8 bytes)"
	utf8Bytes := utf8Bytes fixLineEndings.

	"Clipboard primitive answers an instance of String containing the UTF-8 bytes.
	If not pure ASCII, contents are invalid!! Convert it ASAP into a reasonable ByteArray."
	utf8Bytes class format = ByteArray format  												"True unless image is in 32bit V3 format"
		ifTrue: [ ByteArray adoptInstance: utf8Bytes ]
		ifFalse: [ utf8Bytes := ByteArray withAll: utf8Bytes ].
	^ UnicodeString fromUtf8Bytes: utf8Bytes.! !


!Sample10PythagorasTree methodsFor: 'as yet unclassified' stamp: 'jmv 9/9/2023 16:49:04'!
xdrawTreeOn: aCanvas p1: p1 p2: p2 depth: depth

	| dx dy p3 p4 p5 color |
	
	depth = depthLimit ifTrue: [ ^ self].
	
	color := self color.
	colors ifNotNil: [
		color := colors at: (depth mod: colors size) + 1].
	
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
			
	self xdrawTreeOn: aCanvas p1: p4 p2: p5 depth: depth + 1.
	self xdrawTreeOn: aCanvas p1: p5 p2: p3 depth: depth + 1.! !

!Sample10PythagorasTree methodsFor: 'as yet unclassified' stamp: 'jmv 9/9/2023 16:49:09'!
xwithMultipleColors
	colors := (0 to: 360 count: 15) collect: [ :k | Color hue: k saturation: 0.8 brightness: 0.8]! !


!VectorCanvas methodsFor: 'accessing' stamp: 'jmv 9/9/2023 16:37:28'!
xattachedCanvas: aCanvas
	^attachedCanvas := aCanvas! !


!HybridCanvas methodsFor: 'private' stamp: 'jmv 9/9/2023 16:38:41'!
xcurrentMorph: aMorph drawingMorphStack: morphs currentTransformation: tx transformations: txs cti: i
	"For syncing to our VectorCanvas"
	currentMorph := aMorph.
	drawingMorphStack := morphs.
	currentTransformation := tx.
	transformations := txs.
	cti := i.! !


!Theme class methodsFor: 'user interface' stamp: 'jmv 9/9/2023 16:37:05'!
xloadAdditionalThemes
	"Load additional themes and reopen menu."
	Feature require: #'Theme-Themes'.
	self changeTheme! !


!FormGlyph methodsFor: 'accessing' stamp: 'jmv 9/9/2023 16:37:58'!
xdeltaXToCenter
	^deltaXToCenter! !

!FormGlyph methodsFor: 'accessing' stamp: 'jmv 9/9/2023 16:37:38'!
xdeltaXToCenter: aNumber
	deltaXToCenter := aNumber! !


!TTContourConstruction methodsFor: 'enumerating' stamp: 'jmv 9/9/2023 16:49:34'!
xinitialMoveToAndPathCommands: anAffineTransformationOrNil
	"TrueType reasonably assumes Y coordinates increase upwards."

	| initialMoveToCommand pathCommand firstPoint prevEndPoint endPoints controlPoints cp ep |
	endPoints := OrderedCollection new.
	controlPoints := OrderedCollection new.
	self
		xstartPointDo: [ :firstBezierStart |
			firstPoint := firstBezierStart.
			anAffineTransformationOrNil notNil ifTrue: [
				firstPoint := anAffineTransformationOrNil transform: firstPoint ].
			prevEndPoint := firstPoint ]
		quadraticBeziersDo: [ :controlPoint :endPoint |
			cp := controlPoint.
			ep := endPoint.
			anAffineTransformationOrNil notNil ifTrue: [
				cp := anAffineTransformationOrNil transform: cp.
				ep := anAffineTransformationOrNil transform: ep ].
			controlPoints add: cp - prevEndPoint.
			endPoints add: ep - prevEndPoint.
			prevEndPoint := ep ].
	initialMoveToCommand := PathMoveToCommand new toRelativePoint: firstPoint.
	pathCommand := PathQuadraticCurveToCommand new toRelativePoints: endPoints controlPoints: controlPoints.
	^{initialMoveToCommand. pathCommand}! !

!TTContourConstruction methodsFor: 'enumerating' stamp: 'jmv 9/9/2023 16:49:18'!
xstartPointDo: initialMoveBlock quadraticBeziersDo: bezierBlock
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
	pointsSize := points size.
	firstPointIsControlPoint := controlPointFlags bitBooleanAt: 1.
	firstPointIsControlPoint
		ifFalse: [
			firstBezierStart := points first.
			i := 2 ]
		ifTrue: [
			lastPointIsControlPoint := controlPointFlags bitBooleanAt: pointsSize.
			firstBezierStart := lastPointIsControlPoint
				ifTrue: [points first + points last / 2.0]
				ifFalse: [points last].
			i := 1 ].
	initialMoveBlock value: firstBezierStart.
	lastBezierEnd := firstBezierStart.
	[ i <= pointsSize ] whileTrue: [
		thisPoint := points at: i.
		thisPointIsControlPoint := controlPointFlags bitBooleanAt: i.
		thisPointIsControlPoint
			ifTrue: [
				thisBezierControlPoint := thisPoint.
				nextI := i = pointsSize ifFalse: [ i+1 ] ifTrue: [ 1 ].
				nextPoint := points at: nextI.
				nextPointIsControlPoint := controlPointFlags bitBooleanAt: nextI.
				nextPointIsControlPoint
					ifFalse: [ "Quadratic Bezier with explicit control and end points."
						thisBezierEnd := nextPoint.
						i := i + 2 ]
					ifTrue: [ "Quadratic Bezier with implicit end point."
						thisBezierEnd := thisPoint + nextPoint / 2.0.
						i := i + 1 ].
				]
			ifFalse: ["Line segment requested. Consider it as a bezier with an appropriate control point."
				thisBezierControlPoint := thisPoint + lastBezierEnd / 2.0.
				thisBezierEnd := thisPoint.
				i := i + 1 ].
		"Avoid null beziers. This, besides being being useless, is dangerous.
		Rasterization code uses curve length for computing hop, and this case would result in a division by zero."
		thisBezierEnd = lastBezierEnd ifFalse: [
			bezierBlock value: 	thisBezierControlPoint value: 	thisBezierEnd.
			lastBezierEnd := thisBezierEnd ].
		].
	"Close with a line if needed. TrueType contours must be closed shapes, because of fill."
	lastBezierEnd = firstBezierStart ifFalse: [
		bezierBlock value: lastBezierEnd + firstBezierStart / 2.0 value:  firstBezierStart ].! !


!TTFontDescription methodsFor: 'private-initialization' stamp: 'jmv 9/9/2023 16:38:33'!
xappendGlyphDataTo: stream glyphsArray: glyphsArray indexes: indexes base: baseIndex	 bar: aProgressBarOrNil unknown: theUnknownGlyph scaleArrowGlyphs: arrowGlyphs

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
								self xappendGlyphDataTo: stream glyphsArray: glyphOrArrayOrNumberNil indexes: indexes
									base: nextBaseIndex bar: nil unknown: theUnknownGlyph scaleArrowGlyphs: arrowGlyphs ]
							ifFalse: [
								glyphOrArrayOrNumberNil isNumber
									ifTrue: [ | glyph tx |
										glyph := arrowGlyphs at: glyphOrArrayOrNumberNil.
										tx := AffineTransformation withScale: letterMTopSideBearing asFloat / arrowGlyphs second topSideBearing.
										indexes at: baseIndex + index put: stream position+1.
										glyph addGlyphDataTo: stream tx: tx
										]
									ifFalse: [
										indexes at: baseIndex + index put: stream position+1.
										glyphOrArrayOrNumberNil addGlyphDataTo: stream tx: nil ]]]]].! !

!TTFontDescription methodsFor: 'private-initialization' stamp: 'jmv 9/9/2023 16:50:15'!
xglyphsByUtf8Bytes: glyphsByUTF8 unknownGlyph: theUnknownGlyph scaleArrowGlyphs: arrowGlyphs

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
				self xappendGlyphDataTo: stream glyphsArray: glyphsByUTF8 indexes: contourDataIndexesByUtf8
					base: 0 bar: bar unknown: theUnknownGlyph scaleArrowGlyphs: arrowGlyphs ]].
	contourDataIndexesByUtf8 := contourDataIndexesByUtf8 as: IntegerArray.! !

