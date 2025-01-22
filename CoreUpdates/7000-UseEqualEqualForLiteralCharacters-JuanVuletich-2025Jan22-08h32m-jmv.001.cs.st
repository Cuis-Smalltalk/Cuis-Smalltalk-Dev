'From Cuis7.3 [latest update: #6996] on 22 January 2025 at 8:37:48 am'!

!CompiledMethod methodsFor: 'debugger support' stamp: 'jmv 11/30/2014 11:37'!
mapFromBlockKeys: keys toSchematicTemps: schematicTempNamesString
	"Decode a schematicTempNamesString that encodes the layout of temp names
	 in a method and any closures/blocks within it, matching keys in keys to
	 vectors of temp names."
	| map tempNames |
	map := Dictionary new.
	tempNames := schematicTempNamesString readStream.
	keys do:
		[:key| | tempSequence tempIndex |
		tempSequence := OrderedCollection new.
		tempIndex := 0.
		[(tempNames skipSeparators; peek) ifNil: [true] ifNotNil: [:ch| '[]' includes: ch]] whileFalse:
			[tempNames peek == $(
				ifTrue: [tempSequence addAll: ((self tempsSubSequenceFrom: (tempNames next; yourself)) withIndexCollect:
														[:temp :index|
														{ temp. { tempIndex + 1. index } }]).
						tempNames peek ~~ $) ifTrue: [self error: 'parse error'].
						tempIndex := tempIndex + 1.
						tempNames next]
				ifFalse: [tempSequence addAll: ((self tempsSubSequenceFrom: tempNames) withIndexCollect:
														[:temp :index|
														{ temp. tempIndex := tempIndex + 1 }])]].
		map at: key put: tempSequence asArray.
		[tempNames peek == $]] whileTrue: [tempNames next].
		tempNames peek == $[ ifTrue:
			[tempNames next]].
	^map! !


!SourceCodeInterval methodsFor: 'source code' stamp: 'HAW 7/21/2023 16:59:57'!
trimParentesisOn: aSourceCode
			
	| parentesis |
	
	parentesis := 1.
	start+1 to: stop-1 do: [ :index | | char |
		char := aSourceCode at: index.
		char == $( 
			ifTrue: [ parentesis := parentesis + 1]
			ifFalse: [ char == $) 
				ifTrue: [ 
					parentesis = 1 ifTrue: [ ^self ].
					parentesis := parentesis - 1]]].
		
	^(self trimLeft: 1 right: 1) trimToMatchExpressionOn: aSourceCode ! !

!SourceCodeInterval methodsFor: 'source code' stamp: 'HAW 7/21/2023 16:53:59'!
trimToMatchExpressionOn: aSourceCode

	| initialChar endingChar |
	
	initialChar := aSourceCode at: start ifAbsent: [ nil ].
	endingChar := aSourceCode at: stop ifAbsent: [ nil ].
	
	^(initialChar == $( and: [ endingChar == $) ])
		ifTrue: [ self trimParentesisOn: aSourceCode ]
		ifFalse: [ self trimNoParentesisStartingWith: initialChar endingWith: endingChar on: aSourceCode]! !

!SourceCodeInterval methodsFor: 'private - source code' stamp: 'RNG 5/9/2020 15:39:55'!
canBeExpandedStartingWith: firstChar endingWith: lastChar

	^ (firstChar == $( and: [ lastChar == $) ]) or: [ firstChar == $` and: [ lastChar == $` ] ]! !

!SourceCodeInterval methodsFor: 'private - source code' stamp: 'RNG 5/8/2020 21:30:59'!
canBeTrimmed: initialChar

	^ initialChar notNil and: [ initialChar isSeparator or: [ initialChar == $. ] ]! !

!SourceCodeInterval methodsFor: 'testing' stamp: 'HAW 6/11/2022 19:31:22'!
endsOnDotOn: aSourceCode

	^(self lastCharacterOfRangeOn: aSourceCode) == $.! !

!SourceCodeInterval methodsFor: 'testing' stamp: 'HAW 6/11/2022 19:31:07'!
isEndOfTemporariesDeclarationOn: aSourceCode

	^((self lastCharacterOfRangeOn: aSourceCode) == $|) and: [^(aSourceCode indexOf: $|) < self last]! !


!CharacterSequence methodsFor: 'comparing' stamp: 'ul 11/14/2010 06:02'!
startingAt: keyStart match: text startingAt: textStart
	"Answer whether text matches the pattern in this string.
	Matching ignores upper/lower case differences.
	Where this string contains #, text may contain any character.
	Where this string contains *, text may contain any sequence of characters."
	| anyMatch matchStart matchEnd i matchStr j ii jj |
	i := keyStart.
	j := textStart.

	"Check for any #'s"
	[i > self size ifTrue: [^ j > text size "Empty key matches only empty string"].
	(self at: i) == $#] whileTrue:
		["# consumes one char of key and one char of text"
		j > text size ifTrue: [^ false "no more text"].
		i := i+1.  j := j+1].

	"Then check for *"
	(self at: i) == $*
		ifTrue: [i = self size ifTrue:
					[^ true "Terminal * matches all"].
				"* means next match string can occur anywhere"
				anyMatch := true.
				matchStart := i + 1]
		ifFalse: ["Otherwise match string must occur immediately"
				anyMatch := false.
				matchStart := i].

	"Now determine the match string"
	matchEnd := self size.
	(ii := self indexOf: $* startingAt: matchStart) > 0 ifTrue:
		[ii = matchStart ifTrue: [self error: '** not valid -- use * instead'].
		matchEnd := ii-1].
	(ii := self indexOf: $# startingAt: matchStart) > 0 ifTrue:
		[ii = matchStart ifTrue: [self error: '*# not valid -- use #* instead'].
		matchEnd := matchEnd min: ii-1].
	matchStr := self copyFrom: matchStart to: matchEnd.

	"Now look for the match string"
	[jj := text findString: matchStr startingAt: j caseSensitive: false.
	anyMatch ifTrue: [jj > 0] ifFalse: [jj = j]]
		whileTrue:
		["Found matchStr at jj.  See if the rest matches..."
		(self startingAt: matchEnd+1 match: text startingAt: jj + matchStr size) ifTrue:
			[^ true "the rest matches -- success"].
		"The rest did not match."
		anyMatch ifFalse: [^ false].
		"Preceded by * -- try for a later match"
		j := j+1].
	^ false "Failed to find the match string"! !

!CharacterSequence methodsFor: 'converting' stamp: 'ls 10/28/2003 12:24'!
asSmalltalkComment
	"return this string, munged so that it can be treated as a comment in Smalltalk code.  Quote marks are added to the beginning and end of the string, and whenever a solitary quote mark appears within the string, it is doubled"

	^self species streamContents:  [ :str |
		| quoteCount first |

		str nextPut: $".
	
		quoteCount := 0.
		first := true.
		self do: [ :char |
			char == $"
				ifTrue: [
					first ifFalse: [
						str nextPut: char.
						quoteCount := quoteCount + 1 ] ]
				ifFalse: [
					quoteCount odd ifTrue: [
						"add a quote to even the number of quotes in a row"
						str nextPut: $" ].
					quoteCount := 0.
					str nextPut: char ].
			first := false ]. 

		quoteCount odd ifTrue: [
			"check at the end"
			str nextPut: $". ].

		str nextPut: $".
	].
	! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/19/2024 13:50:36'!
asUnHtml
	"Strip out all Html stuff (commands in angle brackets <>) and convert
the characters &<> back to their real value.  Leave actual cr and tab as
they were in text."
	| in |
	in := self readStream.
	^self species streamContents: [ :out |
		[ in atEnd ] whileFalse: [
			in peek == $<
				ifTrue: [in unCommand] 	"Absorb <...><...>"
				ifFalse: [
					| char |
					(char := in next) == $&
					ifTrue: [
						| rest did |
						rest := in upTo: $;.
						did := out position.
						rest = 'lt' ifTrue: [out nextPut: $<].
						rest = 'gt' ifTrue: [out nextPut: $>].
						rest = 'amp' ifTrue: [out nextPut: $&].
						rest = 'deg' ifTrue: [out nextPut: $¡].
						rest = 'quot' ifTrue: [out nextPut: $"].
						rest first == $# ifTrue: [ out nextPut: (Character codePoint: rest findPositiveInteger) ].
						did = out position ifTrue: [
							out nextPut: $&; nextPutAll: rest.
							"self error: 'unknown encoded HTML char'."
							"Please add it to this method"]]
					ifFalse: [out nextPut: char]].
			]].! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/19/2024 13:51:11'!
keywords
	"Answer an array of the keywords that compose the receiver."
	| keywords |
	keywords := Array streamContents: [ :kwds |
		| kwd isAllValidInIdentifiers |
		kwd := self species writeStream.
		isAllValidInIdentifiers := true.
		1
			to: self size
			do: [ :i |
				| char |
				char := self at: i.
				kwd nextPut: char.
				char == $: & isAllValidInIdentifiers
					ifTrue: [
						kwds nextPut: kwd contents.
						kwd reset.
						isAllValidInIdentifiers := true ]
					ifFalse: [
						char isValidInIdentifiers ifFalse: [ isAllValidInIdentifiers := false ]]].
		kwd isEmpty ifFalse: [ kwds nextPut: kwd contents ]].
	
	^ keywords.! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/6/2024 17:27:30'!
withDoubleColonsDoubled
	"Return a copy of the receiver with any double colons duplicated.
	Useful for turning text into Smalltalk comments."

	^self species streamContents: [ :stream |
		self do: [ :c |
			stream nextPut: c.
			c == $"
				ifTrue: [ stream nextPut: c ]]].! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/19/2024 13:51:42'!
withDoubleColonsUndoubled
	"Do the inverse of #withDoubleColonsDoubled"

	^self species streamContents: [ :stream |
		| lastWasDoubleColon |
		lastWasDoubleColon := false.
		self do: [ :c |
			c == $"
				ifTrue: [
					lastWasDoubleColon
						ifFalse: [
							stream nextPut: c.
							lastWasDoubleColon := true ]
						ifTrue: [
							lastWasDoubleColon := false]]
				ifFalse: [
					stream nextPut: c.
					lastWasDoubleColon := false ]]].! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/8/2022 15:01:25'!
withNewLines
	"Return a copy of the receiver in which backslash (\) characters have been replaced with newLine (i.e. Lf)."

	^ self collect: [ :c | c == $\ ifTrue: [ Character newLineCharacter ] ifFalse: [ c ]].! !

!CharacterSequence methodsFor: 'printing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
storeOn: aStream 
	"Print inside string quotes, doubling inbedded quotes."
	| x |
	aStream nextPut: $'.
	1 to: self size do:
		[:i |
		aStream nextPut: (x := self at: i).
		x == $' ifTrue: [aStream nextPut: x]].
	aStream nextPut: $'! !

!CharacterSequence methodsFor: 'fileman-testing' stamp: 'jmv 2/21/2024 15:09:00'!
isAbsolutePathName
	"Note: On Windows, both 'C:\Users\Someone\file.txt' and '\Users\Someone\file.txt'
	and even '/Users/Someone/file.txt' are considered an absolute pathName.
	This is essentially because FilePlugin can handle them. The gained uniformity with Unix is nice."
	
	self isEmpty ifTrue: [^ false].
	self first isPathSeparator ifTrue: [^ true].
	
	"tilde means user's home folder, if possible.
	https://www.gnu.org/software/bash/manual/html_node/Tilde-Expansion.html"
	self first == $~ ifTrue: [^ true ].

	FileIOAccessor default onWindows
		ifTrue: [
			^ self beginsWithWindowsDriveName and: [ self size = 2 or: [ (self at: 3) isPathSeparator ]]].

	^ false! !

!CharacterSequence methodsFor: 'private' stamp: 'jmv 6/13/2022 09:25:45'!
getEnclosedExpressionFrom: aStream
	"private - get the expression enclosed between '{' and 
	'}' and remove all the characters from the stream"

	| result currentChar |
	result := self species writeStream.
	[aStream atEnd 
		or: [(currentChar := aStream next) == $}]]
			whileFalse: [result nextPut: currentChar].
	^ result contents withBlanksTrimmed.! !

!CharacterSequence methodsFor: 'formatting' stamp: 'jmv 12/19/2024 13:52:18'!
format: aCollection
	"format the receiver with aCollection  
	 
	simplest example:
	'foo {1} bar' format: {Date today}.
	 
	complete example:
	'\{ \} \\ foo {1} bar {2}' format: {12. 'string'}..
	"
	^self species streamContents: [ :out |
		| stream |
		stream := self readStream.
		[stream atEnd]
			whileFalse: [| currentChar | 
				currentChar := stream next.
				currentChar == ${
					ifTrue: [| expression | 
						expression := self getEnclosedExpressionFrom: stream.
						out nextPutAll: (self evaluateExpression: expression parameters: aCollection)]
					ifFalse: [
						currentChar == $\
							ifTrue: [stream atEnd ifFalse: [out nextPut: stream next]]
							ifFalse: [out nextPut: currentChar]]]].! !

!CharacterSequence methodsFor: 'grammar' stamp: 'LC 7/5/2020 16:47:58'!
article
	| article first letter second |
	self isEmpty ifTrue: [^self].
	article := self first isVowel ifTrue: ['an'] ifFalse: ['a'].
	first := self first asLowercase.
	letter := self size = 1.
	second := letter ifFalse: [self second asLowercase].
	(first == $f and: [letter orNot: ['aeiloru' includes: second]])
		ifTrue: [^'an'].
	first == $u ifTrue: [
		(letter or: ['cks' includes: second]) ifTrue: [^'a'].
		second == $n
			ifTrue: [(self size = 2 or: [self third isVowel]) ifTrue: [^'a']]].
	(first == $e and: [second == $u]) ifTrue: [^'a'].
	^article! !

!CharacterSequence methodsFor: 'grammar' stamp: 'LC 4/25/2020 10:24:38'!
lowercasePlural
	| last |
	self exceptionalPlural ifNotNil: [:pl | ^pl].
	self isUninflictedNoun ifTrue: [^self].
	last := self last.
	last == $y ifTrue: [
		#('ay' 'ey' 'oy' 'uy') do: [:t |
			(self endsWith: t) ifTrue: [^self , 's'].
			^self allButLast , 'ies']].
	#('zz' 'ch' 'sh') do: [:t | (self endsWith: t) ifTrue: [^self , 'es']].
	last == $s ifTrue: [
		self = 'its' ifTrue: [^'their'].
		#('bs' 'cs' 'ds' 'ks' 'ls' 'ms' 'rs' 'ts' 'ws')
			do: [:t | (self endsWith: t) ifTrue: [^self]].
		#('sis' 'xis')
			do: [:t | (self endsWith: t) ifTrue: [^(self allButLast: 2) , 'es']]].
	last == $z ifTrue: [^self , 'zes'].
	(last == $x or: [last == $s]) ifTrue: [^self , 'es'].
	(self endsWith: 'man') ifTrue: [^(self allButLast: 2) , 'en'].
	last == $f ifTrue: [^self allButLast , 'ves'].
	(self endsWith: 'fe') ifTrue: [^(self allButLast: 2) , 'ves'].
	^self , 's'! !


!CharacterSequence class methodsFor: 'instance creation' stamp: 'jmv 12/19/2024 13:52:49'!
readFrom: inStream
	"Answer a new String that is determined by reading the stream, inStream.  Embedded
	double quotes become the quote Character."

	^self streamContents: [ :outStream |
		| done |
		"go to first quote"
		inStream skipTo: $'.
		done := false.
		[done or: [inStream atEnd]]
			whileFalse: 
				[ |char|
				char := inStream next.
				char == $'
					ifTrue: 
						[char := inStream next.
						char == $'
							ifTrue: [outStream nextPut: char]
							ifFalse: [done := true]]
					ifFalse: [outStream nextPut: char]]].! !


!String methodsFor: 'testing' stamp: 'fgz 7/1/2024 16:33:58'!
isIntegerDenotation
	"Answer true if the receiver denotes an integer number."
	
	^(self size > 1 and: [
		self first == $- and: [ (self findFirst: [:each | each isDigit not ] startingAt: 2) = 0 ]
			]) or: [ self size > 0 and: [ self allSatisfy: [:each | each isDigit ] ] ]

! !


!Symbol methodsFor: 'accessing' stamp: 'jmv 6/28/2022 14:45:08'!
precedence
	"Answer the receiver's precedence, assuming it is a valid Smalltalk
	message selector or 0 otherwise. The numbers are 1 for unary,
	2 for binary and 3 for keyword selectors."

	| c |
	self isEmpty ifTrue: [^ 0].
	"Consider selectors starting with an underscore $_ as unary, even if Preferences allowUnderscoreSelectors is not set."
	c := self first.
	c isValidInBinarySelectors ifTrue: [^ 2].
	self last == $: ifTrue: [^ 3].
	^ 1! !


!Symbol class methodsFor: 'instance creation' stamp: 'jmv 8/15/2015 10:21'!
readFrom: strm
	"
	Symbol readFrom: '#abc' readStream
	"

	strm peek == $# ifFalse: [self error: 'Symbols must be introduced by #'].
	^ (Scanner new scan: strm) advance  "Just do what the code scanner does"! !

!Symbol class methodsFor: 'services' stamp: 'jmv 2/5/2024 17:59:33'!
possibleSelectorsFor: misspelled 
	"Answer an ordered collection of possible corrections
	for the misspelled selector in order of likelyhood"

	| numArgs candidates lookupString best binary short long first ss |
	lookupString := misspelled asLowercase. "correct uppercase selectors to lowercase"
	numArgs := lookupString numArgs.
	(numArgs < 0 or: [lookupString size < 2]) ifTrue: [^ OrderedCollection new].
	first := lookupString first.
	short := lookupString size - (lookupString size // 4 max: 3) max: 2.
	long := lookupString size + (lookupString size // 4 max: 3).

	"First assemble candidates for detailed scoring"
	candidates := OrderedCollection new.
	self allSymbolTablesDo: [:s | (((ss := s size) >= short	"not too short"
			and: [ss <= long			"not too long"
					or: [(s at: 1) = first]])	"well, any length OK if starts w/same letter"
			and: [s numArgs = numArgs])	"and numArgs is the same"
			ifTrue: [candidates add: s]].

	"Then further prune these by correctAgainst:"
	best := lookupString correctAgainst: candidates.
	((misspelled last ~~ $:) and: [misspelled size > 1]) ifTrue: [
		binary := misspelled, ':'.		"try for missing colon"
		Symbol hasInterned: binary ifTrue: [:him | best addFirst: him]].
	^ best! !


!UnicodeSymbol methodsFor: 'accessing' stamp: 'jmv 6/28/2022 14:45:18'!
precedence
	"Answer the receiver's precedence, assuming it is a valid Smalltalk
	message selector or 0 otherwise. The numbers are 1 for unary,
	2 for binary and 3 for keyword selectors."

	| c |
	self isEmpty ifTrue: [^ 0].
	"Consider selectors starting with an underscore $_ as unary, even if Preferences allowUnderscoreSelectors is not set."
	c := self first.
	c isValidInBinarySelectors ifTrue: [^ 2].
	self last == $: ifTrue: [^ 3].
	^ 1! !


!UnicodeSymbol class methodsFor: 'instance creation' stamp: 'jmv 7/6/2023 11:52:07'!
readFrom: strm
	"
	UnicodeSymbol readFrom: '#abc' readStream
	"

	strm peek == $# ifFalse: [self error: 'Symbols must be introduced by #'].
	^ (Scanner new scan: strm) advance  "Just do what the code scanner does"! !


!SystemDictionary methodsFor: 'query' stamp: 'jmv 10/1/2022 12:02:12'!
classesWithExtensionCategories
	"For a base image, there should be none.
	Smalltalk classesWithExtensionCategories
	"

	Object withAllSubclasses do: [ :cls | cls organization categories do: [ :cat | cat first == $* ifTrue: [cls print ]]]! !

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 8/16/2024 09:21:38'!
processCommandLineArguments: initialOptions
	"
	Smalltalk processCommandLineArguments: true
	Smalltalk processCommandLineArguments: false

	Note: By default, any exceptions thrown during the evaluation of arguments are simply ignored.
	To ease debugging, use the -e argument: Do Not Ignore Exceptions. 
	For example:
		squeak Cuis.image -e -s myScript.st
	If ``myScript.st`` contains an error, a Debugger will pop up.
	
	See comment at
		processInitialCommandLineOption:optionArgument:
		processFinalCommandLineOption:optionArgument:raiseExceptions:
	See displayCommandLineUsageOn:
	"
	| optionString optionArgument optionIndex nextOptionIndex optionLastParameterIndex raiseExceptions dashFinder |

	"Any command line argument that starts with $- is an Option.
	NonOption arguments are parameters for the preceding Option."
	raiseExceptions := false.
	startUpScriptArguments := #().
	"Empty Strings are valid command line arguments, generated by two double quotes. Handle them."
	dashFinder := [ :any | any notEmpty and: [ any first == $- ] ].
	optionIndex := startUpArguments findFirst: dashFinder startingAt: 1.
	[ optionIndex > 0 ] whileTrue: [
		"Find parameters for this option"
		nextOptionIndex := startUpArguments findFirst: dashFinder startingAt: optionIndex+1.
		optionLastParameterIndex := nextOptionIndex = 0
			ifTrue: [startUpArguments size]
			ifFalse: [nextOptionIndex-1].
		optionString := startUpArguments at: optionIndex.
		optionString size > 1 ifTrue: [
			optionString = '--' 						"Including    --    as an option means 'ignore everything after here'"
				ifTrue: [^self].
			optionString = '-e'
				ifTrue: [ raiseExceptions := true ]
				ifFalse: [
					optionArgument := optionLastParameterIndex > optionIndex ifTrue: [startUpArguments at: optionIndex+1].
					startUpScriptArguments := startUpArguments copyFrom: optionIndex+2 to: optionLastParameterIndex.
					initialOptions
						ifTrue: [ self processInitialCommandLineOption: optionString optionArgument: optionArgument ]
						ifFalse: [ self processFinalCommandLineOption: optionString optionArgument: optionArgument raiseExceptions: raiseExceptions ].
					startUpScriptArguments := #() ]
			].
		optionIndex := nextOptionIndex ].! !


!Base64MimeConverter methodsFor: 'conversion' stamp: 'jmv 5/21/2022 22:29:58'!
nextValue
	"The next six bits of data char from the mimeStream, or nil.  Skip all other chars"
	| raw num |
	FromCharTable ifNil: [ self class initializeTables ].
	[
		raw := mimeStream next.
		raw ifNil: [^ nil].	"end of stream"
		raw == $= ifTrue: [^ nil].
		num := FromCharTable at: raw asciiValue + 1.
		num ifNotNil: [^ num].
		"else ignore space, return, tab, ..."
	] repeat! !


!Stream methodsFor: 'nonhomogeneous accessing' stamp: 'jmv 5/26/2022 15:14:19'!
nextNumber
	"Answer a number from the (text) stream."

	|element|
	[(element := self next) isNil or: [element isDigit or: [element == $- or: [element == $. or: [element == $)]]]]] whileFalse.
	element ifNil: [^nil].
	self skipBack.
	element == $) ifTrue: [^nil].
	^Number readFrom: self! !


!PositionableStream methodsFor: 'accessing' stamp: 'jmv 11/16/2015 14:52'!
nextKeyword
	^self
		untilAnySatisfying: [:c | (c isValidInIdentifiers or: [c == $:]) not]! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
unCommand
	"If this read stream is at a <, then skip up to just after the next >.  For removing html commands."
	| char |
	[self peek == $<] whileTrue: ["begin a block"
		[self atEnd == false and: [self next ~~ $>]] whileTrue.
		"absorb characters"
		].
 ! !


!Transcripter methodsFor: 'command line' stamp: 'di 8/12/97 22:11'!
confirm: queryString 
	| choice |
	[true]
		whileTrue: 
			[choice := self request: queryString , '
Please type yes or no followed by return'.
			choice first asUppercase == $Y ifTrue: [^ true].
			choice first asUppercase == $N ifTrue: [^ false]]! !


!Scanner methodsFor: 'expression types' stamp: 'HAW 6/22/2020 16:11:44'!
scanToken

	"Skip delimiters fast, there almost always is one."
	self skipDelimiters.

	mark := source position - 1.
	(tokenType at: 1) == $x "x as first letter"
		ifTrue: [self perform: tokenType "means perform to compute token & type"]
		ifFalse: [token := self step asSymbol "else just unique the first char"].
	^token! !

!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 11/28/2022 11:51:26'!
xBinary

	tokenType := #binary.
	token := source collectionSpecies streamContents: [ :stream |
	stream nextPut: self step.
	[	| type |
		type := self typeTableAt: hereChar.
		(type == #xBinary or: [ type == #xDiacritical ]) and: [
			hereChar ~~ $- or: [aheadChar isDigit not]]
	] whileTrue: [
		stream nextPut: self step]].
	token := token asSymbol.! !

!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 5/21/2022 22:30:01'!
xColon
	"Allow := for assignment"
	
	aheadChar == $= ifTrue:
		[self step.
		tokenType := #leftArrow.
		self step.
		^ token := #':='].
	"Otherwise, just do what normal scan of colon would do"
	tokenType := #colon.
	^ token := self step asSymbol! !

!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 5/23/2022 17:16:31'!
xDoubleQuote
    "Collect a comment."
    "wod 1/10/98: Allow 'empty' comments by testing the first character
for $"" rather than blindly adding it to the comment being collected."
    | aStream stopChar |
    stopChar := DoItCharacter.
    aStream := source collectionSpecies writeStream.
    self step.
    [hereChar == $"]
        whileFalse:
            [(hereChar = stopChar and: [source atEnd])
                ifTrue: [^self offEnd: self class unmatechedCommentQuoteErrorDescription].
            aStream nextPut: self step.].
    self step.
    currentComment == nil
        ifTrue: [currentComment := OrderedCollection with: aStream contents]
        ifFalse: [currentComment add: aStream contents].
    self scanToken! !

!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 1/9/2024 11:37:11'!
xLetter
	"Form a word or keyword."

	| type |
	buffer reset.
	[(type := self typeTableAt: hereChar) == #xLetter
		or: [type == #xDigit
		or: [type == #xUnderscore
		or: [type == #xDiacritical
		or: [ hereChar == $′ ]]]]  						"Also see #isValidInIdentifiers"
	] whileTrue: [
			"open code step for speed"
			buffer nextPut: hereChar.
			hereChar := aheadChar.
			aheadChar := source atEnd
				ifTrue: [DoItCharacter "doit"]
				ifFalse: [source next]].
	tokenType := (type == #colon or: [type == #xColon and: [aheadChar ~~ $=]])
		ifTrue: [
			buffer nextPut: self step.
			"Allow any number of embedded colons in literal symbols"
			[(self typeTableAt: hereChar) == #xColon]
				whileTrue:
					[buffer nextPut: self step].
			#keyword]
		ifFalse: [
			#word].
	token := buffer contents.! !


!Scanner class methodsFor: 'testing' stamp: 'jmv 11/27/2024 14:56:06'!
typeTable: aTypeTable at: aCharacterOrCodePoint

	"Prebuilt tables for the ASCII range"
	aCharacterOrCodePoint isAscii ifTrue: [
		^aTypeTable at: aCharacterOrCodePoint asciiValue ].

	"For CodePoints not in the ASCII range"
	aCharacterOrCodePoint isDiacriticalMark ifTrue: [ ^#xDiacritical ].
	aCharacterOrCodePoint isLetter ifTrue: [ ^#xLetter ].
	aCharacterOrCodePoint isSymbolLike ifTrue: [ ^#xBinary ].
	aCharacterOrCodePoint == $­ ifTrue: [ ^#xBinary ].
	aCharacterOrCodePoint isSeparator ifTrue: [ ^#xDelimiter ].
	"Many should actually be #xIllegal"
	^#xLetter! !


!Parser methodsFor: 'pragmas' stamp: 'jmv 5/21/2022 22:31:42'!
pragmaLiteral: selectorSoFar
	"Read a pragma literal.  As a nicety we allow a variable name (rather
	 than a literal string) as the second argument to primitive:error:"

	(hereType == #string or: [ hereType == #literal or: [ hereType == #number ] ])
		ifTrue: [ ^ self advance ].
	(here == $# and: [ tokenType == #word ])
		ifTrue: [ ^ self advance ].
	(here == #- and: [ tokenType == #number ])
		ifTrue: [ ^ (self advance; advance) negated ].
	(here = 'true' or: [ here = 'false' or: [ here = 'nil' ] ])
		ifTrue: [ ^ (Scanner new scanTokens: self advance) first ].
	"This nicety allows one to supply a primitive error
	 temp as a variable name, rather than a string."
	((selectorSoFar beginsWith: 'primitive:')
	 and: [(selectorSoFar endsWith: 'error:')
	 and: [hereType == #word]]) ifTrue:
		[^self advance].
	^self expected: 'Literal constant'! !

!Parser methodsFor: 'pragmas' stamp: 'jmv 7/12/2023 11:53:03'!
pragmaStatement
	"Read a single pragma statement. Parse all generic pragmas in the form of: <key1: val1 key2: val2 ...> and remember them, including primitives."
	
	| selector arguments words index keyword |
	(hereType = #keyword or: [ hereType = #word or: [ hereType = #binary ] ])
		ifFalse: [  ^ self expected: 'pragma declaration' ].

	" This is a ugly hack into the compiler of the FFI package. FFI should be changed to use propre pragmas that can be parsed with the code here. "
	(here = #apicall: or: [ here = #cdecl: ])
		ifTrue: [ ^ self externalFunctionDeclaration ].

	selector := String new.
	arguments := OrderedCollection new.
	words := OrderedCollection new.
	[ hereType = #keyword or: [ (hereType = #word or: [ hereType = #binary ]) and: [ selector isEmpty ] ] ] whileTrue: [
		index := self startOfNextToken + requestorOffset.
		selector := selector , self advance.
		selector := encoder asciiStringLiteral: selector.
		words add: (index to: self endOfLastToken + requestorOffset).
		(selector last == $: or: [ selector first isLetter not ]) ifTrue: [
			arguments add: (encoder asciiStringLiteral: (self pragmaLiteral: selector)) ] ].
	selector numArgs ~= arguments size
		ifTrue: [ ^ self expected: 'pragma argument' ].
	(Symbol hasInterned: selector 
		ifTrue: [ :value | keyword := value]) 
		ifFalse: [ 
			keyword := self 
				correctSelector: selector wordIntervals: words
				exprInterval: (words first first to: words last last)
				ifAbort: [ ^ self fail ] ].
	self addPragma: (Pragma keyword: keyword arguments: arguments asArray).
	^ true! !

!Parser methodsFor: 'error correction' stamp: 'jmv 4/26/2024 15:20:48'!
correctSelector: proposedKeyword wordIntervals: aSpots exprInterval: expInt ifAbort: abortAction
	"Correct the proposedKeyword to some selector symbol, correcting the original text if such action is indicated.  abortAction is invoked if the proposedKeyword couldn't be converted into a valid selector.  Spots is an ordered collection of intervals within the test stream of the for each of the keyword parts."

	| correctSelector userSelection delta adjustedSpots |
	"If we can't ask the user, assume that the keyword will be defined later"
	self interactive ifFalse: [^proposedKeyword asSymbol].
	
	"If the requestor is of an exotic kind (like a telnet server) we might not be allowed to open a PupUpMenu for querying the user"
	"
	((requestor isKindOf: Editor) or: [ requestor is: #Morph ])
		ifFalse: [ ^ proposedKeyword asSymbol ].
	"

	userSelection := requestor selectionInterval.

	delta := self sourceDelta.
	adjustedSpots := aSpots collect: [ :interval | interval first + delta to: interval last + delta ].
	requestor selectFrom: adjustedSpots first first to: adjustedSpots last last.

	correctSelector := UnknownSelector name: proposedKeyword.
	correctSelector ifNil: [^abortAction value].

	requestor selectInvisiblyFrom: userSelection first to: userSelection last.

	self substituteSelector: correctSelector keywords wordIntervals: adjustedSpots.
	^(proposedKeyword last ~~ $:
	   and: [correctSelector last == $:])
		ifTrue: [abortAction value]
		ifFalse: [correctSelector]! !

!Parser methodsFor: 'error correction' stamp: 'EB 1/27/2020 20:19:48'!
isLastPipeOfEmptyTempsDeclaration: pipeIndex

	| indexOfPreviousPipe sourceCode |
	
	sourceCode := requestor text string.
	indexOfPreviousPipe := sourceCode lastIndexOf: $| startingAt: pipeIndex - 1 endingAt: 1 ifAbsent: [ ^false ].
	^(sourceCode at: pipeIndex) == $| and: [
		(sourceCode copyFrom: indexOfPreviousPipe + 1 to: pipeIndex - 1) allSatisfy: [ :char | char isSeparator ]]! !

!Parser methodsFor: 'error correction' stamp: 'HAW 12/12/2019 17:44:22'!
newRangeRemovingTempsDeclarationFrom: currentSource 
	startingAt: start
	to: originalEnd
	
	| end |
	
	end := self filterSeparatorsToTheRightOn: currentSource startingAt: originalEnd.
	
	(currentSource at: end+1) == $| ifTrue: [ | possibleNewStart |
		possibleNewStart := (self filterSeparatorsToTheLeftOn: currentSource startingAt: start) - 1.
		(currentSource at: possibleNewStart) == $| ifTrue: [ ^Array with: possibleNewStart with: end + 1 ]]. 
	
	^Array with: start with: end
! !

!Parser methodsFor: 'error correction' stamp: 'EB 12/13/2019 01:26:43'!
pasteTempAtMethodLevel: name

	| theTextString |
	
	theTextString := requestor text string.
	tempsMark := 
		(theTextString at: tempsMark) == $| 
			ifTrue: [
  				"Paste it before the second vertical bar"
		 		self pasteTemp: name before: tempsMark ] 
			ifFalse: [ self createTempDeclarationInMethodWith: name ].
	! !


!BlockNode methodsFor: 'printing' stamp: 'hlsf 6/24/2022 09:29:25'!
printOn: aStream indent: level
	| separateLines |
	aStream nextPut: $[.
	self
		printArgumentsOn: aStream
		indent: level.
	separateLines := (self
		printTemporaries: temporaries
		on: aStream
		doPrior: [ aStream space ]) or: [arguments notNil and: [arguments notEmpty] ].
	(Preferences at: #prettyPrintRectangularBlocks)
		ifTrue: [
			"If args+temps > 0 and statements > 1 (or just one complex statement),
			put all statements on separate lines"
			separateLines
				ifTrue: [
					(statements size > 1 or: [
						statements size = 1 and: [ statements first isComplex ]])
							ifTrue: [ aStream newLineTab: (1 max: level) ]
							ifFalse: [ aStream space ] ]
				ifFalse: [
					(statements size = 1 and: [ statements first isComplex not ])
						ifTrue: [ aStream space ]]]
		ifFalse: [
			self isComplex
				ifTrue: [ aStream newLineTab: (1 max: level) ]
				ifFalse: [ aStream space ] ].
	((self printStatementsOn: aStream indent: level) > 0 and: [ aStream peekLast ~~ $] ])
		ifTrue: [ aStream space ].
	aStream nextPut: $]! !


!TTFontReader methodsFor: 'processing' stamp: 'jmv 8/23/2024 14:57:38'!
mapAndStoreGlyphsUsing: characterMap
	"
	https://developer.apple.com/fonts/TrueType-Reference-Manual/RM07/appendixB.html
	"
	| glyphsByUTF8 unknownGlyph nullGlyph spaceGlyph mapSize preferredCodePoint arrowGlyphIndexes leftArrowGlyph upArrowGlyph rightArrowGlyph downArrowGlyph carriageReturnGlyph leftArrowGlyphCodePoint upArrowGlyphCodePoint rightArrowGlyphCodePoint downArrowGlyphCodePoint carriageReturnGlyphCodePoint |
	unknownGlyph := glyphs first.
	nullGlyph := glyphs second.
	mapSize := characterMap size.
	glyphsByUTF8 := Array new: 256.
	leftArrowGlyphCodePoint := self class smalltalkAssignmentArrowGlyphCodePoint.
	upArrowGlyphCodePoint := self class smalltalkReturnArrowGlyphCodePoint.
	rightArrowGlyphCodePoint := 8594.
	downArrowGlyphCodePoint := 8595.
	carriageReturnGlyphCodePoint := 8617.
	
	"Store Unicode arrow glyphs for future use if appropriate,"
	(FallbackArrowGlyphs isNil or: [fontDescription name = 'DejaVuSans']) ifTrue: [
		mapSize > 8617 ifTrue: [								"Usually true. Just in case."
			arrowGlyphIndexes := {leftArrowGlyphCodePoint. upArrowGlyphCodePoint. rightArrowGlyphCodePoint. downArrowGlyphCodePoint. carriageReturnGlyphCodePoint}
					collect: [ :codePoint | (characterMap at: codePoint+1)+1 ].
				(arrowGlyphIndexes allSatisfy: [ :i | i > 1 ]) ifTrue: [
					FallbackArrowGlyphs := arrowGlyphIndexes collect: [ :i | glyphs at: i ]]]].
	"Take arrow glyphs if present, or use fallbacks if not."
	leftArrowGlyph := upArrowGlyph := rightArrowGlyph := downArrowGlyph := carriageReturnGlyph := unknownGlyph.
	mapSize > downArrowGlyphCodePoint ifTrue: [
		leftArrowGlyph := glyphs at: (characterMap at: leftArrowGlyphCodePoint+1)+1.
		upArrowGlyph := glyphs at: (characterMap at: upArrowGlyphCodePoint+1)+1.
		rightArrowGlyph := glyphs at: (characterMap at: rightArrowGlyphCodePoint+1)+1.
		downArrowGlyph := glyphs at: (characterMap at: downArrowGlyphCodePoint+1)+1 ].
	leftArrowGlyph == unknownGlyph ifTrue: [ leftArrowGlyph := FallbackArrowGlyphs at: 1 ].
	upArrowGlyph == unknownGlyph ifTrue: [ upArrowGlyph := FallbackArrowGlyphs at: 2 ].
	rightArrowGlyph == unknownGlyph ifTrue: [ rightArrowGlyph := FallbackArrowGlyphs at: 3 ].
	downArrowGlyph == unknownGlyph ifTrue: [ downArrowGlyph := FallbackArrowGlyphs at: 4 ].
	mapSize > carriageReturnGlyphCodePoint ifTrue: [
		carriageReturnGlyph := glyphs at: (characterMap at: carriageReturnGlyphCodePoint+1)+1 ].
	carriageReturnGlyph == unknownGlyph ifTrue: [ carriageReturnGlyph := FallbackArrowGlyphs at: 5 ].

	0 to: mapSize - 1 do: [ :codePoint | | g array prevByte|

		preferredCodePoint := codePoint.
		(codePoint == $- codePoint and: [mapSize > 16r2212]) ifTrue: [ preferredCodePoint := 16r2212 ]. "WIDE MINUS"
		(codePoint == $* codePoint and: [mapSize > 16r2217]) ifTrue: [ preferredCodePoint := 16r2217 ]. "CENTERED ASTERISK"
		g := glyphs at: (characterMap at: preferredCodePoint+1)+1 ifAbsent: [ unknownGlyph ].
		g == unknownGlyph ifTrue: [
			g := glyphs at: (characterMap at: codePoint+1)+1 ifAbsent: [ unknownGlyph ]].

		array := glyphsByUTF8.
		prevByte := nil.
		Character
			evaluate: [ :byte |
				prevByte notNil ifTrue: [
					array := (array at: prevByte+1) ifNil: [ array at: prevByte+1 put: (Array new: 256) ].
					].
				prevByte := byte.
				]
			withUtf8BytesOf: codePoint.
			array at: prevByte+1 put: g.
		codePoint = 32 ifTrue: [
			spaceGlyph := g ].

		].

	"Synthetic Unicode zero width space glyph. Add others as needed."
	{ Character zeroWidthSpace } do: [ :codePoint | | g array prevByte|
		g := TTGlyph new.					"Suitable as zero width space"
		array := glyphsByUTF8.
		prevByte := nil.
		Character
			evaluate: [ :byte |
				prevByte notNil ifTrue: [
					array := (array at: prevByte+1) ifNil: [ array at: prevByte+1 put: (Array new: 256) ]].
				prevByte := byte ]
			withUtf8BytesOf: codePoint codePoint.
			array at: prevByte+1 put: g ].

	Character separators do: [ :s |
		s codePoint < 256 ifTrue: [
			glyphsByUTF8 at: s codePoint +1 put: spaceGlyph ]].
	1 to: 4 do: [ :i |			glyphsByUTF8 at: 28+i put: i ].
	glyphsByUTF8 at: 13 +1 put: carriageReturnGlyph.
	fontDescription
		glyphsByUtf8Bytes: glyphsByUTF8
		unknownGlyph: unknownGlyph scaleArrowGlyphs: {leftArrowGlyph. rightArrowGlyph. upArrowGlyph. downArrowGlyph. carriageReturnGlyph}! !


!Date class methodsFor: 'squeak protocol' stamp: 'jmv 5/26/2022 16:33:54'!
readFrom: aStream 
	"Read a Date from the stream in any of the forms:
		<day> <month> <year>		(15 April 1982; 15-APR-82; 15.4.82; 15APR82)  
		<month> <day> <year>		(April 15, 1982; 4/15/82)
		<year>-<month>-<day>		(1982-04-15) (ISO8601)"
	| day month year firstAsNumber firstAsNumberSign |
	firstAsNumberSign := aStream peek == $- ifTrue: [-1] ifFalse: [1].
	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	aStream peek isDigit ifTrue: [
		firstAsNumber := (Integer readFrom: aStream) * firstAsNumberSign.
		(firstAsNumber < 0 or: [firstAsNumber > 31])
			ifTrue: [year := firstAsNumber]].

	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	aStream peek isLetter
		ifTrue: ["MonthName-DD-YY or DD-MonthName-YY or YY-MonthName-DD"
			month := String streamContents: [ :strm |
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
		year :=  year < 69 
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
	firstAsNumberSign := aStream peek == $- ifTrue: [-1] ifFalse: [1].
	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	aStream peek isDigit ifTrue: [
		firstAsNumber := (Integer readFrom: aStream) * firstAsNumberSign.
		(firstAsNumber < 0 or: [firstAsNumber > 12])
			ifTrue: [year := firstAsNumber]].

	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	aStream peek isLetter
		ifTrue: ["MonthName-YY or YY-MonthName"
			month := String streamContents: [ :strm |
				[ aStream peek isLetter ] whileTrue: [ strm nextPut: aStream next ]].
			[aStream peek isAlphaNumeric] whileFalse: [aStream skip]]
		ifFalse: ["MM-YY or YY-MM"
			month := year 
				ifNil: ["MM-YY"
					Month nameOfMonth: firstAsNumber ]
				ifNotNil: ["YY-MM"
					Month nameOfMonth: (Integer readFrom: aStream)]].

	year ifNil: [
		[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
		year := Integer readFrom: aStream].
	(year < 100 and: [year >= 0])  ifTrue: [
		year := year < 69 
			ifTrue: [2000 + year]
			ifFalse: [1900 + year]].

	^ self month: month year: year! !


!Week class methodsFor: 'squeak protocol' stamp: 'jmv 5/26/2022 15:07:01'!
readFrom: aStream 
	"Read a Week from the stream in any of the forms:  
		<year>-W<week>			(2009-W01) (ISO8601)"
	| weekNumber yearNumber firstAsNumber firstAsNumberSign |
	firstAsNumberSign := aStream peek == $- ifTrue: [-1] ifFalse: [1].
	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	aStream peek isDigit ifTrue: [
		firstAsNumber := (Integer readFrom: aStream) * firstAsNumberSign.
		(firstAsNumber < 0 or: [firstAsNumber > 12])
			ifTrue: [yearNumber := firstAsNumber]].

	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	aStream next == $W ifFalse: [
		self error: 'Invalid Format' ].

	weekNumber := Integer readFrom: aStream.
	weekNumber < 1 ifTrue: [ self error: 'Invalid Format' ].
	(yearNumber < 100 and: [yearNumber >= 0])  ifTrue: [
		yearNumber := yearNumber < 69 
			ifTrue: [2000 + yearNumber]
			ifFalse: [1900 + yearNumber]].

	^ self yearNumber: yearNumber weekNumber: weekNumber! !


!Year class methodsFor: 'squeak protocol' stamp: 'jmv 5/26/2022 15:07:07'!
readFrom: aStream 

	| year sign |
	sign := aStream peek == $- ifTrue: [-1] ifFalse: [1].
	[aStream peek isDigit] whileFalse: [aStream skip].
	year := (Integer readFrom: aStream) * sign.
	^ self yearNumber: year! !


!Browser methodsFor: 'class functions' stamp: 'jmv 5/21/2022 22:29:19'!
explainSpecial: string 
	"Answer a string explaining the code pane selection if it is displaying 
	one of the special edit functions."

	| classes whole lits reply |
	self isEditingClass
		ifTrue: 
			["Selector parts in class definition"
			string last == $: ifFalse: [^nil].
			lits := Array with:
				#subclass:instanceVariableNames:classVariableNames:poolDictionaries:category:.
			(whole := lits detect: [:each | (each keywords
					detect: [:frag | frag = string] ifNone: nil) notNil]
						ifNone: nil) notNil
				ifTrue: [reply := '"' , string , ' is one part of the message selector ' , whole , '.']
				ifFalse: [^nil].
			classes := Smalltalk allClassesImplementing: whole.
			classes := 'these classes ' , classes printString.
			^reply , '  It is defined in ' , classes , '."
Smalltalk browseAllImplementorsOf: #' , whole].

	editSelection == #editSystemCategories ifTrue: [^nil].
	editSelection == #editMessageCategories ifTrue: [^nil].
	^nil! !

!Browser methodsFor: 'message category functions' stamp: 'jmv 3/14/2024 10:32:52'!
addExtensionCategory
	"Present a choice package names and let the user enter a new category suffx.
	Take the suffix form the selected category."

	| packageNames packageNameIndex |
	
	selectedClassName ifNil: [ ^self ].

	packageNames := (CodePackage installedPackages collect: [ :package |
		package packageName ]) sorted.
	packageNames ifEmpty: [ ^ self ].
	
	packageNameIndex := PopUpMenu 
		withCaption: 'Which package to extend?'
		chooseFrom: packageNames.
	packageNameIndex = 0 ifTrue: [ ^ self ].

	self 
		request: 'Enter the category name'
		initialAnswer: 
			(String streamContents: [ :stream |
				stream
					nextPut: $*;
					nextPutAll: (packageNames at: packageNameIndex).
				selectedMessageCategory ifNotNil: [
					stream
						nextPut: $-;
						nextPutAll: selectedMessageCategory ] ])
		 verifying: [ :newCategoryName | 
			((newCategoryName at: 1) == $* or: [
				self confirm:
					"I know about with #withNewlines ..."
					`'The selected category does not start with an asterix (*).',
					String newLineString,
					'Do you want to use it anyway?'` ]) and: [
						(packageNames includes:				(newCategoryName copyUpTo: $-) allButFirst) or: [
							self confirm:
								`'The selected prefix does not match any package name.', 
								String newLineString,
								'Do you want to use it anyway?'` ] ] ]
		do: [ :newCategoryName | | oldMessageCategoryListIndex |
			oldMessageCategoryListIndex := self messageCategoryListIndex.
			self classOrMetaClassOrganizer
				addCategory: newCategoryName
				before: selectedMessageCategory.
			self changed: #messageCategoryList.
			self messageCategoryListIndex:
				(oldMessageCategoryListIndex = 0
					ifTrue: [ self classOrMetaClassOrganizer categories size + 1 ]
					ifFalse: [ oldMessageCategoryListIndex ]).
			self changed: #messageCategoryList ]! !


!MessageNames methodsFor: 'search' stamp: 'jmv 4/28/2022 11:40:38'!
computeSelectorListFromSearchString
	"Compute selector list from search string"
	| sorted |
	sorted := SortedCollection sortBlock: [ :x :y |
		x asLowercase <= y asLowercase ].
	(searchString findBetweenSubStrs: ';') do: [ :selPat | | raw n m |
		selPat notEmpty ifTrue: [
			(selPat first == $" and: [
				selPat last == $" and: [ selPat size > 2 ]])
				ifTrue: [
					Symbol
						hasInterned:
							(selPat
								copyFrom: 2
								to: selPat size - 1)
						ifTrue: [ :sym |
							sorted add: sym ]]
				ifFalse: [
					raw := ((n := selPat occurrencesOf: $*) > 0 | ((m := selPat occurrencesOf: $#) > 0) and: [ selPat size > (n + m) ])
						ifTrue: [ Symbol selectorsMatching: selPat ]
						ifFalse: [ Symbol selectorsContaining: selPat ].
					sorted addAll: raw ]]].
	selectorList := sorted asArray  select: [ :sym |
		(Smalltalk isThereAnImplementorOf: sym) or: [ Smalltalk isThereAReferenceTo: sym ]].
	^ selectorList.! !


!ChangeSorter methodsFor: 'code pane' stamp: 'jmv 3/27/2024 10:50:49'!
contents: input notifying: aRequestor 
	"Compile the code in aString. Notify aRequestor of any syntax errors. 
	Create an error if the category of the selected message is unknown. 
	Answer false if the compilation fails. Otherwise, if the compilation 
	created a new method, deselect the current selection. Then answer true."

	| codeString category selector class oldSelector |
	codeString := input string.
	(class := self selectedClassOrMetaClass) ifNil:
		[(myChangeSet preambleString == nil or: [codeString isEmpty]) ifTrue: [ ^ false].
		(codeString count: [:char | char == $"]) odd 
			ifTrue: [self inform: 'unmatched double quotes in preamble']
			ifFalse: [(Scanner new scanTokens: codeString) notEmpty ifTrue: [
				self inform: 'Part of the preamble is not within double-quotes.
To put a double-quote inside a comment, type two double-quotes in a row.
(Ignore this warning if you are including a doIt in the preamble.)']].
		myChangeSet preambleString: codeString.
		self currentSelector: nil.  "forces update with no 'unsaved chages' feedback"
		^ true].
	oldSelector := self selectedMessageName.
	category := class organization categoryOfElement: oldSelector.
	selector := class compile: codeString
				classified: category
				notifying: aRequestor.
	selector ifNil: [^ false].
	(self messageList includes: selector)
		ifTrue: [self currentSelector: selector]
		ifFalse: [self currentSelector: oldSelector].
	self update.
	^ true! !


!FileList methodsFor: 'volume list and pattern' stamp: 'jmv 3/28/2023 15:37:43'!
doesPattern: aPattern allow: entry 

	(aPattern = '*' or: [ aPattern match: entry name ]) ifFalse: [ ^false ].

	(showHidden not and: [ entry name first == $. ]) ifTrue: [ ^false ].

	"Hide Mac resurce forks and folder service stores"
	(entry name = '.DS_Store') ifTrue: [ ^false ].
	('._*' match: entry name) ifTrue: [ ^false ].
	^true! !

!FileList methodsFor: 'private' stamp: 'jmv 8/5/2024 16:44:13'!
updateFileList
	"Update my files list with file names in the current directory  
	that match the pattern.
	The pattern string may have embedded newlines or semicolons; these separate different patterns."
	| patterns patternsThatReject patternsThatSelect newList |
	patterns := OrderedCollection new.
	(pattern findTokens: (String with: Character cr with: Character lf with: $;))
		do: [ :each |
			(each includes: $*) | (each includes: $?)
				ifTrue: [ patterns add: (each copyReplaceAll: '?' with: '#')]
				ifFalse: [
					each isEmpty
						ifTrue: [ patterns add: '*']
						ifFalse: [ patterns add: '*' , each , '*']]].
	"A pattern that starts with $/ is used to reject entries
	Rejecting patterns are applied after selecting patterns."
	patternsThatSelect := patterns reject: [ :any | any first == $/ ].
	patternsThatSelect isEmpty ifTrue: [ patternsThatSelect add: '*' ]. 
	patternsThatReject := patterns select: [ :any | any first == $/ ] thenCollect: [ :each | each copyFrom: 2 to: each size ].
	newList := self listForSelectingPatterns: patternsThatSelect rejectingPatterns: patternsThatReject.

	newList = list ifFalse: [
		"If we had a selection, ensure it is valid, or discard it."
		listIndex = 0 ifFalse: [
			(listIndex > newList size or: [(list at: listIndex) ~= (newList at: listIndex)]) ifTrue: [
				listIndex := newList indexOf: (list at: listIndex) ]].
		list := newList.
		listIndex > 0 ifFalse: [ "Had no selection or it was discarded"
			fileName := nil.
			acceptedContentsCache := ''.
			self acceptedContentsChanged.
			self changed: #fileListIndex ].
		self changed: #fileList.
		self triggerEvent: #updateButtonRow ].! !


!DateAndTime class methodsFor: 'squeak protocol' stamp: 'jmv 4/21/2022 12:17:39'!
readFrom: aStream

	| offset date time ch |

	date := Date readFrom: aStream.
	[aStream peek isDigit]
		whileFalse: [aStream next].
	time := Time readFrom: aStream.
	aStream atEnd
		ifTrue: [ offset := self localOffset ]
		ifFalse: [
			ch := aStream next.
			ch == $+ ifTrue: [ch := Character space].
			offset := Duration fromString: (String with: ch), '0:', aStream upToEnd, ':0'].
		
	^ self
		year: date yearNumber
		month: date monthIndex
		day: date dayOfMonth
		hour: time hour
		minute: time minute
		second: time second
		nanoSecond: time nanoSecond
		offset: offset


	"
		DateAndTime readFrom: '-1199-01-05T20:33:14.321-05:00' readStream
		DateAndTime readFrom: ' 2002-05-16T17:20:45.1+01:01' readStream

		DateAndTime readFrom: ' 2002-05-16T17:20:45.02+01:01' readStream

		DateAndTime readFrom: ' 2002-05-16T17:20:45.003+01:01' readStream

		DateAndTime readFrom: ' 2002-05-16T17:20:45.0004+01:01' readStream
  		DateAndTime readFrom: ' 2002-05-16T17:20:45.00005' readStream
		DateAndTime readFrom: ' 2002-05-16T17:20:45.000006+01:01' readStream

		DateAndTime readFrom: ' 2002-05-16T17:20:45.0000007+01:01' readStream
		DateAndTime readFrom: ' 2002-05-16T17:20:45.00000008-01:01' readStream
		DateAndTime readFrom: ' 2002-05-16T17:20:45.000000009+01:01' readStream
		DateAndTime readFrom: ' 2002-05-16T17:20:45.0000000001+01:01' readStream

 		DateAndTime readFrom: ' 2002-05-16T17:20' readStream
		DateAndTime readFrom: ' 2002-05-16T17:20:45' readStream
		DateAndTime readFrom: ' 2002-05-16T17:20:45+01:57' readStream
 		DateAndTime readFrom: ' 2002-05-16T17:20:45-02:34' readStream
 		DateAndTime readFrom: ' 2002-05-16T17:20:45+00:00' readStream
		DateAndTime readFrom: ' 1997-04-26T01:02:03+01:02:3' readStream
 	"! !


!Time class methodsFor: 'smalltalk-80' stamp: 'jmv 11/15/2022 10:33:12'!
readFrom: aStream
	"Read a Time from the stream in the form:
		<hour>:<minute>:<second> <am/pm>

	<minute>, <second> or <am/pm> may be omitted.  e.g. 1:59:30 pm; 8AM; 15:30"

	| hour minute second nanos ampm |
	hour := Integer readFrom: aStream.
	minute := 0.
	second := 0.
	nanos := 0.
	(aStream peekFor: $:) 
		ifTrue: [
			minute := Integer readFrom: aStream.
			(aStream peekFor: $:) ifTrue: [
				second := Integer readFrom: aStream.
				(aStream peekFor: $.)
					ifTrue: [ | nanosStart scale |
						nanosStart := aStream position.
						nanos := Integer readFrom: aStream.
						"As if there were trailing zeros to fill 9 positions"
						scale := 10 raisedToInteger: nanosStart + 9 - aStream position.
						nanos := nanos * scale ]]].
	aStream skipSeparators.
	(aStream atEnd not and: [aStream peek isLetter]) ifTrue: [
		ampm := aStream next asLowercase.
	
	(ampm == $p and: [hour < 12]) ifTrue: [hour := hour + 12].
		(ampm == $a and: [hour = 12]) ifTrue: [hour := 0].
	
	(aStream peekFor: $m) ifFalse: [aStream peekFor: $M ]].
	^ self 
		hour: hour 
		minute: minute 
		second: second 
		nanoSecond: nanos.

	"
	Time readFrom: '2:23:09 pm' readStream
	Time readFrom: '2:23:09.0 pm' readStream
	Time readFrom: '2:23:09.123402 pm' readStream
	Time readFrom: '2:23:09.00123402 pm' readStream
	Time readFrom: '2:23:09.001234020 pm' readStream
	Time readFrom: '2:23:09.000123402 pm' readStream
	"! !


!Number class methodsFor: 'instance creation' stamp: 'jmv 6/6/2022 10:59:12'!
readFrom: stringOrStream 
	"Answer a number as described on aStream.  The number may
	include a leading radix specification, as in 16rFADE"
	| value base aStream sign |
	aStream := stringOrStream isString
		ifTrue: [stringOrStream readStream]
		ifFalse: [stringOrStream].
	(aStream nextMatchAll: 'NaN') ifTrue: [^ Float nan].
	sign := (aStream peekFor: $-) ifTrue: [-1] ifFalse: [aStream peekFor: $+. 1].
	(aStream nextMatchAll: 'Infinity') ifTrue: [^ Float infinity * sign].
	base := 10.
	aStream peek == $.
		ifTrue: [ value := 0 ]
		ifFalse: [
			value := Integer readFrom: aStream base: base.
			(aStream peekFor: $r)
				ifTrue: [
					"<base>r<integer>"
					(base := value) < 2 ifTrue: [
						base = 1 ifTrue: [ ^Integer readBaseOneFrom: aStream ].
						^self error: 'Invalid radix'].
					(aStream peekFor: $-) ifTrue: [sign := sign negated].
					value := Integer readFrom: aStream base: base].
			(aStream peekFor: $x)
				ifTrue: [
					"0x<integer>" "Hexadecimal"
					base := 16.
					(aStream peekFor: $-) ifTrue: [sign := sign negated].
					value := Integer readFrom: aStream base: base]].
	^ self readRemainderOf: value from: aStream base: base withSign: sign.! !

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
					scale := (peekChar == $p ifTrue: [2] ifFalse: [base]) raisedToInteger: exp.
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


!MessageSend methodsFor: 'evaluating' stamp: 'jmv 5/15/2024 15:51:16'!
valueWithPossibleArgument: anObject

	^selector last == $:
		ifFalse: [ receiver perform: selector ]
		ifTrue: [ receiver perform: selector with: anObject ]! !


!Color class methodsFor: 'instance creation' stamp: 'jmv 8/3/2020 17:30:42'!
fromHexString: aString
	"For HTML color spec: #FFCCAA.
	See http://www.w3schools.com/cssref/css_colors_legal.asp
	Also handles 3 digit shorthand."
	"
	Color fromHexString: '#FFCCAA'.
	"
	(aString size = 7 and: [ aString first == $# ]) ifTrue: [
		| aColorHexU red green blue |
		aColorHexU := aString asUppercase.
		red := ('16r', (aColorHexU copyFrom: 2 to: 3)) asNumber/255.
		green := ('16r', (aColorHexU copyFrom: 4 to: 5)) asNumber/255.
		blue := ('16r', (aColorHexU copyFrom: 6 to: 7)) asNumber/255.
		^ self r: red g: green b: blue].
	(aString size = 4 and: [ aString first == $# ]) ifTrue: [
		| aColorHexU red green blue |
		aColorHexU := aString asUppercase.
		red := aColorHexU at: 2.
		green := aColorHexU at: 3.
		blue := aColorHexU at: 4.
		red := ('16r', (String with: red with: red)) asNumber/255.
		green := ('16r', (String with: green with: green)) asNumber/255.
		blue := ('16r', (String with: blue with: blue)) asNumber/255.
		^ self r: red g: green b: blue].
	^ nil! !


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
	[ char == $x ] whileFalse: [
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


!EventSensor methodsFor: 'test' stamp: 'jmv 3/8/2023 10:26:28'!
test
	"
	Sensor test

Also useful:
	Sensor kbdTest
	"
	| c lastStamp stamp |
	c := nil.
	lastStamp := 0.
	[ c == $x asciiValue ] whileFalse: [
		Sensor nextEvent ifNotNil: [ :evt |
			stamp := evt at: 2.
			stamp - lastStamp > 300 ifTrue: [ ' ' print.' ' print ].
			Transcript show: evt; newLine.
			self printEventBuffer: evt.
			(evt first = 2 and: [ evt fourth = 0 ]) ifTrue: [
				c := evt third ].
			lastStamp := stamp]]! !


!Character methodsFor: 'testing' stamp: 'jmv 1/9/2024 11:39:35'!
isValidInIdentifiers
	"Can I be part of an identifier? (unary or keyword selector, or variable name)"

	self isNullCharacter ifTrue: [ ^false ].
	self == $′ ifTrue: [ ^true ]. 		"Also see #xLetter"
	^#(xLetter xDigit xUnderscore xDiacritical) statePointsTo: (Scanner typeTableAt: self).! !

!Character methodsFor: 'testing' stamp: 'jmv 11/16/2015 14:51'!
tokenish
	"Answer whether the receiver is a valid token-character--letter, digit, or 
	colon."

	^self isValidInIdentifiers or: [self == $:]! !

!Character methodsFor: 'converting' stamp: 'jmv 7/5/2024 17:17:06'!
withDiacriticalMark: codePointOrCharacter
	"Answer the character resulting from adding a diacritical mark (accent) to a letter.
	If the result is unsupported in ISO 8859-1, answer the receiver.
	Supported diacritical marks are:
		U+0300 	COMBINING GRAVE ACCENT
		U+0301 	COMBINING ACUTE ACCENT
		U+0302 	COMBINING CIRCUMFLEX ACCENT
		U+0303 	COMBINING TILDE
		U+0308 	COMBINING DIAERESIS
		U+030A 	COMBINING RING ABOVE
		U+030C 	COMBINING CARON
		$a withDiacriticalMark: 16r301
		$N withDiacriticalMark: $~
		$Z withDiacriticalMark: $v
		
		invalid:
		$9 withDiacriticalMark:$v
		$A withDiacriticalMark: $v
		$Á withDiacriticalMark: $v
		$A withDiacriticalMark: 1244
		"
	| answer i |
	i := ((codePointOrCharacter isNumber
			ifTrue: [#(16r300 16r301 16r302 16r303 16r308 16r30A 16r30C)]
			ifFalse: [#($` $' $^ $~ $" $° $v)]) indexOf: codePointOrCharacter
				) + 1.
	answer := (Character accentedLetters detect: [ :group | group first = self ] ifNone: [ ^self ]) at: i.
	^answer == $- ifFalse: [answer] ifTrue: [self]! !

!Character methodsFor: 'fileman-testing' stamp: 'jmv 5/21/2022 22:29:28'!
isDriveSeparator
	^self == $:
	! !


!Character class methodsFor: 'class initialization' stamp: 'jmv 10/4/2024 11:13:02'!
initialize
	"https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt
	http://www.unicode.org/L2/L1999/UnicodeData.html
	Character initialize
	"

	| fields codePoint codePointName generalCategory canonicalCombiningClass characterDecompositionMapping decomposition composition |
	UppercaseLettersTable := ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	LowercaseLettersTable := ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	SymbolsLikeTable := ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	DiacriticalMarksTable := ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	CanonicalCombiningClasses := Dictionary new.
	DecompositionMapping := Dictionary new.
	CompositionMapping := Dictionary new.
	(DirectoryEntry smalltalkImageDirectory // 'UnicodeData.txt') readStreamDo: [ :strm |
		[strm atEnd] whileFalse: [
			fields := strm nextLine substringsSeparatedBy: $;.
			(#(4 5 6) includes: fields first size) ifFalse: [ fields first print. self error: 'Bad UnicodeData.txt file' ].
			codePoint := ('16r', fields first) asNumber.
			codePointName := fields at: 2.
			generalCategory := fields at: 3.
			codePoint <= 16r1FFFF ifTrue: [
				({ 'Lu'. 'Lt' } includes: generalCategory) ifTrue: [ UppercaseLettersTable bitBooleanAt: codePoint+1 put: true ].
				({ 'Ll'. 'Lo' } includes: generalCategory) ifTrue: [ LowercaseLettersTable bitBooleanAt: codePoint+1 put: true ].
				({ 'Pc'. 'Pd'. 'Po'. 'Sm'. 'Sc'. 'Sk'. 'So' } includes: generalCategory) ifTrue: [ SymbolsLikeTable bitBooleanAt: codePoint+1 put: true ].
				({ 'Mn'. 'Mc'. 'Me' } includes: generalCategory) ifTrue: [ DiacriticalMarksTable bitBooleanAt: codePoint+1 put: true ]].
			canonicalCombiningClass := (fields at: 4) asNumber.
			canonicalCombiningClass = 0 ifFalse: [
				CanonicalCombiningClasses at: codePoint put: canonicalCombiningClass ].
			characterDecompositionMapping := fields at: 6.
			(characterDecompositionMapping size > 0 and: [ characterDecompositionMapping first ~~ $< ]) ifTrue: [
				decomposition := characterDecompositionMapping substrings collect: [ :s | ('16r', s) asNumber].
				DecompositionMapping at: codePoint put: decomposition.
				decomposition size > 1 ifTrue: [
					composition := CompositionMapping at: decomposition first ifAbsentPut: [Dictionary new].
					composition at: decomposition second put: codePoint.
					decomposition size > 2 ifTrue: [self error: decomposition printString print].
					]].
			"
			uppercaseMapping := fields at: 13. lowercaseMapping := fields at: 14. titlecaseMapping := fields at: 15.
			Random next > 10.999 | (generalCategory = 'So') ifTrue: [
				{codePoint hex. codePointName. generalCategory} print ].
			decimalDigitValue := fields at: 7. digitValue := fields at: 8. numericValue := fields at: 9.
			decimalDigitValue isEmpty & digitValue isEmpty & numericValue isEmpty ifFalse: [
				{'numeric'. codePoint. codePointName. generalCategory. decimalDigitValue. digitValue. numericValue} print].
			"
			]].
	self initializeNamedCharactersMap.! !

!Character class methodsFor: 'instance creation' stamp: 'jmv 1/8/2024 12:13:35'!
codePoint: anInteger
	"Answer the Character whose value is anInteger.
	
	self assert: (Character codePoint: 16r41) == $A.
	self assert: (Character codePoint: 16r20AC) == $€.
	"

	<primitive: 170>
	^self primitiveFailed! !

!Character class methodsFor: 'static state' stamp: 'jmv 7/25/2024 14:36:48'!
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
		(group first == $ÿ or: [group first == $ß])
			ifFalse: [ | lowercaseChar uppercaseChar |
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
			ifTrue: [ | lowercaseChar |
				lowercase := group first codePoint.
				lowercaseChar := self codePoint: lowercase.
				lowercaseMappingTable
					at: lowercase + 1 put: lowercaseChar.
				uppercaseMappingTable
					at: lowercase + 1 put: lowercaseChar.
				group size > 1
					ifTrue: [|unaccentedLowercase|
						unaccentedLowercase := group second codePoint.
						unaccentedTable
							at: lowercase+1 put: unaccentedLowercase]]].
	^{ lowercaseMappingTable. uppercaseMappingTable. unaccentedTable}.! !


!KeyboardEvent methodsFor: 'testing' stamp: 'jmv 7/3/2023 12:18:01'!
isCloseWindowShortcut

	self flag: #definesKeyboardShortcuts.
	^ (self commandAltKeyPressed or: [ self controlKeyPressed ])
		and: [self keyCharacter == $w].! !

!KeyboardEvent methodsFor: 'testing' stamp: 'HAW 9/23/2018 21:04:09'!
isColon

	^ self keyCharacter == $:.! !

!KeyboardEvent methodsFor: 'testing' stamp: 'db 7/1/2020 17:09:47'!
isQuestionMark
	
	^ self keyCharacter == $? ! !

!KeyboardEvent methodsFor: 'platform peculiarities' stamp: 'jmv 12/29/2011 10:52'!
isCmdAltGreaterThan
	"Answer true if the user pressed cmd/Alt + $>
	We need this special method because of platform differences, especially the weird way the Mac VM reports keystrokes if cmd or control are held down (the character answered is usually the unshifted one, even if shift is pressed, depending on keyboard layout!!)
	Note: On the Mac, this code handles US, German and Spanish layouts. Add more as needed...
	Tweak both #isCmdAltLessThan and #isCmdAltGreaterThan !!"
	| char shifted |

	self commandAltKeyPressed ifFalse: [ ^false ].

	char := self keyCharacter.
	"Do the easy test for non macOS"
	Smalltalk platformName = 'Mac OS' ifFalse: [
		^char == $> ].

	shifted := self shiftPressed.
	
	"For Mac keyboard layouts where $> is unshifted (Is there such a layout?)"
	(shifted not and: [ char == $> ])
		ifTrue: [ ^true ].

	"For Mac keyboard layouts where $> is shift + $. (for example US).
	As cmd is pressed, the Mac VM gives us the unshifted character, i.e. $."
	(shifted and: [ char == $. ])
		ifTrue: [ ^true ].

	"For Mac keyboard layouts where $> is shift + $<.
	In this case, in the German layout, the Mac VM gives us $> (surprising!!)"
	(shifted and: [ char == $> ])
		ifTrue: [ ^true ].

	"For Mac keyboard layouts where $> is shift + $<
	In the Spanish layout, as cmd is pressed, the Mac VM gives us the unshifted character, i.e. $<"
	(shifted and: [ char == $< ])
		ifTrue: [ ^true ].

	"We guess it is not cmd + $> then..."
	^false! !

!KeyboardEvent methodsFor: 'platform peculiarities' stamp: 'jmv 12/29/2011 10:52'!
isCmdAltLessThan
	"Answer true if the user pressed cmd/Alt + $<
	We need this special method because of platform differences, especially the weird way the Mac VM reports keystrokes if cmd or control are held down (the character answered is usually the unshifted one, even if shift is pressed, depending on keyboard layout!!)
	Note: On the Mac, this code handles US, German and Spanish layouts. Add more as needed...
	Tweak both #isCmdAltLessThan and #isCmdAltGreaterThan !!"
	| char shifted |

	self commandAltKeyPressed ifFalse: [ ^false ].

	char := self keyCharacter.
	"Do the easy test for non macOS"
	Smalltalk platformName = 'Mac OS' ifFalse: [
		^char == $< ].

	shifted := self shiftPressed.
	
	"For Mac keyboard layouts where $< is unshifted (for example, German and Spanish)"
	(shifted not and: [ char == $< ])
		ifTrue: [ ^true ].

	"For Mac keyboard layouts where $< is shift + $, (for example US).
	As cmd is pressed, the Mac VM gives us the unshifted character, i.e. $,"
	(shifted and: [ char == $, ])
		ifTrue: [ ^true ].

	"We guess it is not cmd + $< then..."
	^false! !


!Morph methodsFor: 'events-processing' stamp: 'jmv 12/21/2018 09:21:39'!
focusKeyboardFor: aKeyboardEvent

	"If aKeyboardEvent ctrl-tab or shift-ctrl-tab use it to navigate keyboard focus.
	Warning: This doesn't work on Windows... the event is not sent"
	(aKeyboardEvent keyValue = 9 and: [ aKeyboardEvent controlKeyPressed and: [ aKeyboardEvent rawMacOptionKeyPressed not ]])
		ifTrue: [
			aKeyboardEvent shiftPressed
				ifTrue: [ aKeyboardEvent hand keyboardFocusPrevious ]
				ifFalse: [ aKeyboardEvent hand keyboardFocusNext ].
			^ true ].
	"On Windows use at least some keystroke to navigate morphs... even shift-Tab that should navigate backwards"
"
	(aKeyboardEvent keyValue = 9 and: [ aKeyboardEvent shiftPressed and: [ aKeyboardEvent rawMacOptionKeyPressed not ]])
		ifTrue: [
			aKeyboardEvent hand keyboardFocusNext.
			^ true ].
"

	"Cycle through windows with cmdAlt + < and cmdAlt + >.
	VM and platform peculiarities are hidden in #isCmdAltLessThan and #isCmdAltGreaterThan"
	"This was done as an attempt to mimic the Mac OSX keystrokes for 'Move focus to next window in active application'. Unfortunately, it only works if OS X is set to use any other keys for this. If (as for example, with German defaults), OS-X uses these keystrokes, then they are not sent to the VM. This is a long standing issues in Chromium and PhotoShop, for example..."
	self disableCode: [
		aKeyboardEvent isCmdAltLessThan ifTrue: [
			aKeyboardEvent hand activatePreviousWindow.
			^true ].
		aKeyboardEvent isCmdAltGreaterThan ifTrue: [
			aKeyboardEvent hand activateNextWindow.
			^true ]].
	"Alternative for Mac OS-X: option-Tab and option-shift-Tab"
	(aKeyboardEvent keyValue = 9 and: [ aKeyboardEvent rawMacOptionKeyPressed ])
		ifTrue: [
			aKeyboardEvent shiftPressed
				ifTrue: [ aKeyboardEvent hand activatePreviousWindow ]
				ifFalse: [ aKeyboardEvent hand activateNextWindow ].
			^ true ].
	"Alternative for non-Mac OS-X: alt-< and alt->"
	(aKeyboardEvent commandAltKeyPressed and: [ aKeyboardEvent keyCharacter == $< ]) ifTrue: [
		aKeyboardEvent hand activatePreviousWindow.
		^true ].
	(aKeyboardEvent commandAltKeyPressed and: [ aKeyboardEvent keyCharacter == $> ]) ifTrue: [
		aKeyboardEvent hand activateNextWindow.
		^true ].
	^false! !

!Morph methodsFor: 'submorphs-menus' stamp: 'jmv 11/22/2024 11:35:30'!
getMenu
	"Answer the right-click (contextual) menu."
	"Spec for the menu and shortcut keys.
	Each entry is
		#(itemId 			'menu item label'    shortcut    receiverGetter    selector     iconOrNil(optional)    'helpText(optional)'    selectorArgument(optional))
		- itemId is a number (integer or float). It specifies the position of the item in the menu, and is also an id, allowing for overrides and deletions.
		- label is a string or symbol. Symbol means self updating menu entry. nil means just keyboard shortcut, no menu entry.
		- shortcut is a Character or nil
		- receiverGetter sent to the widget to get the actual receiver for selector.
			Typically #window, #model or #editor. If nil, receiver is the widget itself.
		- selector is #aSymbol. Usually no arguments, or one single argument in 8th entry. 
			If a Number (multiple of 1000), indicates a subMenu. ItemsIds must be in range. See BrowserWindow>>#classListMenuSpec.
			If nil, the menu entry is created but disabled. The keyboard shortcut is disabled too.
		- icon may be nil. It is also optional (but must be there if there is help text)
		- help text is optional
		- selectorArgument is optional. If selector takes an argument, and selectorArgument is not provided, the argument is self.
	Entries of size 1 mean 'remove menu item'.
	Entries of size 2 are separators.
	An entry of size 3 (usually with key 0 and at the first position), if present, tells a menu title, whether to #addStayUpIcons.
	"

	| menuNshortcutsSpecs itemSpec menu submenus |
	menuNshortcutsSpecs := self allMenuNshortcutsSpecs.
	menuNshortcutsSpecs ifEmpty: [ ^nil ].
	menu := MenuMorph new.
	submenus := nil.
	menuNshortcutsSpecs keys sorted do: [ :key |
		itemSpec := menuNshortcutsSpecs at: key.
		itemSpec size =1 ifTrue: [
				"Do nothing. This is a marker for removing a previously provided entry" ]
			ifFalse: [ itemSpec size = 2 ifTrue: [
				itemSpec second first == $X ifFalse: [ "Compatibility. Also a marker for removing previous entries."
					self addLineTo: menu fromSpec: itemSpec submenus: submenus ]]
			ifFalse: [ itemSpec size = 3 ifTrue: [
				self addTitleTo: menu fromSpec: itemSpec ]
			ifFalse: [ itemSpec fifth isNumber ifTrue: [
				submenus ifNil: [submenus := Dictionary new ].
				self addSubmenuTo: menu fromSpec: itemSpec submenus: submenus ]
			ifFalse: [
				self addItemTo: menu fromSpec: itemSpec submenus: submenus ]]]]].
	^ menu! !


!MenuItemMorph methodsFor: 'accessing' stamp: 'jmv 5/1/2024 15:54:13'!
contentsWithMarkers: aString inverse: invertMarkers
	"Set the menu item entry. Parse aString for embedded markers."

	| markerIndex marker |
	contentString := nil.	"get rid of old"
	self removeAllMorphs.	"get rid of old markers if updating"
	(aString notEmpty and: [aString first == $<]) 
		ifFalse: [^super contents: aString].
	markerIndex := aString indexOf: $>.
	markerIndex = 0 ifTrue: [^super contents: aString].
	marker := (aString copyFrom: 1 to: markerIndex) asLowercase.
	(#('<on>' '<off>' '<yes>' '<no>') includes: marker) 
		ifFalse: [^super contents: aString].
	contentString := aString.	"remember actual string"
	marker := ((marker = '<on>' or: [marker = '<yes>']) xor: invertMarkers)
				ifTrue: [self onImage]
				ifFalse: [self offImage].
	super contents: (aString copyFrom: markerIndex + 1 to: aString size).
	"And set the marker"
	marker := ImageMorph newWith: marker.
	self addMorphFront: marker position: 1@(self morphHeight +1- marker morphHeight // 2).
	marker lock.! !


!BrowserWindow class methodsFor: 'services' stamp: 'HAW 6/27/2020 19:48:57'!
classNamesFrom: potentialClassNames with: pattern matching: toMatch 

	^ (pattern last == $. or: [pattern last == $ ])
		ifTrue: [potentialClassNames select: [:className | className asLowercase = toMatch]]
		ifFalse: [potentialClassNames select: [:className | className includesSubstring: toMatch caseSensitive: false]]! !


!HierarchyBrowserWindow class methodsFor: 'examples' stamp: 'jmv 5/21/2022 22:33:45'!
example2
	"Put up a ClassListBrowser that shows all classes whose names start with the letter S"

	self forClassesNamed: (Smalltalk allClasses collect: [:c | c name] thenSelect: [:aName | aName first == $S]) title: 'All classes starting with S'

"HierarchyBrowserWindow example2"! !


!WorldMorph methodsFor: 'events' stamp: 'jmv 9/19/2024 12:36:07'!
keyStroke: aKeyboardEvent

	self flag: #definesKeyboardShortcuts.
	super keyStroke: aKeyboardEvent.
	aKeyboardEvent wasHandled ifTrue: [ ^self ].

	"Handle a keystroke event."
	(aKeyboardEvent commandAltKeyPressed or: [ aKeyboardEvent controlKeyPressed ])
		ifTrue: [
			aKeyboardEvent keyCharacter == $b ifTrue: [
				Smalltalk browseFull: nil.
				aKeyboardEvent wasHandled: true. ].
			aKeyboardEvent keyCharacter == $f ifTrue: [
				Smalltalk findAndBrowseClass.
				aKeyboardEvent wasHandled: true. ].
			aKeyboardEvent keyCharacter == $F ifTrue: [
				Smalltalk findInSourceCode.
				aKeyboardEvent wasHandled: true. ].
		].
	"aKeyboardEvent keyCharacter print."! !


!FileIOAccessor methodsFor: 'utilities' stamp: 'jmv 2/21/2024 15:37:28'!
expandTildeFor: aString

	aString size = 0 ifTrue: [ ^aString ].
	aString first == $~ifFalse: [ ^aString ].
	"Basic tilde expansion. Consider $~ to mean user's home folder, if possible.
	See, for example, https://www.gnu.org/software/bash/manual/html_node/Tilde-Expansion.html"
	Smalltalk at: #OSProcess
		ifPresent: [ :osProcess |
			(osProcess thisOSProcess environmentAt: 'HOME')
				ifNotNil: [ :homeVariable |
					^homeVariable, (aString copyFrom: 2 to: aString size) ]
				ifNil: [ self error: 'The $HOME variable could not be resolved' ]]
		ifAbsent: [
			self error: 'Use of tilde (~) to mean $HOME folder requires OSProcess' ]! !

!FileIOAccessor methodsFor: 'utilities' stamp: 'bp 5/10/2024 11:25:01'!
extensionFor: pathName 
	"In original FileMan, accepts only a localName (withouth path separators). Modify it for Cuis for also allowing them, as it is traditional in Squeak / Cuis.
	
	FileIOAccessor default extensionFor: 'writings.txt'
	FileIOAccessor default extensionFor: 'folder.ext/file'
	FileIOAccessor default extensionFor: 'optionalstuff.pck.st'
	FileIOAccessor default extensionFor: 'code.cs.st'
	FileIOAccessor default extensionFor: 'code.cs'
	"
	| index |
	{ '.cs.st' . '.pck.st'. '.ws.st' } do: [ :specialExtension |
		(pathName endsWith: specialExtension)
			ifTrue: [ ^specialExtension copyFrom: 2 to: specialExtension size ]].
	index := pathName
				findLast: [ :c | c == $.].
	^ (index = 0 or: [ pathName indexOfLastPathSeparator > index ])
		ifTrue: ['']
		ifFalse: [pathName copyFrom: index + 1 to: pathName size]! !

!FileIOAccessor methodsFor: 'utilities' stamp: 'jmv 12/20/2024 16:32:24'!
splitNameVersionExtensionFor: fileName
	" answer an array with the root name, version # and extension.
	See comment in senders for more details"

	self baseNameAndExtensionFor: fileName do: [ :b :extension |
		| baseName version i j |
		baseName := b.
		i := j := baseName findLast: [:c | c isDigit not].
		i = 0
			ifTrue: [version := 0]
			ifFalse: [
				(baseName at: i) == $.
					ifTrue: [
						version := (baseName copyFrom: i+1 to: baseName size) asNumber.
						j := j - 1]
					ifFalse: [version := 0].
				baseName := baseName copyFrom: 1 to: j ].
		^ Array with: baseName with: version with: extension ]! !

!FileIOAccessor methodsFor: 'testing' stamp: 'pb 5/25/2016 00:32'!
onUnix
	^self pathNameDelimiter == $/! !

!FileIOAccessor methodsFor: 'testing' stamp: 'pb 5/25/2016 00:32'!
onWindows
	^self pathNameDelimiter == $\! !


!DirectoryEntry methodsFor: 'testing' stamp: 'jmv 10/9/2019 11:28:39'!
isRegularDirectory
	"hidden convention in Unix"
	name first == $. ifTrue: [ ^false ].
	"in MacOS, applications are actually directories, but are usually not treated as such"
	self extension = 'app' ifTrue: [ ^false ].
	"in MacOS, .bundle directories, are resource packages"
	self extension = 'bundle' ifTrue: [ ^false ].
	^true! !


!CodePackage methodsFor: 'testing' stamp: 'jmv 3/16/2012 10:48'!
category: categoryName matches: prefix
	| prefixSize catSize |
	categoryName ifNil: [ ^false ].
	catSize := categoryName size.
	prefixSize := prefix size.
	catSize < prefixSize ifTrue: [ ^false ].
	(categoryName findString: prefix startingAt: 1 caseSensitive: false) = 1
		ifFalse: [ ^false ].
	^(categoryName at: prefix size + 1 ifAbsent: [ ^true ]) == $-! !

!CodePackage methodsFor: 'testing' stamp: 'jfr 8/3/2024 10:43:36'!
isForeignClassExtension: categoryName
	"Answer true if <categoryName> is a foreign class extension reference"
	
	categoryName ifNil: [ ^false ].
	(categoryName size = 0) ifTrue:[^false].
	^ categoryName first == $* and: [(self isYourClassExtension: categoryName) not]! !


!PreferenceSet class methodsFor: 'services' stamp: 'jmv 12/19/2024 12:47:39'!
readAndApplyUserPrefs
	"
	PreferenceSet readAndApplyUserPrefs
	"
	| file |
	file := DirectoryEntry userBaseDirectory // 'UserPrefs.txt'.
	file exists ifTrue: [
		file readStreamDo: [ :stream |
			[ stream atEnd ] whileFalse: [ | line parts |
				line := stream nextLine.
				parts := line substringsSeparatedBy: $=.
				[ | pref value |
					pref := Preferences instanceAt: parts first asSymbol.
					value := parts second.
					value first == $# ifTrue: [ value := (value withoutPrefix: '#') asSymbol ].
					pref value: value.
					pref justReadFromDisk.
				]
					on: Error
					do: [ :error |
						'--- UserPrefts.txt Error' print.
						error print.
						line print ].
				]]].! !


!Utilities class methodsFor: 'summer97 additions' stamp: 'jmv 9/27/2016 21:41:18'!
classFromPattern: pattern withCaption: aCaption
	"If there is a class whose name exactly given by pattern, return it.
	If there is only one class in the system whose name matches pattern, return it.
	Otherwise, put up a menu offering the names of all classes that match pattern, and return the class chosen, else nil if nothing chosen.
	This method ignores tab, space, & newline characters in the pattern"

	| toMatch potentialClassNames classNames exactMatch index |
	(toMatch :=  pattern copyWithoutAll:
			{Character space.  Character newLineCharacter.  Character tab})
		isEmpty ifTrue: [^ nil].
	Symbol hasInterned: toMatch ifTrue:
		[:patternSymbol | Smalltalk at: patternSymbol ifPresent:
			[:maybeClass | (maybeClass class isMeta) ifTrue: [^ maybeClass]]].

	toMatch := (toMatch copyWithout: $.) asLowercase.
	potentialClassNames := Smalltalk classNames asOrderedCollection.
	classNames := pattern last == $. 
		ifTrue: [potentialClassNames select:
					[:nm |  nm asLowercase = toMatch]]
		ifFalse: [potentialClassNames select: 
					[:n | n includesSubstring: toMatch caseSensitive: false]].
	classNames isEmpty ifTrue: [^ nil].
	exactMatch := classNames detect: [:each | each asLowercase = toMatch] ifNone: nil.

	index := classNames size = 1
		ifTrue:	[1]
		ifFalse:	[exactMatch
			ifNil: [(PopUpMenu labelArray: classNames lines: #()) startUpWithCaption: aCaption]
			ifNotNil: [classNames addFirst: exactMatch.
				(PopUpMenu labelArray: classNames lines: #(1)) startUpWithCaption: aCaption]].
	index = 0 ifTrue: [^ nil].
	^ Smalltalk at: (classNames at: index) asSymbol

"
	Utilities classFromPattern: 'CharRecog' withCaption: 'xx'
	Utilities classFromPattern: 'rRecog' withCaption: 'xx'
	Utilities classFromPattern: 'znak' withCaption: 'xx'
	Utilities classFromPattern: 'orph' withCaption: 'xx'
"
! !


!Editor methodsFor: 'new selection' stamp: 'HAW 10/28/2024 12:13:31'!
wordRangeIncluding: aPositionInText
	"Select delimited text or word--the result of double-clicking."

	| direction level string here hereChar start sSize |
	string := self privateCurrentString.
	sSize := string size.
	sSize < 2 ifTrue: [^1 to: 1].
	here := aPositionInText min: sSize max: 2.
	"no delimiters -- select a token"
	direction := -1.
	level := 1.
	[level > 0 and: [direction > 0
			ifTrue: [here < sSize]
			ifFalse: [here > 1]]]
		whileTrue: [
			hereChar := string at: (here := here + direction).
			"token scan goes left, then right"
			hereChar isValidInIdentifiers 	"Consider $: as a word separator"
				ifTrue: [
					here = 1
						ifTrue: [
							start := 1.
							"go right if hit string start"
							direction := 1]]
				ifFalse: [
					direction < 0
						ifTrue: [
							start := here + 1.
							"go right if hit non-token"
							direction := 1]
						ifFalse: [level := 0]]].
	level > 0 ifTrue: [
		here := here + direction ].
	"If a word ends with $: (a keyword), consider it part of the word"
	hereChar == $: ifTrue: [here := here + 1].
	^start to: here - 1! !

!Editor methodsFor: 'typing/selecting keys' stamp: 'jmv 11/26/2024 11:54:17'!
normalCharacter: aKeyboardEvent 
	"A nonspecial character is to be added to the stream of characters."

	| character i replacement |

	"Ignore keystrokes that could be keyboard shortcuts, even if not defined as such."
	((aKeyboardEvent keyValue between: 32 and: 126) and: [ aKeyboardEvent commandAltKeyPressed ])
		ifTrue: [ ^true ].

	"Convert strings like '\+161 ' (š) to unicode"
	aKeyboardEvent isSpace ifTrue: [ | string |
		string := self privateCurrentString.
		self pointIndex - 1 to: (self pointIndex - 7 max: 1) by: -1 do: [ :index |
		(string at: index) == $\ ifTrue: [ | key |
			key := string copyFrom: index + 1 to: self pointIndex - 1.
			(key size > 2 and: [ (key at: 1) == $+ ])
				ifTrue: [ | stream number value |
					stream := ReadStream on: key asUppercase from: 2 to: key size.
					[ number := Integer readFrom: stream  base: 16 ] on: Error do: [ ^ false ].
					value := Character codePoint: number.
					self
						selectFrom: index to: self pointIndex - 1;
						replaceSelectionWith: (UnicodeString with: value).
						^ false ] ] ] ].
	character := aKeyboardEvent keyCharacter.
	"Linux VMs don't automatically replace combining marks + space with ASCII,
	as Windows and Mac VMs do. It seems it is an app responsibility to do that in Linux."
	character isDiacriticalMark ifTrue: [
		i := self pointIndex-1.
		(i > 0 and: [ (self privateCurrentString at: i) == $  ]) ifTrue: [
			replacement := character caseOf: {
				[ `Character codePoint: 16r300` ] -> [ '`' ].
				[ `Character codePoint: 16r301` ] -> [ '''' ].
				[ `Character codePoint: 16r302` ] -> [ '^' ].
				[ `Character codePoint: 16r303` ] -> [ '~' ].
				[ `Character codePoint: 16r308` ] -> [ '"' ].
			} otherwise: [].
			replacement notNil ifTrue: [
				self selectFrom: i to: i; replaceSelectionWith: replacement.
				^ false ]
			]
		].
	self addString: (UnicodeString with: character).
	^false! !


!SmalltalkEditor methodsFor: 'editing keys' stamp: 'jmv 6/10/2024 17:24:53'!
encloseWith: aCharacter
	"Insert or remove bracket characters around the current selection."
	"This is a user command, and generates undo"

	| startIndex stopIndex oldSelection new enlargedBy |
	"This implementation is only for commenting and uncommenting code.
	Other cases are handled by the super implementation.
	This implementation only handles String and discards Text attributes."
	aCharacter == $" ifFalse: [
		^super encloseWith: aCharacter ].

	startIndex := self startIndex.
	stopIndex := self stopIndex.
	oldSelection := self selection string.
	((startIndex > 1 and: [stopIndex <= model textSize])
			and: [ (model actualContents at: startIndex-1) == $"
				and: [(model actualContents at: stopIndex) == $"]])
		ifTrue: [
			"already enclosed in double colons; make selection no longer a comment"
			self selectFrom: startIndex-1 to: stopIndex.
			self replaceSelectionWith: oldSelection withDoubleColonsUndoubled ]
		ifFalse: [
			"not enclosed in double colons; make selection a comment"
			new := oldSelection withDoubleColonsDoubled.
			enlargedBy := new size - oldSelection size.
			self replaceSelectionWith: $" asString, new, $" asString.
			self selectFrom: startIndex+1 to: stopIndex + enlargedBy ].
	^ true.! !

!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 9/17/2016 18:06:28'!
explainChar: string
	"Does string start with a special character?"

	| char |
	char := string at: 1.
	char == $. ifTrue: [^'Period marks the end of a Smalltalk statement.  A period in the middle of a number means a decimal point.  (The number is an instance of class Float).'].
	char == $' ifTrue: [^'The characters between two single quotes are made into an instance of class String'].
	char == $" ifTrue: [^'Double quotes enclose a comment.  Smalltalk ignores everything between double quotes.'].
	char == $# ifTrue: [^'The characters following a hash mark are made into an instance of class Symbol.  If parenthesis follow a hash mark, an instance of class Array is made.  It contains literal constants.'].
	(char == $( or: [char == $)]) ifTrue: [^'Expressions enclosed in parenthesis are evaluated first'].
	(char == $[ or: [char == $]]) ifTrue: [^'The code inside square brackets is an unevaluated block of code.  It becomes an instance of BlockClosure and is usually passed as an argument.'].
	(char == ${ or: [char == $}]) ifTrue: [^ 'A sequence of expressions separated by periods, when enclosed in curly braces, are evaluated to yield the elements of a new Array'].
	(char == $< or: [char == $>]) ifTrue: [^'<primitive: xx> means that this method is usually preformed directly by the virtual machine.  If this method is primitive, its Smalltalk code is executed only when the primitive fails.'].
	char == $^ ifTrue: [^'Up arrow means return from this method.  The value returned is the expression following the ^'].
	char == $| ifTrue: [^'Vertical bars enclose the names of the temporary variables used in this method.  In a block, the vertical bar separates the argument names from the rest of the code.'].
	char == $_ ifTrue: [^'Left arrow means assignment.  The value of the expression after the left arrow is stored into the variable before it.'].
	char == $; ifTrue: [^'Semicolon means cascading.  The message after the semicolon is sent to the same object which received the message before the semicolon.'].
	char == $: ifTrue: [^'A colon at the end of a keyword means that an argument is expected to follow.  Methods which take more than one argument have selectors with more than one keyword.  (One keyword, ending with a colon, appears before each argument).', '\\' withNewLines, 'A colon before a variable name just inside a block means that the block takes an agrument.  (When the block is evaluated, the argument will be assigned to the variable whose name appears after the colon).'].
	char == $$ ifTrue: [^'The single character following a dollar sign is made into an instance of class Character'].
	char == $- ifTrue: [^'A minus sign in front of a number means a negative number.'].
	char == $e ifTrue: [^'An e in the middle of a number means that the exponent follows.'].
	char == $r ifTrue: [^'An r in the middle of a bunch of digits is an instance of Integer expressed in a certain radix.  The digits before the r denote the base and the digits after it express a number in that base.'].
	char = Character space ifTrue: [^'the space Character'].
	char = Character tab ifTrue: [^'the tab Character'].
	char = Character cr ifTrue: [^'the carriage return Character'].
	char = Character lf ifTrue: [^'newline. The line feed Character'].
	^nil! !

!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 9/22/2024 22:18:22'!
explainDelimiter: string
	"Is string enclosed in delimiters?"

	| str |
	(string at: 1) isLetter ifTrue: [^nil].  "only special chars"
	(string first = string last) ifTrue:
			[^ self explainChar: (String with: string first)]
		ifFalse:
			[(string first == $( and: [string last == $)]) ifTrue:
				[^ self explainChar: (String with: string first)].
			(string first == $[ and: [string last == $]]) ifTrue:
				[^ self explainChar: (String with: string first)].
			(string first == ${ and: [string last == $}]) ifTrue:
				[^ self explainChar: (String with: string first)].
			(string first == $< and: [string last == $>]) ifTrue:
				[^ self explainChar: (String with: string first)].
			(string first == $# and: [string last == $)]) ifTrue:
				[^'An instance of class Array.  The Numbers, Characters, or Symbols between the parenthesis are the elements of the Array.'].
			string first == $# ifTrue:
				[^'An instance of class Symbol.'].
			(string first == $$ and: [string size = 2]) ifTrue:
				[^'An instance of class Character.  This one is the character ', (String with: string last), '.'].
			(string first == $:) ifTrue:
				[str := string allButFirst.
				(self explainTemp: str) ifNotNil: [
					^'An argument to this block will be bound to the temporary variable ',
						str, '.']]].
	^ nil! !

!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 5/22/2022 20:43:16'!
explainNumber: string 
	"Is string a Number?"

	| strm c |
	(c := string at: 1) isDigit ifFalse: [(c == $- and: [string size > 1 and: [(string at: 2) isDigit]])
			ifFalse: [^nil]].
	strm := string readStream.
	c := Number readFrom: strm.
	strm atEnd ifFalse: [ ^nil ].
	c printString = string
		ifTrue: [ ^ string , ' is a ' , c class name ]
		ifFalse: [ ^ string , ' (= ' , c printString , ') is a ' , c class name ]! !

!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 11/26/2024 10:25:33'!
explainPartSel: string 
	"Is this a fragment of a multiple-argument selector sent in this method?"
	| lits whole reply classes s msg |
	(msg := self codeProvider selectedMessageName) ifNil: [^ nil].  "not in a message"
	string last == $: ifFalse: [^ nil].
	"Name of this method"
	lits := Array with: msg.
	(whole := lits detect: [:each | (each keywords detect: [:frag | frag = string]
					ifNone: nil) notNil]
				ifNone: nil)
		ifNotNil: [
			reply := ', which is the selector of this very method!!'.
			s := self class plateA]
		ifNil: [ 
			"Selectors called from this method"
			lits := (self codeProvider selectedClassOrMetaClass compiledMethodAt: msg) messages.
			(whole := lits detect: [:each | (each keywords detect: [ :frag | frag = string ]
							ifNone: nil) notNil]
						ifNone: nil) notNil
				ifFalse: [string = 'primitive:'
					ifTrue: [^self explainChar: '<']
					ifFalse: [^nil]].
			reply := '.'.
			s := self class plateB].
		classes := Smalltalk allClassesImplementing: whole.
		classes size > 12
			ifTrue: [classes := 'many classes']
			ifFalse: [classes := 'these classes ' , classes printString].
		^ String streamContents: [:str |
			str
				nextPutAll: string;
				nextPutAll: ' is one part of the message selector ';
				nextPutAll: whole;
				nextPutAll: reply;
				nextPutAll: '  It is defined in ';
				nextPutAll: classes;
				nextPutAll: s]! !

!SmalltalkEditor methodsFor: 'typing/selecting keys' stamp: 'jmv 6/10/2024 16:45:42'!
argPrev
	"Invoked by cmd-Q.
	 Search backwards from the start of the selection for a colon followed by
		a space.  Place the text cursor after the space.  If none are found, place the
		text cursor at the start of the text.."

	| t i |
	t := model actualContents.
	i := self stopIndex.
	i > 1 ifTrue: [
		i := i -2.
		[i > 0 and: [ (t at: i) ~~ $  or: [(t at: i-1) ~~ $: ]]] whileTrue: [
			i := i -1 ].
		self deselectAndPlaceCursorAt: i + 1.
	].
	^true! !

!SmalltalkEditor methodsFor: 'typing/selecting keys' stamp: 'jmv 12/7/2022 09:05:55'!
newLine: aKeyboardEvent
	"Replace the current text selection with a newLine (i.e. LF) followed by as many tabs
	as there are leading tabs on the current line (+/- bracket count)."

	| char s i tabCount stopIndex newLineString |
	s := self privateCurrentString.
	stopIndex := self stopIndex.
	i := stopIndex.
	tabCount := 0.
	[ (i := i-1) > 0 and: [ (char := s at: i) isLineSeparator not ] ] whileTrue: [
		"Count brackets"
		char == $[ ifTrue: [tabCount := tabCount + 1].
		char == $] ifTrue: [tabCount := tabCount - 1]].
	[ (i := i + 1) < stopIndex and: [ (char := s at: i) isSeparator ] ] whileTrue: [
		"Count leading tabs"
		char = Character tab ifTrue: [ tabCount := tabCount + 1 ]].
	"Now inject newline with tabCount tabs, generating a new undoable command"
	newLineString := UnicodeString streamContents: [ :strm | strm newLineTab: tabCount ].
	model startNewUndoRedoCommand.
	self replaceSelectionWith: newLineString.
	^ false! !

!SmalltalkEditor methodsFor: 'typing/selecting keys' stamp: 'jmv 10/22/2024 10:56:14'!
normalCharacter: aKeyboardEvent 
	"A nonspecial character is to be added to the stream of characters."

	| string key |

	"To enable:
	Preferences name: #autoReplaceNamedCharacters category: #gui value: true.
	Preferences saveToDisk: #autoReplaceNamedCharacters.
	PreferenceSet saveUserPrefs.
	"
	"To disable:
	Preferences name: #autoReplaceNamedCharacters category: #gui value: false.
	Preferences saveToDisk: #autoReplaceNamedCharacters.
	PreferenceSet saveUserPrefs.
	"
	"Allow to enter named (otherwise untypable) characters like the alpha greek letter as \alpha."
	(Preferences at: #autoReplaceNamedCharacters) ifTrue: [
		aKeyboardEvent keyCharacter isLetter ifFalse: [
			string := self privateCurrentString.
			"Look backwards and find a character that is not a letter (we want to find '\' just a few characters behind):"
			self pointIndex - 1 to: (self pointIndex - 20 max: 1) by: -1 do: [:i|
				(string at: i) == $\ ifTrue: [
					key := string copyFrom: i + 1 to: self pointIndex - 1.
					Character namedCharactersMap at: key ifPresent: [ :value |
						self selectFrom: i to: self pointIndex - 1; replaceSelectionWith: (UnicodeString with: value) ].
					^ super normalCharacter: aKeyboardEvent]]]].

	^ super normalCharacter: aKeyboardEvent! !


!TextAction methodsFor: 'as yet unclassified' stamp: 'tk 12/16/97 16:44'!
analyze: aString
	"Analyze the selected text to find both the parameter to store and the text to emphesize (may be different from original selection).  Does not return self!!.  May be of the form:
3+4
<3+4>
Click Here<3+4>
<3+4>Click Here
"
	"Obtain the showing text and the instructions"
	| b1 b2 trim param show |
	b1 := aString indexOf: $<.
	b2 := aString indexOf: $>.
	(b1 < b2) & (b1 > 0) ifFalse: ["only one part"
		param := self validate: aString.
		^ Array with: param with: (param size = 0 ifTrue: [nil] ifFalse: [param])].
	"Two parts"
	trim := aString withBlanksTrimmed.
	trim first == $< 
		ifTrue: [ trim last == $>
			ifTrue: ["only instructions" 
				param := self validate: (aString copyFrom: b1+1 to: b2-1).
				show := param size = 0 ifTrue: [nil] ifFalse: [param]]
			ifFalse: ["at the front"
				param := self validate: (aString copyFrom: b1+1 to: b2-1).
				show := param size = 0 ifTrue: [nil] 
						ifFalse: [aString copyFrom: b2+1 to: aString size]]]
		ifFalse: [ trim last == $>
			ifTrue: ["at the end"
				param := self validate: (aString copyFrom: b1+1 to: b2-1).
				show := param size = 0 ifTrue: [nil] 
						ifFalse: [aString copyFrom: 1 to: b1-1]]
			ifFalse: ["Illegal -- <> has text on both sides"
				show := nil]].
	^ Array with: param with: show
! !


!AutoCompleter methodsFor: 'keyboard - private' stamp: 'jmv 10/22/2024 16:38:26'!
lastCharacterWantsAutoComplete

	| lastCharIndex lastCharacter |
	textMorph editor hasSelection ifTrue: [ ^ false ].

	lastCharIndex := textMorph editor startIndex-1.
	lastCharIndex <= 0 ifTrue: [ ^ false ].
	lastCharacter := model actualContents at: lastCharIndex.

	^ lastCharacter isAlphaNumeric or: [
		lastCharacter isValidInBinarySelectors or: [ lastCharacter == $: ]]! !

!AutoCompleter methodsFor: 'entries' stamp: 'HAW 2/8/2019 17:27:05'!
insertSelected
	
	| entry editor selEnd str |
	
	textMorph ifNil: [ ^false ].
	
	entry := self selectedEntryFormatted.
	(self canSelect: entry) ifFalse: [ ^true ].
	
	editor := textMorph editor.
	str := model actualContents string.
	selEnd := position.
	[selEnd < str size and: [ (str at: selEnd+1) tokenish ]] whileTrue: [ selEnd := selEnd + 1 ].
	(selEnd < str size and: [ (str at: selEnd+1) == $ ]) ifTrue: [ selEnd := selEnd + 1].
	editor selectFrom: position-prefix size+1 to: selEnd.
	editor
		replaceSelectionWith: entry;
		deselectAndPlaceCursorAt: position - prefix size + 1 + (self newCursorPosition: entry).
	textMorph redrawNeeded.
	menuMorph delete.
	menuMorph := nil.
	
	^ true! !


!MethodReference methodsFor: 'testing' stamp: 'HAW 6/11/2022 20:04:20'!
isMessageListTitle

	^(self actualClass isKindOf: Object) and: [methodSymbol first == $_]
! !


!ChangeSet methodsFor: 'class changes' stamp: 'jmv 6/1/2022 11:07:41'!
fatDefForClass: class

	| newDef oldDef oldStrm newStrm outStrm oldVars newVars addedVars |
	newDef := class definition.
	oldDef := (self changeRecorderFor: class) priorDefinition.
	oldDef ifNil: [^ newDef].
	oldDef = newDef ifTrue: [^ newDef].

	oldStrm := oldDef readStream.
	newStrm := newDef readStream.
	outStrm := String writeStream.

	"Merge inst vars from old and new defs..."
	oldStrm upToAll: 'instanceVariableNames'; upTo: $'.
	outStrm 
		nextPutAll: (newStrm upToAll: 'instanceVariableNames'); 
		nextPutAll: 'instanceVariableNames:'.
	newStrm peek == $: ifTrue: [newStrm next].	"may or may not be there, but already written"
	outStrm
		nextPutAll: (newStrm upTo: $'); nextPut: $'.
	oldVars := (oldStrm upTo: $') findTokens: Character separators.
	newVars := (newStrm upTo: $') findTokens: Character separators.
	addedVars := oldVars asSet addAll: newVars; removeAll: oldVars; asOrderedCollection.
	oldVars , addedVars do: [:var | outStrm nextPutAll: var; space].
	outStrm nextPut: $'.

	class isMeta ifFalse: [
		"Merge class vars from old and new defs..."
		oldStrm upToAll: 'classVariableNames:'; upTo: $'.
		outStrm nextPutAll: (newStrm upToAll: 'classVariableNames:'); nextPutAll: 'classVariableNames:';
			nextPutAll: (newStrm upTo: $'); nextPut: $'.
		oldVars := (oldStrm upTo: $') findTokens: Character separators.
		newVars := (newStrm upTo: $') findTokens: Character separators.
		addedVars := oldVars asSet addAll: newVars; removeAll: oldVars; asOrderedCollection.
		oldVars , addedVars do: [:var | outStrm nextPutAll: var; space].
		outStrm nextPut: $'].

	outStrm nextPutAll: newStrm upToEnd.
	^ outStrm contents
! !


!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 9/22/2021 15:42:21'!
removeDotsAt: anInterval in: sourceCode
	
	| lastToReplace |
	
	lastToReplace := anInterval last.
	[(sourceCode at: lastToReplace) == $. ] whileTrue: [ lastToReplace := lastToReplace - 1].
	
	^anInterval first to: lastToReplace! !


!InlineMethod methodsFor: 'applying primitives - private' stamp: 'FB 7/4/2022 21:18:15'!
replaceRange: aSourceCodeInterval withNewSourceCode: newSourceCode inMethod: aCompiledMethod

	| currentUpdates sourceCodeReplacement|
	
	sourceCodeReplacement := newSourceCode.
	((aSourceCodeInterval last < aCompiledMethod sourceCode size) and: [((aCompiledMethod sourceCode at: aSourceCodeInterval last + 1) == $.)]
		and: [newSourceCode endsWith: '.']) ifTrue: [sourceCodeReplacement := newSourceCode copyUpToLast: $.].
	"It's important to use < and not <= because temporaries declarations are added to the changes collection
	after the changes concerning the inlining of the implementor code, but they need to be replaced first - Fernando"
	currentUpdates := updatedSendersCode at: aCompiledMethod methodReference ifAbsent: (SortedCollection 
		sortBlock: [ :left :right | left key first < right key first ]).
	currentUpdates add: (aSourceCodeInterval -> sourceCodeReplacement).
	updatedSendersCode at: aCompiledMethod methodReference put: currentUpdates.
	! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'FB 8/27/2022 21:06:09'!
addParenthesesIfNeededTo: anExpression

	"TODO: duplicated code in InlineTemporaryVariable"

	| firstCharacterIsOpeningParentheses lastCharacterIsClosingParentheses |
	
 	firstCharacterIsOpeningParentheses := (anExpression at: (anExpression firstNonSeparator)) == $(.
	lastCharacterIsClosingParentheses := (anExpression at: (anExpression lastNonSeparator)) == $).

	^(firstCharacterIsOpeningParentheses and: [lastCharacterIsClosingParentheses])
		ifTrue: [^anExpression]
		ifFalse: [ (anExpression endsWith: '.')
			ifTrue: [^('(', anExpression copyUpToLast: $.), ').']
	 		ifFalse: [^'(', anExpression, ')']
		].! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'FB 6/12/2022 18:33:45'!
indentationUpTo: anIndex in: aSourceCode

	| indentation currentIndex character|

	currentIndex := anIndex - 1.
	((aSourceCode at: currentIndex) == $^) ifTrue: [currentIndex := currentIndex  -1]. "Skip the return character if present"
	indentation := ''.

	[currentIndex > 0 and: [
		character := (aSourceCode at: currentIndex).
		(character = Character tab) or: [character = Character space]
	]] whileTrue: [
		indentation := indentation, character asString.
		currentIndex := currentIndex - 1.
	].

	^(character = Character newLineCharacter) ifFalse: [^''] ifTrue: [^indentation reversed].! !


!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'HAW 6/11/2022 19:21:17'!
addParenthesesIfNeededTo: anExpression

	| firstCharacterIsOpeningParentheses lastCharacterIsClosingParentheses |

 	firstCharacterIsOpeningParentheses := (anExpression at: (anExpression firstNonSeparator)) == $(.
	lastCharacterIsClosingParentheses := (anExpression at: (anExpression lastNonSeparator)) == $).

	^(firstCharacterIsOpeningParentheses and: [lastCharacterIsClosingParentheses])
		ifTrue: [anExpression]
		ifFalse: ['(', anExpression, ')'].! !


!SHParserST80 methodsFor: 'scan' stamp: 'jmv 5/24/2022 11:20:42'!
scanBinary
	| c d |
	c := self currentChar.
	currentTokenSourcePosition := sourcePosition.
	currentToken := source class with: c.
	d := self nextChar.
	((self isBinarySelectorCharacter: c) or: [c == $:]) ifFalse: [^currentToken].
	(c == $: and: [d == $=]) 
		ifTrue: [" := assignment"
			currentToken := currentToken , (source class with: d).
			self nextChar.
			^currentToken].
	"|| empty temp declaration"
	"This makes || colorize as an invalid method definition.
	It is better to allow #|| as valid selector, and mark an empty temp declaration as invalid instead!!"
	"(c == $| and: [d == $|])
		ifTrue: [^currentToken]."
	c := d.
	[
		d := self peekChar.
		c == $-
			ifTrue: [ d isDigit not ]
			ifFalse: [ self isBinarySelectorCharacter: c ]
	]
		whileTrue: [
			currentToken := currentToken copyWith: c.
			c := self nextChar ].
	^currentToken! !

!SHParserST80 methodsFor: 'scan' stamp: 'jmv 5/21/2022 22:45:35'!
scanComment
	| c s e |
	s := sourcePosition.
	
	[sourcePosition := sourcePosition + 1.
	(c := self currentChar) 
		ifNil: [
			self rangeType: #unfinishedComment start: s end: source size.
			^self error	": 'unfinished comment'"].
	c == $"] 
		whileFalse: [].
	e := sourcePosition.
	s < e ifTrue: [self rangeType: #comment start: s end: e].
	self nextChar.
	self scanWhitespace! !

!SHParserST80 methodsFor: 'scan' stamp: 'jmv 5/21/2022 22:45:41'!
scanIdentifier
	| c start |
	start := sourcePosition.
	[ (c := self nextChar) isValidInIdentifiers ] whileTrue: [].
	(c == $: and: [(self isBinarySelectorCharacter: self peekChar) not]) 
		ifTrue: [self nextChar].
	currentToken := source copyFrom: start to: sourcePosition - 1.
	currentTokenSourcePosition := start! !

!SHParserST80 methodsFor: 'scan' stamp: 'jmv 5/23/2022 12:46:55'!
scanNumber
	| start c nc base |
	start := sourcePosition.
	self skipDigits.
	c := self currentChar.
	('rx' includes: c)
		ifTrue: [
			base := Integer readFrom: ((source copyFrom: start to: sourcePosition - 1) readStream).
			self peekChar == $- ifTrue: [self nextChar].
			self skipBigDigits: base.
			c := self currentChar.
			c == $. 
				ifTrue: [
					(self isBigDigit: self nextChar base: base) 
						ifFalse: [sourcePosition := sourcePosition - 1]
						ifTrue: [self skipBigDigits: base]].
			c := self currentChar.
			('deqp'includes: c)
				ifTrue: [
					((nc := self nextChar) isDigit or: [nc == $- and:[self peekChar isDigit]]) 
						ifFalse: [sourcePosition := sourcePosition - 1]
						ifTrue: [self skipDigits]].
			c == $s 
				ifTrue: [
					self nextChar isDigit 
						ifFalse: [sourcePosition := sourcePosition - 1]
						ifTrue: [self skipDigits]].
			currentToken := source copyFrom: start to: sourcePosition - 1.
			^currentTokenSourcePosition := start].
	c == $s 
		ifTrue: [
			self nextChar isDigit 
				ifFalse: [sourcePosition := sourcePosition - 1]
				ifTrue: [self skipDigits.].
			currentToken := source copyFrom: start to: sourcePosition - 1.
			^currentTokenSourcePosition := start].
	c == $. 
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
			((nc := self nextChar) isDigit or: [nc == $-  and:[self peekChar isDigit]]) 
				ifFalse: [sourcePosition := sourcePosition - 1]
				ifTrue: [self skipDigits]].
	c == $s 
		ifTrue: [
			self nextChar isDigit 
				ifFalse: [sourcePosition := sourcePosition - 1]
				ifTrue: [self skipDigits]].
	currentToken := source copyFrom: start to: sourcePosition - 1.
	^currentTokenSourcePosition := start! !

!SHParserST80 methodsFor: 'scan' stamp: 'jmv 5/21/2022 22:45:45'!
scanWhitespace
	| c |
	
	[c := self currentChar.
	c notNil and: [c isSeparator]] 
		whileTrue: [sourcePosition := sourcePosition + 1].
	c == $" ifTrue: [self scanComment]! !

!SHParserST80 methodsFor: 'token testing' stamp: 'jmv 5/21/2022 22:35:28'!
isBinary
	| c |
	(currentToken isNil or: [self isName or: [self isKeyword]]) 
		ifTrue: [^false].
	"Special case: '::' is not a binary selector but the Chain operator"
	(sourcePosition - currentTokenSourcePosition = 1 and: [
		(source at: currentTokenSourcePosition ifAbsent: nil) == $: and: [		
			(source at: sourcePosition ifAbsent: nil) == $: ]])
				ifTrue: [^ false ].
	1 to: currentToken size do: [ :i |
		c := currentToken at: i.
		((self isBinarySelectorCharacter: c) or: [c == $:])
			ifFalse: [^false]].
	^true! !

!SHParserST80 methodsFor: 'token testing' stamp: 'jmv 5/21/2022 22:35:32'!
isKeyword
	^ currentTokenFirst isValidStartOfIdentifiers and: [ currentToken last == $: ]! !

!SHParserST80 methodsFor: 'character testing' stamp: 'jmv 12/14/2018 15:39:18'!
isBinarySelectorCharacter: aCharacter
	aCharacter == $: ifTrue: [^ false].
	^aCharacter isValidInBinarySelectors! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:35:36'!
parseArray
	[currentTokenFirst == $)] whileFalse: [self parseLiteralArrayElement].
	self scanPast: #arrayEnd! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:35:41'!
parseBacktick
	self enterBlock.
	self scanPast: #backtick.
	currentTokenFirst == $| ifTrue: [self parseBlockTemporaries].
	self parseStatementList.
	self failUnless: currentTokenFirst == $`.
	self scanPast: #backtick.
	self leaveBlock! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:35:49'!
parseBlock

	"Just read $["
	blockDepths add: blockDepth+1.
	blockDepthsStartIndexes add: sourcePosition-1.

	self enterBlock.
	self scanPast: #blockStart level: bracketDepth.
	currentTokenFirst == $: ifTrue: [self parseBlockArguments].
	currentTokenFirst == $| ifTrue: [self parseBlockTemporaries].
	self parseStatementList.
	self failUnless: currentTokenFirst == $].

	"Just read $]"
	blockDepths add: blockDepth-1.
	blockDepthsStartIndexes add: sourcePosition.

	self scanPast: #blockEnd level: bracketDepth.
	self leaveBlock! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:35:55'!
parseBlockArguments
	[ currentTokenFirst == $: ]
		whileTrue: [
			self scanPast: #blockArgColon.
			self failUnless: self isName.
			self scanPast: #blockPatternArg ].
	currentTokenFirst == $| 
		ifTrue: [ self scanPast: #blockArgsBar ]! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 9/13/2024 11:32:52'!
parseBlockTemporaries

	currentToken = '||' "Empty temp declaration without whitespace"
		ifTrue: [ ^self scanPast: #blockTempBar ].

	currentTokenFirst == $| 
		ifTrue: [
			self scanPast: #blockTempBar.
			[self isName] 
				whileTrue: [self scanPast: #blockPatternTempVar].
			self failUnless: currentToken = '|'.
			self scanPast: #blockTempBar]! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:36:05'!
parseBraceArray
	self parseStatementListForBraceArray.
	self failUnless: currentTokenFirst == $}.
	self scanPast: #rightBrace level: braceDepth.
	braceDepth := braceDepth - 1! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:36:12'!
parseByteArray
	"Literal ByteArray or literal FloatArray"
	[currentTokenFirst == $]] whileFalse: [
		currentTokenFirst isDigit | (currentTokenFirst == $-)
			ifTrue: [
				"do not parse the number, can be time consuming"
				self scanPast: #number]
			ifFalse: [
				self failWhen: currentTokenFirst == $. .
				self error]].
	self scanPast: #arrayEnd! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 9/16/2024 10:17:00'!
parseCascadeAndChain

	| beforeKeywordPos |
	beforeKeywordPos := currentTokenSourcePosition.
	self parseKeyword.

	"At least one message send before cascade and chain"
	beforeKeywordPos =​ currentTokenSourcePosition ifTrue: [ ^self ].

	currentTokenFirst == $; ifTrue:
		[
			self scanPast: #cascadeSeparator.
			^self parseCascadeAndChain
		].
	currentTokenFirst == $: ifTrue:
		[
			self scanPast: #chainSeparator.

			"These lines implement double colon chains"
			currentTokenFirst == $: ifFalse: [^self].
			self scanPast: #chainSeparator.
			
			^self parseCascadeAndChain
		].! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:36:25'!
parseExternalCall
	self scanNext.
	self failWhen: currentToken isNil.
	self scanPast: #externalCallType.
	currentToken = '*' 
		ifTrue: [self scanPast: #externalCallTypePointerIndicator].
	currentTokenFirst isDigit 
		ifTrue: [self scanPast: #integer]
		ifFalse: 	[
			self failUnless: currentTokenFirst == $'.
			self parseString].
	self failUnless: currentTokenFirst == $(.
	self scanPast: #leftParenthesis.
	[currentTokenFirst ~~ $)] 
		whileTrue: [
			self failWhen: currentToken isNil.
			self scanPast: #externalCallType.
			currentToken = '*' 
				ifTrue: [self scanPast: #externalCallTypePointerIndicator]].
	self scanPast: #rightParenthesis.
	currentToken = 'module:' 
		ifTrue: [
			self scanPast: #module.
			self failUnless: currentTokenFirst == $'.
			self parseString].
	self failUnless: currentToken = '>'.
	self scanPast: #primitiveOrExternalCallEnd! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:44:55'!
parseLiteral: inArray 
	currentTokenFirst == $$ 
		ifTrue: [
			| pos |
			self failWhen: self currentChar isNil.
			self rangeType: #'$'.
			pos := currentTokenSourcePosition + 1.
			self nextChar.
			self scanPast: #character start: pos end: pos.
			^self ].
	currentTokenFirst isDigit 
		ifTrue: [
			"do not parse the number, can be time consuming"
			self scanPast: #number.
			^self ].
	currentToken = '-' 
		ifTrue: [
			| c |
			c := self currentChar.
			(inArray and: [c isNil or: [c isDigit not]]) 
				ifTrue: [
					"single - can be a symbol in an Array"
					self scanPast: #symbol.
					^self ].
			self scanPast: #-.
			self failWhen: currentToken isNil.
			"token isNil ifTrue: [self error: 'Unexpected End Of Input']."
			"do not parse the number, can be time consuming"
			self scanPast: #number.
			^self ].
	currentTokenFirst == $' ifTrue: [
		self parseString.
		^self ].
	currentTokenFirst == $# ifTrue: [
		self parseSymbol.
		^self ].
	(inArray and: [currentToken notNil]) ifTrue: [
		self scanPast: #symbol.
		^self ].
	self failWhen: currentTokenFirst == $. .
	self error	": 'argument missing'"! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:36:39'!
parseLiteralArrayElement
	 currentTokenFirst isValidStartOfIdentifiers ifTrue: [
		#true = currentToken ifTrue: [
			self scanPast: #true.
			^ self ].
		#false = currentToken ifTrue: [
			self scanPast: #false.
			^ self ].
		#nil = currentToken ifTrue: [
			self scanPast: #nil.
			^ self ].
		self scanPast: #symbol.
		^ self ].
	currentTokenFirst == $( ifTrue: [
		self scanPast: #arrayStart.
		self parseArray.
		^ self ].
	self parseLiteral: true! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 9/13/2024 11:33:12'!
parseMethodTemporaries

	currentToken = '||' "Empty temp declaration without whitespace"
		ifTrue: [ ^self scanPast: #methodTempBar ].

	currentTokenFirst == $| 
		ifTrue: [
			self scanPast: #methodTempBar.
			[self isName] 
				whileTrue: [self scanPast: #patternTempVar].
			self failUnless: currentToken = '|'.
			self scanPast: #methodTempBar]! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 8/23/2024 15:13:54'!
parseStatement
	(currentToken isNil or: [	currentToken size = 1]) ifTrue: [
		currentTokenFirst == $^ ifTrue: [self scanPast: #return].
		currentTokenFirst = Character smalltalkUpArrow ifTrue: [self scanPast: #return]].
	self parseExpression.! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:37:00'!
parseStatementList
	
	[[currentTokenFirst == $.] whileTrue: [self scanPast: #statementSeparator].
	(currentToken notNil and: [currentTokenFirst ~~ $]]) 
		ifTrue: [self parseStatement].
	currentTokenFirst == $.] 
			whileTrue: [self scanPast: #statementSeparator]! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 9/13/2024 11:38:10'!
parseStatementListForBraceArray
	"same as parseStatementList, but does not allow empty statements e.g {...$a...}.
	A single terminating . IS allowed e.g. {$a.} "

	
	[currentTokenFirst ~~ $} ifTrue: [self parseExpression].
	currentTokenFirst == $.] 
		whileTrue: [self scanPast: #statementSeparator]! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:37:09'!
parseString
	| first c last |
	first := sourcePosition.
	[
		(c := self currentChar) ifNil: [
			self
				rangeType: #unfinishedString
				start: first - 1
				end: source size.
			self error": 'unfinished string'" ].
		c ~~ $' or: [
			self peekChar == $' and: [
				sourcePosition := sourcePosition + 1.
				true ]]] whileTrue: [ sourcePosition := sourcePosition + 1 ].
	last := sourcePosition.
	self nextChar.
	self
		scanPast: #string
		start: first - 1
		end: last! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:37:15'!
parseStringOrSymbol

	currentTokenFirst == $' ifTrue: [ ^self parseString ].
	currentTokenFirst == $# ifTrue: [ ^self parseSymbol ].
	self error! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:45:04'!
parseSymbol
	| c |
	currentToken = '#' 
		ifTrue: [
			"if token is just the #, then scan whitespace and comments
			and then process the next character.
			Allows space between the # and the start of the symbol 
			e.g. # (),  #  a, #  'sym' "
			self rangeType: #symbol.
			self scanWhitespace].
	c := self currentChar.
	self failWhen: (c isNil or: [c isSeparator]).
	c == $( 
		ifTrue: [
			self nextChar.
			self scanPast: #arrayStart start: currentTokenSourcePosition end: currentTokenSourcePosition + 1.
			^self parseArray].
	c == $' ifTrue: [
		self parseSymbolString.
		^self ].
	c == $[ ifTrue: [
			self nextChar.
			self scanPast: #arrayStart start: currentTokenSourcePosition end: currentTokenSourcePosition + 1.
			^self parseByteArray].
	(self isBinarySelectorCharacter: c) ifTrue: [
		self parseSymbolSelector.
		^self ].
	( c isValidStartOfIdentifiers or: [c == $:]) ifTrue: [
		self parseSymbolIdentifier.
		^self].
	self parseCharSymbol! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:37:29'!
parseSymbolIdentifier
	| c start end |
	c := self currentChar.
	self failUnless: ( c isValidStartOfIdentifiers or: [ c == $: ]).
	start := sourcePosition.
	[
		c := self nextChar.
		c isValidInIdentifiers or: [ c == $: ]
	] whileTrue: [].
	end := sourcePosition - 1.
	self scanPast: #symbol start: start - 1 end: end! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:37:34'!
parseSymbolString
	| first c last |
	first := sourcePosition.
	self nextChar.
	[
		(c := self currentChar) 
			ifNil: [
				self rangeType: #unfinishedString start: first end: source size.
				self error	": 'unfinished string'"].
		c ~~ $' or: [
			self peekChar == $' 
				ifTrue: [sourcePosition := sourcePosition + 1. true] 
				ifFalse: [false]]
	] whileTrue: [sourcePosition := sourcePosition + 1].
	last := sourcePosition.
	self nextChar.
	self scanPast: #stringSymbol start: first - 1 end: last! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:45:14'!
parseTerm
	self failWhen: currentToken isNil.
	currentTokenFirst == $( 
		ifTrue: [
			bracketDepth := bracketDepth + 1.
			self scanPast: #leftParenthesis level: bracketDepth.
			self parseExpression.
			self failUnless: currentTokenFirst == $).
			self scanPast: #rightParenthesis level: bracketDepth.
			bracketDepth := bracketDepth - 1.
			^self ].
	currentTokenFirst == $[ ifTrue: [^self parseBlock].
	currentTokenFirst == $` ifTrue: [^self parseBacktick].
	currentTokenFirst == ${ 
		ifTrue: [
			braceDepth := braceDepth + 1.
			self scanPast: #leftBrace level: braceDepth.
			self parseBraceArray.
			^self ].
	self isName ifTrue: [^self scanPast: (self resolve: currentToken)].
	self parseLiteral: false! !

Character initialize!
