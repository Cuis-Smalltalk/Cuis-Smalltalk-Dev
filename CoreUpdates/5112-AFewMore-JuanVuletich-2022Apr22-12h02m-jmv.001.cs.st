'From Cuis 6.0 [latest update: #5111] on 22 April 2022 at 12:06:34 pm'!

!Symbol methodsFor: 'converting' stamp: 'jmv 4/21/2022 14:47:23'!
uncapitalized
	^ self asPlainString uncapitalized asSymbol! !


!Message methodsFor: 'stub creation' stamp: 'jmv 4/21/2022 14:42:52'!
argumentNameAt: anIndex havingNamed: alreadyNamedArguments

	| argumentName |
	
	argumentName _ (self arguments at: anIndex) argumentName.
	[alreadyNamedArguments includes: argumentName] whileTrue: [argumentName _ argumentName, anIndex printString].
	alreadyNamedArguments add: argumentName.

	^argumentName! !


!EventSensor methodsFor: 'test' stamp: 'jmv 4/21/2022 14:43:09'!
printEventBuffer: evtBuf
	"Print the event buffer, currently only used by the method `test`."

	| type buttons macRomanCode modifiers pressType stamp unicodeCodePoint |
	type _ evtBuf first.
	stamp _ evtBuf second.
	stamp = 0 ifTrue: [ stamp := Time millisecondClockValue ]. "VMs report events using #millisecondClockValue"
	type = EventSensor eventTypeMouse
		ifTrue: [ | position |
			position _ evtBuf third @ evtBuf fourth.
			buttons _ evtBuf fifth.
			modifiers _ evtBuf sixth.
			Transcript
				newLine;
				show: 'Mouse';
				show: ' position:', position printString;
				show: ' buttons:', buttons printString;
				show: ' modifiers:', modifiers printString.
			].
	type = EventSensor eventTypeMouseScroll
		ifTrue: [ | delta |
			delta _ evtBuf third @ evtBuf fourth.
			buttons _ evtBuf fifth.
			modifiers _ evtBuf sixth.
			Transcript
				newLine;
				show: 'Scroll';
				show: ' delta:', delta printString;
				show: ' buttons:', buttons printString;
				show: ' modifiers:', modifiers printString.
			].
	type = EventSensor eventTypeKeyboard 
		ifTrue: [
			macRomanCode _ evtBuf third.
			unicodeCodePoint _ evtBuf sixth.
			pressType _ evtBuf fourth.
			modifiers _ evtBuf fifth.
			pressType = EventSensor eventKeyDown ifTrue: [
				type _ #keyDown].
			pressType = EventSensor eventKeyUp ifTrue: [
				type _ #keyUp].
			pressType = EventSensor eventKeyChar ifTrue: [
				type _ #keystroke].
			Transcript
				newLine;
				show: type;
				show: ' macRomanCode:', macRomanCode printString, '-', 
					(String with: (Character numericValue: (Character macRomanToLatin1: macRomanCode))), '-';
				show: ' unicodeCodePoint:', unicodeCodePoint printString.
			(Character iso8859s15CodeForUnicodeCodePoint: unicodeCodePoint) ifNotNil: [ :latin15 |
				Transcript show: '-', (String with: (Character numericValue: latin15)), '-' ].
			Transcript
				show: ' modifiers:', modifiers printString.
			(modifiers anyMask: 8) ifTrue: [ Transcript show: ' [commandWinAlt]' ].
			(modifiers anyMask: 4) ifTrue: [ Transcript show: ' [macOption]' ].
			(modifiers anyMask: 2) ifTrue: [ Transcript show: ' [control]' ].
			(modifiers anyMask: 1) ifTrue: [ Transcript show: ' [shift]' ].
			].! !


!String methodsFor: 'converting' stamp: 'jmv 4/21/2022 14:43:55'!
asLowercase
	"Answer a String made up from the receiver whose characters are all 
	lowercase."

	^ self copy asPlainString translateToLowercase! !

!String methodsFor: 'converting' stamp: 'jmv 4/21/2022 14:44:01'!
asUppercase
	"Answer a String made up from the receiver whose characters are all 
	uppercase."

	^self copy asPlainString translateToUppercase! !

!String methodsFor: 'converting' stamp: 'jmv 4/21/2022 14:46:00'!
uncapitalized
	"Answer an object like the receiver but with first character downshifted if necesary"
	"'MElViN' uncapitalized"
	"#Will uncapitalized"
	| answer |
	self isEmpty ifTrue: [^ self].
	answer _ self isString
				ifTrue: ["don't modify receiver"
					self copy]
				ifFalse: [self asPlainString].
	answer at: 1 put: (answer at: 1) asLowercase.
	^ answer! !


!Symbol methodsFor: 'converting' stamp: 'jmv 4/21/2022 14:47:21'!
capitalized
	^ self asPlainString capitalized asSymbol! !


!Symbol class methodsFor: 'access' stamp: 'jmv 4/21/2022 14:47:39'!
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
		each numArgs < 0 and: [each asPlainString uncapitalized numArgs < 0]].

"Symbol selectorsContaining: 'scon'"! !

!Symbol class methodsFor: 'access' stamp: 'jmv 4/21/2022 14:47:44'!
selectorsMatching: aStringPattern
	"Answer a list of selectors that match aStringPattern within them. Case-insensitive.
	 Does return symbols that begin with a capital letter."

	| selectorList |

	selectorList := OrderedCollection new.

	aStringPattern isEmpty ifTrue: [^selectorList].

	self allSymbolTablesDo:
		[:each | (aStringPattern match: each) ifTrue: [selectorList add: each]].

	^selectorList reject: "reject non-selectors, but keep ones that begin with an uppercase"
		[:each | each numArgs < 0 and: [each asPlainString uncapitalized numArgs < 0]]

	"Symbol selectorsMatching: 'parse:*'"! !


!ContentPack class methodsFor: 'code pack' stamp: 'jmv 4/21/2022 14:52:08'!
compilePayloadWith: contentMap

	| category |
	category _ self generatedMethodsCategory.
	self
		compile: 'contentMap' , String newLineString , '	^ ' , contentMap asString
		classified: category.

	self
		compile: 'objectCount' , String newLineString , '	^ ' , payload size asString
		classified: category.

	payload withIndexDo: [ :blob :index | | selector |
		selector _ 'object', index printString.
		self
			compile: selector, String newLineString, '	^ ', blob surroundedBySingleQuotes
			classified: category ]! !


!SmartRefStream methodsFor: 'read write' stamp: 'jmv 4/21/2022 14:50:36'!
mapClass: incoming
	"See if the old class named nm exists.  If so, return it.  If not, map it to a new class, and save the mapping in renamed.  "

	| cls oldVer sel nm |

	self flag: #bobconv.	


	nm _ renamed at: incoming ifAbsent: [incoming].	"allow pre-mapping around collisions"
	(nm endsWith: ' class') 
		ifFalse: [cls _ Smalltalk at: nm ifAbsent: nil.
			cls ifNotNil: [^ cls]]  	"Known class.  It will know how to translate the instance."
		ifTrue: [cls _ Smalltalk at: nm substrings first asSymbol ifAbsent: nil.
			cls ifNotNil: [^ cls class]]. 	"Known class.  It will know how to translate the instance."
	oldVer _ self versionSymbol: (structures at: nm).
	sel _ nm asPlainString.
	sel at: 1 put: (sel at: 1) asLowercase.
	sel _ sel, oldVer.	"i.e. #rectangleoc4"
	Symbol hasInterned: sel ifTrue: [:symb | 
		(self class canUnderstand: sel asSymbol) ifTrue: [
			cls _ self perform: sel asSymbol]].	"This class will take responsibility"
	cls ifNil: [cls _ self writeClassRenameMethod: sel was: nm
					fromInstVars: (structures at: nm).
			   cls class == String ifTrue: [cls _ nil]].
	cls ifNotNil: [renamed at: nm put: cls name].
	^ cls
! !

!SmartRefStream methodsFor: 'read write' stamp: 'jmv 4/21/2022 14:50:49'!
versionSymbol: instVarList
	"Create the symbolic code (like a version number) for this class in some older version.  First initials of all the inst vars, followed by the class version number.  Returns a string, caller makes it into a compound selector.  "

	| str |
	str _ instVarList size = 1 ifFalse: [''] ifTrue: ['x'].		"at least one letter"
	2 to: instVarList size do: [:ind |
		str _ str, (instVarList at: ind) first asPlainString].
	str _ str, instVarList first printString.	"the number"
	^ str

" | list | list _ (Array with: Paragraph classVersion), (Paragraph alistInstVarNames).
(SmartRefStream  on: (DummyStream on: nil)) versionSymbol: list
"! !


!Process methodsFor: 'printing' stamp: 'jmv 4/21/2022 14:51:05'!
browserPrintStringFull: aBoolean

	^String streamContents: [ :stream |
		aBoolean ifTrue: [
			stream
				nextPutAll: self statusString;
				space ].
		priority printOn: stream.
		stream nextPutAll: ' ('.
		self hash printOn: stream.
		stream
			nextPutAll: ') ';
			nextPutAll: self name.
		self animatedUI ifNotNil: [ :ui |
			stream nextPutAll: ' - '.
			ui printOn: stream ].
		aBoolean ifTrue: [
			stream
				nextPutAll: ': ';
				nextPutAll: suspendedContext printString ]]! !


!Parser methodsFor: 'scanning' stamp: 'jmv 4/21/2022 14:51:40'!
transformVerticalBarAndUpArrowIntoABinarySelector
	"Transform a vertical bar and or a up arrow into a binary selector.
	Eventually aggregate a serie of immediately following vertical bars, up arrows and a binary selector.
	Note that this aggregation cannot occur at scan time, because a pair of vertical bars can be encountered in two valid constructs:
	- either as an empty temporaries specification,
	- or as a local temporaries specification in a block of arity > 0.
	Colon $: can be used as binary, but '::' means Chain."
	| toMakeBinary |
	toMakeBinary _ #(verticalBar upArrow colon).
	"Special case: '::' is not a binary selector but the Chain operator"
	(hereType = #colon and: [tokenType = #colon]) ifTrue: [^ self ].
	(toMakeBinary identityIncludes: hereType) ifFalse: [
		^ self ].
	here := here asPlainString.
	hereType := #binary.
	[(toMakeBinary identityIncludes: tokenType) and: [hereMark + here size = mark]]
		whileTrue: [
			here := here , token asPlainString.
			hereEnd := hereEnd + 1.
			self scanToken].
	(tokenType == #binary and: [hereMark + here size = mark])
		ifTrue: [
			here := here asPlainString , token.
			hereType := #binary.
			hereEnd := hereEnd + token size.
			self scanToken].! !


!DecompilerConstructor methodsFor: 'constructor' stamp: 'jmv 4/21/2022 14:51:55'!
codeInst: index

	^InstanceVariableNode new
		name: (instVars at: index + 1 ifAbsent: ['unknown', index printString])
		index: index + 1! !


!SerializableClosureDecompilerConstructor methodsFor: 'constructor' stamp: 'jmv 4/21/2022 14:52:15'!
codeInst: index

	^ SerializableClosureInstanceVariableNode new
		name: (instVars at: index + 1 ifAbsent: ['unknown', index printString])
		index: index + 1! !


!TextEditor methodsFor: 'attributes' stamp: 'jmv 4/21/2022 14:55:04'!
offerFontStyleMenu
	"This is a user command, and generates undo"

	| emphases menuStrings entries reply code startIndex attribute |
	startIndex _ self startIndex.
	code _ model actualContents emphasisAt: startIndex.
	emphases _ #(bold italic underlined struckThrough superscript subscript withST80Glyphs).
	menuStrings _ Array streamContents: [ :strm |
		strm nextPut:(code isZero ifTrue: ['<on>'] ifFalse: ['<off>']), 'normal'.
		emphases do: [ :emph |
			strm nextPut:
				((code anyMask: (TextEmphasis perform: emph) emphasisCode)
					ifTrue: ['<on>'] ifFalse: ['<off>']),
				emph asPlainString ]].
	entries _ `#(normal)`, emphases.
	reply _ (SelectionMenu labelList: menuStrings lines: #(1) selections: entries) startUpMenu.
	reply ifNotNil: [
		attribute _ TextEmphasis perform: reply.
		((menuStrings at: (entries indexOf: reply)) beginsWith: '<on>')
			ifTrue: [ self unapplyAttributes: {attribute} ]
			ifFalse: [ self applyAttribute:  attribute ]].
	morph updateFromTextComposition.! !

!TextEditor methodsFor: 'model access' stamp: 'jmv 4/21/2022 14:55:17'!
wordUnder: aPositionInText
	
	| wordUnderCursorRange word indexOfSpace |
	
	wordUnderCursorRange := self wordRangeUnder: aPositionInText.
	word := (model actualContents copyFrom: wordUnderCursorRange first to: wordUnderCursorRange last) asPlainString.
	
	"I have to handle the edge case where the cursor is for example between a ' and the first letter of the word.
	In that case the range will include words with spaces - Hernan"
	indexOfSpace := word indexOf: $  ifAbsent: [ ^word ].
	
	^word first: indexOfSpace -1 
	
			! !


!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'jmv 4/21/2022 14:55:41'!
withMethodNodeAndClassDo: aBlock ifErrorsParsing: anErrorBlock

	| selectedClass methodNode |

	selectedClass := self selectedClassOrMetaClassOrUndefinedObject.
	[ 
		[ methodNode := selectedClass methodNodeFor: model actualContents asPlainString ]
			on: UndeclaredVariableWarning do: [ :ex | ex resume ]
	] on: Error do: [ :anError |  ^ anErrorBlock value: anError ].

	^aBlock value: methodNode value: selectedClass.! !

!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 4/21/2022 14:55:29'!
explainClass: symbol 
	"Is symbol a class variable or a pool variable?"
	| provider class reply classes |
	provider _ self codeProvider.
	(provider respondsTo: #selectedClassOrMetaClass)
		ifFalse: [^ nil].
	(class _ provider selectedClassOrMetaClass) ifNil: [^ nil].
	"no class is selected"
	(class isKindOf: Metaclass)
		ifTrue: [class _ class soleInstance].
	classes _ (Array with: class)
				, class allSuperclasses.
	"class variables"
	reply _ classes detect: [:each | (each classVarNames detect: [:name | symbol = name]
					ifNone: nil)
					notNil]
				ifNone: nil.
	reply ifNotNil: [
		^ String streamContents: [:str |
			str
				nextPutAll: symbol;
				nextPutAll: ' is a class variable, defined in class ';
				nextPutAll: reply printString, '\' withNewLines;
				nextPutAll: 'Smalltalk browseAllCallsOn: (';
				nextPutAll: reply printString;
				nextPutAll: ' classPool associationAt: #';
				nextPutAll: symbol;
				nextPutAll: ').']].
	"pool variables"
	classes do: [:each | (each sharedPools
			detect: [:pool | (pool includesKey: symbol)
					and: 
						[reply _ pool.
						true]]
			ifNone: nil)
			notNil].
	reply
		ifNil: [(Undeclared includesKey: symbol)
				ifTrue: [
					^ String streamContents: [:str |
						str
							nextPutAll: symbol;
							nextPutAll: ' is an undeclared variable.';
							nextPutAll: 'Smalltalk browseAllCallsOn: (Undeclared associationAt: #';
							nextPutAll: symbol;
							nextPutAll: ').']]]
		ifNotNil: 
			[classes _ Array streamContents: [ :strm |
				Smalltalk
					allBehaviorsDo: [:each | (each sharedPools
							detect: 
								[:pool | 
								pool == reply]
							ifNone: nil)
							notNil ifTrue: [strm nextPut: each]]].
			"Perhaps not print whole list of classes if too long. (unlikely)"
			^ String streamContents: [:str |
				str
					nextPutAll: symbol;
					nextPutAll: ' is a pool variable from the pool ';
					nextPutAll: (Smalltalk keyAtIdentityValue: reply) asPlainString;
					nextPutAll: ', which is used by the following classes ';
					nextPutAll: classes printString , '\' withNewLines;
					nextPutAll: 'Smalltalk browseAllCallsOn: (';
					nextPutAll: (Smalltalk keyAtIdentityValue: reply) asPlainString;
					nextPutAll: ' bindingOf: #';
					nextPutAll: symbol;
					nextPutAll: ').']].
	^ nil! !


!TextReplaceCommand methodsFor: 'printing' stamp: 'jmv 4/21/2022 14:56:16'!
printOn: aStream
	old isEmpty ifTrue: [
		^aStream nextPutAll: 'typed: '; nextPutAll: new asPlainString withDescriptiveLineEndings surroundedBySingleQuotes].
	new isEmpty ifTrue: [
		^aStream nextPutAll: 'deleted: '; nextPutAll: old asPlainString withDescriptiveLineEndings surroundedBySingleQuotes ].
	aStream
		nextPutAll: 'replaced: ';
		nextPutAll: old asPlainString withDescriptiveLineEndings surroundedBySingleQuotes;
		nextPutAll: ' with: ';
		nextPutAll: new asPlainString withDescriptiveLineEndings surroundedBySingleQuotes ! !


!FileIOAccessor methodsFor: 'accessing' stamp: 'jmv 4/21/2022 14:56:34'!
slash
	"Given that FileMan supports $/ as path delimiter regardless of the platform,
	this method is mostly for FileMan's own use, and general usage is discouraged.
	Just use '/' "
	slash ifNil: [ slash _ String with: self pathNameDelimiter ].
	^slash! !


!Utilities class methodsFor: 'tailoring system' stamp: 'jmv 4/21/2022 14:57:58'!
stripMethods: tripletList messageCode: messageString
	"Used to 'cap' methods that need to be protected for proprietary reasons, etc.; call this with a list of triplets of symbols of the form  (<class name>  <#instance or #class> <selector name>), and with a string to be produced as part of the error msg if any of the methods affected is reached"

	| aClass sel keywords codeString |
	tripletList do:
		[:triplet |  
			(aClass _ (Smalltalk at: triplet first ifAbsent: nil)) ifNotNil: [
				triplet second == #class ifTrue: [
					aClass _ aClass class].
				sel _ triplet third.
				keywords _ sel keywords.
				(keywords size = 1 and: [keywords first asSymbol isKeyword not])
					ifTrue: [
						codeString _ keywords first]
					ifFalse: [
						codeString _ ''.
						keywords withIndexDo:
							[:kwd :index |
								codeString _ codeString, ' ', (keywords at: index), ' ',
									'arg', index printString]].
				codeString _ codeString, '
	self codeStrippedOut: ', (messageString surroundedBySingleQuotes).

				aClass compile: codeString classified: 'stripped']]! !

!Utilities class methodsFor: 'vm statistics' stamp: 'jmv 4/21/2022 14:58:36'!
vmStatisticsReportOn: str
	"
	(TextModel new contents: Utilities vmStatisticsReportString) openLabel: 'VM Statistics'
	"

	| params oldSpaceEnd youngSpaceEnd memorySize fullGCs fullGCTime fullGCCompactionTime incrGCs incrGCTime tenureCount upTime
	  upTime2 fullGCs2 fullGCTime2  incrGCs2 incrGCTime2 tenureCount2 freeSize youngSize used spur timeReporter idleMs idleMs2 codeCompactionTime codeCompactionsOrNil codeCompactionTime2 codeCompactions2 maxParams |
	"Pad to protect for a VM that does not answer the expected parameter array"
	maxParams := 63. "max position referenced in this method"
	params := Array streamContents: [ :strm |
				strm nextPutAll: Smalltalk getVMParameters.
				[strm position < maxParams] whileTrue: [strm nextPut: nil]].
	oldSpaceEnd			:= params at: 1.
	youngSpaceEnd		:= params at: 2.
	memorySize			:= params at: 3.
	fullGCs			:= params at: 7.
	fullGCTime			:= params at: 8.
	incrGCs			:= params at: 9.
	incrGCTime			:= params at: 10.
	tenureCount			:= params at: 11.
	fullGCCompactionTime	:= params at: 18.
	codeCompactionsOrNil	:= params at: 62.
	codeCompactionTime	:= params at: 63.
	
	upTime := (params at: 20) ~= 0 "utcMicrosecondClock at startup in later Spur VMs"
				ifTrue: [Time primUtcMicrosecondClock - (params at: 20) + 500 // 1000]
				ifFalse: [Time primMillisecondClock]. "Fall back on old microsecond clock; Good for 47.5 days"
	spur := (params at: 41)
				ifNotNil: [ :p | p anyMask: 16]
				ifNil: [false].
	timeReporter := [:time| | seconds |
					seconds := time / 1000.
					seconds >= (60*60*24)
						ifTrue:
							[str print: seconds / 60 // 60 // 24; nextPut: $d; space;
								print: seconds / 60 // 60 \\ 24; nextPut: $h; space]
						ifFalse:
							[seconds >= (60*60) ifTrue:
								[str print: (seconds / 60 // 60); nextPut: $h; space]].
					str
						print: (seconds / 60 \\ 60) asInteger; nextPut: $m; space;
						print: (seconds \\ 60) asInteger; nextPut: $s].
	str	nextPutAll: 'uptime		'.
	timeReporter value: upTime.
	(idleMs := params at: 16) ~= 0 ifTrue:
		[str tab; nextPutAll: '(runtime '.
		 idleMs := idleMs // 1000.
		 timeReporter value: upTime - idleMs.
		 str nextPutAll: ', idletime '.
		 timeReporter value: idleMs.
		 str nextPut: $)].
	str newLine.

	str	nextPutAll: 'memory			';
		nextPutAll: (self formatVMParameter: memorySize); nextPutAll: ' bytes'; newLine.
	str	nextPutAll:	'	old		';
		nextPutAll: (self formatVMParameter: oldSpaceEnd); nextPutAll: ' bytes (';
		print: oldSpaceEnd / memorySize * 100 fractionDigits: 1; nextPutAll: '%)'; newLine.

	spur ifTrue:
		[(youngSize := (params at: 19)  * 7 // 5) = 0 ifTrue: [params at: 44].
		 youngSize := youngSize roundUpTo: 1024.
		 str	nextPutAll: '	young		';
			nextPutAll: (self formatVMParameter: youngSize); nextPutAll: ' bytes (';
			print: youngSize / memorySize * 100 fractionDigits: 1; nextPutAll: '%)'; newLine.
		youngSize := youngSpaceEnd. "used eden"
		freeSize := (params at: 54) + (params at: 44) - youngSize.
		used := youngSize + oldSpaceEnd - freeSize.
		str	nextPutAll: '	used		';
			nextPutAll: (self formatVMParameter: used); nextPutAll: ' bytes (';
			print: used / memorySize * 100 fractionDigits: 1; nextPutAll: '%)'; newLine.
		str	nextPutAll: '	free		';
			nextPutAll: (self formatVMParameter: freeSize); nextPutAll: ' bytes (';
			print: freeSize / memorySize * 100 fractionDigits: 1; nextPutAll: '%)'; newLine]
		ifFalse: "Earlier VM and V3 object memory"
		[str	nextPutAll: '	young		';
			nextPutAll: (self formatVMParameter: (youngSpaceEnd - oldSpaceEnd)); nextPutAll: ' bytes (';
			print: (youngSpaceEnd - oldSpaceEnd / memorySize * 100) fractionDigits: 1; nextPutAll: '%)'; newLine.
		str	nextPutAll: '	used		';
			nextPutAll: (self formatVMParameter: youngSpaceEnd); nextPutAll: ' bytes (';
			print: (youngSpaceEnd / memorySize * 100) fractionDigits: 1; nextPutAll: '%)'; newLine.
		str	nextPutAll: '	free		';
			nextPutAll: (self formatVMParameter: (memorySize - youngSpaceEnd)); nextPutAll: ' bytes (';
			print: (memorySize - youngSpaceEnd / memorySize * 100) fractionDigits: 1; nextPutAll: '%)'; newLine].

	str	nextPutAll: 'GCs					';
		nextPutAll: (self formatVMParameter: (fullGCs + incrGCs)).
	fullGCs + incrGCs > 0 ifTrue:
		[str
			nextPutAll: ' ('; 
			print: upTime / (fullGCs + incrGCs) fractionDigits: 1; 
			nextPutAll: ' ms between GCs'.
		 idleMs > 0 ifTrue:
			[str
				space;
				print: upTime - idleMs / (fullGCs + incrGCs) fractionDigits: 1; 
				nextPutAll: ' ms runtime between GCs'].
		 str nextPut: $)].
	str newLine.
	str	nextPutAll: '	full				';
		nextPutAll: (self formatVMParameter: fullGCs); nextPutAll: ' totalling '; nextPutAll: (self formatVMParameter: fullGCTime); nextPutAll: ' ms (';
		print: fullGCTime / (upTime - idleMs) * 100 fractionDigits: 2;
		nextPutAll: '% runtime)';
		nextPutAll: ', avg '; print: (fullGCTime / (fullGCs max: 1)) fractionDigits: 1; nextPutAll: ' ms'.
	(spur and: [fullGCCompactionTime ~= 0]) ifTrue:
		[str	newLine; nextPutAll: '		marking			';
			nextPutAll: (self formatVMParameter: (fullGCTime - fullGCCompactionTime)); nextPutAll: ' ms (';
			print: (fullGCTime - fullGCCompactionTime) / fullGCTime * 100 fractionDigits: 1;
			nextPutAll: '%) avg '; print: (fullGCTime - fullGCCompactionTime / (fullGCs max: 1)) fractionDigits: 1;
			nextPutAll: ' ms,'; newLine;
			nextPutAll: '		compacting			';
			nextPutAll: (self formatVMParameter: fullGCCompactionTime); nextPutAll: ' ms (';
			print: fullGCCompactionTime / fullGCTime * 100 fractionDigits: 1;
			nextPutAll: '%) avg '; print: fullGCCompactionTime / (fullGCs max: 1) fractionDigits: 1;
			nextPutAll: ' ms'].
	str	newLine.
	str	nextPutAll: (spur ifTrue: ['	scavenges				'] ifFalse: ['	incr			']);
		nextPutAll: (self formatVMParameter: incrGCs); nextPutAll: ' totalling '; nextPutAll: (self formatVMParameter: incrGCTime); nextPutAll: ' ms (';
		print: incrGCTime / (upTime - idleMs) * 100 fractionDigits: 2;
		nextPutAll: '% runtime), avg '; print: incrGCTime / incrGCs fractionDigits: 1; nextPutAll: ' ms'; newLine.
	str	nextPutAll: '	tenures				';
		nextPutAll: (self formatVMParameter: tenureCount).
	tenureCount = 0 ifFalse:
		[spur
			ifTrue: [str nextPutAll: ' (avg '; print: tenureCount // (incrGCs max: 1); nextPutAll: ' tenures per scavenge)']
			ifFalse: [str nextPutAll: ' (avg '; print: incrGCs // tenureCount; nextPutAll: ' GCs/tenure)']].
	str	newLine.

	codeCompactionsOrNil ifNotNil:
		[str
			nextPutAll: '	code compactions				'.
		codeCompactionsOrNil = 0
			ifTrue: [str
				nextPutAll: codeCompactionsOrNil printString; newLine]
			ifFalse: [str
				nextPutAll: (self formatVMParameter: codeCompactionsOrNil); nextPutAll: ' totalling '; nextPutAll: (self formatVMParameter: codeCompactionTime); nextPutAll: ' ms (';
				print: codeCompactionTime / (upTime - idleMs) * 100 fractionDigits: 3;
				nextPutAll: '% runtime), avg '; print: codeCompactionTime / codeCompactionsOrNil fractionDigits: 1; nextPutAll: ' ms'; newLine]].

LastStats ifNil: [LastStats := Array new: 9]
ifNotNil: [
	LastStats size < 9 ifTrue:
		[LastStats := LastStats, (Array new: 9 - LastStats size withAll: 0)].
	upTime2 := upTime - (LastStats at: 1).
	fullGCs2 := fullGCs - (LastStats at: 2).
	fullGCTime2 := fullGCTime - (LastStats at: 3).
	incrGCs2 := incrGCs - (LastStats at: 4).
	incrGCTime2 := incrGCTime - (LastStats at: 5).
	tenureCount2 := tenureCount - (LastStats at: 6).
	idleMs2 := idleMs - (LastStats at: 7).
	codeCompactionsOrNil ifNotNil:
		[codeCompactions2	:= codeCompactionsOrNil - (LastStats at: 8).
		 codeCompactionTime2	:= codeCompactionTime - (LastStats at: 9)].

	str	nextPutAll: 'GCs since last view					';
		nextPutAll: (self formatVMParameter: (fullGCs2 + incrGCs2)).
	fullGCs2 + incrGCs2 > 0 ifTrue:
		[str
			nextPutAll: ' ('; 
			print: upTime2 // (fullGCs2 + incrGCs2); 
			nextPutAll: ' ms between GCs'.
			idleMs2 > 0 ifTrue:
				[str
					nextPutAll: ', ';
					print: upTime2 - idleMs2 // (fullGCs2 + incrGCs2);
					nextPutAll: ' ms runtime between GCs'].
		 str nextPut: $)].
	str	newLine.
	str	nextPutAll: '	uptime				'; print: (upTime2 / 1000.0) fractionDigits: 1;
		nextPutAll: ' s, runtime ';		   print: (upTime2 - idleMs2 / 1000.0) fractionDigits: 1;
		nextPutAll: ' s, idletime ';		   print: (idleMs2 / 1000.0) fractionDigits: 1; nextPutAll: ' s'; newLine.
	str	nextPutAll: '	full				';
		nextPutAll: (self formatVMParameter: fullGCs2).
	fullGCs2 + fullGCTime2 > 0 ifTrue:
		[str	nextPutAll: ' totalling '; nextPutAll: (self formatVMParameter: fullGCTime2); nextPutAll: ' ms (';
			print: fullGCTime2 / (upTime2 - idleMs2) * 100 fractionDigits: 1;
			nextPutAll: '% runtime)'.
		 str	nextPutAll: ', avg '; print: fullGCTime2 / (fullGCs2 max: 1) fractionDigits: 1; nextPutAll: ' ms'].
	str	newLine.
	str	nextPutAll: (spur ifTrue: ['	scavenges				'] ifFalse: ['	incr			']);
		nextPutAll: (self formatVMParameter: incrGCs2); nextPutAll: ' totalling '; nextPutAll: (self formatVMParameter: incrGCTime2); nextPutAll: ' ms (';
		print: incrGCTime2 / (upTime2 - idleMs2) * 100 fractionDigits: 1;
		nextPutAll: '% runtime), avg '; print: incrGCTime2 / (incrGCs2 max: 1) fractionDigits: 1; nextPutAll: ' ms'.
	str	newLine.
	str	nextPutAll: '	tenures				';
		nextPutAll: (self formatVMParameter: tenureCount2).
	tenureCount2 = 0 ifFalse:
		[spur
			ifTrue: [str nextPutAll: ' (avg '; print: tenureCount2 // (incrGCs2 max: 1); nextPutAll: ' tenures per scavenge)']
			ifFalse: [str nextPutAll: ' (avg '; print: incrGCs2 // tenureCount2; nextPutAll: ' GCs/tenure)']].
	str	newLine.
	codeCompactionsOrNil ifNotNil:
		[str	nextPutAll: '	code compactions				';
			nextPutAll: (self formatVMParameter: codeCompactions2); nextPutAll: ' totalling '; nextPutAll: (self formatVMParameter: codeCompactionTime2); nextPutAll: ' ms (';
			print: codeCompactionTime2 / (upTime2 - idleMs2) * 100 fractionDigits: 3;
			nextPutAll: '% runtime)'.
		 codeCompactions2 > 0 ifTrue:
			[str nextPutAll: ', avg '; print: codeCompactionTime2 / codeCompactions2 fractionDigits: 1; nextPutAll: ' ms'; newLine.]]
].
	LastStats := { upTime. fullGCs. fullGCTime. incrGCs. incrGCTime. tenureCount. idleMs. codeCompactionsOrNil. codeCompactionTime }.! !


!Cursor class methodsFor: 'instance creation' stamp: 'jmv 4/21/2022 14:59:41'!
resizeForEdge: aSymbol 
	"Cursor resizeForEdge: #top"
	"Cursor resizeForEdge: #bottomLeft"

	"Do not erase this literal, as it helps 'senders' workproperly, and it protects these methods from accidental deletion"
	self nominallyUnsent: #(#resizeBottomCursor #resizeBottomLeftCursor #resizeBottomRightCursor #resizeLeftCursor #resizeRightCursor #resizeTopCursor #resizeTopLeftCursor #resizeTopRightCursor ).
	
	^ self perform: ('resize' , aSymbol capitalized, 'Cursor') asSymbol! !


!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 4/21/2022 15:00:08'!
editBalloonHelpContent: aString
	self
		request: 'Edit the balloon help text for ' , self shortPrintString
		initialAnswer: (aString ifNil: [self noHelpString] ifNotNil: [aString])
		do: [:reply|
			(reply isEmpty or: [reply asPlainString = self noHelpString])
				ifTrue: [self setBalloonText: nil]
				ifFalse: [self setBalloonText: reply]]! !


!PluggableListMorph methodsFor: 'model access' stamp: 'jmv 4/21/2022 15:01:58'!
keyboardSearch: aChar 
	| oldSelection nextSelection max milliSeconds nextSelectionList nextSelectionText |
	nextSelection _ oldSelection _ self getCurrentSelectionIndex.
	max _ self maximumSelection.
	milliSeconds _ Time localMillisecondClock.
	milliSeconds - lastKeystrokeTime > 300 ifTrue: ["just use the one current character for selecting"
		lastKeystrokes _ ''].
	lastKeystrokes _ lastKeystrokes copyWith: aChar asLowercase.
	lastKeystrokeTime _ milliSeconds.
	nextSelectionList _ OrderedCollection newFrom: (list copyFrom: oldSelection + 1 to: max).
	nextSelectionList addAll: (list copyFrom: 1 to: oldSelection).
	"Get rid of blanks and style used in some lists"
	nextSelectionText _ nextSelectionList detect: [:a | a asString withBlanksTrimmed asLowercase beginsWith: lastKeystrokes]
				ifNone: [^ self flash"match not found"].
	self owningWindow ifNotNil: [ :w |
		(w okToChangeDueTo: self) ifFalse: [^ self]].
	nextSelection _ list findFirst: [:a | a == nextSelectionText].
	"No change if model is locked"
	oldSelection == nextSelection ifTrue: [^ self flash].
	^ self setSelectionIndex: nextSelection! !


!SystemWindow methodsFor: 'menu' stamp: 'jmv 4/21/2022 15:03:31'!
addTileResizerMenuTo: aMenu
	"We can look at preferences here to decide what too do"
	(Preferences tileResizerInWindowMenu) ifFalse: [
		aMenu add: 'resize full' 		action: #resizeFull icon: #resizeFullIcon;
		add: 'resize top' 				action: #resizeTop icon: #resizeTopIcon;
		add: 'resize left' 				action: #resizeLeft icon: #resizeLeftIcon;
		add: 'resize bottom' 			action: #resizeBottom icon: #resizeBottomIcon;
		add: 'resize right' 				action: #resizeRight icon: #resizeRightIcon;
		add: 'resize top left' 			action: #resizeTopLeft icon: #resizeTopLeftIcon;
		add: 'resize top right' 		action: #resizeTopRight icon: #resizeTopRightIcon;
		add: 'resize bottom left' 		action: #resizeBottomLeft icon: #resizeBottomLeftIcon;
		add: 'resize bottom right' 	action: #resizeBottomRight icon: #resizeBottomRightIcon]
	ifTrue: [ |resizeMorph|
		"Use embedded resize morph"
		resizeMorph _ TileResizeMorph new
							selectionColor: (self widgetsColor adjustSaturation: -0.2 brightness: 0.25) ;
							action: [:resize | |resizeMsg|
								resizeMsg _ ('resize', resize asPlainString capitalized) asSymbol.
								self perform: resizeMsg.
								aMenu delete];
							yourself.
		aMenu addMorphBack: resizeMorph].
	^aMenu.! !


!CodeWindow methodsFor: 'GUI building' stamp: 'jmv 4/21/2022 15:04:42'!
optionalButtonRow
	"Answer a row of control buttons"

	| row buttons widths buttonColor |
	buttons _ OrderedCollection new.
	widths _ OrderedCollection new.
	buttonColor _ self buttonColor.
	self optionalButtonTuples do: [ :tuple | | button |
		widths add: tuple first.
		button _ PluggableButtonMorph
			model: self
			stateGetter: nil
			action: tuple third.
		button color: buttonColor.
		button label: tuple second.
		tuple size > 3 ifTrue: [ button setBalloonText: tuple fourth ].
		tuple size > 4 ifTrue: [ button secondaryActionSelector: tuple fifth ].
		buttons add: button ].
	row _ LayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.
	row
		addMorphs: buttons
		widthProportionalTo: widths.
	^ row.! !


!ChangeListWindow methodsFor: 'GUI building' stamp: 'jmv 4/21/2022 15:04:52'!
optionalButtonRow
	"Answer a row of buttons to occur in a tool pane"

	| row buttons widths buttonColor |
	buttons _ OrderedCollection new.
	widths _ OrderedCollection new.
	buttonColor _ self buttonColor.
	self optionalModelButtonTuples do: [ :tuple | | button |
		widths add: tuple first.
		button _ PluggableButtonMorph 
					model: model
					stateGetter: nil
					action: tuple third.
		button color: buttonColor.
		button label: tuple second.
		buttons add: button.
		button setBalloonText: tuple fourth].
	buttons add: self lineDiffButton.
	widths add: 14.
	buttons add: self wordDiffButton.
	widths add: 16.
	model wantsPrettyDiffOption ifTrue: [
		buttons add:  self prettyLineDiffButton.
		widths add: 21.
		buttons add:  self prettyWordDiffButton.
		widths add: 23 ].
	row _ LayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.
	row addMorphs: buttons widthProportionalTo: widths.
	^row! !


!DebuggerWindow methodsFor: 'GUI building' stamp: 'jmv 4/21/2022 15:05:08'!
customButtonRow
	"Answer a button pane affording the user one-touch access to certain functions; the pane is given the formal name 'customButtonPane' by which it can be retrieved by code wishing to send messages to widgets residing on the pane"

	| button buttons row buttonColor |
	
	buttons _ OrderedCollection new.
	buttonColor _ self buttonColor.
	"button with target = self"
	button _ PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #proceed.
	button color: buttonColor.
	button label: 'Proceed'.
	button setBalloonText: 'close the debugger and proceed.'.
	buttons add: button.
	"buttons with model target"
	self customButtonSpecs do: [ :tuple |
		button _ PluggableButtonMorph 
					model: self
					stateGetter: nil
					action: tuple second.
		button color: buttonColor.
		button label: tuple first.
		tuple size > 2 ifTrue: [button setBalloonText: tuple third].
		buttons add: button].

	row _ LayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.
	row addMorphs: buttons.
	^row! !


!ProcessBrowserWindow methodsFor: 'menu commands' stamp: 'jmv 4/21/2022 15:05:23'!
changePriority
	| newPriority rule |
	rule _ (model class rulesFor: model selectedProcess) second.
	rule
		ifFalse: [self inform: 'Nope, won''t change priority of ' , model selectedProcess name.
			^ self].
	self request: 'New priority'
		initialAnswer: model selectedProcess priority printString
		verifying: [:aString| ([aString asNumber asInteger] on: Error do: []) isInteger]
		do: [:aString|
			newPriority _ aString asNumber asInteger.
			(newPriority between: 1 and: Processor highestPriority)
			ifTrue:
				[model class setProcess: model selectedProcess toPriority: newPriority.
				model updateProcessList]
			ifFalse: [self inform: 'Bad priority']]! !


!AutoCompleterMorph methodsFor: 'drawing' stamp: 'jmv 4/21/2022 15:06:17'!
calculateWidth

	| width font |

	width _ 120.
	font _ self class listFont.

	1
		to: self entryCount
		do: [ :index | width _ width max: (font widthOfString: (completer entries at: index) asPlainString)].

	self entryCount > self itemsPerPage ifTrue: [ width _ width + ScrollBar scrollbarThickness ].

	^ width ! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'jmv 4/21/2022 15:06:25'!
drawItemOf: index on: aCanvas width: width top: itemTop

	| rectangle entry |
	
	rectangle _ 1@itemTop extent: width@self itemHeight.
	index = self selected ifTrue: [ aCanvas fillRectangle: rectangle color: (Theme current listHighlightFocused: true) ].

	entry _ completer entries at: index.
	aCanvas
		drawString: entry asPlainString
		at: rectangle topLeft
		font: self class listFont
		color: (self colorOf: entry).
	! !


!TheWorldMenu methodsFor: 'private' stamp: 'jmv 4/21/2022 15:07:11'!
pvtNewMenuForSplitLists: splitLists
| firstChar lastChar menu subMenu |
menu := MenuMorph new defaultTarget: self.
	1
		to: splitLists size
		do: [ :i |
			firstChar := i = 1
				ifTrue: [ $A ]
				ifFalse: [ 
					(splitLists at: i) first name first: 3 ].
			lastChar := i = splitLists size
				ifTrue: [ $Z ]
				ifFalse: [ 
					(splitLists at: i) last name first: 3 ].
			subMenu := MenuMorph new.
			(splitLists at: i) do: [ :cl |
				subMenu
					add: cl name
					target: self
					action: #newMorphOfClass:event:
					argument: cl ].
			menu
				add: (String with: firstChar) , ' - ' , (String with: lastChar)
				subMenu: subMenu ].
	^ menu.! !


!ChangeRecord methodsFor: 'access' stamp: 'jmv 4/22/2022 12:04:22'!
fileOutOn: aFileStream
	"File the receiver out on the given file stream"

	| aString |
	type == #method
		ifTrue: [
			aFileStream newLine; nextPut: $!!.
			aString _  class printString
							, (meta ifTrue: [' class methodsFor: ']
									ifFalse: [' methodsFor: '])
							, category asString printString.
			stamp ifNotNil: [
				aString _ aString, ' stamp: ''', stamp, ''''].
			aFileStream nextChunkPut: aString.
			aFileStream newLine ].

	type == #preamble ifTrue: [ aFileStream nextPut: $!! ].

	type == #classComment
		ifTrue: [
			aFileStream nextPut: $!!.
			aFileStream nextChunkPut: class printString, ' commentStamp: ', stamp storeString.
			aFileStream newLine ].
		
	type == #classDefinition ifTrue: [
		aFileStream nextPut: $!!.
		aFileStream nextChunkPut: 
			'classDefinition: ', 
			(self isMetaClassChange ifTrue: [self changeClassName, ' class'] ifFalse: [self changeClassName]) printString,
			' category: ', self category printString.
		aFileStream newLine ].

	aFileStream nextChunkPut: self string.
	
	type == #method ifTrue: [ aFileStream nextChunkPut: ' '; newLine ].
	type == #classComment ifTrue: [ aFileStream newLine ].
	aFileStream newLine! !


!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 4/21/2022 15:08:37'!
postscriptString
	"Answer the string representing the postscript.  "

	^ postscript
		ifNil: [postscript]
		ifNotNil: [postscript actualContents asPlainString]! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 4/21/2022 15:08:40'!
preambleString
	"Answer the string representing the preamble"

	^ preamble
		ifNotNil: [ preamble actualContents asPlainString ]! !


!PseudoClass methodsFor: 'removing' stamp: 'jmv 4/21/2022 15:09:34'!
removeAllUnmodified
	| stClass |
	self exists ifFalse:[^self].
	self removeUnmodifiedMethods: self selectors.
	stClass := self realClass.
	(self hasDefinition and:[stClass definition = self definition]) ifTrue:[definition := nil].
	(self hasComment and:[stClass comment asPlainString = self comment asPlainString]) ifTrue:[ self classComment: nil].
	metaClass ifNotNil: [ metaClass removeAllUnmodified]! !

!PseudoClass methodsFor: 'removing' stamp: 'jmv 4/21/2022 15:09:46'!
removeUnmodifiedMethods: aCollection
	| stClass |
	self exists ifFalse:[^self].
	stClass := self realClass.
	aCollection do:[:sel|
		(self sourceCodeAt: sel) = (stClass sourceCodeAt: sel ifAbsent:['']) asPlainString ifTrue:[
			self removeMethod: sel.
		].
	].
	self organization removeEmptyCategories.! !

