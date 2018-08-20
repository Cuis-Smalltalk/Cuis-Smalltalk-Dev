'From Cuis 5.0 of 7 November 2016 [latest update: #3403] on 11 August 2018 at 7:33:37 pm'!
!classDefinition: #MessageNode category: #'Compiler-ParseNodes'!
ParseNode subclass: #MessageNode
	instanceVariableNames: 'receiver selector precedence special arguments sizes equalNode caseErrorNode originalReceiver originalSelector originalArguments keywordRanges '
	classVariableNames: 'MacroEmitters MacroPrinters MacroSelectors MacroSizers MacroTransformers '
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!
!classDefinition: #MethodNode category: #'Compiler-ParseNodes'!
ParseNode subclass: #MethodNode
	instanceVariableNames: 'selectorOrFalse precedence arguments block primitive encoder temporaries properties sourceText locationCounter localsPool selectorKeywordsRanges '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

!CodeProvider methodsFor: 'message list menu' stamp: 'HAW 8/11/2018 18:52:25'!
inspectCompiledMethod
	"Open an Inspector on the CompiledMethod itself"

	self selectedMessageName ifNotNil: [
		(self selectedClassOrMetaClass compiledMethodAt: self selectedMessageName)
			inspect ]! !


!Behavior methodsFor: 'methods' stamp: 'HAW 8/11/2018 18:49:25'!
methodNodeFor: aSourceCode

	|  parser methodNode |

	parser := self parserClass new
		encoderClass: EncoderForV3PlusClosures;
		yourself.
		
	methodNode := parser parse: aSourceCode class: self.
	methodNode sourceText: aSourceCode.
	
	^methodNode
			! !


!ParseNode methodsFor: 'testing' stamp: 'HAW 8/11/2018 19:00:35'!
isInstanceVariableNode

	^false! !

!ParseNode methodsFor: 'testing' stamp: 'HAW 8/11/2018 19:26:31'!
isMessageNamed: aSelector

	^ false! !


!Encoder methodsFor: 'temps' stamp: 'HAW 8/11/2018 19:09:30'!
bindArg: aName range: aRange
 
	^ self addMultiRange: aRange for: (self bindArg: aName)
! !

!Encoder methodsFor: 'temps' stamp: 'HAW 8/11/2018 19:13:38'!
bindTemp: aName range: aRange

	^ self addMultiRange: aRange for: (self bindTemp: aName)! !

!Encoder methodsFor: 'encoding' stamp: 'HAW 8/11/2018 19:14:52'!
encodeLiteral: object range: aRange

	^ self addMultiRange: aRange for: (self encodeLiteral: object)! !

!Encoder methodsFor: 'source mapping' stamp: 'HAW 8/11/2018 19:11:31'!
addMultiRange: aRange for: aNode

	| ranges |
	
	"I'm using an OrderedCollection because ranges are added in order, while parsing the source code.
	If this constrain is not hold, a SortedCollection should be used - Hernan"
	ranges := sourceRanges at: aNode ifAbsentPut: [ OrderedCollection new ].
	ranges add: aRange.
	
	^aNode ! !

!Encoder methodsFor: 'source mapping' stamp: 'HAW 8/11/2018 19:30:24'!
messageSendSelectorKeywordRangesOf: aSelector ifAbsent: aBlock
	
	| ranges sortedRanges |
	
	ranges := sourceRanges keys 
		select: [ :aParseNode | aParseNode isMessageNamed: aSelector ] 
		thenCollect: [ :aMessageSendNode | aMessageSendNode keywordRanges ].
		
	ranges isEmpty ifTrue: [ ^aBlock value ].
	sortedRanges := ranges asSortedCollection: [ :left :right | left first first < right first first ].
	
	^sortedRanges ! !

!Encoder methodsFor: 'source mapping' stamp: 'HAW 8/11/2018 18:59:11'!
rangesForInstanceVariable: aName ifAbsent: aBlock

	^ self 
		rangesForVariable: aName 
		checkingType: [ :variableNode | variableNode isInstanceVariableNode ]
		ifAbsent: aBlock ! !

!Encoder methodsFor: 'source mapping' stamp: 'HAW 8/11/2018 18:59:25'!
rangesForTemporaryVariable: aName ifAbsent: aBlock

	^ self 
		rangesForVariable: aName 
		checkingType: [ :variableNode | variableNode isTemp ]
		ifAbsent: aBlock ! !

!Encoder methodsFor: 'source mapping' stamp: 'HAW 8/11/2018 18:58:07'!
rangesForVariable: aName checkingType: nodeTypeCheckBlock ifAbsent: anAbsentBlock

	| variableNode |
	
	variableNode := scopeTable at: aName ifAbsent: [ ^anAbsentBlock value ].
	(nodeTypeCheckBlock value: variableNode) ifFalse: [ ^anAbsentBlock value ].
	
	^sourceRanges at: variableNode ifAbsent: anAbsentBlock ! !


!InstanceVariableNode methodsFor: 'testing' stamp: 'HAW 8/11/2018 19:00:19'!
isInstanceVariableNode

	^true! !


!MessageNode methodsFor: 'source ranges' stamp: 'HAW 8/11/2018 19:24:24'!
keywordRanges

	^keywordRanges! !

!MessageNode methodsFor: 'testing' stamp: 'HAW 8/11/2018 19:23:56'!
isMessageNamed: aSelector

	^aSelector == selector key! !

!MessageNode methodsFor: 'initialization' stamp: 'HAW 8/11/2018 19:21:33'!
receiver: aReceiver selector: aSelector arguments: args precedence: aPrecedence from: anEncoder sourceRange: aSourceRange keywordsRanges: wordsRanges
	
	keywordRanges := wordsRanges.
	
	^self receiver: aReceiver selector: aSelector arguments: args precedence: aPrecedence from: anEncoder sourceRange: aSourceRange ! !


!MethodNode methodsFor: 'source mapping' stamp: 'HAW 8/11/2018 19:01:49'!
rangesForInstanceVariable: aName ifAbsent: aBlock

	^encoder rangesForInstanceVariable: aName ifAbsent: aBlock
! !

!MethodNode methodsFor: 'source mapping' stamp: 'HAW 8/11/2018 19:01:59'!
rangesForTemporaryVariable: aName ifAbsent: aBlock

	^encoder rangesForTemporaryVariable: aName ifAbsent: aBlock
! !

!MethodNode methodsFor: 'source mapping' stamp: 'HAW 8/11/2018 19:06:30'!
selectorKeywordsRanges

	^selectorKeywordsRanges! !

!MethodNode methodsFor: 'initialization' stamp: 'HAW 8/11/2018 19:27:21'!
selector: selOrFalse arguments: args precedence: p temporaries: temps block: blk encoder: anEncoder primitive: prim properties: propDict selectorKeywordsRanges: range
	
	selectorKeywordsRanges := range.
	
	^self selector: selOrFalse arguments: args precedence: p temporaries: temps block: blk encoder: anEncoder primitive: prim properties: propDict ! !


!CompiledMethod methodsFor: 'decompiling' stamp: 'HAW 8/11/2018 18:50:26'!
methodNode
	"Return the parse tree that represents self"
	
	| aClass source |
	
	aClass := self methodClass.
	source := self
				getSourceFor: (self selector ifNil: [self defaultSelector])
				in: aClass.
				
	^ aClass methodNodeFor: source
	! !


!BrowserWindow class methodsFor: 'browser menues' stamp: 'HAW 8/11/2018 18:51:50'!
messageListMenuOptions
	
	^`{
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'what to show...'.
				#selector 		-> 		#offerWhatToShowMenu.
				#icon 			-> 		#preferencesIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		20.
				#label 			-> 		'toggle break on entry'.
				#object 			-> 		#model.
				#selector 		-> 		#toggleBreakOnEntry.
				#icon 			-> 		#debugIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'browse full (b)'.
				#selector 		-> 		#browseMethodFull.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		20.
				#label 			-> 		'browse hierarchy (h)'.
				#selector 		-> 		#browseHierarchy.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		30.
				#label 			-> 		'browse method (O)'.
				#selector 		-> 		#openSingleMessageBrowser.
				#icon 			-> 		#scriptIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		40.
				#label 			-> 		'browse protocol (p)'.
				#selector 		-> 		#browseFullProtocol.
				#icon 			-> 		#spreadsheetIcon
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		10.
				#label 			-> 		'fileOut'.
				#object 			-> 		#model.
				#selector 		-> 		#fileOutMessage.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		20.
				#label 			-> 		'inspect CompiledMethod'.
				#object 			-> 		#model.
				#selector 		-> 		#inspectCompiledMethod.
				#icon 			-> 		#exploreIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
				#itemOrder 		-> 		10.
				#label 			-> 		'senders of... (n)'.
				#selector 		-> 		#browseSendersOfMessages.
				#icon 			-> 		#mailForwardIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
				#itemOrder 		-> 		20.
				#label 			-> 		'implementors of... (m)'.
				#selector 		-> 		#browseMessages.
				#icon 			-> 		#developmentIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
				#itemOrder 		-> 		30.
				#label 			-> 		'inheritance (i)'.
				#selector 		-> 		#methodHierarchy.
				#icon 			-> 		#goDownIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
				#itemOrder 		-> 		40.
				#label 			-> 		'versions (v)'.
				#selector 		-> 		#browseVersions.
				#icon 			-> 		#clockIcon
			} asDictionary.
			{
				#itemGroup 	-> 		50.
				#itemOrder 		-> 		10.
				#label 			-> 		'inst var refs...'.
				#selector 		-> 		#browseInstVarRefs.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#itemGroup 	-> 		50.
				#itemOrder 		-> 		20.
				#label 			-> 		'inst var defs...'.
				#selector 		-> 		#browseInstVarDefs.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#itemGroup 	-> 		50.
				#itemOrder 		-> 		30.
				#label 			-> 		'class var refs...'.
				#selector 		-> 		#browseClassVarRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 	-> 		50.
				#itemOrder 		-> 		40.
				#label 			-> 		'class variables'.
				#selector 		-> 		#browseClassVariables.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 	-> 		50.
				#itemOrder 		-> 		50.
				#label 			-> 		'class refs (N)'.
				#selector 		-> 		#browseClassRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 	-> 		60.
				#itemOrder 		-> 		10.
				#label 			-> 		'remove method (x)'.
				#object 			-> 		#model.
				#selector 		-> 		#removeMessage.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			{
				#itemGroup 	-> 		60.
				#itemOrder 		-> 		20.
				#label 			-> 		'run test (t)'.
				#object 			-> 		#model.
				#selector 		-> 		#runMethodTest.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 	-> 		60.
				#itemOrder 		-> 		30.
				#label 			-> 		'debug test (r)'.
				#object 			-> 		#model.
				#selector 		-> 		#debugMethodTest.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 	-> 		70.
				#itemOrder 		-> 		10.
				#label 			-> 		'more...'.
				#selector 		-> 		#openMessageListMenu2.
				#icon 			-> 		#listAddIcon
			} asDictionary.
		}`.
	! !

!methodRemoval: CodeProvider #exploreCompiledMethod!
CodeProvider removeSelector: #exploreCompiledMethod!

!InstanceVariableNode reorganize!
('visiting' accept:)
('code generation (new scheme)' emitCodeForStore:encoder: emitCodeForStorePop:encoder: emitCodeForValue:encoder: sizeCodeForStore: sizeCodeForStorePop: sizeCodeForValue:)
('initialization' name:index:)
('testing' isInstanceVariableNode)
!

!classDefinition: #MessageNode category: #'Compiler-ParseNodes'!
ParseNode subclass: #MessageNode
	instanceVariableNames: 'receiver selector precedence special arguments sizes equalNode caseErrorNode originalReceiver originalSelector originalArguments keywordRanges'
	classVariableNames: 'MacroEmitters MacroPrinters MacroSelectors MacroSizers MacroTransformers'
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

!MessageNode reorganize!
('visiting' accept: argumentsInEvaluationOrder)
('code generation (closures)' analyseTempsWithin:rootNode:assignmentPools:)
('equation translation' arguments arguments: eval receiver receiver: selector)
('printing' macroPrinter precedence printCaseOn:indent: printIfNil:indent: printIfNilNotNil:indent: printIfOn:indent: printKeywords:arguments:on:indent: printOn:indent: printOn:indent:precedence: printParenReceiver:on:indent: printReceiver:on:indent: printRepeatOn:indent: printToDoOn:indent: printWhileOn:indent: printWithClosureAnalysisCaseOn:indent: printWithClosureAnalysisIfNil:indent: printWithClosureAnalysisIfNilNotNil:indent: printWithClosureAnalysisIfOn:indent: printWithClosureAnalysisKeywords:arguments:on:indent: printWithClosureAnalysisOn:indent: printWithClosureAnalysisOn:indent:precedence: printWithClosureAnalysisParenReceiver:on:indent: printWithClosureAnalysisReceiver:on:indent: printWithClosureAnalysisToDoOn:indent: printWithClosureAnalysisWhileOn:indent:)
('source ranges' keywordRanges)
('testing' canCascade ensureCanCascade: isComplex isMessage isMessage:receiver:arguments: isMessageNamed: isMessageNode isNilIf isOptimized isOptimizedLoop isReturningIf toDoIncrement: toDoLimit:)
('cascading' cascadeReceiver receiver:arguments:precedence:)
('private' checkBlock:as:from:maxArgs: ifNilReceiver pvtCheckForPvtSelector:)
('code generation' emitCodeForEffect:encoder: emitCodeForRepeat:encoder:value: emitCodeForToDo:encoder:value: emitCodeForValue:encoder: sizeCodeForCase:value: sizeCodeForEffect: sizeCodeForRepeat:value: sizeCodeForToDo:value: sizeCodeForValue: sizeCodeForWhile:value:)
('code generation (new scheme)' emitCodeForCase:encoder:value: emitCodeForIf:encoder:value: emitCodeForIfNil:encoder:value: emitCodeForWhile:encoder:value: sizeCodeForIf:value: sizeCodeForIfNil:value:)
('macro transformations' noteSpecialSelector: toDoFromWhileWithInit: transform: transformAnd: transformBoolean: transformCase: transformIfFalse: transformIfFalseIfTrue: transformIfNil: transformIfNilIfNotNil: transformIfNotNilIfNil: transformIfTrue: transformIfTrueIfFalse: transformOr: transformRepeat: transformToDo: transformWhile:)
('initialization' receiver:selector:arguments:precedence: receiver:selector:arguments:precedence:from: receiver:selector:arguments:precedence:from:sourceRange: receiver:selector:arguments:precedence:from:sourceRange:keywordsRanges: selector:)
!

!classDefinition: #MethodNode category: #'Compiler-ParseNodes'!
ParseNode subclass: #MethodNode
	instanceVariableNames: 'selectorOrFalse precedence arguments block primitive encoder temporaries properties sourceText locationCounter localsPool selectorKeywordsRanges'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!
