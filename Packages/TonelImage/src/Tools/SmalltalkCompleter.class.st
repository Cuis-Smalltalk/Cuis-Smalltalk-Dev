"
An autocompleter specialized in Smalltalk code
"
Class {
	#name : 'SmalltalkCompleter',
	#superclass : 'AutoCompleter',
	#instVars : [
		'parser',
		'selectorsClasses',
		'possibleInvalidSelectors',
		'canShowSelectorDocumentation',
		'possibleBinarySendRange',
		'allRanges',
		'currentRange'
	],
	#classVars : [
		'AccessLock',
		'EntriesLimit',
		'Selectors'
	],
	#category : 'Tools-Autocompletion',
	#package : 'Tools',
	#tag : 'Autocompletion'
}

{ #category : 'class initialization' }
SmalltalkCompleter class >> addSelector: aSymbol [

	self protected: [
		Selectors at: aSymbol ifAbsentPut: 0 ].
]

{ #category : 'class initialization' }
SmalltalkCompleter class >> addSelector: thisSelector method: aCompiledMethod methodTimeStamp: aTimeStampString updateSortingOfSentMessages: aBoolean [

	| thisMethodSortingValue sentMessages existingSortingValue |
	thisMethodSortingValue := CompiledMethod authorAndStamp: aTimeStampString partsDo: [ :authorInitials :dateAndTime |
		dateAndTime
			ifNil: [ 0 ]
			ifNotNil: [ (dateAndTime - (DateAndTime fromString: '01/01/1996 00:00')) totalMinutes max: 0 ]].
	sentMessages := aCompiledMethod messages.
	self protected: [
		thisMethodSortingValue > (Selectors at: thisSelector ifAbsent: [-1]) ifTrue:
			[
			Selectors at: thisSelector put: thisMethodSortingValue.
			aBoolean ifTrue:
				[
				sentMessages do:
					[ :sentMsg |
					existingSortingValue := Selectors at: sentMsg ifAbsent: [-1].
					"If found at Selectors, no need to do the more expensive search in all classes"
					(existingSortingValue > -1 or: [ Smalltalk isThereAnImplementorOf: sentMsg ]) ifTrue: 
						"This means add it with value 0 if it is not already there"
						[ thisMethodSortingValue > existingSortingValue ifTrue:
							[ Selectors at: sentMsg put: thisMethodSortingValue ]]]]]].
]

{ #category : 'accessing' }
SmalltalkCompleter class >> changeEntriesLimitTo: aNewLimit during: aBlock [

	| previousLimit |
	
	previousLimit := EntriesLimit.
	EntriesLimit := aNewLimit.
	
	^aBlock ensure: [ EntriesLimit := previousLimit ]
]

{ #category : 'system change notifications' }
SmalltalkCompleter class >> classRemoved: aClass fromCategory: aCategoryName [

	self protected: [
		aClass selectorsDo: [ :selector |
			(Smalltalk isThereAnImplementorOf: selector exceptFor: { aClass . aClass class }) ifFalse: [
				Selectors removeKey: selector ifAbsent: nil ]].
		aClass class selectorsDo: [ :selector |
			(Smalltalk isThereAnImplementorOf: selector exceptFor: { aClass . aClass class }) ifFalse: [
				Selectors removeKey: selector ifAbsent: nil ]]]
]

{ #category : 'accessing' }
SmalltalkCompleter class >> entriesLimit [
	
	^EntriesLimit 
]

{ #category : 'class initialization' }
SmalltalkCompleter class >> initialize [
	"
	It takes about 6 seconds, mostly because of the time to fetch method stamps from source files...
	[ SmalltalkCompleter initialize ]timeToRun
	Selectors inspect
	"
	| maxSortValue |
	
	EntriesLimit := 100.
	
	SystemChangeNotifier uniqueInstance
		removeActionsWithReceiver: self.	 "avoid double registration"

	SystemChangeNotifier uniqueInstance
		when: #classRemoved send: #classRemoved:fromCategory: to: self;
		when: #methodAddedInProtocolTimeStamp send: #methodAdded:selector:inProtocol:class:methodTimeStamp:requestor: to: self;
		when: #methodRemoved send: #methodRemoved:selector:inProtocol:class: to: self.
	
	self protected: [
		Selectors := Trie new.
		"
		Smalltalk allBehaviorsDo: [:class |
			class selectorsAndMethodsDo: [ :sel :method |
				Selectors at: sel ifAbsentPut: [ 0 ].
				method messages do: [ :sentMsg |
					Selectors at: sentMsg put: (Selectors at: sentMsg ifAbsent: [ 0 ]) + 1 ]]].
		"
		Smalltalk allBehaviorsDo: [:class |
			class selectorsAndMethodsDo: [ :sel :method |
				self addSelector: sel method: method
					methodTimeStamp: method authorAndStamp updateSortingOfSentMessages: false ]].
		""
	
		"The following might not be found in #messages. Give them maximum priority."
		maxSortValue := SmallInteger maxVal.
		"From MessageNode>>#initialize"
		#(	ifTrue: ifFalse: ifTrue:ifFalse: ifFalse:ifTrue:
			and: or:
			whileFalse: whileTrue: whileFalse whileTrue
			to:do: to:by:do:
			caseOf: caseOf:otherwise:
			ifNil: ifNotNil:  ifNil:ifNotNil: ifNotNil:ifNil:) do: [ :sel |
				Selectors at: sel put: maxSortValue ].
	
		maxSortValue := SmallInteger maxVal-1.
		"From SystemDictionary >>#recreateSpecialObjectsArray"
		(1 to: Smalltalk specialSelectorSize) do:  [ :i | | sym |
			sym := Smalltalk specialSelectorAt: i.
			(Selectors includesKey: sym)
				ifTrue: [ Selectors at: sym put: maxSortValue ]]]
]

{ #category : 'testing' }
SmalltalkCompleter class >> isForCurrentTypeSystem [

	self subclassResponsibility 
]

{ #category : 'services' }
SmalltalkCompleter class >> isThereAnImplementorOf: aSymbol [
	| answer |
	self protected: [
		answer := Selectors includesKey: aSymbol ].
	^answer
]

{ #category : 'system change notifications' }
SmalltalkCompleter class >> methodAdded: aMethod selector: aSymbol inProtocol: aCategoryName class: aClass methodTimeStamp: aTimeStampString requestor: requestor [

	self addSelector: aSymbol method: aMethod
		methodTimeStamp: aTimeStampString updateSortingOfSentMessages: true.
]

{ #category : 'system change notifications' }
SmalltalkCompleter class >> methodRemoved: aMethod selector: aSymbol inProtocol: protocol class: aClass [

	self protected: [
		(Smalltalk isThereAnImplementorOf: aSymbol) not ifTrue: [
			Selectors removeKey: aSymbol ifAbsent: nil ]]
]

{ #category : 'synchronization' }
SmalltalkCompleter class >> protected: aBlock [

	^(AccessLock ifNil: [ AccessLock := Mutex new ]) critical: aBlock
]

{ #category : 'services' }
SmalltalkCompleter class >> thatStartsCaseSensitive: prefix [
	self protected: [
		Selectors forPrefix: prefix keysAndValuesDo: [ :key :value | ^key ] ].
	^nil
]

{ #category : 'instance creation' }
SmalltalkCompleter class >> withModel: aStringHolder [

	"Looks it its subclasses the right autocompleter depending on the Type System. If you do not have
	Live Typing installed you will see only one subclass but if Live Typing is intalled LiveTypeingSmalltalkCompleter
	subclass is added - Hernan"
	
	| smalltalkCompleterClass |
	
	smalltalkCompleterClass := self subclasses detect: [ :aSubclass | aSubclass isForCurrentTypeSystem ].
	
	^smalltalkCompleterClass new setModel: aStringHolder
]

{ #category : 'message entries for unknown class - private' }
SmalltalkCompleter >> add: selector and: lastUsedTime to: selectorsToShow [

	^ selectorsToShow size < EntriesLimit
		ifTrue: [ self add: selector and: lastUsedTime whenNotFullTo: selectorsToShow ]
		ifFalse: [ self add: selector and: lastUsedTime whenFullTo: selectorsToShow ]	
]

{ #category : 'message entries for unknown class - private' }
SmalltalkCompleter >> add: selector and: lastUsedTime whenFullTo: selectorsToShow [

	selectorsToShow
		findBinaryIndex: [ :selectorAndTime | selectorAndTime second < lastUsedTime ifTrue: [ -1 ] ifFalse: [ 1 ]]
		do: [ :found | ]
		ifNone: [ :leftBound :rightBound | self insert: selector and: lastUsedTime at: rightBound to: selectorsToShow ].
		
	^selectorsToShow 

]

{ #category : 'message entries for unknown class - private' }
SmalltalkCompleter >> add: selector and: lastUsedTime whenNotFullTo: selectorsToShow [
			
	selectorsToShow add: { selector . lastUsedTime }.
	
	^selectorsToShow size = EntriesLimit 
		ifTrue: [ self sortByLastUsedTime: selectorsToShow ]
		ifFalse: [ selectorsToShow ]
	
]

{ #category : 'accessing' }
SmalltalkCompleter >> allSource [
	^model actualContents string
]

{ #category : 'entries - private' }
SmalltalkCompleter >> canComputeMessageEntriesFor: prevRange  [

	^ prevRange rangeType notNil 
]

{ #category : 'testing' }
SmalltalkCompleter >> canSelect: anEntry [

	^ (self isCategoryEntry: anEntry) not
]

{ #category : 'testing' }
SmalltalkCompleter >> canShowSelectorDocumentation [

	^canShowSelectorDocumentation 
]

{ #category : 'message entries - private' }
SmalltalkCompleter >> classOfLiteral: aLiteral in: aClass  [
	
	| compilerClass |
	
	compilerClass := aClass ifNil: [ Compiler ] ifNotNil: [ aClass compilerClass ].
	
	^ (compilerClass evaluate: aLiteral) class 
]

{ #category : 'message entries - private' }
SmalltalkCompleter >> classOfLocalBindingNamed: aName in: aClass  [
	
	^  (aClass localBindingOf: aName) ifNotNil: [ :aBinding | aBinding value class ]
]

{ #category : 'entries' }
SmalltalkCompleter >> computeEntries [

	| allSource source contextClass specificModel separator fragmentStart fragmentEnd done |

	specificModel := self textProviderOrModel.
	contextClass := self selectedClassOrMetaClassIn: specificModel.

	allSource := self allSource.
	(specificModel is: #CodeProvider)
		ifTrue: [source := allSource copyFrom: 1 to: position]
		ifFalse: [
			separator := String newLineString, String newLineString.
			fragmentStart := 1.
			done := false.
			[done] whileFalse: [
				fragmentEnd := allSource
					indexOfSubCollection: separator
					startingAt: fragmentStart
					ifAbsent: [allSource size].
				fragmentEnd >= position ifTrue: [
					source := allSource copyFrom: fragmentStart to: position.
					done := true ].
				fragmentStart := fragmentEnd+separator size ]].

	allRanges := self parse: source in: contextClass and: specificModel.
	currentRange := allRanges lastIfEmpty: [ ^entries := #() ].
	possibleInvalidSelectors := #().

	currentRange end = source size
		ifTrue: [ self computeEntriesOfMessageOrIdentifiersFor: source in: contextClass and: specificModel ]
	 	ifFalse: [ self computeMessageEntriesWithEmptyPrefixFor: source in: contextClass and: specificModel ].

	
]

{ #category : 'entries - private' }
SmalltalkCompleter >> computeEntriesOfMessageOrIdentifiersFor: allSource in: contextClass and: specificModel [

	prefix := allSource copyFrom: currentRange start to: currentRange end.
	
	currentRange correspondsToMessage ifTrue: [ 
		^self computeMessageEntriesWhenSendingMessageFor: allSource in: contextClass and: specificModel ].	
	(currentRange correspondsToPartialOrFullIdentifier: parser) ifTrue: [ ^self computeIdentifierEntries ].
	currentRange correspondsToMethodDefinition ifTrue: [ ^self computeMethodNamesFor: contextClass ].
	currentRange correspondsToSymbol ifTrue: [ ^self computeSymbols ].
	
	"If we don't know what to do, do nothing"
	entries := #() 
	
]

{ #category : 'message entries - private' }
SmalltalkCompleter >> computeEntriesOfUnaryMessageReturnNamed: aSelector     [
	
	self subclassResponsibility 
]

{ #category : 'identifier entries - private' }
SmalltalkCompleter >> computeIdentifierEntries [
	
	canShowSelectorDocumentation := false.
	entries := self computeIdentifierEntriesBeginningWith: prefix.
]

{ #category : 'identifier entries - private' }
SmalltalkCompleter >> computeIdentifierEntriesBeginningWith: aPrefix [
	"Use an aux Set to avoid duplicates, but keep the order given."

	| entriesSet lastTitle candidates |
	entriesSet := Set new.
	lastTitle := nil.
	
	candidates := Array streamContents: [ :strm |
		parser namesBeginningWith: aPrefix do: [ :identifier :kindOfIdentifierTitle |
			(entriesSet includes: identifier) ifFalse: [
				kindOfIdentifierTitle = lastTitle ifFalse: [
					strm nextPut: kindOfIdentifierTitle.
					lastTitle := kindOfIdentifierTitle ].
				entriesSet  add: identifier.
				strm nextPut: identifier ]]].
	entriesSet size = 1 ifTrue: [
		^ Array with: entriesSet anyOne ]
		ifFalse: [ ^ candidates ]
]

{ #category : 'message entries - private' }
SmalltalkCompleter >> computeMessageEntriesFor: allSource in: contextClass and: specificModel [
	
	| id rangeType |

	canShowSelectorDocumentation := true.
	id := allSource copyFrom: currentRange start to: currentRange end.
	rangeType := currentRange rangeType.

	rangeType == #globalVar
		ifTrue: [ ^self computeMessageEntriesForClass: (Smalltalk at: id asSymbol) class ].
	rangeType == #self
		ifTrue: [ ^self computeMessageEntriesForClass: contextClass ].
	(rangeType == #super and: [ contextClass notNil ])
		ifTrue: [ ^self computeMessageEntriesForClass: contextClass superclass ].
	rangeType == #true 
		ifTrue: [ ^self computeMessageEntriesForClass: True ].
	rangeType == #false
		ifTrue: [ ^self computeMessageEntriesForClass: False ].
	rangeType == #nil
		ifTrue: [ ^self computeMessageEntriesForClass: UndefinedObject ].
	rangeType == #character
		ifTrue: [ ^self computeMessageEntriesForClass: Character ].
	rangeType == #number
		ifTrue: [ ^self computeMessageEntriesForClass: (self classOfLiteral: id in: contextClass) ].
	rangeType == #string
		ifTrue: [ ^self computeMessageEntriesForClass: (self classOfLiteral: id in: contextClass) ].
	rangeType == #symbol
		ifTrue: [ ^self computeMessageEntriesForClass: (self classOfLiteral: id in: contextClass) ].
	rangeType == #stringSymbol
		ifTrue: [ ^self computeMessageEntriesForClass: (self classOfLiteral: id in: contextClass) ].
	rangeType == #instVar
		ifTrue: [ ^specificModel computeMessageEntriesIn: self ofInstVarNamed: id ].
	rangeType == #methodArg
		ifTrue: [ ^specificModel computeMessageEntriesIn: self ofTempVarNamed: id ].
	rangeType == #tempVar
		ifTrue: [ ^specificModel computeMessageEntriesIn: self ofTempVarNamed: id ].
	rangeType == #blockArg
		ifTrue: [ ^specificModel computeMessageEntriesIn: self ofBlockArgNamed: id ].
	rangeType == #blockTempVar
		ifTrue: [ ^specificModel computeMessageEntriesIn: self ofBlockTempVarNamed: id ].
	rangeType == #workspaceVar
		ifTrue: [ ^self computeMessageEntriesForClassOrNil: (specificModel classOfWorkspaceVarNamed: id) ].
	rangeType == #thisContext
		ifTrue: [ ^self computeMessageEntriesForClass: (specificModel classOfThisContext) ]. 
	rangeType == #classVar
		ifTrue: [ ^self computeMessageEntriesForClassOrNil: (self classOfLocalBindingNamed: id in: contextClass) ].
	rangeType == #poolConstant
		ifTrue: [ ^self computeMessageEntriesForClassOrNil: (self classOfLocalBindingNamed: id in: contextClass) ].
	(rangeType beginsWith: #blockEnd)
		ifTrue: [ ^self computeMessageEntriesForClass: BlockClosure ].
	rangeType == #arrayEnd
		ifTrue: [ ^self computeMessageEntriesForClass: Array ].
	(rangeType beginsWith: #rightBrace)
		ifTrue: [ ^self computeMessageEntriesForClass: Array ].
	rangeType == #unary
		ifTrue: [ ^self computeEntriesOfUnaryMessageReturnNamed: id ].
	(rangeType beginsWith: #rightParenthesis)
		ifTrue: [ ^self computeMessageEntriesOfEnclosedExpressionReturn].
	rangeType == #cascadeSeparator
		ifTrue: [ ^self computeMessageEntriesOfCascadeReceiver]. 

	self computeMessageEntriesForUnknowClass.	
	
]

{ #category : 'message entries - private' }
SmalltalkCompleter >> computeMessageEntriesForClass: aClass   [

	self isPreviousMessageSendBinary
		ifTrue: [ self computeMessageEntriesWithBinaryMessageForClass: aClass ]
		ifFalse: [ self computeMessageEntriesWithoutBinaryMessageForClass: aClass ].
		
	self ifEmptyEntriesShowAllPrefixedSelectors
]

{ #category : 'message entries - private' }
SmalltalkCompleter >> computeMessageEntriesForClassOrNil: aClassOrNil   [

	aClassOrNil 
		ifNil: [ self computeMessageEntriesForUnknowClass ]
		ifNotNil: [ self computeMessageEntriesForClass: aClassOrNil ].
	

]

{ #category : 'message entries for unknown class - private' }
SmalltalkCompleter >> computeMessageEntriesForUnknowClass [

	selectorsClasses := #().
	possibleInvalidSelectors := IdentitySet new.
	canShowSelectorDocumentation := true.
	entries := self computeMessageEntriesForUnknowClassAddingPossibleInvalidSelectorsTo: possibleInvalidSelectors.
]

{ #category : 'message entries for unknown class - private' }
SmalltalkCompleter >> computeMessageEntriesForUnknowClassAddingPossibleInvalidSelectorsTo: aCollection [

	| selectorsToShow |
	selectorsToShow := OrderedCollection new.
	
	self class protected: [
		Selectors forPrefix: prefix keysAndValuesDo: [ :selector :lastUsedTime |
				selectorsToShow := self add: selector and: lastUsedTime to: selectorsToShow.
				(Object canUnderstand: selector) ifFalse: [ aCollection add: selector ]]].
				
	selectorsToShow size < EntriesLimit ifTrue: [ selectorsToShow :=  self sortByLastUsedTime: selectorsToShow ].
	
	" To see the timestamps in the menu (need to tweak #insertCompletion: to activate. Right now, just for debugging)
	entries := selectorsToShow collect: [ :ary | ary first, '(', ((DateAndTime fromString: '01/01/1996 00:00') + ary second minutes) printString,')' ]
	"
	^ selectorsToShow collect: [ :selectorAndTime | selectorAndTime first ]
	

]

{ #category : 'message entries - private' }
SmalltalkCompleter >> computeMessageEntriesOfCascadeReceiver    [
	
	self subclassResponsibility 
]

{ #category : 'message entries - private' }
SmalltalkCompleter >> computeMessageEntriesOfEnclosedExpressionReturn    [

	self subclassResponsibility 
]

{ #category : 'message entries - private' }
SmalltalkCompleter >> computeMessageEntriesWhenSendingMessageFor: allSource in: contextClass and: specificModel  [
	
	allRanges removeLast.
	currentRange := allRanges lastIfEmpty: [ SHRange nilObject ].
	possibleBinarySendRange := self lookForBinarySendRange.
	
	^ (self canComputeMessageEntriesFor: currentRange) 
		ifTrue: [ self computeMessageEntriesFor: allSource in: contextClass and: specificModel ]
		ifFalse: [ self computeMessageEntriesForUnknowClass ]

]

{ #category : 'message entries - private' }
SmalltalkCompleter >> computeMessageEntriesWithBinaryMessageForClass: aClass  [

	selectorsClasses := Array with: aClass.
	
	entries := self unaryAndBinarySelectorsOf: aClass beginningWith: prefix.
		
]

{ #category : 'message entries - private' }
SmalltalkCompleter >> computeMessageEntriesWithEmptyPrefixFor: allSource in: contextClass and: specificModel  [
	
	possibleBinarySendRange := self lookForBinarySendRange.
	prefix := ''.

	self computeMessageEntriesFor: allSource in: contextClass and: specificModel .
]

{ #category : 'message entries - private' }
SmalltalkCompleter >> computeMessageEntriesWithoutBinaryMessageForClass: aClass [

	selectorsClasses := Array with: aClass.
	
	entries := self selectorsOf: aClass beginningWith: prefix.
]

{ #category : 'identifier entries - private' }
SmalltalkCompleter >> computeMethodNamesFor: aContextClass [

	canShowSelectorDocumentation := false.
	entries := self computeMethodNamesFor: aContextClass andMethodPrefix: prefix
]

{ #category : 'identifier entries - private' }
SmalltalkCompleter >> computeMethodNamesFor: aContextClass andMethodPrefix: aMethodStartingWith [

	| existing |
	existing := aContextClass selectors.
	^(aContextClass superclass allSelectors 
		select: [ :aSelector | (aSelector beginsWith: aMethodStartingWith) and: [(existing includes: aSelector) not] ] 
		thenCollect: [ :aSelector | aContextClass methodHeaderFor: aSelector ]) asSortedCollection
]

{ #category : 'entries - private' }
SmalltalkCompleter >> computeSymbols [
	
	| prefixWithoutNumeral |
	
	canShowSelectorDocumentation := false.
	prefixWithoutNumeral := prefix allButFirst.
	entries := (Symbol allSymbolsSelect: [:aSymbol | aSymbol beginsWith: prefixWithoutNumeral ]) sorted.
	
]

{ #category : 'selector documentation' }
SmalltalkCompleter >> documentationOf: aMethod [

	self subclassResponsibility 
]

{ #category : 'keyboard' }
SmalltalkCompleter >> handleLeftArrowKeystrokeBefore: kbEvent [
	canShowSelectorDocumentation ifTrue: [
		menuMorph hideSelectorDocumentation ].
	^ true
]

{ #category : 'keyboard' }
SmalltalkCompleter >> handleRightArrowKeystrokeBefore: kbEvent [
	canShowSelectorDocumentation ifTrue: [
		menuMorph showSelectorDocumentation ].
	^ true
]

{ #category : 'message entries - private' }
SmalltalkCompleter >> ifEmptyEntriesShowAllPrefixedSelectors [

	entries isEmpty ifTrue: [ self computeMessageEntriesForUnknowClass ] 
]

{ #category : 'message entries for unknown class - private' }
SmalltalkCompleter >> insert: selector and: lastUsedTime at: insertionIndex to: selectorsToShow [

	insertionIndex <= EntriesLimit ifTrue: [ selectorsToShow insert: { selector . lastUsedTime } shiftingRightAt: insertionIndex ].
	
	^selectorsToShow 
]

{ #category : 'testing' }
SmalltalkCompleter >> isCategoryEntry: anEntry [

	^anEntry beginsWith: AutoCompleterSelectorsCollector categoryEntryHeader
]

{ #category : 'testing' }
SmalltalkCompleter >> isPossibleInvalidEntry: anEntry [

	^possibleInvalidSelectors includes: anEntry 
]

{ #category : 'message entries - private' }
SmalltalkCompleter >> isPreviousMessageSendBinary [

	^possibleBinarySendRange notNil and: [ possibleBinarySendRange rangeType = #binary ].

	
]

{ #category : 'keyboard - private' }
SmalltalkCompleter >> lastCharacterWantsAutoComplete [

	| lastCharIndex lastCharacter |
	textMorph editor hasSelection ifTrue: [ ^ false ].

	lastCharIndex := textMorph editor startIndex-1.
	lastCharIndex <= 0 ifTrue: [ ^ false ].
	lastCharacter := model actualContents at: lastCharIndex.

	^ lastCharacter isAlphaNumeric 
		or: [ lastCharacter isValidInBinarySelectors 
		or: [ lastCharacter == $: 
		or: [ lastCharacter = Character space and: [ Preferences at: #spaceTabOpensAutoCompletion ]]]]
]

{ #category : 'message entries - private' }
SmalltalkCompleter >> lookForBinarySelectorAfter: aStopToken startingAt: anIndex with: aCandidate [

	| rangeAtCurrentIndex currentIndex |
	
	currentIndex := anIndex.
	[ rangeAtCurrentIndex := allRanges at: currentIndex.
	rangeAtCurrentIndex rangeType ~= aStopToken and: [ currentIndex > 1 ]] whileTrue: [ currentIndex := currentIndex - 1 ].

	^currentIndex > 1
		ifTrue: [ allRanges at: currentIndex - 1 ]
		ifFalse: [ aCandidate ].

]

{ #category : 'message entries - private' }
SmalltalkCompleter >> lookForBinarySelectorAfterArrayStartStartingAt: anIndex with: aCandidate [
 
	| foundRange foundRangeIndex |
	
	foundRange := self lookForBinarySelectorAfter: #arrayStart startingAt: anIndex with: aCandidate.
	
	^foundRange rangeType = #symbol
		ifTrue: [ 
			foundRangeIndex := allRanges indexOf: foundRange.
			allRanges at: foundRangeIndex - 1 ifAbsent: [ aCandidate ]]
		ifFalse: [ aCandidate ]
]

{ #category : 'message entries - private' }
SmalltalkCompleter >> lookForBinarySendRange [

	| penultimate currentIndex currentRangeType |

	currentIndex := self lookForNoUnaryMessageSend.
	penultimate := allRanges at: currentIndex - 1 ifAbsent: [ SHRange nilObject ].

	currentRangeType := (allRanges at: currentIndex) rangeType.
	currentRangeType = #rightParenthesis ifTrue: [ 
		^self lookForBinarySelectorAfter: #leftParenthesis startingAt: currentIndex with: penultimate ].	
	currentRangeType = #rightBrace ifTrue: [
		^self lookForBinarySelectorAfter: #leftBrace startingAt: currentIndex with: penultimate ].	
	currentRangeType = #blockEnd ifTrue: [
		^self lookForBinarySelectorAfter: #blockStart startingAt: currentIndex with: penultimate ].	
	currentRangeType = #arrayEnd ifTrue: [
		^self lookForBinarySelectorAfterArrayStartStartingAt: currentIndex with: penultimate ].	

	^({#'$'. #symbol} includes: penultimate rangeType)
		ifTrue: [ allRanges at: currentIndex - 2 ifAbsent: [ SHRange nilObject ] ]
		ifFalse: [ penultimate ]
]

{ #category : 'message entries - private' }
SmalltalkCompleter >> lookForNoUnaryMessageSend [
		
	| currentIndex currentRangeType |
	
	currentIndex := allRanges size.
	[ currentRangeType := (allRanges at: currentIndex) rangeType.
	currentRangeType = #unary and: [ currentIndex > 1 ]] whileTrue: [ currentIndex := currentIndex - 1 ].

	^currentIndex
]

{ #category : 'menu morph' }
SmalltalkCompleter >> morphClass [
	^SmalltalkCompleterMorph
]

{ #category : 'entries' }
SmalltalkCompleter >> newCursorPosition: anEntry [

	^currentRange correspondsToMethodDefinition
		ifTrue: [ anEntry size - 1]
		ifFalse: [ 
			currentRange correspondsToSymbol 
				ifTrue: [anEntry size + 1]
				ifFalse: [ anEntry indexOf: $ ]]
]

{ #category : 'testing' }
SmalltalkCompleter >> opensWithTab [

	^true
]

{ #category : 'entries - private' }
SmalltalkCompleter >> parse: source in: contextClass and: specificModel [
	
	| isMethod |
	parser := SHParserST80 new.
	parser
		workspace: ((specificModel is: #providesBindings) ifTrue: [specificModel]);
		classOrMetaClass: contextClass;
		source: source.

	isMethod := (specificModel is: #Browser)
		ifTrue: [ specificModel isEditingMethod ]
		ifFalse: [ specificModel is: #CodeProvider ].
	parser parse: isMethod.

	^ parser rangesWithoutExcessCode.
]

{ #category : 'accessing' }
SmalltalkCompleter >> possibleInvalidSelectors [
	
	^ possibleInvalidSelectors
]

{ #category : 'entries - private' }
SmalltalkCompleter >> selectedClassOrMetaClassIn: specificModel [

	(specificModel is: #CodeProvider) ifTrue: [ ^ specificModel selectedClassOrMetaClass ].
	
	"I can not use #selectedClassOrMetaClass becuase it changes with the selection but when compiling to evaluate it assumes object as receiver - Hernan"
	^ (specificModel isKindOf: Inspector) ifTrue: [ specificModel objectClass ] 
]

{ #category : 'entries' }
SmalltalkCompleter >> selectedEntryFormatted [

	| selectedEntry |
	
	selectedEntry := self entries at: menuMorph selected.
	^currentRange correspondsToSymbol
		ifTrue: [ '#', selectedEntry ]
		ifFalse: [ selectedEntry separateKeywords ]
]

{ #category : 'entries - private' }
SmalltalkCompleter >> selectorsClasses [

	^selectorsClasses 
]

{ #category : 'message entries - private' }
SmalltalkCompleter >> selectorsOf: aClass beginningWith: aPrefix [

	^ (AutoCompleterSelectorsCollector for: aPrefix) 
		addSelectorsOf: aClass;
		entriesToShow
]

{ #category : 'message entries for unknown class - private' }
SmalltalkCompleter >> sortByLastUsedTime: selectorsToShow [

	^selectorsToShow asArray sort: [ :leftSelectorAndTime :rightSelectorAndTime | leftSelectorAndTime second > rightSelectorAndTime second ]
]

{ #category : 'entries - private' }
SmalltalkCompleter >> textProviderOrModel [

	^ (model is: #hasTextProvider) ifTrue: [ model textProvider ] ifFalse: [ model ].
]

{ #category : 'message entries - private' }
SmalltalkCompleter >> unaryAndBinarySelectorsOf: aClass beginningWith: aPrefix  [

	^ (AutoCompleterSelectorsCollector for: aPrefix) 
		addUnaryAndBinarySelectorsOf: aClass;
		entriesToShow
]
