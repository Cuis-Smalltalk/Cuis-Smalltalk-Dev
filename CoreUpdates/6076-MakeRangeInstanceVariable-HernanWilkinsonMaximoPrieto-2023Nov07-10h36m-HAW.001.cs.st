'From Cuis 6.0 [latest update: #6068] on 7 November 2023 at 10:56:46 am'!
!classDefinition: #SmalltalkCompleter category: #'Tools-Autocompletion'!
AutoCompleter subclass: #SmalltalkCompleter
	instanceVariableNames: 'parser selectorsClasses possibleInvalidSelectors canShowSelectorDocumentation possibleBinarySendRange allRanges currentRange '
	classVariableNames: 'AccessLock EntriesLimit Selectors '
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 11/7/2023 10:49:43'!
computeEntriesOfUnaryMessageReturnNamed: aSelector    
	
	self subclassResponsibility ! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 11/7/2023 10:53:53'!
computeMessageEntriesFor: allSource in: contextClass and: specificModel  
	
	| id rangeType |

	canShowSelectorDocumentation := true.
	id := allSource copyFrom: currentRange start to: currentRange end.
	rangeType := currentRange rangeType.

	rangeType == #globalVar
		ifTrue: [ ^self computeMessageEntriesForClass: (Smalltalk at: id asSymbol) class ].
	rangeType == #self
		ifTrue: [ ^self computeMessageEntriesForClass: contextClass ].
	rangeType == #super
		ifTrue: [ ^self computeMessageEntriesForClass: contextClass superclass ].
	rangeType == #true 
		ifTrue: [ ^self computeMessageEntriesForClass: True ].
	rangeType == #false
		ifTrue: [ ^self computeMessageEntriesForClass: False ].
	rangeType == #nil
		ifTrue: [ ^self computeMessageEntriesForClass: UndefinedObject ].
	rangeType == #character
		ifTrue: [ ^self computeMessageEntriesForClass: (id first asciiValue ifNotNil: [Character] ifNil: [UnicodeCodePoint]) ].
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

	self computeMessageEntriesForUnknowClass 
	
	! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 11/7/2023 10:51:34'!
computeMessageEntriesOfCascadeReceiver   
	
	self subclassResponsibility ! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 11/7/2023 10:50:41'!
computeMessageEntriesOfEnclosedExpressionReturn   

	self subclassResponsibility ! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 11/7/2023 10:55:32'!
computeMessageEntriesWhenSendingMessageFor: allSource in: contextClass and: specificModel 
	
	allRanges removeLast.
	currentRange := allRanges lastIfEmpty: [ SHRange nilObject ].
	possibleBinarySendRange := self lookForBinarySendRange.
	
	^ (self canComputeMessageEntriesFor: currentRange) 
		ifTrue: [ self computeMessageEntriesFor: allSource in: contextClass and: specificModel ]
		ifFalse: [ self computeMessageEntriesForUnknowClass ]
! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 11/7/2023 10:48:31'!
computeMessageEntriesWithEmptyPrefixFor: allSource in: contextClass and: specificModel 
	
	possibleBinarySendRange := self lookForBinarySendRange.
	prefix := ''.

	self computeMessageEntriesFor: allSource in: contextClass and: specificModel .! !

!SmalltalkCompleter methodsFor: 'entries - private' stamp: 'HAW 11/7/2023 10:55:32'!
computeEntriesOfMessageOrIdentifiersFor: allSource in: contextClass and: specificModel

	prefix := allSource copyFrom: currentRange start to: currentRange end.
	(parser isMessage: currentRange rangeType) ifTrue: [ 
		^self computeMessageEntriesWhenSendingMessageFor: allSource in: contextClass and: specificModel ].	
	(parser isPartialOrFullIdentifier: currentRange rangeType) ifTrue: [ ^self computeIdentifierEntries ].
	(parser isMethodDefinition: currentRange rangeType) ifTrue: [ ^self computeMethodNamesFor: contextClass ].
	(parser isSymbol: currentRange rangeType) ifTrue: [ ^self computeSymbols ].
	
	"If we don't know what to do, do nothing"
	entries := #() ! !


!DynamicTypingSmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 11/7/2023 10:49:43'!
computeEntriesOfUnaryMessageReturnNamed: aSelector    
	
	self computeMessageEntriesForUnknowClass 
	! !

!DynamicTypingSmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 11/7/2023 10:51:34'!
computeMessageEntriesOfCascadeReceiver   
	
	self computeMessageEntriesForUnknowClass
	
! !

!DynamicTypingSmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 11/7/2023 10:50:41'!
computeMessageEntriesOfEnclosedExpressionReturn   

	self computeMessageEntriesForUnknowClass 
	
! !


!SmalltalkCompleter methodsFor: 'entries' stamp: 'HAW 11/7/2023 10:53:53'!
computeEntries

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

	! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 11/7/2023 10:53:41'!
lookForBinarySelectorAfter: aStopToken startingAt: anIndex with: aCandidate

	| rangeAtCurrentIndex currentIndex |
	
	currentIndex := anIndex.
	[ rangeAtCurrentIndex := allRanges at: currentIndex.
	rangeAtCurrentIndex rangeType ~= aStopToken and: [ currentIndex > 1 ]] whileTrue: [ currentIndex := currentIndex - 1 ].

	^currentIndex > 1
		ifTrue: [ allRanges at: currentIndex - 1 ]
		ifFalse: [ aCandidate ].
! !

!SmalltalkCompleter methodsFor: 'entries - private' stamp: 'HAW 11/7/2023 10:42:06'!
computeSymbols
	
	| prefixWithoutNumeral |
	
	canShowSelectorDocumentation := false.
	prefixWithoutNumeral := prefix allButFirst.
	entries := (Symbol allSymbolsSelect: [:aSymbol | aSymbol beginsWith: prefixWithoutNumeral ]) sorted.
	! !

!methodRemoval: DynamicTypingSmalltalkCompleter #computeMessageEntriesOfCascadeReceiverAt stamp: 'HAW 11/7/2023 10:51:34'!
DynamicTypingSmalltalkCompleter removeSelector: #computeMessageEntriesOfCascadeReceiverAt!
!methodRemoval: DynamicTypingSmalltalkCompleter #computeMessageEntriesOfEnclosedExpressionReturnAt: stamp: 'HAW 11/7/2023 10:50:23'!
DynamicTypingSmalltalkCompleter removeSelector: #computeMessageEntriesOfEnclosedExpressionReturnAt:!
!methodRemoval: DynamicTypingSmalltalkCompleter #computeMessageEntriesOfCascadeReceiverAt: stamp: 'HAW 11/7/2023 10:51:13'!
DynamicTypingSmalltalkCompleter removeSelector: #computeMessageEntriesOfCascadeReceiverAt:!
!methodRemoval: DynamicTypingSmalltalkCompleter #computeMessageEntriesOfEnclosedExpressionReturnAt stamp: 'HAW 11/7/2023 10:50:41'!
DynamicTypingSmalltalkCompleter removeSelector: #computeMessageEntriesOfEnclosedExpressionReturnAt!
!methodRemoval: DynamicTypingSmalltalkCompleter #computeEntriesOfUnaryMessageReturnNamed:at: stamp: 'HAW 11/7/2023 10:49:43'!
DynamicTypingSmalltalkCompleter removeSelector: #computeEntriesOfUnaryMessageReturnNamed:at:!
!methodRemoval: SmalltalkCompleter #computeMessageEntriesWithEmptyPrefixFor:at:in:and: stamp: 'HAW 11/7/2023 10:46:14'!
SmalltalkCompleter removeSelector: #computeMessageEntriesWithEmptyPrefixFor:at:in:and:!
!methodRemoval: SmalltalkCompleter #computeMessageEntriesOfEnclosedExpressionReturnAt: stamp: 'HAW 11/7/2023 10:50:23'!
SmalltalkCompleter removeSelector: #computeMessageEntriesOfEnclosedExpressionReturnAt:!
!methodRemoval: SmalltalkCompleter #computeEntriesOfMessageOrIdentifiersFor:at:in:and: stamp: 'HAW 11/7/2023 10:45:47'!
SmalltalkCompleter removeSelector: #computeEntriesOfMessageOrIdentifiersFor:at:in:and:!
!methodRemoval: SmalltalkCompleter #computeMessageEntriesOfCascadeReceiverAt stamp: 'HAW 11/7/2023 10:51:34'!
SmalltalkCompleter removeSelector: #computeMessageEntriesOfCascadeReceiverAt!
!methodRemoval: SmalltalkCompleter #computeEntriesOfUnaryMessageReturnNamed:at: stamp: 'HAW 11/7/2023 10:49:43'!
SmalltalkCompleter removeSelector: #computeEntriesOfUnaryMessageReturnNamed:at:!
!methodRemoval: SmalltalkCompleter #computeMessageEntriesFor:at:in:and: stamp: 'HAW 11/7/2023 10:48:31'!
SmalltalkCompleter removeSelector: #computeMessageEntriesFor:at:in:and:!
!methodRemoval: SmalltalkCompleter #computeMessageEntriesOfCascadeReceiverAt: stamp: 'HAW 11/7/2023 10:51:13'!
SmalltalkCompleter removeSelector: #computeMessageEntriesOfCascadeReceiverAt:!
!methodRemoval: SmalltalkCompleter #computeMessageEntriesWhenSendinMessageFor:in:and: stamp: 'HAW 11/7/2023 10:55:32'!
SmalltalkCompleter removeSelector: #computeMessageEntriesWhenSendinMessageFor:in:and:!
!methodRemoval: SmalltalkCompleter #computeMessageEntriesOfEnclosedExpressionReturnAt stamp: 'HAW 11/7/2023 10:50:41'!
SmalltalkCompleter removeSelector: #computeMessageEntriesOfEnclosedExpressionReturnAt!
!classDefinition: #SmalltalkCompleter category: #'Tools-Autocompletion'!
AutoCompleter subclass: #SmalltalkCompleter
	instanceVariableNames: 'parser selectorsClasses possibleInvalidSelectors canShowSelectorDocumentation possibleBinarySendRange allRanges currentRange'
	classVariableNames: 'AccessLock EntriesLimit Selectors'
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!

!SmalltalkCompleter reorganize!
('entries' computeEntries newCursorPosition: selectedEntryFormatted)
('testing' canSelect: canShowSelectorDocumentation isCategoryEntry: isPossibleInvalidEntry: opensWithTab)
('message entries - private' classOfLiteral:in: classOfLocalBindingNamed:in: computeEntriesOfUnaryMessageReturnNamed: computeMessageEntriesFor:in:and: computeMessageEntriesForClass: computeMessageEntriesForClassOrNil: computeMessageEntriesOfCascadeReceiver computeMessageEntriesOfEnclosedExpressionReturn computeMessageEntriesWhenSendingMessageFor:in:and: computeMessageEntriesWithBinaryMessageForClass: computeMessageEntriesWithEmptyPrefixFor:in:and: computeMessageEntriesWithoutBinaryMessageForClass: ifEmptyEntriesShowAllPrefixedSelectors isPreviousMessageSendBinary lookForBinarySelectorAfter:startingAt:with: lookForBinarySelectorAfterArrayStartStartingAt:with: lookForBinarySendRange lookForNoUnaryMessageSend selectorsOf:beginningWith: unaryAndBinarySelectorsOf:beginningWith:)
('identifier entries - private' computeIdentifierEntries computeIdentifierEntriesBeginningWith: computeMethodNamesFor: computeMethodNamesFor:andMethodPrefix:)
('message entries for unknown class - private' add:and:to: add:and:whenFullTo: add:and:whenNotFullTo: computeMessageEntriesForUnknowClass computeMessageEntriesForUnknowClassAddingPossibleInvalidSelectorsTo: insert:and:at:to: sortByLastUsedTime:)
('selector documentation' documentationOf:)
('accessing' allSource possibleInvalidSelectors)
('menu morph' morphClass)
('keyboard' handleLeftArrowKeystrokeBefore: handleRightArrowKeystrokeBefore:)
('entries - private' canComputeMessageEntriesFor: computeEntriesOfMessageOrIdentifiersFor:in:and: computeSymbols parse:in:and: selectedClassOrMetaClassIn: selectorsClasses textProviderOrModel)
!

