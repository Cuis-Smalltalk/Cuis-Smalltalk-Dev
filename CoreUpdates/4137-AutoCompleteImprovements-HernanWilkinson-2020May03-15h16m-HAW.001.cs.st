'From Cuis 5.0 [latest update: #4132] on 6 May 2020 at 11:46:42 pm'!
!classDefinition: #SmalltalkCompleter category: #'Tools-Autocompletion'!
AutoCompleter subclass: #SmalltalkCompleter
	instanceVariableNames: 'parser selectorsClasses possibleInvalidSelectors canShowSelectorDocumentation possibleBinarySendRange allRanges '
	classVariableNames: 'AccessLock EntriesLimit Selectors '
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!
!classDefinition: #SHParserST80 category: #'Tools-Syntax Highlighting'!
Object subclass: #SHParserST80
	instanceVariableNames: 'classOrMetaClass source workspace arguments sourcePosition currentToken currentTokenFirst temporaries instanceVariables errorBlock currentTokenSourcePosition blockDepth bracketDepth ranges blockDepths blockDepthsStartIndexes allSource braceDepth '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Syntax Highlighting'!

!SequenceableCollection methodsFor: 'accessing' stamp: 'HAW 5/4/2020 00:39:03'!
lastIfEmpty: aBlock
	
	^self ifEmpty: aBlock ifNotEmpty: [ self at: self size ]! !


!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 5/4/2020 02:07:45'!
computeEntriesOfMessageOrIdentifiersFor: allSource at: range in: contextClass and: specificModel
	
	prefix _ allSource copyFrom: range start to: range end.
	(parser isMessage: range rangeType) ifTrue: [ 
		^self computeMessageEntriesWhenSendinMessageFor: allSource in: contextClass and: specificModel ].	
	(parser isPartialOrFullIdentifier: range rangeType) ifTrue: [ ^self computeIdentifierEntries ].
	
	"If we don't know what to do, do nothing"
	entries _ #() ! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 5/4/2020 15:42:32'!
computeMessageEntriesWhenSendinMessageFor: allSource in: contextClass and: specificModel 
	
	| lastRange |
	
	allRanges removeLast.
	lastRange _ allRanges lastIfEmpty: [ SHRange nilObject ].
	possibleBinarySendRange _ self lookForBinarySendRange.
	
	^ (self canComputeMessageEntriesFor: lastRange) 
		ifTrue: [ self computeMessageEntriesFor: allSource at: lastRange in: contextClass and: specificModel ]
		ifFalse: [ self computeMessageEntriesForUnknowClass ]
! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 5/4/2020 15:42:32'!
computeMessageEntriesWithEmptyPrefixFor: allSource at: range in: contextClass and: specificModel 
	
	possibleBinarySendRange _ self lookForBinarySendRange.
	prefix _ ''.

	self computeMessageEntriesFor: allSource at: range in: contextClass and: specificModel .! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 5/4/2020 15:49:19'!
lookForBinarySelectorAfter: aStopToken startingAt: anIndex with: aCandidate

	| currentRange currentIndex |
	
	currentIndex := anIndex.
	[ currentRange := allRanges at: currentIndex.
	currentRange rangeType ~= aStopToken and: [ currentIndex > 1 ]] whileTrue: [ currentIndex := currentIndex - 1 ].

	^currentIndex > 1
		ifTrue: [ allRanges at: currentIndex - 1 ]
		ifFalse: [ aCandidate ].
! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 5/4/2020 15:43:49'!
lookForBinarySelectorAfterArrayStartStartingAt: anIndex with: aCandidate
 
	| foundRange foundRangeIndex |
	
	foundRange := self lookForBinarySelectorAfter: #arrayStart startingAt: anIndex with: aCandidate.
	
	^foundRange rangeType = #symbol
		ifTrue: [ 
			foundRangeIndex := allRanges indexOf: foundRange.
			allRanges at: foundRangeIndex - 1 ifAbsent: [ aCandidate ]]
		ifFalse: [ aCandidate ]! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 5/4/2020 15:45:27'!
lookForBinarySendRange

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
		ifFalse: [ penultimate ]! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 5/4/2020 15:46:02'!
lookForNoUnaryMessageSend
		
	| currentIndex currentRangeType |
	
	currentIndex := allRanges size.
	[ currentRangeType := (allRanges at: currentIndex) rangeType.
	currentRangeType = #unary and: [ currentIndex > 1 ]] whileTrue: [ currentIndex := currentIndex - 1 ].

	^currentIndex! !


!SmalltalkCompleter methodsFor: 'entries' stamp: 'HAW 5/4/2020 02:09:47'!
computeEntries

	| allSource contextClass specificModel range |

	allSource _ model actualContents string.
	specificModel _ self textProviderOrModel.
	contextClass _ self selectedClassOrMetaClassIn: specificModel. 
	allRanges _ self parse: allSource in: contextClass and: specificModel.
	"For debugging porpouses: 
	allRanges collect: [ :r | r rangeType ]
	"
	range _ allRanges lastIfEmpty: [ ^entries _ #() ].
	possibleInvalidSelectors _ #().
	
	range end = position
		ifTrue: [ self computeEntriesOfMessageOrIdentifiersFor: allSource at: range in: contextClass and: specificModel ]
	 	ifFalse: [ self computeMessageEntriesWithEmptyPrefixFor: allSource at: range in: contextClass and: specificModel ].

	! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 5/4/2020 16:06:03'!
computeMessageEntriesFor: allSource at: range in: contextClass and: specificModel  
	
	| id rangeType |

	canShowSelectorDocumentation _ true.
	id _ allSource copyFrom: range start to: range end.
	rangeType _ range rangeType.

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
		ifTrue: [ ^self computeMessageEntriesForClass: id first class ].
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
		ifTrue: [ ^self computeEntriesOfUnaryMessageReturnNamed: id at: range ].
	(rangeType beginsWith: #rightParenthesis)
		ifTrue: [ ^self computeMessageEntriesOfEnclosedExpressionReturnAt: range ].
	rangeType == #cascadeSeparator
		ifTrue: [ ^self computeMessageEntriesOfCascadeReceiverAt: range ]. 

	self computeMessageEntriesForUnknowClass 
	
	! !


!SHParserST80 methodsFor: 'parse' stamp: 'HAW 5/3/2020 21:54:53'!
parse: isAMethod 
	"Parse the receiver's text. If isAMethod is true
    then treat text as a method, if false as an
    expression with no message pattern"

	| continue prevSourcePosition |
	self initializeInstanceVariables.
	sourcePosition _ 1.
	arguments _ Dictionary new.
	temporaries _ Dictionary new.
	blockDepth _ bracketDepth _ braceDepth _ 0.
	blockDepths _ OrderedCollection with: blockDepth.
	blockDepthsStartIndexes _ OrderedCollection with: sourcePosition.
	ranges ifNil: [ ranges := OrderedCollection new: 100] ifNotNil: [ ranges reset].
	errorBlock _ [^false].
	[
		self scanNext.
		isAMethod 
			ifTrue: [
				self parseMessagePattern.
				self parsePragmaSequence].
		self parseMethodTemporaries.
		isAMethod ifTrue: [self parsePragmaSequence].
		"Iterate once for methods, but pontentially several times for workspaces
		(to recover after errors, for possible good next lines or chunks)"
		continue _ true.
		[ continue ] whileTrue: [
			prevSourcePosition _ sourcePosition.
			self parseStatementList.
			continue _ sourcePosition > prevSourcePosition.
			isAMethod
				ifTrue: [
					"Only if we are parsing a method, consider everything after this point as error."
					currentToken ifNotNil: [ self error ].
					continue _ false]
				ifFalse: [
					sourcePosition > source size ifTrue: [continue _ false]]].
	] ensure: [errorBlock _ nil].
	^true! !

!SHParserST80 methodsFor: 'parse' stamp: 'HAW 5/3/2020 21:56:35'!
parseBraceArray
	self parseStatementListForBraceArray.
	self failUnless: currentTokenFirst == $}.
	self scanPast: #rightBrace level: braceDepth.
	braceDepth := braceDepth - 1! !

!SHParserST80 methodsFor: 'parse' stamp: 'HAW 5/3/2020 21:57:08'!
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

!methodRemoval: SmalltalkCompleter #lookForBinarySelectorAfter:in:startingAt:with: stamp: 'HAW 5/4/2020 15:39:49'!
SmalltalkCompleter removeSelector: #lookForBinarySelectorAfter:in:startingAt:with:!
!methodRemoval: SmalltalkCompleter #computeMessageEntriesWhenSendinMessageFor:using:in:and: stamp: 'HAW 5/4/2020 02:07:45'!
SmalltalkCompleter removeSelector: #computeMessageEntriesWhenSendinMessageFor:using:in:and:!
!methodRemoval: SmalltalkCompleter #computeEntriesOfMessageOrIdentifiersFor:using:at:in:and: stamp: 'HAW 5/4/2020 02:05:30'!
SmalltalkCompleter removeSelector: #computeEntriesOfMessageOrIdentifiersFor:using:at:in:and:!
!methodRemoval: SmalltalkCompleter #moveUpTo:from: stamp: 'HAW 5/3/2020 22:40:36'!
SmalltalkCompleter removeSelector: #moveUpTo:from:!
!methodRemoval: SmalltalkCompleter #xinitialize stamp: 'HAW 5/3/2020 23:06:39'!
SmalltalkCompleter removeSelector: #xinitialize!
!methodRemoval: SmalltalkCompleter #moveUpTo:from:in: stamp: 'HAW 5/3/2020 22:40:47'!
SmalltalkCompleter removeSelector: #moveUpTo:from:in:!
!methodRemoval: SmalltalkCompleter #initialize stamp: 'HAW 5/3/2020 23:01:57'!
SmalltalkCompleter removeSelector: #initialize!
!methodRemoval: SmalltalkCompleter #startingAt:with: stamp: 'HAW 5/4/2020 15:43:49'!
SmalltalkCompleter removeSelector: #startingAt:with:!
!methodRemoval: SmalltalkCompleter #eatParenthesisFrom:in: stamp: 'HAW 5/3/2020 22:40:50'!
SmalltalkCompleter removeSelector: #eatParenthesisFrom:in:!
!methodRemoval: SmalltalkCompleter #possibleBinarySendRangeFrom: stamp: 'HAW 5/4/2020 15:41:57'!
SmalltalkCompleter removeSelector: #possibleBinarySendRangeFrom:!
!methodRemoval: SmalltalkCompleter #possibleBinarySendRangeFrom stamp: 'HAW 5/4/2020 15:42:32'!
SmalltalkCompleter removeSelector: #possibleBinarySendRangeFrom!
!methodRemoval: SmalltalkCompleter #lookForBinarySelectorAfter:in:startingAt: stamp: 'HAW 5/3/2020 23:51:40'!
SmalltalkCompleter removeSelector: #lookForBinarySelectorAfter:in:startingAt:!
!methodRemoval: SmalltalkCompleter #lookForBinarySelectorAfterArrayStartIn:startingAt:with: stamp: 'HAW 5/4/2020 15:43:23'!
SmalltalkCompleter removeSelector: #lookForBinarySelectorAfterArrayStartIn:startingAt:with:!
!methodRemoval: SmalltalkCompleter #computeMessageEntriesWithEmptyPrefixFor:using:at:in:and: stamp: 'HAW 5/4/2020 02:06:28'!
SmalltalkCompleter removeSelector: #computeMessageEntriesWithEmptyPrefixFor:using:at:in:and:!
!methodRemoval: SmalltalkCompleter #lookForBinarySelectorAfterArrayStartIn:startingAt: stamp: 'HAW 5/3/2020 23:51:43'!
SmalltalkCompleter removeSelector: #lookForBinarySelectorAfterArrayStartIn:startingAt:!
!classDefinition: #SmalltalkCompleter category: #'Tools-Autocompletion'!
AutoCompleter subclass: #SmalltalkCompleter
	instanceVariableNames: 'parser selectorsClasses possibleInvalidSelectors canShowSelectorDocumentation possibleBinarySendRange allRanges'
	classVariableNames: 'AccessLock EntriesLimit Selectors'
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!

!SmalltalkCompleter reorganize!
('entries' computeEntries newCursorPosition: selectedEntryFormatted)
('entries - private ' canComputeMessageEntriesFor: computeEntriesOfMessageOrIdentifiersFor:at:in:and: parse:in:and: selectedClassOrMetaClassIn: selectorsClasses textProviderOrModel)
('testing' canSelect: canShowSelectorDocumentation isCategoryEntry: isPossibleInvalidEntry: opensWithTab)
('message entries - private' classOfLiteral:in: classOfLocalBindingNamed:in: computeEntriesOfUnaryMessageReturnNamed:at: computeMessageEntriesFor:at:in:and: computeMessageEntriesForClass: computeMessageEntriesForClassOrNil: computeMessageEntriesOfCascadeReceiverAt: computeMessageEntriesOfEnclosedExpressionReturnAt: computeMessageEntriesWhenSendinMessageFor:in:and: computeMessageEntriesWithBinaryMessageForClass: computeMessageEntriesWithEmptyPrefixFor:at:in:and: computeMessageEntriesWithoutBinaryMessageForClass: ifEmptyEntriesShowAllPrefixedSelectors isPreviousMessageSendBinary lookForBinarySelectorAfter:startingAt:with: lookForBinarySelectorAfterArrayStartStartingAt:with: lookForBinarySendRange lookForNoUnaryMessageSend selectorsOf:beginningWith: unaryAndBinarySelectorsOf:beginningWith:)
('identifier entries - private' computeIdentifierEntries computeIdentifierEntriesBeginningWith:)
('message entries for unknown class - private' add:and:to: add:and:whenFullTo: add:and:whenNotFullTo: computeMessageEntriesForUnknowClass computeMessageEntriesForUnknowClassAddingPossibleInvalidSelectorsTo: insert:and:at:to: sortByLastUsedTime:)
('selector documentation' documentationOf:)
('accessing' changePositionTo: possibleInvalidSelectors)
!

!classDefinition: #SHParserST80 category: #'Tools-Syntax Highlighting'!
Object subclass: #SHParserST80
	instanceVariableNames: 'classOrMetaClass source workspace arguments sourcePosition currentToken currentTokenFirst temporaries instanceVariables errorBlock currentTokenSourcePosition blockDepth bracketDepth ranges blockDepths blockDepthsStartIndexes allSource braceDepth'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Syntax Highlighting'!
