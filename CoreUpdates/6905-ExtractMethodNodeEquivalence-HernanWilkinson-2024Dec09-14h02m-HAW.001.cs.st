'From Cuis7.3 [latest update: #6904] on 9 December 2024 at 2:04:51 pm'!
!classDefinition: #ExtractMethodReplacementsFinder category: #'Tools-Refactoring'!
Object subclass: #ExtractMethodReplacementsFinder
	instanceVariableNames: 'intervalToExtract sourceMethod replacements newMessage sourceCodeToExtract sizeToExtract findingStrategy nodesToExtract '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #NodesInRangeFinder category: #'Tools-Refactoring'!
Object subclass: #NodesInRangeFinder
	instanceVariableNames: 'range sourceMethod sourceMethodNode statements sourceRangeIndexByNode firstNodeFound lastNodeRange sourceCode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractMethodReplacement category: #'Tools-Refactoring'!
Refactoring subclass: #ExtractMethodReplacement
	instanceVariableNames: 'intervalToExtract newMessage methodToExtractFrom callingExpression sendCollaboration '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ExtractMethodReplacementsFinder methodsFor: 'private-replacement finding' stamp: 'HAW 11/4/2024 00:12:58'!
defineFindingStrategy
	
	| useStringEquality |
	
	"Sadly, finding the right ranges when extracting literal nodes is almost imposible becuase literal nodes can have many
	ranges. An alternative algorithm could be to create an 'expanded AST' without reused nodes and each node with its own
	source range. Nodes of that AST could be a polimorphic proxy of the real node, adding only the source range, something
	to try ...
	For the moment, if the selection to extract has only literal nodes, uses string comparison - Hernan"
	useStringEquality := nodesToExtract allSatisfy: [ :aNode | aNode isLiteralNode ].
	
	^useStringEquality 
		ifTrue: [ [ :aMethod | self findReplacementsUsingStringEqualityIn: aMethod ]]
		ifFalse: [ [ :aMethod | self findReplacementsUsingNodeEquivalenceIn: aMethod ]]! !

!ExtractMethodReplacementsFinder methodsFor: 'private-replacement finding' stamp: 'HAW 11/4/2024 00:09:42'!
findReplacementInHierarchy

	sourceMethod methodClass withAllSubclassesDo: [ :aClass | self findReplacementsAt: aClass]! !

!ExtractMethodReplacementsFinder methodsFor: 'private-replacement finding' stamp: 'HAW 11/2/2024 20:15:03'!
findReplacementsUsingNodeEquivalenceIn: aMethod

	| rangesToExtract |
		
	rangesToExtract := sourceMethod compiledMethod = aMethod
		ifTrue: [  aMethod sourceRangesOfNodesEquivalentTo: nodesToExtract keepingSelection: intervalToExtract ]	
		ifFalse: [ aMethod sourceRangesOfNodesEquivalentTo: nodesToExtract ].
		
	rangesToExtract do: [ :sourceRangeToExtract | self addReplacementAt: sourceRangeToExtract in: aMethod ].! !

!ExtractMethodReplacementsFinder methodsFor: 'private-replacement finding' stamp: 'HAW 11/1/2024 15:58:34'!
findReplacementsUsingStringEqualityIn: aMethod

	| sourceCode foundIntervalToExtract sourceCodeToExtractStart theSizeToExtract |
	
	sourceCode := aMethod sourceCode.
	theSizeToExtract := intervalToExtract size - 1.
	sourceCodeToExtractStart := 1.
	
	[ sourceCodeToExtractStart := sourceCode indexOfSubCollection: sourceCodeToExtract startingAt: sourceCodeToExtractStart.
	sourceCodeToExtractStart ~= 0 ] whileTrue: [ 
		foundIntervalToExtract := sourceCodeToExtractStart to: sourceCodeToExtractStart + theSizeToExtract.
		self addReplacementAt: foundIntervalToExtract in: aMethod.
		sourceCodeToExtractStart := foundIntervalToExtract last + 1 ] 
		
	 ! !

!ExtractMethodReplacementsFinder methodsFor: 'private-replacement finding' stamp: 'HAW 11/3/2024 17:36:58'!
nodesToExtract

	^(NodesInRangeFinder in: sourceMethod for: intervalToExtract) value! !


!NodesInRangeFinder methodsFor: 'initialization' stamp: 'HAW 11/2/2024 20:33:48'!
initializeIn: aSourceMethod for: anIntervalToExtract 

	range := anIntervalToExtract.
	sourceMethod := aSourceMethod.
	! !

!NodesInRangeFinder methodsFor: 'evaluating' stamp: 'HAW 11/3/2024 23:48:20'!
value
	
	| nodeAtLastPositionFound |

	"Because some nodes have many ranges, like the literal nodes, we count the time we pass by a node that is equivalent
	to the index in the source ranges collection. sourceRangeIndexByNode is used to count per node - Hernan"
	sourceMethodNode := sourceMethod methodNode.
	sourceCode := sourceMethod sourceCode.
	statements := OrderedCollection new.
	sourceRangeIndexByNode := IdentityBag new.
	firstNodeFound := false.

	sourceMethodNode nodesDo: [ :aNode | | nodeRange |
		sourceRangeIndexByNode add: aNode.
		nodeRange := self completeSourceRangeOf: aNode.
		nodeAtLastPositionFound := self ifInsideRangeAddToStatements: aNode locatedAt: nodeRange.
		nodeAtLastPositionFound ifTrue: [ ^statements ]].
			
	self error: 'nodes not found'
	! !

!NodesInRangeFinder methodsFor: 'evaluating - private' stamp: 'HAW 11/3/2024 17:31:50'!
addToStatementsIf: aNode isAtStartingPositionWhenLocatedAt: nodeRange

	(self isFirstInRange: aNode locatedAt: nodeRange)
		ifTrue: [
			firstNodeFound := true.
			statements add: aNode.
			range last <= nodeRange last ifTrue: [ ^true ].
			lastNodeRange := nodeRange ].

	^false! !

!NodesInRangeFinder methodsFor: 'evaluating - private' stamp: 'HAW 11/3/2024 00:35:32'!
addToStatementsIf: aNode isInsideExtractionWhenLoacatedAt: nodeRange

	(self notSubNodeOfLastNode: nodeRange) ifTrue: [
		statements add: aNode.
		(self isAtEndPosition: nodeRange) ifTrue: [ ^true ].
		lastNodeRange := nodeRange.].
	
	^false! !

!NodesInRangeFinder methodsFor: 'evaluating - private' stamp: 'HAW 11/3/2024 23:32:33'!
completeSourceRangeOf: aNode

	| nodeRange nodeRanges |
	
	"Some nodes do not have ranges at all like BlockNode - Hernan"
	nodeRanges := sourceMethodNode completeSourceRanges at: aNode ifAbsent: [ #() ].
	nodeRange := nodeRanges at: (sourceRangeIndexByNode occurrencesOf: aNode) ifAbsent: [ `0 to: 0` ].
	nodeRange := nodeRange asSourceCodeInterval trimToMatchExpressionOn: sourceCode.
	
	^nodeRange
		! !

!NodesInRangeFinder methodsFor: 'evaluating - private' stamp: 'HAW 11/3/2024 17:31:50'!
ifInsideRangeAddToStatements: aNode locatedAt: nodeRange

	^ firstNodeFound
		ifTrue: [ self addToStatementsIf: aNode isInsideExtractionWhenLoacatedAt: nodeRange]
		ifFalse: [ self addToStatementsIf: aNode isAtStartingPositionWhenLocatedAt: nodeRange]! !

!NodesInRangeFinder methodsFor: 'evaluating - private' stamp: 'HAW 11/3/2024 00:35:32'!
isAtEndPosition: nodeRange

	^ range last = nodeRange last! !

!NodesInRangeFinder methodsFor: 'evaluating - private' stamp: 'HAW 11/3/2024 23:55:29'!
isAtStartPosition: nodeRange

	^ range first <= nodeRange first! !

!NodesInRangeFinder methodsFor: 'evaluating - private' stamp: 'HAW 11/3/2024 00:40:38'!
isCascadeMessage: aNode insideRange: nodeRange

	^ aNode isCascadeNode and: [ range last + 2 = nodeRange last ]! !

!NodesInRangeFinder methodsFor: 'evaluating - private' stamp: 'HAW 11/3/2024 17:34:59'!
isFirstInRange: aNode locatedAt: nodeRange

	^(self isAtStartPosition: nodeRange) and: [ self isInsideRange: aNode locatedAt: nodeRange ]! !

!NodesInRangeFinder methodsFor: 'evaluating - private' stamp: 'HAW 11/3/2024 17:34:49'!
isInsideRange: aNode locatedAt: nodeRange

	^ range last >= nodeRange last or: [self isCascadeMessage: aNode insideRange: nodeRange]! !

!NodesInRangeFinder methodsFor: 'evaluating - private' stamp: 'HAW 11/3/2024 00:34:53'!
notSubNodeOfLastNode: nodeRange
	
	^lastNodeRange last < nodeRange first! !


!NodesInRangeFinder class methodsFor: 'instance creation' stamp: 'HAW 11/2/2024 20:34:06'!
in: aSourceMethod for: anIntervalToExtract 

	^self new initializeIn: aSourceMethod for: anIntervalToExtract ! !


!ExtractMethodReplacement methodsFor: 'initialization' stamp: 'HAW 11/4/2024 00:17:27'!
initializeSendCollaboration

	sendCollaboration := 'self ', self newMessageString.
	self shouldBeEnclosedWithParens ifTrue: [ sendCollaboration := '(' , sendCollaboration , ')' ]
		! !


!SourceCodeInterval methodsFor: 'source code' stamp: 'HAW 11/1/2024 16:21:07'!
trimNoParentesisStartingWith: initialChar endingWith: endingChar on: aSourceCode

	| startOffset endOffset |

	startOffset := self trimmedOffsetFor: initialChar.
	endOffset := self trimmedOffsetFor: endingChar.
	^(startOffset = 1 or: [ endOffset = 1])
		ifTrue: [ (self trimLeft: startOffset right: endOffset) trimToMatchExpressionOn: aSourceCode ]
		ifFalse: [ self ]! !


!ExtractMethodReplacementsFinder methodsFor: 'private-replacement finding' stamp: 'HAW 11/4/2024 00:18:06'!
createReplacementAt: foundIntervalToExtract in: aMethod
	
	^ExtractMethodReplacement 
		fromInterval: foundIntervalToExtract asSourceCodeInterval 
		of: aMethod asMethodReference 
		to: newMessage
		! !

!ExtractMethodReplacementsFinder methodsFor: 'private-replacement finding' stamp: 'HAW 11/4/2024 18:47:14'!
findReplacementsAt: aClass

	"I live this implementationt instead of 
	
	aClass methodsDo: findingStrategy
	
	for debugging porpouses. We can do Through with this one - Hernan"
	aClass methodsDo: [ :aMethod | findingStrategy value: aMethod ]
	! !

!ExtractMethodReplacementsFinder methodsFor: 'evaluating' stamp: 'HAW 11/4/2024 00:08:21'!
value

	sourceCodeToExtract := sourceMethod sourceCode copyFrom: intervalToExtract first to: intervalToExtract last.
	nodesToExtract := self nodesToExtract.
	replacements := OrderedCollection new.
	findingStrategy := self defineFindingStrategy.
	
	self findReplacementInHierarchy.! !


!ExtractMethodReplacement methodsFor: 'initialization' stamp: 'HAW 11/4/2024 00:16:58'!
initializeFrom: anIntervalToExtract of: aMethodToExtractCodeFrom to: aNewMessage

	intervalToExtract := anIntervalToExtract.
	methodToExtractFrom := aMethodToExtractCodeFrom.
	newMessage := 	aNewMessage.
	self initializeSendCollaboration ! !

!ExtractMethodReplacement methodsFor: 'private - source code' stamp: 'HAW 11/4/2024 00:17:27'!
updatedSourceCodeOfExistingMethod

	^ methodToExtractFrom sourceCode
		copyReplaceFrom: intervalToExtract first  
		to: intervalToExtract last 
		with: sendCollaboration! !

!ExtractMethodReplacement methodsFor: 'adjustment' stamp: 'HAW 11/4/2024 00:17:27'!
adjustmentForNextReplacement
	
	^sendCollaboration size - intervalToExtract size! !


!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'private' stamp: 'HAW 11/2/2024 02:27:22'!
initializeParseNodesMatchingSelectionInterval

	initialNodeAncestors := methodNode parseNodesPathAt: intervalToExtract first ifAbsent: [ self signalSourceCodeContainsAnInvalidExpressionError ].
	finalNodeAncestors := methodNode parseNodesPathAt: intervalToExtract last ifAbsent: [ self signalSourceCodeContainsAnInvalidExpressionError ].
	initialNode := initialNodeAncestors detect: [ :aNodeRange | aNodeRange key isCascadeNode ] ifNone: [ initialNodeAncestors first ].
	finalNode := finalNodeAncestors first! !

!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'private' stamp: 'HAW 11/4/2024 00:30:46'!
intervalCoversCompleteAstNodes 

	^ (self trimmed: (SourceCodeInterval from: initialNode value first to: finalNode value last)) = intervalToExtract
		or: [ (initialNode value first to: (self trimmed: finalNode value) last) = intervalToExtract ]! !

!methodRemoval: ExtractMethodReplacement #initializeCallingExpression stamp: 'HAW 12/9/2024 14:03:36'!
ExtractMethodReplacement removeSelector: #initializeCallingExpression!
!methodRemoval: ExtractMethodReplacementsFinder #findReplacementsIn: stamp: 'HAW 12/9/2024 14:03:36'!
ExtractMethodReplacementsFinder removeSelector: #findReplacementsIn:!
!classDefinition: #ExtractMethodReplacementsFinder category: #'Tools-Refactoring'!
Object subclass: #ExtractMethodReplacementsFinder
	instanceVariableNames: 'intervalToExtract sourceMethod replacements newMessage sourceCodeToExtract nodesToExtract findingStrategy'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractMethodReplacement category: #'Tools-Refactoring'!
Refactoring subclass: #ExtractMethodReplacement
	instanceVariableNames: 'intervalToExtract newMessage methodToExtractFrom sendCollaboration'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
