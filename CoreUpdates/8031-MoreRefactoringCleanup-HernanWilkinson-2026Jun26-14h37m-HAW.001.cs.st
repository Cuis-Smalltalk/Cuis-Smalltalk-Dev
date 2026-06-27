'From Cuis7.9 [latest update: #8029] on 26 June 2026 at 3:02:45 pm'!

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'HAW 26/Jun/2026, 5:44:48 pm (UTC)'!
findAssignmentOfUsage

	| lastAssignment assignments |

	assignments := self selectCompleteRangesOfAssignmentsToTemporaryFrom: methodOrBlockNodeDeclaringTemporary thatMatch:
		[:range | (range includesAllOf: usageToInline) or: [range last < usageToInline first] ].

	lastAssignment := assignments detectMax: [:assignment | assignment last].

	lastAssignment 
		ifNotNil: [ ^lastAssignment ] .

	self class signalAssignmentNotFound.! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'HAW 26/Jun/2026, 5:47:48 pm (UTC)'!
inlineAssignmentNode: anAssignmentNode withExpression: anExpression upTo: anIndex

	| assignmentNodeValue |

	assignmentNodeValue := anAssignmentNode value.
	assignmentNodeValue isMessageNode
		ifTrue: [
			assignmentNodeValue isUnaryMessageSend
				ifTrue: [self inlineTemporaryNodeWithLiteralOrUnaryMessageExpression: anExpression upTo: anIndex].
			assignmentNodeValue isInfix
				ifTrue: [self inlineTemporaryNodeWithBinaryMessageExpression: anExpression upTo: anIndex].
			assignmentNodeValue isKeywordMessageSend
				ifTrue: [self inlineTemporaryNodeWithKeywordMessageExpression: anExpression upTo: anIndex]]
		ifFalse: [self inlineTemporaryNodeWithLiteralOrUnaryMessageExpression: anExpression upTo: anIndex].! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'HAW 26/Jun/2026, 5:46:33 pm (UTC)'!
inlineFromTemporaryDeclaration

	self assertTemporaryHasAtMostOneAssignment.

	assignmentToInlineRange := self findOnlyAssignmentRangeInNode: methodOrBlockNodeDeclaringTemporary.

	assignmentToInlineRange
		ifNil: [
			self assertUnassignedTemporaryIsNotReferenced.
			self removeTemporaryVariableDeclaration ]
		ifNotNil: [self inlineAssignment].! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'HAW 26/Jun/2026, 5:49:04 pm (UTC)'!
inlineTemporaryNodeWithKeywordMessageExpression: anExpression upTo: anIndex

	| referencesToInline |

	referencesToInline := self findReferencesBetweenAssignmentAndIndex: anIndex.
	referencesToInline do: [:range |
		| parentNode  |

		parentNode := (methodNode parseNodesPathAt: range first ifAbsent: [self shouldNotHappen]) second key.

		(parentNode isMessageNode)
			ifTrue: [
				parentNode isUnaryMessageSend ifTrue: [self replaceRange: range withParenthesisedExpression: anExpression ].
				(parentNode isInfix or: [ parentNode isKeywordMessageSend]) ifTrue: [
					(self tempNodeIsReceiverOrArgumentOfReceiverWhenUsedIn: range)
						ifTrue: [self replaceRange: range withParenthesisedExpression: anExpression]
						ifFalse: [self replaceRange: range withExpression: anExpression ]]]
			ifFalse: [self replaceRange: range withExpression: anExpression.]].! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'HAW 26/Jun/2026, 5:50:29 pm (UTC)'!
range: aRange isArgumentIn: aBinaryMessageNode

	^ (methodNode consolidateAsCollection: (methodNode rangeForNode: aBinaryMessageNode ifAbsent: [self shouldNotHappen]))
		anySatisfy: [:aBinaryMessageNodeRange | aBinaryMessageNodeRange includesAllOf: aRange]! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'HAW 26/Jun/2026, 5:51:25 pm (UTC)'!
rangeIsLeftPartOfAssignmentToTemporary: aRange

	methodNode nodesDo: [:aNode |
		(self nodeIsAssignmentToTemporaryToInline: aNode)	
			ifTrue: [ | ranges completeRanges |
			ranges := methodNode consolidateAsCollection: (methodNode rangeForNode: aNode ifAbsent: [self shouldNotHappen ]).
			completeRanges := methodNode consolidateAsCollection:(methodNode completeSourceRangesOf: aNode ifAbsent: [self shouldNotHappen ]).
			((completeRanges anySatisfy: [:aCompleteRange | aCompleteRange includesAllOf: aRange])
				and: [ranges noneSatisfy: [:aNonCompleteRange | aNonCompleteRange includesAllOf: aRange ]]) ifTrue: [^true]]].

	^false! !


!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 26/Jun/2026, 5:53:54 pm (UTC)'!
addParenthesesIfNeededTo: anExpression

	"TODO: duplicated code in InlineTemporaryVariable"

	| firstCharacterIsOpeningParentheses lastCharacterIsClosingParentheses |
	
 	firstCharacterIsOpeningParentheses := (anExpression at: (anExpression firstNonSeparator)) == $(.
	lastCharacterIsClosingParentheses := (anExpression at: (anExpression lastNonSeparator)) == $).

	^(firstCharacterIsOpeningParentheses and: [lastCharacterIsClosingParentheses])
		ifTrue: [anExpression]
		ifFalse: [ (anExpression endsWith: '.')
			ifTrue: [('(', anExpression copyUpToLast: $.), ').']
	 		ifFalse: ['(', anExpression, ')']].! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 26/Jun/2026, 5:55:21 pm (UTC)'!
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

	^ character = Character newLineCharacter
		ifTrue: [indentation reversed]
		ifFalse: [''] ! !

!methodRemoval: NewInstanceVariablePrecondition #assertIsNotAlreadyDefinedInSuperclasses stamp: 'HAW 26/Jun/2026, 5:57:31 pm (UTC)'!
NewInstanceVariablePrecondition removeSelector: #assertIsNotAlreadyDefinedInSuperclasses!
