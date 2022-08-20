'From Cuis 6.0 [latest update: #5324] on 12 July 2022 at 7:15:01 pm'!

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 7/12/2022 18:58:46'!
assertTemporaryIsNotAssignedAfterUsedInBlock
	
	| assignmentToTemporaryPositions |
	
	assignmentToTemporaryPositions := self selectCompleteRangesOfAssignmentsToTemporaryFrom: methodNode 
		thatMatch: [:assignmentRange | rangeOfNodeDeclaringTemporary includesAllOf: assignmentRange.].
	
	methodNode completeSourceRanges associationsDo: [:parseNodeAndRanges | (parseNodeAndRanges key isBlockNode 
		and: [parseNodeAndRanges value anySatisfy: [:range | (rangeOfNodeDeclaringTemporary includesAllOf: range) 
				and: [assignmentToTemporaryPositions anySatisfy: [:tempPosition | tempPosition first > range last]]]])
					ifTrue: [self class signalTemporaryAssignedAfterUsedInBlock]]
	
! !


!InlineTemporaryVariable class methodsFor: 'errors' stamp: 'FB 7/12/2022 18:23:57'!
signalTemporaryAssignedAfterUsedInBlock

	^self refactoringError: (self temporaryAssignedAfterUsedInBlockErrorMessage)





! !

!InlineTemporaryVariable class methodsFor: 'errors' stamp: 'FB 7/12/2022 18:24:47'!
temporaryAssignedAfterUsedInBlockErrorMessage

	^'Temporary is assigned after being referenced in a block'.





! !


!InlineTemporaryVariable methodsFor: 'applying' stamp: 'FB 7/12/2022 18:52:14'!
apply
	
	self usageToInlineIsTemporaryDeclaration
		ifTrue: [self inlineFromTemporaryDeclaration]
		ifFalse: [self inlineFromNonDeclarationUsage].

	self compileChanges.

! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 7/12/2022 18:52:32'!
inlineAssignment

	| endOfNextAssignmentIndex endOfRefactoringScopeIndex |

	endOfNextAssignmentIndex := self findLastIndexOfFirstAssignmentAfter: assignmentToInlineRange.
	self assertTemporaryIsNotAssignedAfterUsedInBlock.
	self computeEndOfNodeEnclosingAssignment.
	endOfRefactoringScopeIndex := endOfNextAssignmentIndex ifNil: [endOfNodeEnclosingAssignment ].
	"self halt".
	self inlineAssignment: assignmentToInlineRange upTo: endOfRefactoringScopeIndex.

	(self atMostOneAssignment and: [self noReferencesToTemporaryAfter: endOfRefactoringScopeIndex ])
		ifTrue: [self removeTemporaryVariableDeclaration].




! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 7/12/2022 19:07:10'!
inlineAssignment: assignmentToInline upTo: anIndex

	| assignmentNode assignmentNodeValue expression expressionRange |

	assignmentNode := self assignmentNodeToInline: assignmentToInline.
	assignmentNodeValue := assignmentNode value.
	expressionRange := (methodNode completeSourceRangesOf: assignmentNodeValue ifAbsent: [self shouldNotHappen])
		detect: [:range | assignmentToInline includesAllOf: range].
	expression := methodNode sourceText copyFrom: expressionRange first to: expressionRange last.

	self inlineAssignmentNode: assignmentNode withExpression: expression upTo: anIndex;
		removeAssignment: assignmentToInline.! !


!InlineTemporaryVariableApplier class methodsFor: 'refactoring - creation' stamp: 'FB 7/5/2022 18:28:22'!
on: anEditor for: aSelectionInterval of: aMethodToRefactor

	| noTempFoundBlock tempToInline usageInterval |

	noTempFoundBlock := [
		"This case will usually end in a failure because a temporary variable is not related to the selected interval"
		tempToInline := anEditor wordUnder: aSelectionInterval first.
		usageInterval := aSelectionInterval.
	].
	
	anEditor
		withNodeUnderCursorDo: [:aNode | | methodNode sourceRanges |
			methodNode := aMethodToRefactor methodNode.
			sourceRanges := methodNode completeSourceRanges.
			(aNode isVariableNode and: [methodNode argumentNames noneSatisfy: [:argument | argument = aNode name]])
				ifTrue: [
					tempToInline := aNode name.
					usageInterval := self findSourceRangeOfNode: aNode containingIndex: anEditor startIndex using: sourceRanges.
				]
				ifFalse: [ | assignmentNodeAndRange |
					assignmentNodeAndRange := (methodNode parseNodesPathAt: aSelectionInterval first ifAbsent: 	[self shouldNotHappen.])
						detect: [:aNodeAndRange | aNodeAndRange key isAssignmentNode]
						ifNone: [noTempFoundBlock value].
					tempToInline ifNil: [
						tempToInline := assignmentNodeAndRange key variable name.
						usageInterval := self findLeftmostSourceRangeOfNode: assignmentNodeAndRange key variable
							inside: assignmentNodeAndRange value using: sourceRanges.
					]
				]
		]
		ifAbsent: [
			noTempFoundBlock value.
		].

	^self new initializeOn: anEditor codeProvider forTemporary: tempToInline
		at: usageInterval of: aMethodToRefactor ! !

!methodRemoval: InlineTemporaryVariable class #signalTemporaryUsedInBlockSurroundedWithAssignments stamp: 'FB 7/12/2022 19:05:20'!
InlineTemporaryVariable class removeSelector: #signalTemporaryUsedInBlockSurroundedWithAssignments!
!methodRemoval: InlineTemporaryVariable class #temporaryUsedInBlockSurroundedWithAssignmentsErrorMessage stamp: 'FB 7/12/2022 19:04:48'!
InlineTemporaryVariable class removeSelector: #temporaryUsedInBlockSurroundedWithAssignmentsErrorMessage!
!methodRemoval: InlineTemporaryVariable #assertTemporaryNotUsedInBlockBetween:and: stamp: 'FB 7/12/2022 19:07:25'!
InlineTemporaryVariable removeSelector: #assertTemporaryNotUsedInBlockBetween:and:!

!InlineTemporaryVariable reorganize!
('applying' apply)
('initialization' initializeOldVariableNode:usage:method:methodNode:)
('applying primitives - private' addParenthesesIfNeededTo: assertTemporaryHasAtMostOneAssignment assertTemporaryIsNotAssignedAfterUsedInBlock assertUnassignedTemporaryIsNotReferenced assignmentNodeToInline: assignmentsToVariableCount atMostOneAssignment closestAssignmentBeforeUsage: compileChanges computeEndOfNodeEnclosingAssignment findAssignmentOfUsage findLastIndexOfFirstAssignmentAfter: findOnlyAssignmentRangeInNode: findReferencesBetweenAssignmentAndIndex: inlineAssignment inlineAssignment:upTo: inlineAssignmentNode:withExpression:upTo: inlineFromNonDeclarationUsage inlineFromTemporaryDeclaration inlineTemp:usedIn:insideBinaryMessageNode:withBinaryExpression: inlineTemporaryNodeWithBinaryMessageExpression:upTo: inlineTemporaryNodeWithKeywordMessageExpression:upTo: inlineTemporaryNodeWithLiteralOrUnaryMessageExpression:upTo: noReferencesToTemporaryAfter: nodeIsAssignmentToTemporaryToInline: range:isArgumentIn: rangeIsLeftPartOfAssignmentToTemporary: rangeIsPartOfTemporaryDeclaration: removeAssignment: removeTemporaryVariableDeclaration replaceRange:withExpression: replaceRange:withParenthesisedExpression: selectCompleteRangesOfAssignmentsToTemporaryFrom:thatMatch: tempNodeIsReceiverOrArgumentOfReceiverWhenUsedIn: tempNodeToInline temporaryNode:referencedIn:isArgumentIn: usageToInlineIsTemporaryDeclaration)
!

