'From Cuis 6.0 [latest update: #5466] on 11 September 2022 at 8:38:37 pm'!

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 9/11/2022 20:30:18'!
assertTemporaryNotAssignedInBlock

	methodNode completeSourceRangesDo: [:node :ranges |
		
		(node isBlockNode and: [ranges first last < usageToInline first] and: [
			(self selectCompleteRangesOfAssignmentsToTemporaryFrom: node 
				thatMatch: 	[:assignmentRange | rangeOfNodeDeclaringTemporary includesAllOf: assignmentRange ]) isEmpty not.
		]) ifTrue: [self class signalTemporaryIsAssignedInsideInBlockBeforeReference]
	]! !


!InlineTemporaryVariable class methodsFor: 'errors' stamp: 'FB 9/11/2022 19:44:54'!
signalTemporaryIsAssignedInsideInBlockBeforeReference

	^self refactoringError: (self temporaryIsAssignedInBlockBeforeReferenceErrorMessage)





! !

!InlineTemporaryVariable class methodsFor: 'errors' stamp: 'FB 9/11/2022 19:45:59'!
temporaryIsAssignedInBlockBeforeReferenceErrorMessage

	^'Temporary to inline is previously is assigned inside a block'





! !


!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 9/11/2022 19:02:44'!
assertTemporaryIsNotAssignedAfterUsedInBlock
	
	| assignmentToTemporaryPositions |
	
	assignmentToTemporaryPositions := self selectCompleteRangesOfAssignmentsToTemporaryFrom: methodNode 
		thatMatch: [:assignmentRange | rangeOfNodeDeclaringTemporary includesAllOf: assignmentRange.].
	
	methodNode completeSourceRanges associationsDo: [:parseNodeAndRanges | (parseNodeAndRanges key isBlockNode 
		and: [parseNodeAndRanges value anySatisfy: [:range | (rangeOfNodeDeclaringTemporary includesAllOf: range)
				and: [(methodNode positionsForTemporaryVariable: variableToInline ifAbsent: [#()]) 
					anySatisfy: [:tempReferenceInterval | range includesAllOf: tempReferenceInterval]]
				and: [assignmentToTemporaryPositions anySatisfy: [:tempAssignmentPosition | tempAssignmentPosition first > range last]]]])
					ifTrue: [self class signalTemporaryAssignedAfterUsedInBlock]]
	
! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 9/11/2022 20:27:56'!
inlineAssignment

	| endOfNextAssignmentIndex endOfRefactoringScopeIndex |

	endOfNextAssignmentIndex := self findLastIndexOfFirstAssignmentAfter: assignmentToInlineRange.
	self assertTemporaryNotAssignedInBlock.
	self assertTemporaryIsNotAssignedAfterUsedInBlock.
	self computeEndOfNodeEnclosingAssignment.
	endOfRefactoringScopeIndex := endOfNextAssignmentIndex ifNil: [endOfNodeEnclosingAssignment ].
	
	self inlineAssignment: assignmentToInlineRange upTo: endOfRefactoringScopeIndex.

	(self atMostOneAssignment and: [self noReferencesToTemporaryAfter: endOfRefactoringScopeIndex ])
		ifTrue: [self removeTemporaryVariableDeclaration].




! !

