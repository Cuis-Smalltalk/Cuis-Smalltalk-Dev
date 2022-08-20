'From Cuis 6.0 [latest update: #5324] on 18 August 2022 at 7:35:44 pm'!

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 8/18/2022 19:06:13'!
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

