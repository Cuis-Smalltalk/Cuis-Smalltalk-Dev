'From Cuis 5.0 [latest update: #4074] on 30 March 2020 at 5:35:06 pm'!
"Change Set:		4075-CuisCore-AuthorName-2020Mar30-17h23m
Date:			30 March 2020
Author:			Nahuel Garbezza

Validate temporaries declaration cannot be extracted to a method if it is used outside of the selection interval"!


!MethodNode methodsFor: 'testing' stamp: 'RNG 3/30/2020 17:32:21'!
anyParseNodeWithin: aSourceCodeInterval satisfy: aCondition

	self completeSourceRangesDo: [ :parseNode :sourceRanges |
		(aCondition value: parseNode) ifTrue: [
			sourceRanges anySatisfy: [ :sourceRange | aSourceCodeInterval rangeIncludes: sourceRange first ]
				:: ifTrue: [ ^ true ]]].
	
	^ false! !


!ExtractMethodExpressionValidation methodsFor: 'validation - private' stamp: 'RNG 3/30/2020 17:31:54'!
isNotATempDeclarationWithUsagesOutOfIntervalToExtract

	initialNode key isTemporariesDeclaration ifFalse: [ ^ true ].
	
	^ (methodNode
		anyParseNodeWithin: (intervalToExtract last to: sourceCode size)
		satisfy: [ :parseNode |
			parseNode isVariableNode
			and: [ initialNode key declaresVariable: parseNode ] ]) not! !

!ExtractMethodExpressionValidation methodsFor: 'validation - private' stamp: 'RNG 3/30/2020 17:33:26'!
isNotDeclaredWithinIntervalToExtract: aVariableNode

	^ (methodNode
		anyParseNodeWithin: intervalToExtract
		satisfy: [ :parseNode | parseNode isTemporariesDeclaration
			and: [ parseNode declaresVariable: aVariableNode ] ]) not! !

!ExtractMethodExpressionValidation methodsFor: 'validation - private' stamp: 'RNG 3/30/2020 17:33:23'!
thereAreNoLocalVariableAssignmentsWithoutDeclaration

	^ (methodNode
		anyParseNodeWithin: intervalToExtract
		satisfy: [ :parseNode |
			parseNode isAssignmentToTemporary
				and: [ self isNotDeclaredWithinIntervalToExtract: parseNode variable ] ]) not! !


!ExtractMethodExpressionValidation methodsFor: 'validation - private' stamp: 'RNG 3/30/2020 17:32:59'!
containsValidNodes

	^ self isNotLeftSideOfAssignment
		and: [ self thereAreNoLocalVariableAssignmentsWithoutDeclaration ]
		and: [ self thereAreNoReturnExpressions ]
		and: [ self isNotATempDeclarationWithUsagesOutOfIntervalToExtract ]
		and: [ self isNotATempDeclaration ]! !

!methodRemoval: ExtractMethodExpressionValidation #thereAreNoLocalVariableAssignments stamp: 'RNG 3/30/2020 17:33:03'!
ExtractMethodExpressionValidation removeSelector: #thereAreNoLocalVariableAssignments!
