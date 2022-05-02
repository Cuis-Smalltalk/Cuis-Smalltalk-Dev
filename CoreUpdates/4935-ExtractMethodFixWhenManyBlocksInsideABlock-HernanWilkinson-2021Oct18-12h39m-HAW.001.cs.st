'From Cuis 5.0 [latest update: #4913] on 18 October 2021 at 6:47:02 pm'!

!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'private' stamp: 'HAW 10/18/2021 18:33:35'!
findSourceRangeOfCloserStatementIn: listOfAncestors ifNone: noneBlock

	^ (listOfAncestors
		detect: [ :assoc | assoc key isBlockNode ]
		ifFound: [ :assoc | listOfAncestors before: assoc ifNone: noneBlock ]
		ifNone: noneBlock) value! !


!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'private' stamp: 'HAW 10/18/2021 18:33:59'!
intervalMatchesBeginningOfStatement 

	^ (self findSourceRangeOfCloserStatementIn: initialNodeAncestors ifNone: [ initialNodeAncestors last ]) first = intervalToExtract first! !

!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'private' stamp: 'HAW 10/18/2021 18:34:28'!
intervalMatchesEndOfStatement

	| closerStatementLastPosition |
	
	closerStatementLastPosition := (self findSourceRangeOfCloserStatementIn: finalNodeAncestors ifNone: [ finalNodeAncestors first ]) last.
	^ closerStatementLastPosition = intervalToExtract last or: [ closerStatementLastPosition - 1 = intervalToExtract last ].! !

!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'private' stamp: 'HAW 10/18/2021 17:49:29'!
isLeftSideOfAssignment

	^ initialNodeAncestors size > 1
		and: [ self startAndEndParseNodesAreTheSame ]
		and: [ initialNodeAncestors second key isAssignmentNode ]
		and: [ initialNodeAncestors second key variable = initialNode key ]
		and: [ initialNodeAncestors second key variable isInstanceVariableNode not ]! !

!methodRemoval: SourceCodeOfMethodToBeExtractedPrecondition #findSourceRangeOfCloserStatementIn: stamp: 'HAW 10/18/2021 18:34:44'!
SourceCodeOfMethodToBeExtractedPrecondition removeSelector: #findSourceRangeOfCloserStatementIn:!
