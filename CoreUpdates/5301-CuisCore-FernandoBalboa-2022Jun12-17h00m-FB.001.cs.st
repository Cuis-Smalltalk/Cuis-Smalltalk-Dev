'From Cuis 6.0 [latest update: #5235] on 12 June 2022 at 6:16:14 pm'!

!InlineTemporaryVariableApplier class methodsFor: 'refactoring - creation' stamp: 'FB 6/12/2022 18:06:43'!
findLeftmostSourceRangeOfNode: aParseNode inside: anInterval using: completeSourceRanges

	^((completeSourceRanges associations detect: [:aNodeAndRange | aNodeAndRange key equivalentTo: aParseNode]) value)
		detect: [:nodeRange | anInterval includesAllOf: nodeRange].! !

!methodRemoval: InlineTemporaryVariableApplier class #findSourceRangeOfNode:inside:using: stamp: 'FB 6/12/2022 18:06:43'!
InlineTemporaryVariableApplier class removeSelector: #findSourceRangeOfNode:inside:using:!

!InlineTemporaryVariableApplier class reorganize!
('refactoring - creation' createAndValueHandlingExceptions: findLeftmostSourceRangeOfNode:inside:using: on:for:of: refactoringClass)
!
