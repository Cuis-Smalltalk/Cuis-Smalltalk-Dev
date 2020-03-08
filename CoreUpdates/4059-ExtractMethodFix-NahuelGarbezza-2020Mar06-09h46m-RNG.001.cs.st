'From Cuis 5.0 [latest update: #4057] on 7 March 2020 at 8:14:42 pm'!
"Change Set:		4058-CuisCore-AuthorName-2020Mar06-09h46m
Date:			7 March 2020
Author:			Nahuel Garbezza

Fix flaky test on the ExtractMethod refactoring"!


!Encoder methodsFor: 'source mapping' stamp: 'RNG 3/6/2020 16:41:44'!
criteriaToSortSourceRangeBetween: parseNodeWithSourceRangeOne and: parseNodeWithSourceRangeTwo

	| sourceRangeOne sourceRangeTwo |
	sourceRangeOne _ parseNodeWithSourceRangeOne value.
	sourceRangeTwo _ parseNodeWithSourceRangeTwo value.
	^ sourceRangeOne first > sourceRangeTwo first
		or: [ sourceRangeOne first = sourceRangeTwo first
			and: [ sourceRangeOne last <= sourceRangeTwo last ] ]! !


!Encoder methodsFor: 'source mapping' stamp: 'RNG 3/6/2020 16:40:40'!
parseNodesPathAt: aPosition using: completeSourceRanges ifAbsent: aBlockClosure

	| nodesWithRangeAtPosition |
	nodesWithRangeAtPosition _ SortedCollection sortBlock: [ :parseNodeWithSourceRangeOne :parseNodeWithSourceRangeTwo |
		self criteriaToSortSourceRangeBetween: parseNodeWithSourceRangeOne and: parseNodeWithSourceRangeTwo ].

	completeSourceRanges associationsDo: [ :nodeAtRange |
		self withRangesOf: nodeAtRange including: aPosition do: [ :aRange |
			nodesWithRangeAtPosition add: (nodeAtRange key -> aRange) ] ].

	^ nodesWithRangeAtPosition
		ifEmpty: aBlockClosure
		ifNotEmpty: [ nodesWithRangeAtPosition ]! !

