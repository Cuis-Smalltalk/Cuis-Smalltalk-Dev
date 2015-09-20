'From Cuis 4.2 of 25 July 2013 [latest update: #2504] on 19 September 2015 at 9:01:08.086699 pm'!

!IndentingListItemMorph methodsFor: 'accessing' stamp: 'SqR 9/17/2015 03:29'!
beFullyExpandedAsPer: aBlock

	| allChildren |
	allChildren _ OrderedCollection new: 10.
	self recursiveAddTo: allChildren.
	allChildren do:
		[:each |
			| shouldExpandEach |
			shouldExpandEach := each canExpand and: [aBlock value: each].
			shouldExpandEach ~= each isExpanded ifTrue:
				[
					each toggleExpandedState.
					each beFullyExpandedAsPer: aBlock
				]
		]! !

