'From Cuis 4.2 of 25 July 2013 [latest update: #2439] on 12 September 2015 at 12:29:30.611165 pm'!

!HierarchicalListMorph methodsFor: 'commands' stamp: 'SqR 9/12/2015 12:17'!
expandAllAsPer: aBlock

	scroller submorphs isEmpty ifTrue: [^self].
	scroller submorphs first beFullyExpandedAsPer: aBlock.
	scroller adjustExtent.
	self setScrollDeltas! !


!IndentingListItemMorph methodsFor: 'accessing' stamp: 'SqR 9/12/2015 12:22'!
beFullyExpandedAsPer: aBlock

	| allChildren |
	allChildren _ OrderedCollection new: 10.
	self recursiveAddTo: allChildren.
	allChildren do:
		[:each |
			| shouldExpandEach |
			shouldExpandEach := each isExpanded not
				:: and: [each canExpand]
				:: and: [aBlock value: each].
			shouldExpandEach ifTrue:
				[
					each toggleExpandedState.
					each beFullyExpandedAsPer: aBlock
				]
		]! !


!HierarchicalListMorph methodsFor: 'commands' stamp: 'SqR 9/12/2015 12:17'!
expandAll

	^self expandAllAsPer: [:each | true]! !


!IndentingListItemMorph methodsFor: 'accessing' stamp: 'SqR 9/12/2015 12:16'!
beFullyExpanded

	self beFullyExpandedAsPer: [:each | true]! !

