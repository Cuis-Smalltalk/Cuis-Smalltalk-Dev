'From Cuis7.9 [latest update: #8166] on 12 September 2026 at 5:16:59 pm'!

!EquivalentNodesFinder methodsFor: 'private - visiting' stamp: 'HAW 12/Sep/2026, 6:23:50 pm (UTC)'!
allNodesToLookForBelongTo: aCascadeNode

	^nodesToLookFor allSatisfy: [ :aNode |
		(aCascadeNode isEquivalentToReceiverNode: aNode)
			or: [ aCascadeNode isEquivalentToAMessageNode: aNode ] ]! !

!EquivalentNodesFinder methodsFor: 'private - visiting' stamp: 'HAW 12/Sep/2026, 6:23:50 pm (UTC)'!
anyNodeToLookForIsAMessageOf: aCascadeNode

	^nodesToLookFor anySatisfy: [ :aNode |
		aCascadeNode isEquivalentToAMessageNode: aNode ]! !


!EquivalentNodesFinder methodsFor: 'private - visiting' stamp: 'HAW 12/Sep/2026, 6:23:50 pm (UTC)'!
areNodesToLookForAPartiallyCascadeNode: aCascadeNode

	^(self anyNodeToLookForIsAMessageOf: aCascadeNode)
		and: [ self allNodesToLookForBelongTo: aCascadeNode ]! !


!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'private' stamp: 'HAW 12/Sep/2026, 6:12:20 pm (UTC)'!
initializeParseNodesMatchingSelectionInterval

	initialNodeAncestors := methodNode
		parseNodesPathAt: intervalToExtract first
		ifAbsent: [ self signalSourceCodeContainsAnInvalidExpressionError ].
	finalNodeAncestors := methodNode
		parseNodesPathAt: intervalToExtract last
		ifAbsent: [ self signalSourceCodeContainsAnInvalidExpressionError ].
	initialNode := initialNodeAncestors first.
	finalNode := finalNodeAncestors first! !

