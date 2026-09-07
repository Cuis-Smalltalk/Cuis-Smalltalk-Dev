'From Cuis7.9 [latest update: #8159] on 7 September 2026 at 5:48:47 pm'!

!VariableScopeFinder methodsFor: 'visiting' stamp: 'jmv 7/Sep/2026, 8:34:10 pm (UTC)'!
visitBacktickNode: aBacktickNode
	^nil! !

!VariableScopeFinder methodsFor: 'visiting' stamp: 'jmv 7/Sep/2026, 8:48:10 pm (UTC)'!
visitMessageNodeInCascade: aMessageNode
	"Answer the minimum enclosing node for aVariabe or nil if none.
	 If the variable is accessed in more than one subexpression then aMessageNode is the
	 enclosing node, otherwise it is which ever single node that includes it, if any."
	^self
		enclosingNodeFor: [:aBlock|
							aMessageNode argumentsInEvaluationOrder do: aBlock]
		of: aMessageNode! !

!VariableScopeFinder methodsFor: 'visiting' stamp: 'jmv 7/Sep/2026, 8:36:42 pm (UTC)'!
visitTemporariesDeclarationNode: aTemporariesDeclarationNode
	^nil! !

!VariableScopeFinder methodsFor: 'visiting' stamp: 'jmv 7/Sep/2026, 8:37:17 pm (UTC)'!
visitTemporaryDeclarationNode: aTemporaryDeclarationNode
	^nil! !

