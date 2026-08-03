'From Cuis7.9 [latest update: #8094] on 27 July 2026 at 9:07:18 pm'!

!CompiledMethod methodsFor: 'method node' stamp: 'JEC 28/Jul/2026, 12:02:55 am (UTC)'!
methodNodeWithoutImplicitSelfReturn
	
	^self notOptimizedMethodNode withoutImplicitSelfReturn ! !


!ExtractMethodParameterizableReplacement methodsFor: 'private' stamp: 'JEC 28/Jul/2026, 12:03:27 am (UTC)'!
allArgumentNodesOf: aNode

	| messageNode |

	messageNode := aNode isMessageNode
		ifTrue: [ aNode ]
		ifFalse: [ aNode value ]. "Here the node is an assignment node and the value assigning is a message node"

	^ messageNode argumentNodesSourceRangesIn: methodToExtractFrom notOptimizedMethodNode! !

