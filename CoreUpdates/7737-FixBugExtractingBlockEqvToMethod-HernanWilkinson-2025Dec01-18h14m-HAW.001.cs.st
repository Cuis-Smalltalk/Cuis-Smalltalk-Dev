'From Cuis7.5 [latest update: #7735] on 1 December 2025 at 6:16:31 pm'!

!EquivalentNodesFinder methodsFor: 'visiting' stamp: 'HAW  1/Dec/2025 18:14:15'!
visitBlockNode: blockNode

	| blockStatements |

	"See ExtractMethodFinderTest>>#test30DoesNotFailWhenLookingForABlockEquivalentToAMethod"
	blockNode = methodNode block ifFalse: [
		self addToEquivalentsIfOneStatementEquivalentTo: blockNode ].

	super visitBlockNode: blockNode.
	
	nodesToLookForSize > 1 ifTrue: [ 
		blockStatements := blockNode statements copy. 
		"I need to add temp declaration to statements becuase user could have selected the temp declaration
		See 
		- ExtractMethodFinderTest>>test22FindsTemporaryWhenSelectingDefinitionInInnerClosure
		- ExtractMethodFinderTest>>test22_1_FindsTemporaryWhenSelectingDefinitionInInnerClosure - Hernan"
		blockNode declaresAnyVariable ifTrue: [ blockStatements addFirst: blockNode temporariesDeclaration ].
		self addEquivalentNodesOf: blockStatements ]
		! !

