'From Cuis7.5 [latest update: #7364] on 21 July 2025 at 6:41:39 pm'!

!EquivalentNodesFinder methodsFor: 'visiting' stamp: 'HAW 7/21/2025 18:33:05'!
visitBlockNode: blockNode

	| blockStatements |

	self addToEquivalentsIfOneStatementEquivalentTo: blockNode.

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

