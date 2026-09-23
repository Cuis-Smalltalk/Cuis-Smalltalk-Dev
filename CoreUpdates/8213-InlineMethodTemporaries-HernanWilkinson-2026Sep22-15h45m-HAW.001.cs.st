'From Cuis7.9 [latest update: #8184] on 22 September 2026 at 6:48:57 pm'!

!InlineMethod methodsFor: 'temporaries declaration - private' stamp: 'HAW 22/Sep/2026, 9:40:55 pm (UTC)'!
declareNecessaryTemporaries

	temporariesToDeclareByInsertionPoint keysAndValuesDo: [:aSenderAndPosition :temporariesToDeclare |
		| sender positionToInsertTemporaries temporariesString senderMethodNode nodeAtTempPosition |
	
		sender := aSenderAndPosition key.
		positionToInsertTemporaries := aSenderAndPosition value.
		temporariesString := temporariesToDeclare reduce: [:t1 : t2 | t1 value, ' ', t2 value].
		senderMethodNode := sender notOptimizedMethodNode.
		nodeAtTempPosition := (senderMethodNode 
			parseNodesPathAt: positionToInsertTemporaries 
			ifAbsent: [self shouldNotHappen ]) first key.
		nodeAtTempPosition isTemporariesDeclaration
			ifTrue: [ 
				self 
					replaceRange: (positionToInsertTemporaries to: positionToInsertTemporaries)
					withNewSourceCode: temporariesString, ' |'
					inMethod: sender ]
			ifFalse: [
				"The declaration goes before the first statement, which stays as it is"
				self 
					replaceRange: (positionToInsertTemporaries to: positionToInsertTemporaries - 1)
					withNewSourceCode: (self declarationOf: temporariesString insertedAt: positionToInsertTemporaries in: senderMethodNode)
					inMethod: sender ]
	]! !

