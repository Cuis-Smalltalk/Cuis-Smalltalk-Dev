'From Cuis 5.0 [latest update: #3969] on 12 December 2019 at 10:54:47 pm'!

!Parser methodsFor: 'error correction' stamp: 'EB 12/12/2019 20:48:34'!
declarationRangesForTemps: someTempNodes
	
	^someTempNodes collect: [ :temporaryNode | 
		(encoder rangeForNode: temporaryNode ifAbsent: []) first ].! !

!Parser methodsFor: 'error correction' stamp: 'EB 12/12/2019 20:58:49'!
rangesForRemovableUnusedTemps
	
	| tempsToRemove |
	
	tempsToRemove := SortedCollection sortBlock: [ :leftRange :rightRange | leftRange first < rightRange first ].
	
	parseNode nodesDo: [ :node |
		node isBlockNode ifTrue: [
			tempsToRemove addAll: (self rangesForRemovableUnusedTempsInBlockNode: node) ]].
	
	^tempsToRemove! !

!Parser methodsFor: 'error correction' stamp: 'EB 12/12/2019 20:57:18'!
rangesForRemovableUnusedTempsInBlockNode: aBlockNode
	
	| removableTemps unusedTemps |
	
	unusedTemps := self unusedTempsOf: aBlockNode.
	removableTemps := self selectRemovableUnusedTempsFrom: unusedTemps.
	^self declarationRangesForTemps: removableTemps.! !

!Parser methodsFor: 'error correction' stamp: 'EB 12/12/2019 20:57:44'!
selectRemovableUnusedTempsFrom: someUnusedTemps

	^someUnusedTemps select: [ :temporaryNode |
		(UnusedVariable name: temporaryNode name) and: [
			temporaryNode isUndefTemp 
				ifTrue: [ true ] 
				ifFalse: [ 
					self inform: 'You''ll first have to remove the\statement where it''s stored into' withNewLines.
					false ]
			]
		].! !

!Parser methodsFor: 'error correction' stamp: 'EB 12/12/2019 22:50:35'!
unusedTempsOf: aBlockNode

	^aBlockNode temporaries select: [ :temporaryNode | temporaryNode isUnusedTemp ]! !


!Parser methodsFor: 'expression types' stamp: 'EB 12/12/2019 22:51:14'!
method: noPattern doIt: doIt context: ctxt 
	" pattern [ | temporaries ] block => MethodNode."

	| sap block primitives temporaries messageComment methodNode |
	
	sap := self pattern: noPattern inContext: ctxt.
	"sap={selector, arguments, precedence}"
	self properties selector: (sap at: 1).
	encoder selector: (sap at: 1).
	(sap at: 2) do: [:argNode | argNode beMethodArg].
	
	doIt ifFalse: [self pragmaSequence].
	temporaries := self temporaries.
	messageComment := currentComment.
	currentComment := nil.
	doIt ifFalse: [self pragmaSequence].
	primitives := self pragmaPrimitives.
	
	self statements: #() innerBlock: doIt.
	parseNode temporaries: temporaries.
	block := parseNode.
	doIt 
		ifTrue: [block returnLast]
		ifFalse: [block returnSelfIfNoOther: encoder].
	hereType == #doIt ifFalse: [^self expected: 'Nothing more'].
	
	methodNode := self newMethodNode comment: messageComment.
	methodNode
		selector: (sap at: 1)
		arguments: (sap at: 2)
		precedence: (sap at: 3)
		temporaries: temporaries
		block: block
		encoder: encoder
		primitive: primitives
		properties: properties
		selectorKeywordsRanges: (sap at: 4).
		
	self interactive ifTrue: [ self performInteractiveChecks: methodNode ].
	
	^methodNode! !

!Parser methodsFor: 'error correction' stamp: 'EB 12/12/2019 20:56:04'!
removeUnusedTemps
	"Scan for unused temp names, and prompt the user about the prospect of removing each one found"

	self removeUnusedTempsRanges: self rangesForRemovableUnusedTemps.
! !

!methodRemoval: Parser #ifHasToRemove:addTo: stamp: 'EB 12/12/2019 20:43:12'!
Parser removeSelector: #ifHasToRemove:addTo:!
