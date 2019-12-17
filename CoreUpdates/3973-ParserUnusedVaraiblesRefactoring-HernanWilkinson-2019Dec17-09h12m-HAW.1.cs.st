'From Cuis 5.0 [latest update: #3972] on 17 December 2019 at 9:16:27 am'!

!Parser methodsFor: 'error correction' stamp: 'HAW 12/17/2019 09:13:58'!
rangesForRemovableUnusedTempsOf: aMethodNode
	
	| tempsToRemove |
	
	tempsToRemove := SortedCollection sortBlock: [ :leftRange :rightRange | leftRange first < rightRange first ].
	
	tempsToRemove addAll: (self rangesForRemovableUnusedTempsInBlockNode: aMethodNode). 
	aMethodNode nodesDo: [ :node |
		node isBlockNode ifTrue: [
			tempsToRemove addAll: (self rangesForRemovableUnusedTempsInBlockNode: node) ]].
	
	^tempsToRemove! !

!Parser methodsFor: 'error correction' stamp: 'HAW 12/17/2019 09:13:21'!
removeUnusedTempsOf: aMethodNode
	"Scan for unused temp names, and prompt the user about the prospect of removing each one found"

	self removeUnusedTempsRanges: (self rangesForRemovableUnusedTempsOf: aMethodNode)! !


!Parser methodsFor: 'expression types' stamp: 'HAW 3/11/2019 09:01:34'!
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

!Parser methodsFor: 'error correction' stamp: 'HAW 12/17/2019 09:15:06'!
rangesForRemovableUnusedTempsInBlockNode: aNodeWithTemporaries
	
	| removableTemps unusedTemps |
	
	unusedTemps := self unusedTempsOf: aNodeWithTemporaries.
	removableTemps := self selectRemovableUnusedTempsFrom: unusedTemps.
	
	^self declarationRangesForTemps: removableTemps.! !

!Parser methodsFor: 'error correction' stamp: 'HAW 12/17/2019 09:14:42'!
unusedTempsOf: aNodeWithTemporaries

	^aNodeWithTemporaries temporaries select: [ :temporaryNode | temporaryNode isUnusedTemp ]! !

