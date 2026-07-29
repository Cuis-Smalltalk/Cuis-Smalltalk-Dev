'From Cuis7.9 [latest update: #8098] on 29 July 2026 at 2:09:23 pm'!

!BlockNode methodsFor: 'initialization' stamp: 'HAW 30/Oct/2024, 12:27:45 pm (UTC)'!
removeImplicitSelfReturnIn: aMethodNode

	| potentiaImplicitSelfReturn |

	potentiaImplicitSelfReturn := statements lastIfEmpty: [ ^self ].
	(potentiaImplicitSelfReturn isImplicitSelfReturnIn: aMethodNode) ifTrue: [ statements removeLast ].
! !


!MethodNode methodsFor: 'converting' stamp: 'HAW 30/Oct/2024, 12:22:34 pm (UTC)'!
withoutImplicitSelfReturn
	
	block removeImplicitSelfReturnIn: self.
	^self! !

!methodRemoval: BlockNode #removeImplicitSelfReturnWhenIn: stamp: 'jmv 29/Jul/2026, 5:08:07 pm (UTC)'!
BlockNode removeSelector: #removeImplicitSelfReturnWhenIn:!
