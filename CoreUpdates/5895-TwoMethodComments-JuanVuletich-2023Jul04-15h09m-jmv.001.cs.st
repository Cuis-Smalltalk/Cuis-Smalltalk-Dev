'From Cuis 6.0 [latest update: #5894] on 4 July 2023 at 4:02:56 pm'!

!AdditionalMethodState methodsFor: 'testing' stamp: 'jmv 7/4/2023 16:01:43'!
isMethodProperties
	"AdditionalMethodState holds
		Pragmas
		Method Properties (in the form of Associations)
	if any CompiledMethod needs them."

	^true! !


!CompiledMethod methodsFor: 'accessing-pragmas & properties' stamp: 'jmv 7/4/2023 16:00:46'!
withPropertiesDo: withBlock ifSelector: notBlock
	"Evaluate withBlock with our AdditionalMethodState,
	or evaluate notBlock with our selector
		(if we don't hold any Pragmas or properties in an AdditionalMethodState)"

	| penultimalLiteral |
	penultimalLiteral := self penultimateLiteral.
	^penultimalLiteral isMethodProperties 
		ifTrue: [ withBlock value: penultimalLiteral ]
		ifFalse: [ notBlock value: penultimalLiteral ]! !
