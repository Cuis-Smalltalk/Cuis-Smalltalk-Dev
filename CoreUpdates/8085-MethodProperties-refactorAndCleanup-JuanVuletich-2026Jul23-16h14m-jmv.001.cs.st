'From Cuis7.9 [latest update: #8084] on 23 July 2026 at 4:16:57 pm'!

!AdditionalMethodState commentStamp: '<historical>' prior: 0!
I am class holding state for compiled methods. All my instance variables should be actually part of the CompiledMethod itself, but the current implementation of the VM doesn't allow this.  Currently I hold the selector and any pragmas or properties the compiled method has.  Pragmas and properties are stored in indexable fields; pragmas as instances of Pragma, variable bindings as instances of Association or ReadOnlyVariableBinding.
!


!CompiledMethod methodsFor: 'accessing' stamp: 'jmv 23/Jul/2026, 6:34:42 pm (UTC)'!
properties: anAdditionalMethodState
	"Set the penultimate literal of the receiver to anAdditionalMethodState."

	self penultimateLiteral:
			(anAdditionalMethodState isEmpty
				ifTrue: [anAdditionalMethodState selector]
				ifFalse: [anAdditionalMethodState
							setMethod: self;
							yourself]).! !

!CompiledMethod methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'jmv 23/Jul/2026, 6:30:34 pm (UTC)'!
withPropertiesDo: withBlock ifSelector: notBlock
	"Evaluate withBlock with our AdditionalMethodState,
	or evaluate notBlock with our selector
		(if we don't hold any Pragmas or properties in an AdditionalMethodState)"

	| penultimalLiteral |
	penultimalLiteral := self penultimateLiteral.
	^penultimalLiteral class == AdditionalMethodState
		ifTrue: [ withBlock value: penultimalLiteral ]
		ifFalse: [ notBlock value: penultimalLiteral ]! !


!AdditionalMethodState methodsFor: 'properties' stamp: 'jmv 23/Jul/2026, 6:53:40 pm (UTC)'!
propertyOrPragmaAt: aKey
	"Answer the element (property or pragma) associated with aKey.
	Note: For Associations and ReadOnlyVariableBinding answer the association itself, not the value
		(different from #at:, for instance).
	Answer nil if not found."

	| propertyOrPragma "<Association|ReadOnlyVariableBinding|Pragma>" |
	1 to: self basicSize do: [:i |
		propertyOrPragma := self basicAt: i.
		propertyOrPragma key == aKey ifTrue:
			[^propertyOrPragma]].
	^nil! !

!AdditionalMethodState methodsFor: 'testing' stamp: 'eem 3/Dec/2013, 2:42:00 pm (UTC)'!
analogousCodeTo: anAdditionalMethodState
	| bs |
	self class == anAdditionalMethodState class ifFalse:
		[^false].
	(bs := self basicSize) = anAdditionalMethodState basicSize ifFalse:
		[^false].
	1 to: bs do:
		[:i|
		((self basicAt: i) analogousCodeTo: (anAdditionalMethodState basicAt: i)) ifFalse:
			[^false]].
	^true! !

!AdditionalMethodState methodsFor: 'testing' stamp: 'jmv 23/Jul/2026, 6:37:25 pm (UTC)'!
hasAtLeastTheSamePropertiesAs: anAdditionalMethodState
	"Answer if the recever has at least the same properties as the argument.
	 N.B. The receiver may have additional properties and still answer true."

	anAdditionalMethodState keysAndValuesDo: [ :k :v |
		v isPragma
			"ifTrue: [Pragmas have already been checked]"
			ifFalse: [
				(self includesKey: k) ifFalse: [^false]]].
	^true! !

!methodRemoval: AdditionalMethodState #isMethodProperties stamp: 'jmv 23/Jul/2026, 7:15:29 pm (UTC)'!
AdditionalMethodState removeSelector: #isMethodProperties!
!methodRemoval: AdditionalMethodState #properties stamp: 'jmv 23/Jul/2026, 7:16:10 pm (UTC)'!
AdditionalMethodState removeSelector: #properties!
!methodRemoval: Object #isMethodProperties stamp: 'jmv 23/Jul/2026, 7:15:31 pm (UTC)'!
Object removeSelector: #isMethodProperties!
