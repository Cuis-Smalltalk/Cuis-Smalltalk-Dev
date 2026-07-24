'From Cuis7.9 [latest update: #8091] on 24 July 2026 at 4:09:05 pm'!

!BytecodeEncoder class methodsFor: 'testing' stamp: 'jmv 24/Jul/2026, 6:55:37 pm (UTC)'!
canBeSpecialLiteral: aLiteral
	"This check can be used to prevent unnecessary use of #scanBlockOrNilForLiteral:. For performance, this method summarizes specializations from all known bytecode encoders. It is not meant to be refined per bytecode encoder."

	aLiteral isLiteral ifFalse: [^false]. "a common case; don't waste time analysing..."
	
	aLiteral isSymbol ifTrue: [^ Smalltalk specialSelectors identityIncludes: aLiteral].
	aLiteral isCharacter ifTrue: [^ aLiteral asInteger <= 65535].
	aLiteral isInteger ifTrue: [^ aLiteral between: -32768 and: 32767].

	aLiteral == true ifTrue: [^ true].
	aLiteral == false ifTrue: [^ true].
	aLiteral == nil ifTrue: [^ true].

	^ false! !


!EncoderForSistaV1 class methodsFor: 'testing' stamp: 'jmv 24/Jul/2026, 6:56:17 pm (UTC)'!
canBeSpecialLiteral: aLiteral
	"This check can be used to prevent unnecessary use of #scanBlockOrNilForLiteral:."

	aLiteral isLiteral ifFalse: [^false]. "a common case; don't waste time analysing..."

	aLiteral isSymbol ifTrue: [^ Smalltalk specialSelectors identityIncludes: aLiteral].
	aLiteral isCharacter ifTrue: [^ aLiteral codePoint <= 65535].
	aLiteral isInteger ifTrue: [^ aLiteral between: -32768 and: 32767].

	aLiteral == true ifTrue: [^ true].
	aLiteral == false ifTrue: [^ true].
	aLiteral == nil ifTrue: [^ true].

	^ false! !


!EncoderForV3PlusClosures class methodsFor: 'testing' stamp: 'jmv 24/Jul/2026, 6:56:23 pm (UTC)'!
canBeSpecialLiteral: aLiteral
	"This check can be used to prevent unnecessary use of #scanBlockOrNilForLiteral:."

	aLiteral isLiteral ifFalse: [^false]. "a common case; don't waste time analysing..."

	aLiteral isSymbol ifTrue: [^ Smalltalk specialSelectors identityIncludes: aLiteral].
	aLiteral isInteger ifTrue: [^ aLiteral between: -1 and: 2].

	aLiteral == true ifTrue: [^ true].
	aLiteral == false ifTrue: [^ true].
	aLiteral == nil ifTrue: [^ true].

	^ false! !


!CompiledMethod methodsFor: 'literals' stamp: 'jmv 24/Jul/2026, 7:08:55 pm (UTC)'!
literalStrings
	"Note: Not calling #allLiteralsDo:
	This does not go into #withPropertiesDo:ifSelector: or #penultimateLiteral,
	so it doesn't iterate an AdditionalMethodState, so sending #isVariableBinding is safe.
	See comment at Association>>#isVariableBinding"

	| litStrs |
	litStrs := OrderedCollection new.
	self literalsDo: [ :lit | 
		(lit isVariableBinding)
			ifTrue: [litStrs addLast: lit key]
			ifFalse: [(lit isSymbol)
				ifTrue: [litStrs addAll: lit keywords]
				ifFalse: [litStrs addLast: lit printString]]].
	^ litStrs! !

!CompiledMethod methodsFor: 'testing' stamp: 'jmv 24/Jul/2026, 7:08:41 pm (UTC)'!
hasVariableBindingTo: aClass 
	"Note: Not calling #allLiteralsDo:
	This does not go into #withPropertiesDo:ifSelector: or #penultimateLiteral,
	so it doesn't iterate an AdditionalMethodState, so sending #isVariableBinding is safe.
	See comment at Association>>#isVariableBinding"

	self literalsDo: [ :aLiteral |
		(aLiteral isVariableBinding and: [ aLiteral value = aClass ]) ifTrue: [ ^true ]].
	
	^false! !


!Association methodsFor: 'testing' stamp: 'jmv 24/Jul/2026, 7:05:18 pm (UTC)'!
isVariableBinding
	"Return true if I can behave as a literal variable binding in CompiledMethod and Parser.
	This could mean answering true to things that are indeed NOT variable bindings (such as parts of Dictionaries other than Smalltalk and #classPool's).
	Still:
		+ Instances held in a AdditionalMethodState stored in the 'properties' ivar in Parser are actual variable bindings.
		+ Instances held in a CompiledMethod are actual variable bindings.
		+ Instances held in a AdditionalMethodState of a CompiledMethod are actually method properties and NOT variable bindings.
	See senders. Use with care."

	^true! !


!ReadOnlyVariableBinding methodsFor: 'testing' stamp: 'jmv 24/Jul/2026, 7:05:23 pm (UTC)'!
isVariableBinding
	"Return true if I can behave as a literal variable binding in CompiledMethods.
	Instances of this class are always variable bindings.
	See senders."

	^true! !

