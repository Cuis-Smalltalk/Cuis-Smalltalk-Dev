'From Cuis 6.0 [latest update: #5642] on 20 January 2023 at 7:47:47 pm'!

!Encoder methodsFor: 'encoding' stamp: 'jmv 1/20/2023 19:47:04'!
encodeLiteral: literalObject
	| object |
	object _ literalObject.
	"Prefer String over UnicodeString for literals, if ASCII only."
	(object isUnicodeString and: [ object isSymbol not and: [ object isAscii ]]) ifTrue: [
		object _ object asByteString ].
	(object isUnicodeCodePoint and: [ object asciiValue notNil ]) ifTrue: [
		object _ object asCharacter ].
	^self
		name: object
		key: (class literalScannedAs: object notifying: self)
		class: LiteralNode
		type: LdLitType
		set: litSet! !

