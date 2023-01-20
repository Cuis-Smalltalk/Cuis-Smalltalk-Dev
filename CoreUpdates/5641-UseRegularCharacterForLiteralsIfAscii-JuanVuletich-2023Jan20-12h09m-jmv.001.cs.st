'From Cuis 6.0 [latest update: #5640] on 20 January 2023 at 12:10:11 pm'!

!Encoder methodsFor: 'encoding' stamp: 'jmv 1/20/2023 12:09:36'!
encodeLiteral: literalObject
	| object |
	object _ literalObject.
	"Prefer String over UnicodeString for literals, if ASCII only."
	(object isUnicodeString and: [ object isAscii ]) ifTrue: [
		object _ object asByteString ].
	(object isUnicodeCodePoint and: [ object asciiValue notNil ]) ifTrue: [
		object _ object asCharacter ].
	^self
		name: object
		key: (class literalScannedAs: object notifying: self)
		class: LiteralNode
		type: LdLitType
		set: litSet! !

