'From Cuis 6.0 [latest update: #5636] on 20 January 2023 at 10:21:09 am'!

!Encoder methodsFor: 'encoding' stamp: 'jmv 1/20/2023 10:20:21'!
encodeLiteral: literalObject
	| object |
	object _ literalObject.
	"Prefer String over UnicodeString for literals, if ASCII only."
	(object isUnicodeString and: [ object isAscii ]) ifTrue: [
		object _ object asByteString ].
	^self
		name: object
		key: (class literalScannedAs: object notifying: self)
		class: LiteralNode
		type: LdLitType
		set: litSet! !

