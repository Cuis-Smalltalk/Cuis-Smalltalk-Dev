'From Cuis 6.0 [latest update: #5659] on 30 January 2023 at 8:58:55 pm'!

!Encoder methodsFor: 'encoding' stamp: 'jmv 1/30/2023 20:57:27'!
asciiStringLiteral: literalObject
	"Convert UnicodeStrings to Strings if ASCII.
	Same for UnicodeCodePoint and Character.
	Recursive for Arrays"
	
	literalObject class == Array ifTrue: [
		^literalObject collect: [ :each | self asciiStringLiteral: each ]].

	"Prefer String over UnicodeString for literals, if ASCII only."
	(literalObject class == UnicodeString and: [ literalObject isAscii ]) ifTrue: [
		^ literalObject asByteString ].
	
	"Prefer Character over UnicodeCodePoint for literals, if ASCII only."
	(literalObject class == UnicodeCodePoint and: [ literalObject asciiValue notNil ]) ifTrue: [
		^ literalObject asCharacter ].

	^literalObject! !


!Encoder methodsFor: 'encoding' stamp: 'jmv 1/30/2023 20:57:51'!
encodeLiteral: literalObject

	| object |
	object _ self asciiStringLiteral: literalObject.
	^self
		name: object
		key: (class literalScannedAs: object notifying: self)
		class: LiteralNode
		type: LdLitType
		set: litSet.! !

