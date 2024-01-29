'From Cuis6.3 [latest update: #6184] on 9 January 2024 at 12:08:51 pm'!

!Encoder methodsFor: 'encoding' stamp: 'jmv 1/9/2024 11:23:12'!
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
	"bueh......"
	literalObject class == UnicodeCodePoint ifTrue: [
		^ Character codePoint: literalObject codePoint ].

	^literalObject! !

