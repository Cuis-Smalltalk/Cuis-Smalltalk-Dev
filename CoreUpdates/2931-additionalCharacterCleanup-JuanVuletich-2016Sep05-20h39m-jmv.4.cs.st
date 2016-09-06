'From Cuis 4.2 of 25 July 2013 [latest update: #2929] on 5 September 2016 at 9:08:36 pm'!

!Character class methodsFor: 'instance creation' stamp: 'jmv 9/5/2016 21:06:16'!
safeCodePoint: asciiCodeOrCodePoint 
	"Answer the Character whose value is anInteger.
	Handle unicode code points > 255 without errors, trying to answer something reasonable"

	"Note: senders of #value:or: in '1002-RTFParser.cs' has many automatic conversion to ISO-8859-15 characters, that would be valuable here."
	(#(16r2019 16r201B) includes: asciiCodeOrCodePoint) ifTrue: [
		^$' ].
	(#(16r201C 16r201D 16r201F) includes: asciiCodeOrCodePoint) ifTrue: [
		^$" ].
	^(self codePoint: asciiCodeOrCodePoint)
		ifNil: [Character numericValue: 255 ]! !


!Character class methodsFor: 'class initialization' stamp: 'jmv 9/2/2016 11:05:19'!
initializeClassificationTable
	"Initialize the classification table. The classification table is a
	compact encoding of upper and lower cases of characters with
		- bits 0-7: The lower case value of this character.
		- bits 8-15: The upper case value of this character.
		- bit 16: lowercase bit (e.g., isLowercase == true)
		- bit 17: uppercase bit (e.g., isUppercase == true).
	The UnaccentedTable holds for each numeric value, the corresponding unaccented character"

	LowercaseBit _ 1 bitShift: 16.
	UppercaseBit _ 1 bitShift: 17.

	"Initialize the letter bits (e.g., isLetter == true)"
	LetterBits _ LowercaseBit bitOr: UppercaseBit.

	ClassificationTable _ Array new: 256.
	UnaccentedTable _ ByteArray new: 256.
	"Initialize the defaults (neither lower nor upper case)"
	0 to: 255 do: [ :i |
		ClassificationTable at: i+1 put: (i bitShift: 8) + i.
		UnaccentedTable at: i+1 put: i].

	"Initialize character pairs (upper-lower case)"
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :group |
		| uppercase lowercase unaccentedUppercase unaccentedLowercase |
		group size > 1 ifTrue: [
			uppercase _ group first numericValue.
			lowercase _ group second numericValue.
			ClassificationTable at: uppercase+1 put: (uppercase bitShift: 8) + lowercase + UppercaseBit.
			ClassificationTable at: lowercase+1 put: (uppercase bitShift: 8) + lowercase + LowercaseBit.
			group size > 2
				ifTrue: [
					unaccentedUppercase _ group third numericValue.
					unaccentedLowercase _ group fourth numericValue.
					UnaccentedTable at: uppercase+1 put: unaccentedUppercase.
					UnaccentedTable at: lowercase+1 put: unaccentedLowercase ].
		] ifFalse: [
			lowercase _ group first numericValue.
			ClassificationTable at: lowercase+1 put: (lowercase bitShift: 8) + lowercase + LowercaseBit.
		]
	]! !

!Character class methodsFor: 'instance creation' stamp: 'jmv 9/5/2016 20:47:25'!
codePoint: codePoint
	"
	Answer nil if the Unicode codePoint is not a valid ISO 8859-15 character
	
	self assert: (Character codePoint: 16r41) = $A.
	self assert: (Character codePoint: 16r20AC) = $¤.
	"
	^ (self iso8859s15CodeForUnicodeCodePoint: codePoint)
		ifNotNil: [ :code | Character numericValue: code ]! !

!methodRemoval: String class #value:!
String class removeSelector: #value:!
!methodRemoval: Character class #safeValue:!
Character class removeSelector: #safeValue:!
