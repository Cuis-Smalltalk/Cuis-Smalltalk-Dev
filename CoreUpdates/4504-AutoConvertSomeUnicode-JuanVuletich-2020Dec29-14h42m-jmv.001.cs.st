'From Cuis 5.0 [latest update: #4503] on 29 December 2020 at 2:43:10 pm'!

!Character class methodsFor: 'instance creation' stamp: 'jmv 12/29/2020 14:32:32'!
codePoint: codePoint trySimilar: aBoolean
	"
	Answer an appropriante Character.
	If we don't have a suitable Character for codePoint, answer nil.
	But if aBoolean, answer a similar Character if possible.

	self assert: (Character codePoint: 16r41 convertToASCII: false) = $A.
	self assert: (Character codePoint: 16r20AC convertToASCII: false) = $¤.
	"
	| codePoints similarCharacters |
	aBoolean ifTrue: [
		(codePoint between: 16r2018 and: 16r201B) ifTrue: [
			^$' ].
		(codePoint between: 16r201C and: 16r201F) ifTrue: [
			^$" ].
		(codePoint between: 16r2000 and: 16r200D) ifTrue: [
			^$  ].
		(codePoint between: 16r2010 and: 16r2015) ifTrue: [
			^$- ].
		(codePoint between: 16r2024 and: 16r2026) ifTrue: [
			^$- ].
		codePoints _ #(16r2190 16r2191 16r2022 16r2023 16r2027 16r2032 16r2033 16r2035 16r2036 16r2039 16r203A).
		similarCharacters _ #($_ $^ $° $° $- $'  $" $` $" $< $>).
		(codePoints statePointsTo: codePoint) ifTrue: [
			^ similarCharacters at:  (codePoints indexOf: codePoint) ]].

	^ (self iso8859s15CodeForUnicodeCodePoint: codePoint)
		ifNotNil: [ :code | Character numericValue: code ]! !


!Character class methodsFor: 'instance creation' stamp: 'jmv 12/29/2020 14:04:54'!
codePoint: codePoint
	"
	Answer nil if the Unicode codePoint is not a valid ISO 8859-15 character
	
	self assert: (Character codePoint: 16r41) = $A.
	self assert: (Character codePoint: 16r20AC) = $¤.
	"
	^ self codePoint: codePoint trySimilar: false! !


!String class methodsFor: 'instance creation' stamp: 'jmv 12/29/2020 13:03:32'!
addUnicodeCodePoint: codePoint to: strm hex: useHexForNCRs
	"Convert the given Unicode codePoint to  the internal encoding: ISO Latin 9 (ISO 8859-15)"
	"For unicode chars not in ISO Latin 9 (ISO 8859-15), embed Decimal NCRs or Hexadecimal NCRs according to useHex.
	
	See http://en.wikipedia.org/wiki/Numeric_character_reference
	See http://rishida.net/tools/conversion/. Tests prepared there.
	
	Note: The conversion of NCRs is reversible. See #asUtf8:
	This allows handling the full Unicode in Cuis tools, that can only display the Latin alphabet, by editing the NCRs.
	The conversions can be done when reading / saving files, or when pasting from Clipboard and storing back on it."

	(Character codePoint: codePoint trySimilar: true)
		ifNotNil: [ :char | strm nextPut: char]
		ifNil: [
			useHexForNCRs
				ifTrue: [
					strm nextPutAll: '&#x'.
					codePoint printOn: strm base: 16 length: 4 padded: true.
					strm nextPut: $; ]
				ifFalse: [
					strm nextPutAll: '&#'.
					codePoint printOn: strm base: 10.
					strm nextPut: $; ]]! !

!methodRemoval: Character class #safeCodePoint: stamp: 'jmv 12/29/2020 14:42:38'!
Character class removeSelector: #safeCodePoint:!
