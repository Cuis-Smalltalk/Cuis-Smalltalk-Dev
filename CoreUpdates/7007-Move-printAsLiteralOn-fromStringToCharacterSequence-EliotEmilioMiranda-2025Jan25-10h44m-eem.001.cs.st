'From Cuis7.3 [latest update: #7006] on 25 January 2025 at 10:45:45 am'!

!CharacterSequence methodsFor: 'printing' stamp: 'eem 4/3/2017 15:30'!
printAsLiteralOn: aStream
	"Print inside string quotes, doubling embedded quotes."
	self storeOn: aStream! !

!methodRemoval: String #printAsLiteralOn: stamp: 'eem 1/25/2025 10:44:57'!
String removeSelector: #printAsLiteralOn:!
