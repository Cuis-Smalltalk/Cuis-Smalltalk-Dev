'From Cuis 6.0 [latest update: #5581] on 7 December 2022 at 11:25:28 am'!

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 12/7/2022 11:14:56'!
findCannonicalCompositionAndHash
	"Find our cannonical composition (NFC). If we are found to be in NFC, store our hash.
	Otherwise, store our NFC. Queries of #hash will be forwarded to it."

	| cannonicalComposition |
	"Ascii is already normalized to NFC."
	self isAscii ifTrue: [
		^self computeHash ].

	"Find our cannonical composition"
	cannonicalComposition _ self basicCannonicalDecomposition basicCannonicallyOrdered basicCannonicalComposition.
	(String is: bytes equalTo: cannonicalComposition bytes)
		ifTrue: [
			"If we were already in NFC, compute and store our hash"
			self computeHash ]
		ifFalse: [
			"Otherwise, point to our NFC, and ensure it has its hash, for us to query when needed."
			hash _ cannonicalComposition.
			cannonicalComposition computeHash ].! !

