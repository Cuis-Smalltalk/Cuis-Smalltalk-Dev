'From Cuis 6.0 [latest update: #5636] on 20 January 2023 at 9:51:30 am'!

!Symbol class methodsFor: 'instance creation' stamp: 'jmv 1/20/2023 09:51:14'!
intern: aStringOrSymbol
	"If argument is String or ASCII-only UnicodeString, create a Symbol.
	If argument is UnicodeString, and it includes non-ASCII characters, create a UnicodeSymbol.
	This is done for the benefit of (for instance) the SqueakSSL VM plugin,
		that fails if primitive names or module are not instance of Symbol."

	| preferByteStringForAscii |
	^ (self lookup: aStringOrSymbol) ifNil: [
		NewSymbols add:
			(aStringOrSymbol isSymbol
				ifTrue: [ aStringOrSymbol ]
				ifFalse: [
					preferByteStringForAscii _ aStringOrSymbol.
					(aStringOrSymbol isUnicodeString and: [ aStringOrSymbol isAscii ])
						ifTrue: [preferByteStringForAscii _ aStringOrSymbol asByteString].
					preferByteStringForAscii class symbolClassToUse basicFromString: preferByteStringForAscii ]) ].! !

