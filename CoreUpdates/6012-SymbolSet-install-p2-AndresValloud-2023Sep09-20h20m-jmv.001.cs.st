'From Cuis 6.0 [latest update: #6011] on 9 September 2023 at 8:21:15 pm'!

!Symbol class methodsFor: 'instance creation' stamp: 'sqr 9/9/2023 20:20:52'!
intern: aStringOrSymbol
	"If argument is String or ASCII-only UnicodeString, create a Symbol.
	If argument is UnicodeString, and it includes non-ASCII characters, create a UnicodeSymbol.
	This is done for the benefit of (for instance) the SqueakSSL VM plugin,
		that fails if primitive names or module are not instance of Symbol."

	| preferByteStringForAscii |
	Symbols intern: aStringOrSymbol.
	^ (self lookup: aStringOrSymbol) ifNil: [
		NewSymbols add:
			(aStringOrSymbol isSymbol
				ifTrue: [ aStringOrSymbol ]
				ifFalse: [
					preferByteStringForAscii := aStringOrSymbol.
					(aStringOrSymbol isUnicodeString and: [ aStringOrSymbol isAscii ])
						ifTrue: [preferByteStringForAscii := aStringOrSymbol asByteString].
					preferByteStringForAscii class symbolClassToUse basicFromString: preferByteStringForAscii ]) ].! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Symbol rehash.!

