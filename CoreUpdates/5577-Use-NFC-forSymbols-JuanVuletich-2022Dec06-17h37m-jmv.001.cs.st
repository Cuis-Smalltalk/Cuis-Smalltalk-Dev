'From Cuis 6.0 [latest update: #5572] on 6 December 2022 at 5:42:12 pm'!

!Utf8Symbol class methodsFor: 'private' stamp: 'jmv 12/6/2022 17:42:02'!
basicFromString: aString
	"Private use. Answers an instance that is not yet interned.
	Can break the Symbol invariant: forAllSymbols, (s1 = s2) => (s1 == s2)
	Please don't call lightly."

	| cannonicalComposition utf8Bytes |
	cannonicalComposition _ aString cannonicalComposition.
	utf8Bytes _ cannonicalComposition asUtf8Bytes.
	^self basicNew privateBytes: utf8Bytes codePointCount: cannonicalComposition size.! !

