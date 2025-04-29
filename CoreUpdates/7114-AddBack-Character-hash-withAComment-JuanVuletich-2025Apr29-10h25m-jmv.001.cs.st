'From Cuis7.3 [latest update: #7113] on 29 April 2025 at 10:42:29 am'!

!Character methodsFor: 'comparing' stamp: 'jmv 4/29/2025 10:41:03'!
hash
	"Hash is reimplemented because #hash for long Strings relies on it, and especially on being the same as the codePoint's for the ASCII range. Also see the comment at Integer>>#hash.
	| longString |
	longString := (1 to: 100)
		inject: ''
		into: [ :prev :each |
			prev , 'A lot of stuff, needs over 2000 chars!! ' ].
	self assert: longString asByteString hash = longString asUnicodeString hash.
	self assert: longString asByteString = longString asUnicodeString.
	"
	^self codePoint hash! !

