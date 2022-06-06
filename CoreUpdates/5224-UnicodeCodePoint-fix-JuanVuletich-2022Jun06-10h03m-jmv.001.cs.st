'From Cuis 6.0 [latest update: #5220] on 6 June 2022 at 10:03:46 am'!

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 6/6/2022 10:03:32'!
asLowercase
self flag: #unicodeNeedsImprovement.
	self asCharacter ifNotNil: [ :ch | ^UnicodeCodePoint codePoint: ch asUnaccented asLowercase codePoint ].
	^self! !

