'From Cuis 6.0 [latest update: #5873] on 21 June 2023 at 8:37:19 am'!

!Character class methodsFor: 'constants' stamp: 'jmv 6/21/2023 07:40:40'!
irregularIso8859s15Codes
	IrregularIso8859s15Codes ifNil: [ self initializeUnicodeCodePoints ].
	^IrregularIso8859s15Codes! !

!Character class methodsFor: 'constants' stamp: 'jmv 10/8/2018 18:37:36'!
unicodeCodePoints
	UnicodeCodePoints ifNil: [ self initializeUnicodeCodePoints ].
	^UnicodeCodePoints! !


!Character methodsFor: 'accessing' stamp: 'jmv 10/8/2018 18:39:19'!
codePoint
	"
	self assert: $A codePoint hex = '16r41'.
	self assert: $€ codePoint hex = '16r20AC'.
	"
	^self class unicodeCodePoints at: self numericValue + 1! !


!Character class methodsFor: 'class initialization' stamp: 'jmv 6/21/2023 08:35:40'!
releaseClassState
	UnaccentedTable := nil.
	UnicodeCodePoints := nil.
	IrregularIso8859s15Codes := nil.! !

!Character class methodsFor: 'converting' stamp: 'jmv 6/21/2023 08:14:39'!
iso8859s15CodeForUnicodeCodePoint: codePoint
	"
	Answer nil if the Unicode codePoint is not a valid ISO 8859-15 character
	
	self assert: (Character iso8859s15CodeForUnicodeCodePoint: 16r41) = $A iso8859s15Code.
	self assert: (Character iso8859s15CodeForUnicodeCodePoint: 16r20AC) = $€ iso8859s15Code.
	"

	"For most 8 bit Characters, ISO 8859-15 and Unicode Code Point are the same. Fast path."
	(codePoint < 256 and: [ (self unicodeCodePoints at: codePoint+1) = codePoint ])
		ifTrue: [ ^codePoint ].

	^ self irregularIso8859s15Codes at: codePoint ifAbsent: nil.! !

