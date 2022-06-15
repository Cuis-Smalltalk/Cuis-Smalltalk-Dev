'From Cuis 6.0 [latest update: #5259] on 10 June 2022 at 3:00:53 pm'!

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/10/2022 14:40:29'!
asLowercase
	"Answer a String made up from the receiver whose characters are all 
	lowercase."

	^ self collect: [ :each | each asLowercase ].! !


!String methodsFor: 'converting' stamp: 'jmv 6/10/2022 14:39:20'!
asLowercase
	"Answer a String made up from the receiver whose characters are all 
	lowercase."

	^ self copy asPlainString translateToLowercase! !


!Utf32String methodsFor: 'comparing' stamp: 'jmv 6/10/2022 14:44:47'!
sameAs: aString
	"Answer whether the receiver sorts equal to aString. The 
	collation sequence is ascii with case differences ignored."

	self = aString ifTrue: [ ^true ].
self flag: #slowUnicode.
	^self asLowercase = aString asLowercase! !


!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 6/10/2022 14:49:49'!
asLowercase
self flag: #unicodeNeedsImprovement.
	self asCharacter ifNotNil: [ :ch | ^UnicodeCodePoint codePoint: ch asLowercase codePoint ].
	^self! !


!Utf32String methodsFor: 'accessing' stamp: 'jmv 6/10/2022 14:56:13'!
isLastLineSeparator

	| s |
	s _ codePoints size.
	s = 0 ifTrue: [ ^false ].
	(#(10 13 12 16r0085 16r2028 16r2029) statePointsTo: 	(codePoints at: s)) ifTrue: [ ^true ].
	^false.
	! !

!Utf32String methodsFor: 'accessing' stamp: 'jmv 6/10/2022 14:54:09'!
isLastSeparator

	| s |
	s _ codePoints size.
	s = 0 ifTrue: [ ^false ].
	(#(32 9) statePointsTo: 	(codePoints at: s)) ifTrue: [ ^true ].
	self isLastLineSeparator ifTrue: [ ^true ].
	^false.! !


!Utf8String methodsFor: 'accessing' stamp: 'jmv 6/10/2022 14:56:07'!
isLastLineSeparator

	| s butButLastByte butLastByte lastByte |
	s _ bytes size.
	s = 0 ifTrue: [ ^false ].
	(#(10 13 12) statePointsTo: 	(bytes at: s)) ifTrue: [ ^true ].
	s = 1 ifTrue: [ ^false ].
	lastByte _ bytes at: s.
	butLastByte _ bytes at: s-1.
	(butLastByte = 16rC2 and: [ lastByte = 16r85 ]) ifTrue: [ ^true ]. "Next Line, NEL"
	s = 2 ifTrue: [ ^false ].
	butButLastByte _ bytes at: s-2.
	(butButLastByte = 16rE2 and: [butLastByte = 16r80 and: [ "Line Separator and Paragraph Separator"
		lastByte = 16rA8 or: [lastByte = 16rA9]]]) ifTrue: [ ^true ].
	^false.
	! !

!Utf8String methodsFor: 'accessing' stamp: 'jmv 6/10/2022 14:54:00'!
isLastSeparator

	| s |
	s _ bytes size.
	s = 0 ifTrue: [ ^false ].
	(#(32 9) statePointsTo: 	(bytes at: s)) ifTrue: [ ^true ].
	self isLastLineSeparator ifTrue: [ ^true ].
	^false.! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/10/2022 14:44:51'!
sameAs: aString
	"Answer whether the receiver sorts equal to aString. The 
	collation sequence is ascii with case differences ignored."

	self = aString ifTrue: [ ^true ].
self flag: #slowUnicode.
	^self asLowercase = aString asLowercase! !

