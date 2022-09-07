'From Cuis 6.0 [latest update: #5472] on 6 September 2022 at 10:09:33 pm'!

!Inspector methodsFor: 'constants' stamp: 'jmv 9/6/2022 22:05:01'!
printStringLimit

	^4000! !


!Character methodsFor: 'printing' stamp: 'jmv 9/6/2022 22:01:14'!
printOn: aStream 
	aStream nextPut: $$.
	aStream nextPut: self.
	self numericValue <= 32 ifTrue: [
		aStream nextPut: $(.
		aStream nextPutAll: self hex.
		aStream nextPut: $) ].! !


!UnicodeCodePoint methodsFor: 'printing' stamp: 'jmv 9/6/2022 21:56:28'!
isLiteral
self flag: #unicodeNeedsImprovement.
"If #storeOn: calls
	self asCharacter ifNotNil: [ :c | c storeOn: aStream ].
then this method can be
	^ self asCharacter notNil
or
	self asCharacter ifNotNil: [ :c | ^c isLiteral ].
	^false
"
	^super isLiteral! !


!Array methodsFor: 'printing' stamp: 'jmv 9/6/2022 21:49:02'!
storeOn: aStream 
	"Use the literal form if possible."

	self isLiteral
		ifTrue: [
			aStream nextPut: $#; nextPut: $(.
			self do: [ :element | 
				element storeOn: aStream.
				aStream space].
			aStream nextPut: $)]
		ifFalse: [super storeOn: aStream]! !

