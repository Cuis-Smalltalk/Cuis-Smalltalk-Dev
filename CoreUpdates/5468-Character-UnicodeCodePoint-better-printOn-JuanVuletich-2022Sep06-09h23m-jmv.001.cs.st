'From Cuis 6.0 [latest update: #5467] on 6 September 2022 at 9:24:09 am'!

!Character methodsFor: 'printing' stamp: 'jmv 9/6/2022 09:05:00'!
printOn: aStream 
	aStream nextPut: $$.
	aStream nextPut: self.
	self isAlphaNumeric ifFalse: [
		aStream nextPut: $(.
		aStream nextPutAll: self hex.
		aStream nextPut: $) ].! !


!UnicodeCodePoint methodsFor: 'printing' stamp: 'jmv 9/6/2022 09:03:28'!
printOn: aStream 
	aStream nextPut: $$.
	self asCharacter ifNotNil: [ :c | aStream nextPut: c ].
	aStream nextPut: $(.
	aStream nextPutAll: self hex.
	aStream nextPut: $).! !

