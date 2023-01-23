'From Cuis 6.0 [latest update: #5646] on 22 January 2023 at 3:55:05 pm'!

!Character methodsFor: 'printing' stamp: 'jmv 1/22/2023 15:49:21'!
printOn: aStream 
	aStream nextPut: $$.
	aStream nextPut: self.
	self numericValue <= 32 ifTrue: [
		aStream nextPut: $".
		aStream nextPutAll: self hex.
		aStream nextPut: $" ].! !


!UnicodeCodePoint methodsFor: 'printing' stamp: 'jmv 1/22/2023 15:53:13'!
printOn: aStream 
	aStream nextPut: $$.
	aStream nextPut: self.
	self value <= 32 ifTrue: [
		aStream nextPut: $".
		aStream nextPutAll: self hex.
		aStream nextPut: $" ].! !

!UnicodeCodePoint methodsFor: 'printing' stamp: 'jmv 1/22/2023 15:54:21'!
storeOn: aStream
	"Character literals are preceded by '$'."
	aStream nextPut: $$; nextPut: self! !

