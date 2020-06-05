'From Cuis 5.0 [latest update: #4204] on 4 June 2020 at 9:32:52 pm'!

!Form methodsFor: 'fileIn/Out' stamp: 'jmv 6/4/2020 21:30:35'!
storeOn: aStream base: anInteger 
	"Store the receiver out as an expression that can be evaluated to recreate a Form with the same contents as the original."

	aStream nextPut: $(.
	aStream nextPutAll: self species name.
	aStream newLineTab: 1.
	aStream nextPutAll: 'extent: '.
	self extent printOn: aStream.
	aStream newLineTab: 1.
	aStream nextPutAll: 'depth: '.
	self nativeDepth printOn: aStream.
	aStream newLineTab: 1.
	aStream nextPutAll: 'fromArray: #('.
	self storeBitsOn:aStream base:anInteger.
	aStream nextPut: $).
	aStream newLineTab: 1.
	aStream nextPutAll: 'offset: '.
	self offset printOn: aStream.
	aStream nextPut: $).
! !

