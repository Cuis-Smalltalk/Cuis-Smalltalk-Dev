'From Cuis 5.0 [latest update: #4426] on 27 October 2020 at 10:40:23 am'!

!BlockClosure methodsFor: 'printing' stamp: 'jmv 10/27/2020 10:40:01'!
storeOn: aStream
	"
	[] storeString
	"
	aStream nextPut: $(.
	self asSerializable storeOn: aStream.
	aStream nextPutAll: ' asEvaluable)'! !

