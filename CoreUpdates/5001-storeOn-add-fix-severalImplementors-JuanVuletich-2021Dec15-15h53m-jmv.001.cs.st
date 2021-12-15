'From Cuis 5.0 [latest update: #5000] on 15 December 2021 at 4:11:45 pm'!

!ProcessorScheduler methodsFor: 'printing' stamp: 'jmv 12/15/2021 15:28:04'!
storeOn: aStream
	self == Processor ifTrue: [
		^ aStream nextPutAll: 'Processor'].
	^ super storeOn: aStream! !


!StandardFileStream methodsFor: 'printing' stamp: 'jmv 12/15/2021 16:00:31'!
storeOn: aStream
	"A file can't be serialized"

	^ self printOn: aStream! !


!Process methodsFor: 'printing' stamp: 'jmv 12/15/2021 15:39:58'!
storeOn: aStream
	aStream nextPutAll: '("a Process. Can not be serialized")'.! !


!DisplayScreen methodsFor: 'printing' stamp: 'jmv 12/15/2021 15:43:57'!
storeOn: aStream
	self == Display ifTrue: [
		^ aStream nextPutAll: 'Display'].
	^ super storeOn: aStream! !


!CompiledMethod methodsFor: 'printing' stamp: 'jmv 12/15/2021 16:10:47'!
storeOn: aStream

	^ self printOn: aStream! !

