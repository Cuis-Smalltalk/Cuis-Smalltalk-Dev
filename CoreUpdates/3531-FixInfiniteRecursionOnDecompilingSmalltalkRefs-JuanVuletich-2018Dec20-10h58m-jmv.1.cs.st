'From Cuis 5.0 of 7 November 2016 [latest update: #3529] on 20 December 2018 at 11:08:36 am'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 12/20/2018 11:07:32'!
storeOn: aStream
	self == Smalltalk ifTrue: [
		^ aStream nextPutAll: 'Smalltalk'].
	^ super storeOn: aStream! !

