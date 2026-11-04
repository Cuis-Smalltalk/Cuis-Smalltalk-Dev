'From Cuis7.9 [latest update: #8311] on 4 November 2026 at 9:01:48 am'!

!Metaclass methodsFor: 'fileIn/Out' stamp: 'jmv 4/Nov/2026, 11:59:05 am (UTC)'!
fileOutInitializerOn: aStream
	(self methodDict includesKey: #initialize) ifTrue: [
		aStream newLine.
		aStream nextChunkPut: thisClass name , ' initialize'.
		aStream newLine.
		].! !

