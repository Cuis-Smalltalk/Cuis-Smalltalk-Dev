'From Cuis 6.0 [latest update: #5205] on 30 May 2022 at 12:15:50 pm'!

!WriteStream methodsFor: 'fileIn/Out' stamp: 'jmv 5/30/2022 12:12:54'!
timeStamp
	"Append the current time to the receiver as a String."
	self nextChunkPut:	"double string quotes and !!s"
		(self collectionSpecies streamContents: [:s | Smalltalk timeStamp: s]) printString.
	self newLine! !

