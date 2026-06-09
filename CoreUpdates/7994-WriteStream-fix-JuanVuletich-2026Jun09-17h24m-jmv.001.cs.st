'From Cuis7.9 [latest update: #7993] on 9 June 2026 at 5:25:05 pm'!

!WriteStream methodsFor: 'fileIn/Out' stamp: 'jmv 9/Jun/2026 17:24:50'!
writeSystemTimeStamp
	"Append the current time to the receiver as a String."
	self nextChunkPut:	"double string quotes and !!s"
		(String streamContents: [:s | Smalltalk writeSystemTimeStamp: s]) printString.
	self newLine! !

