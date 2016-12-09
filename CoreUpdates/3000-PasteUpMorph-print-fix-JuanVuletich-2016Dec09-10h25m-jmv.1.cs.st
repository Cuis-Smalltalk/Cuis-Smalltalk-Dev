'From Cuis 5.0 of 7 November 2016 [latest update: #2981] on 9 December 2016 at 10:27:21 am'!

!PasteUpMorph methodsFor: 'printing' stamp: 'jmv 12/9/2016 10:25:13'!
printOn: aStream
	"Reimplemented to add a tag showing that the receiver is currently functioning as a 'world', if it is"

	self isWorldMorph
		ifTrue: [aStream nextPutAll: ' [world]']
		ifFalse: [super printOn: aStream]! !

