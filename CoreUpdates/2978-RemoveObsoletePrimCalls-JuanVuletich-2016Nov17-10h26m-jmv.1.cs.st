'From Cuis 5.0 of 7 November 2016 [latest update: #2977] on 17 November 2016 at 10:29:07 am'!

!PositionableStream methodsFor: 'testing' stamp: 'jmv 11/17/2016 10:28:06'!
atEnd
	"Answer whether the receiver can access any more objects."

	^position >= readLimit! !


!ReadStream methodsFor: 'accessing' stamp: 'jmv 11/17/2016 09:59:57'!
next
	"Answer the next object in the Stream represented by the receiver."

	^position >= readLimit
		ifFalse: [collection at: (position _ position + 1)]! !


!WriteStream methodsFor: 'accessing' stamp: 'jmv 11/17/2016 10:28:35'!
nextPut: anObject 
	"Insert the argument at the next position in the Stream represented by the receiver."

	position >= writeLimit
		ifTrue: [^ self pastEndPut: anObject]
		ifFalse: [
			position _ position + 1.
			^collection at: position put: anObject]! !


!ReadWriteStream methodsFor: 'accessing' stamp: 'jmv 11/17/2016 10:28:45'!
next
	"Return the next object in the Stream represented by the receiver."

	"treat me as a FIFO"
	^ position >= readLimit
		ifFalse: [collection at: (position _ position + 1)]! !


!StandardFileStream methodsFor: 'read, write, position' stamp: 'jmv 11/17/2016 10:00:56'!
basicNext
	"Answer the next byte from this file, or nil if at the end of the file."
	
	| count |
	collection ifNotNil: [
		position < readLimit 
			ifFalse: [ 
				readLimit := self primRead: fileID into: collection startingAt: 1 count: collection size.
				position := 0.
				readLimit = 0 ifTrue: [ ^nil ] ].
		^collection at: (position := position + 1) ].	
	count _ self primRead: fileID into: buffer1 startingAt: 1 count: 1.
	^count = 1
		ifTrue: [ buffer1 at: 1 ]! !

