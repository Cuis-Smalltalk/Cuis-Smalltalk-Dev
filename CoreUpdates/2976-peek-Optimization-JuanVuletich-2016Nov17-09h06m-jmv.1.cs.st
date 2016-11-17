'From Cuis 5.0 of 7 November 2016 [latest update: #2975] on 17 November 2016 at 9:08:34 am'!

!PositionableStream methodsFor: 'accessing' stamp: 'jmv 11/17/2016 09:06:59'!
peek
	"Answer what would be returned if the message next were sent to the 
	receiver. If the receiver is at the end, answer nil."

	| nextObject |
	position < readLimit ifTrue: [
		^collection at: position+1 ].
	self atEnd ifTrue: [^nil].
	nextObject _ self next.
	position _ position - 1.
	^nextObject! !


!StandardFileStream methodsFor: 'read, write, position' stamp: 'jmv 11/17/2016 09:07:20'!
peek
	"Answer what would be returned if the message next were sent to the receiver. If the receiver is at the end, answer nil.  "
	| next |
	position < readLimit ifTrue: [
		^collection at: position+1 ].
	self atEnd ifTrue: [^ nil].
	next _ self basicNext.
	self position: self position - 1.
	^ next! !

