'From Cuis 6.0 [latest update: #5177] on 20 May 2022 at 10:36:46 am'!

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 5/20/2022 10:30:37'!
getPreambleFrom: aFileStream at: position
	|  writeStream c p |
	writeStream _ String new writeStream.
	p _ position - 1.
	c _ nil.
	aFileStream position: p.
	aFileStream atEnd ifTrue: [ ^ nil ].
	[ p >= 0 and: [ c ~~ $!! ]] whileTrue: [
		aFileStream position: p.
		c _ aFileStream next.
		p _ p - 1 ].
	[ p >= 0] whileTrue: [
		aFileStream position: p.
		c _ aFileStream next.
		c == $!!
			ifTrue: [^ writeStream contents reverse ]
			ifFalse: [ writeStream nextPut: c ].
		p _ p - 1 ].
	^ nil! !


!StandardFileStream methodsFor: 'read, write, position' stamp: 'jmv 5/19/2022 18:23:42'!
next
	"Answer the next byte or character (depending on mode) from this file, or nil if at the end of the file."
	
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

!StandardFileStream methodsFor: 'read, write, position' stamp: 'jmv 5/20/2022 10:31:01'!
peek
	"Answer what would be returned if the message next were sent to the receiver. If the receiver is at the end, answer nil.  "
	| next |
	position < readLimit ifTrue: [
		^collection at: position+1 ].
	self atEnd ifTrue: [^ nil].
	next _ self next.
	self position: self position - 1.
	^ next! !

!methodRemoval: StandardFileStream #basicNext stamp: 'jmv 5/20/2022 10:29:57'!
StandardFileStream removeSelector: #basicNext!
