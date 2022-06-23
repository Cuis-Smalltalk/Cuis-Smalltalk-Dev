'From Cuis 6.0 [latest update: #5308] on 23 June 2022 at 4:41:00 pm'!

!StandardFileStream methodsFor: 'read, write, position' stamp: 'jmv 6/23/2022 16:39:16'!
nextPutAll: aString
	"Write all the characters of the given string to this file."

	| byteString |
	rwmode ifFalse: [^ self error: 'Cannot write a read-only file'].
	collection ifNotNil: [ 
		position < readLimit ifTrue: [ self flushReadBuffer ] ].
	byteString _ aString isString ifTrue: [ aString asByteString ] ifFalse: [ aString ].
	self primWrite: fileID from: byteString startingAt: 1 count: byteString basicSize.
	^ aString
! !

