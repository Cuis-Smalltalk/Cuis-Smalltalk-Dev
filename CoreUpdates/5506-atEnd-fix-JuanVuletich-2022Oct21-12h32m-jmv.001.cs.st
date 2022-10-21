'From Cuis 6.0 [latest update: #5505] on 21 October 2022 at 12:39:14 pm'!

!StandardFileStream methodsFor: 'read, write, position' stamp: 'jmv 10/21/2022 12:32:08'!
atEnd
	"Answer whether the receiver is at its end.  "
	
	collection ifNotNil: [
		position < readLimit ifTrue: [ ^false ] ].
	"
	^self primAtEnd: fileID
	"
	^self position >= self size! !


!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 10/21/2022 12:32:19'!
atEnd
	"Answer whether the receiver is at its end.  "
	
	collection ifNotNil: [
		position < readLimit ifTrue: [ ^false ] ].
	"
	^self primAtEnd: fileID
	"
	^self position >= self size! !

