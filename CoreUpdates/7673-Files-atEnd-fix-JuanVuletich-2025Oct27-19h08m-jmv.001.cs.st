'From Cuis7.5 [latest update: #7672] on 27 October 2025 at 7:10:57 pm'!

!UniFileStream methodsFor: 'access' stamp: 'jmv 10/27/2025 19:10:14'!
size
	"Answer the size of the file in bytes."

	^ self primSize: fileID! !

!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 10/27/2025 19:10:24'!
atEnd
	"Answer whether the receiver is at its end."
	collection ifNotNil: [
		position < readLimit ifTrue: [ 
			^false ] ].

	^ (self primAtEnd: fileID) and: [ (self primGetPosition: fileID) = (self primSize: fileID) ]! !

