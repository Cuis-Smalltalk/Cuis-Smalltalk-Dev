'From Cuis 6.0 [latest update: #5998] on 8 September 2023 at 2:38:00 pm'!

!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 9/8/2023 14:22:49'!
atEnd
	"Answer whether the receiver is at its end.
	Warning: #atEnd and #size only considers changes done on the file system over this very instance.
	They don't take into account changes to files done in other instances, or even outside of Cuis.
	To verify this run the following snippet. Before you do, create experiment.txt with, i.e., 4 characters
	Then run it, and during halt, add a couple of characters and save the file. Then resume.
			| entry |
			'---- Start' print.
			entry := 'experiment.txt' asFileEntry.
			entry readStreamDo: [ :s |
				s nextBytes: 1000.
				{s atEnd. s position. s size. s nextByte} print.
				self halt.
				{s atEnd. s position. s size. s nextByte} print.
				entry readStreamDo: [ :s2 |
					s2 position: s position.
					{s2 atEnd. s2 position. s2 size. s2 nextByte} print ].
				{s atEnd. s position. s size. s nextByte} print.
				].
			'done'
	"
	
	| answer |
	collection ifNotNil: [
		position < readLimit ifTrue: [ 
			^false ] ].

	answer := self primAtEnd: fileID.
	answer ifTrue: [
		"This puzzling line is needed for #testUpTo2IncompleteRecords to pass, at least on MacOS.
		Weird stuff going in inside the primitives!!"
		self primSize: fileID ].
	^answer! !

