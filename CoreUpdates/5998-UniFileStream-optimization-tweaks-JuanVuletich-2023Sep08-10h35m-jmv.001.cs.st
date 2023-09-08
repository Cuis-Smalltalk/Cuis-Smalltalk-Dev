'From Cuis 6.0 [latest update: #5997] on 8 September 2023 at 10:36:50 am'!

!UniFileStream methodsFor: 'access' stamp: 'jmv 9/8/2023 10:24:38'!
size
	"Answer the size of the file in bytes.
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

	^ self primSize: fileID! !

!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 9/8/2023 10:25:15'!
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

	collection ifNotNil: [
		position < readLimit ifTrue: [ ^false ] ].

	^self primAtEnd: fileID! !

!UniFileStream methodsFor: 'private' stamp: 'jmv 9/8/2023 10:33:04'!
bytesUpToEnd
	"Leave stream at the end position."

	^self nextBytes: self size - self position! !

!UniFileStream methodsFor: 'private' stamp: 'jmv 9/7/2023 15:18:06'!
nextBytes: n
	"Read n bytes, or until the end of the file."

	| bytes |
	bytes := ByteArray new: n.
	^ self nextBytes: n into: bytes startingAt: 1.! !

