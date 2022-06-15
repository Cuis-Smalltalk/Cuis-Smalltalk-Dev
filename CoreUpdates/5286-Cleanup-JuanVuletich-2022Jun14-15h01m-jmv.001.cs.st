'From Cuis 6.0 [latest update: #5284] on 14 June 2022 at 3:05:28 pm'!

!EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 6/14/2022 15:02:10'!
position: pos

	super position: pos.
	lastWrittenElement _ nil.
	numberOfCodePointsWritten _ nil.! !


!EncodedFileStream methodsFor: 'open/close' stamp: 'jmv 6/14/2022 15:04:29'!
open: fileName forWrite: writeMode
	"Open the file with the given name. If writeMode is true, allow writing, otherwise open the file in read-only mode."
	"Changed to do a GC and retry before failing ar 3/21/98 17:25"
	fileID _ EncodedFileStream retryWithGC: [ self primOpen: fileName asUtf8Bytes writable: writeMode ]
					until: [ :id | id notNil ]
					forFileNamed: fileName.
	fileID ifNil: [^ nil].  "allows sender to detect failure"
	name _ fileName.
	"jmv: Register after setting name. Name is assumed to be defined for registered objects."
	self register.
	rwmode _ writeMode.
	buffer1 _ ByteArray new: 1.
	self useUtf8String.
	self enableReadBuffering.! !

!EncodedFileStream methodsFor: 'read, write, position' stamp: 'jmv 6/14/2022 15:02:29'!
position: pos
	"Set the receiver's position as indicated."

	collection ifNotNil: [
		position < readLimit ifTrue: [
			| newPosition |
			newPosition := pos - (self primGetPosition: fileID) + readLimit.
			newPosition < 0 ifTrue: [
					| offsetPos |
					self primSetPosition: fileID to: (offsetPos := pos - (collection size // 4) max: 0).
					readLimit := self primRead: fileID into: collection startingAt: 1 count: collection size.
					position := pos - offsetPos.
					^self ].
			newPosition < readLimit 
				ifTrue: [
					position := newPosition.
					^self ]
				ifFalse: [
					readLimit := position := 0 ] ] ].
	^self primSetPosition: fileID to: pos! !

!methodRemoval: EncodedFileStream class #positionValidatedSenders stamp: 'jmv 6/14/2022 15:03:01'!
EncodedFileStream class removeSelector: #positionValidatedSenders!
!methodRemoval: EncodedReadStream #position: stamp: 'jmv 6/14/2022 15:01:51'!
EncodedReadStream removeSelector: #position:!
