'From Cuis7.9 [latest update: #7992] on 9 June 2026 at 5:05:19 pm'!

!FileEntry methodsFor: 'private' stamp: 'jmv 9/Jun/2026 15:06:50'!
forceWriteStream
	"If the file already exists, delete it first without asking. Do not raise FileExistsException.
	Note: You need to eventually close the stream.
	Usually prefer #forceWriteStreamDo: that closes the file for you.
	Creates the directory if it doesn't exist."

	| stream |
	self invalidate.
	self parent exists ifFalse: [self parent assureExistence].
	stream := self fileAccessor privateForceNewFile: self.
	stream writeOnly.
	^stream! !

