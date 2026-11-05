'From Cuis7.9 [latest update: #8325] on 5 November 2026 at 5:34:25 pm'!

!FileEntry methodsFor: 'accessing' stamp: 'jmv 5/Nov/2026, 8:33:00 pm (UTC)'!
appendStream
	"Note: You need to eventually close the stream.
	Usually prefer #appendStreamDo: that closes the file for you."

	self invalidate.
	"Not needed. The line below also works if file already exists.
	self exists ifFalse: [
		^ self writeStream ]."
	^ (self fileAccessor privateWriteableFile: self) setToEnd! !

!FileEntry methodsFor: 'accessing' stamp: 'jmv 5/Nov/2026, 7:13:22 pm (UTC)'!
writeStream
	"If the file already exists raise FileExistsException.
	Note: You need to eventually close the stream.
	Usually prefer #writeStreamDo: that closes the file for you.
	Creates the directory if it doesn't exist.
	Note: Answer nil if for whatever reason the file can not be created.
	"

	self invalidate.
	self parent assureExistence.
	^self fileAccessor privateNewFile: self! !

!FileEntry methodsFor: 'private' stamp: 'jmv 5/Nov/2026, 7:13:15 pm (UTC)'!
forceWriteStream
	"If the file already exists, delete it first without asking. Do not raise FileExistsException.
	Note: You need to eventually close the stream.
	Usually prefer #forceWriteStreamDo: that closes the file for you.
	Creates the directory if it doesn't exist."

	| stream |
	self invalidate.
	self parent assureExistence.
	stream := self fileAccessor privateForceNewFile: self.
	stream writeOnly.
	^stream! !

