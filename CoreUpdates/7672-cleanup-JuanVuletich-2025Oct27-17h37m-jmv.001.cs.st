'From Cuis7.5 [latest update: #7671] on 27 October 2025 at 5:37:28 pm'!

!FileEntry methodsFor: 'actions-file' stamp: 'jmv 10/27/2025 16:21:57'!
binaryContents: aByteArray
	self forceWriteStreamDo: [ :stream |
		self setContentsOf: stream binary to: aByteArray ].! !

!FileEntry methodsFor: 'actions-file' stamp: 'jmv 10/27/2025 16:22:01'!
textContents: aString
	self forceWriteStreamDo: [ :stream |
		self setContentsOf: stream to: aString ].! !

