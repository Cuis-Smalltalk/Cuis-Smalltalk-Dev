'From Cuis 4.2 of 25 July 2013 [latest update: #2512] on 28 September 2015 at 2:05:06.076571 pm'!

!FmFileEntry methodsFor: 'enumeration' stamp: 'jmv 9/28/2015 14:03'!
assureExistence
	self exists ifTrue: [^self].
	self parent assureExistence.
	self forceWriteStream: [ :stream | ]! !

!FmFileEntry methodsFor: 'actions-file' stamp: 'jmv 9/28/2015 14:04'!
binaryContents: aByteArray
	self forceWriteStream: [ :stream |
		self setContentsOf: stream binary to: aByteArray ]! !

!FmFileEntry methodsFor: 'actions-file' stamp: 'jmv 9/28/2015 14:04'!
textContents: aString
	self forceWriteStream: [ :stream |
		self setContentsOf: stream to: aString ]! !

