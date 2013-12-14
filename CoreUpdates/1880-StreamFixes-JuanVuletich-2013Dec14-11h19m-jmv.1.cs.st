'From Cuis 4.2 of 25 July 2013 [latest update: #1879] on 14 December 2013 at 11:20:26.281251 am'!

!ImageReadWriter methodsFor: 'private' stamp: 'jmv 12/14/2013 11:14'!
binaryStream: aStream
	stream _ aStream! !


!ImageReadWriter class methodsFor: 'instance creation' stamp: 'jmv 12/14/2013 11:14'!
onBinaryStream: aStream
	"Answer an instance of the receiver for encoding and/or decoding images on the given."

	^ self new binaryStream: aStream
! !


!Form class methodsFor: 'instance creation' stamp: 'jmv 12/14/2013 10:51'!
fromBinaryStream: aBinaryStream
	"Read a Form or ColorForm from given file, using the first byte of the file to guess its format. Currently handles: GIF, uncompressed BMP, and both old and new DisplayObject writeOn: formats, JPEG, and PCX. Return nil if the file could not be read or was of an unrecognized format."

	| firstByte |
	firstByte _ aBinaryStream next.
	firstByte = 1 ifTrue: [
		"old Squeakform format"
		^ self new readFromOldFormat: aBinaryStream].
	firstByte = 2 ifTrue: [
		"new Squeak form format"
		^ self new readFrom: aBinaryStream].

	"Try for JPG, GIF, or PCX..."
	"Note: The following call closes the stream."
	^ ImageReadWriter formFromStream: aBinaryStream
! !


!ImageReadWriter class methodsFor: 'image reading/writing' stamp: 'jmv 12/14/2013 11:18'!
formFromStream: aBinaryStream
	"Answer a ColorForm stored on the given stream.  closes the stream"
	| reader readerClass form  |

	readerClass _ self withAllSubclasses
		detect: [ :subclass | subclass understandsImageFormat: aBinaryStream reset ]
		ifNone: [
			aBinaryStream close.
			^self error: 'image format not recognized'].
	reader _ readerClass onBinaryStream: aBinaryStream reset.
	form _ reader nextImage.
	reader close.
	^ form! !

!ImageReadWriter class methodsFor: 'image reading/writing' stamp: 'jmv 12/14/2013 11:09'!
putForm: aForm onFileNamed: fileName
	"Store the given form on a file of the given name."

	| writer |
	writer _ self onBinaryStream: (FileStream newFileNamed: fileName) binary.
	writer nextPutImage: aForm.
	writer close.
! !

!ImageReadWriter class methodsFor: 'image reading/writing' stamp: 'jmv 12/14/2013 11:09'!
putForm: aForm onStream: aWriteStream
	"Store the given form on a file of the given name."

	| writer |
	writer _ self onBinaryStream: aWriteStream.
	writer nextPutImage: aForm.
	writer close! !

!ImageReadWriter class methodsFor: 'image reading/writing' stamp: 'jmv 12/14/2013 11:14'!
understandsImageFormat: aStream

	^(self onBinaryStream: aStream) understandsImageFormat! !


!BMPReadWriter methodsFor: 'reading' stamp: 'jmv 12/14/2013 10:48'!
nextImage
	| colors |
	self readHeader.
	biBitCount = 24 ifTrue:[^self read24BmpFile].
	"read the color map"
	colors := self readColorMap.
	^self readIndexedBmpFile: colors! !


!BMPReadWriter class methodsFor: 'testing' stamp: 'jmv 12/14/2013 10:49'!
displayAllFrom: fd
	"BMPReadWriter displayAllFrom: FileDirectory default"
	fd fileNames do:[:fName|
		(fName endsWith: '.bmp') ifTrue:[
			[(Form fromBinaryStream: (fd readOnlyFileNamed: fName) binary) display.
			Display forceDisplayUpdate] on: Error do:[:nix|].
		].
	].
	fd directoryNames do:[:fdName|
		self displayAllFrom: (fd directoryNamed: fdName)
	].! !

!BMPReadWriter class methodsFor: 'testing' stamp: 'jmv 12/14/2013 10:50'!
readAllFrom: fd
	"MessageTally spyOn:[BMPReadWriter readAllFrom: FileDirectory default]"
	fd fileNames do:[:fName|
		(fName endsWith: '.bmp') ifTrue:[
			[Form fromBinaryStream: (fd readOnlyFileNamed: fName) binary] on: Error do:[:nix].
		].
	].
	fd directoryNames do:[:fdName|
		self readAllFrom: (fd directoryNamed: fdName)
	].! !


!JPEGReadWriter2 class methodsFor: 'image reading/writing' stamp: 'jmv 12/14/2013 11:10'!
putForm: aForm quality: quality progressiveJPEG: progressiveFlag onFileNamed: fileName
	"Store the given Form as a JPEG file of the given name, overwriting any existing file of that name. Quality goes from 0 (low) to 100 (high), where -1 means default. If progressiveFlag is true, encode as a progressive JPEG."

	| writer |
	FileDirectory deleteFilePath: fileName.
	writer _ self onBinaryStream: (FileStream newFileNamed: fileName) binary.
	writer nextPutImage: aForm quality: quality progressiveJPEG: progressiveFlag.
	writer close.
! !


!DummyStream methodsFor: 'as yet unclassified' stamp: 'jmv 12/14/2013 10:56'!
useBytes
	"A DummyStream can't switch between bytes and characters, but it doesn't matter.
	After all, all contents is discarded"! !


!FileStream methodsFor: 'accessing' stamp: 'jmv 12/14/2013 11:02'!
contentsOfEntireFile
	"Read all of the contents of the receiver."

	| s |
	self readOnly.
	self reset.
	s _ self next: self size.
	self close.
	^s! !


!StandardFileStream methodsFor: 'open/close' stamp: 'jmv 12/14/2013 11:02'!
reopen
	"Close and reopen this file. The file position is reset to zero."
	"Details: Files that were open when a snapshot occurs are no longer valid when the snapshot is resumed. This operation re-opens the file if that has happened."

	| binary |
	binary _ self isBinary.
	fileID ifNotNil: [
		collection ifNotNil: [
			position < readLimit ifTrue: [
				self flushReadBuffer ] ].
		self primCloseNoError: fileID ].
	self open: name forWrite: rwmode.
	binary ifTrue: [self binary]
! !


!RWBinaryOrTextStream methodsFor: 'as yet unclassified' stamp: 'jmv 12/14/2013 11:00'!
reset
	"Set the receiver's position to the beginning of the sequence of objects."

	super reset.
	isBinary ifNil: [isBinary _ false].
	collection class == ByteArray ifTrue: ["Store as String and convert as needed."
		collection _ collection asString ]! !

!methodRemoval: ImageReadWriter #on:!
ImageReadWriter removeSelector: #on:!
!methodRemoval: ImageReadWriter #onBinaryStream:!
ImageReadWriter removeSelector: #onBinaryStream:!
