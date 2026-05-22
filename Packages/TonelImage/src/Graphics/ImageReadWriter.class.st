"
Copyright (c) Kazuki Yasumatsu, 1995. All rights reserved.

I am an abstract class to provide for encoding and/or decoding an image on a stream.

Instance Variables:
	stream		<ReadStream | WriteStream>	stream for image storages

Class Variables:
	ImageNotStoredSignal		<Signal>	image not stored error signal
	MagicNumberErrorSignal		<Signal>	magic number error signal

Subclasses must implement the following messages:
	accessing
		nextImage
		nextPutImage:
	testing
		canUnderstand         (added tao 10/26/97)
"
Class {
	#name : 'ImageReadWriter',
	#superclass : 'Object',
	#instVars : [
		'stream',
		'fileSize'
	],
	#category : 'Graphics-Files',
	#package : 'Graphics',
	#tag : 'Files'
}

{ #category : 'image reading/writing' }
ImageReadWriter class >> allTypicalFileExtensions [
	"Answer a collection of file extensions (lowercase) which files that my subclasses can read might commonly have"
	"ImageReadWriter allTypicalFileExtensions"
	| extensions |
	extensions := Set new.
	self allSubclassesDo: [ :cls | extensions addAll: cls typicalFileExtensions ].
	^extensions
]

{ #category : 'image reading/writing' }
ImageReadWriter class >> bytesFor: aForm [
	| estimate extent |
	extent := aForm extent.
	estimate := extent x * extent y // 2.	"It is likely it will be compressed..."
	^ByteArray streamContents: [ :strm |
			self
				putForm: aForm
				onStream: strm ]
			estimatedSize: estimate
]

{ #category : 'instance creation' }
ImageReadWriter class >> fileReaderServicesForFileEntry: filename suffix: suffix [
	"FileList buttons that read a TrueType font on contents."

	( self allTypicalFileExtensions includes: suffix) ifTrue: [
		^ { self serviceReadImage } ].

	^#()
]

{ #category : 'image reading/writing' }
ImageReadWriter class >> formFromFileEntry: aFileEntry [
	"Answer a Form stored on the file with the given name."
	
	^ aFileEntry readStreamDo: [ :stream |
		stream useBytes.
		self formFromStream: stream ]
]

{ #category : 'image reading/writing' }
ImageReadWriter class >> formFromFileNamed: fileName [
	"Answer a Form stored on the file with the given name."
	
	^fileName asFileEntry readStreamDo: [ :stream |
		stream useBytes.
		self formFromStream: stream ]
]

{ #category : 'image reading/writing' }
ImageReadWriter class >> formFromStream: aBinaryStream [
	"Answer a Form stored on the given stream."
	| reader readerClass form  featureName |

	readerClass := self withAllSubclasses
		detect: [ :subclass | subclass understandsImageFormat: aBinaryStream reset ]
		ifNone: [
			featureName := 'Graphics-Files-Additional'.
			^(FeatureRequirement name: featureName) isAlreadySatisfied
				ifTrue: [ self error: 'Unsupported image file format.' ]
				ifFalse: [
					self error: 'Unsupported image format. Try "', featureName, '".']].
	reader := readerClass onBinaryStream: aBinaryStream reset.
	form := reader nextImage.
	^ form
]

{ #category : 'instance creation' }
ImageReadWriter class >> imageMorphFromFileEntry: imageFileEntry [
	"Import an image from a file"
	
	(ImageMorph newWith: (ImageReadWriter formFromFileEntry: imageFileEntry) ) openInWorld.
	
]

{ #category : 'instance creation' }
ImageReadWriter class >> onBinaryStream: aStream [
	"Answer an instance of the receiver for encoding and/or decoding images on the given."

	^ self new binaryStream: aStream

]

{ #category : 'image reading/writing' }
ImageReadWriter class >> putForm: aForm onFile: fileEntry [
	"Store the given form on a file of the given name."

	fileEntry forceWriteStreamDo: [ :stream |
		(self onBinaryStream: stream binary) nextPutImage: aForm ]
]

{ #category : 'image reading/writing' }
ImageReadWriter class >> putForm: aForm onFileNamed: filename [
	"Store the given form on a file of the given name."

	self putForm: aForm onFile: filename asFileEntry
]

{ #category : 'image reading/writing' }
ImageReadWriter class >> putForm: aForm onStream: aWriteStream [
	"Store the given form on a file of the given name."

	(self onBinaryStream: aWriteStream)
		nextPutImage: aForm
]

{ #category : 'instance creation' }
ImageReadWriter class >> serviceReadImage [
	"Answer the service of importing an image"

	^ (SimpleServiceEntry
		provider: self 
		label: 'import as ImageMorph'
		selector: #imageMorphFromFileEntry:
		description: 'import image as ImageMorph'
		buttonLabel: 'import image'
		icon: #imageIcon
		) argumentGetter: [ :fileList | fileList selectedFileEntry ]
]

{ #category : 'subclass picking' }
ImageReadWriter class >> subclassFor: fileExtension [
	"
	ImageReadWriter subclassFor: 'bmp'
	"
	| fn |
	fn := fileExtension asLowercase.
	self allSubclassesDo: [ :each |
		(each typicalFileExtensions includes: fn) ifTrue: [
			^ each ]].
	^ nil
]

{ #category : 'image reading/writing' }
ImageReadWriter class >> typicalFileExtensions [
	"Answer a collection of file extensions (lowercase) which files that I can read might commonly have"
	^#()
]

{ #category : 'image reading/writing' }
ImageReadWriter class >> understandsImageFormat: aStream [

	^(self onBinaryStream: aStream) understandsImageFormat
]

{ #category : 'image reading/writing' }
ImageReadWriter class >> write: aForm onFileNamed: filename [

	(self subclassFor: (FileIOAccessor default extensionFor: filename))
		ifNotNil: [ :cls |
			cls putForm: aForm onFileNamed: filename ]
		ifNil: [
			self write: aForm onFileNamed: filename, '.bmp' ]
]

{ #category : 'stream access' }
ImageReadWriter >> atEnd [

	^stream atEnd
]

{ #category : 'private' }
ImageReadWriter >> binaryStream: aStream [
	stream := aStream.
	fileSize := stream size
]

{ #category : 'private' }
ImageReadWriter >> changePadOfBits: bits width: width height: height depth: depth from: oldPad [
to: newPad
	"Change padding size of bits."

	| srcRowByteSize dstRowByteSize newBits srcRowBase rowEndOffset |
	(#(8 16 32) includes: oldPad)
		ifFalse: [^self error: 'Invalid pad: ', oldPad printString].
	(#(8 16 32) includes: newPad)
		ifFalse: [^self error: 'Invalid pad: ', newPad printString].
	srcRowByteSize := width * depth + oldPad - 1 // oldPad * (oldPad / 8).
	srcRowByteSize * height = bits size
		ifFalse: [^self error: 'Incorrect bitmap array size.'].
	dstRowByteSize := width * depth + newPad - 1 // newPad * (newPad / 8).
	newBits := ByteArray new: dstRowByteSize * height.
	srcRowBase := 1.
	rowEndOffset := dstRowByteSize - 1.
	1 to: newBits size by: dstRowByteSize do:
		[:dstRowBase |
		newBits replaceFrom: dstRowBase
			to: dstRowBase + rowEndOffset
			with: bits
			startingAt: srcRowBase.
		srcRowBase := srcRowBase + srcRowByteSize].
	^newBits
]

{ #category : 'stream access' }
ImageReadWriter >> contents [

	^stream contents
]

{ #category : 'private' }
ImageReadWriter >> hasMagicNumber: aByteArray [
	| position |
	position := stream position.
	((stream size - position) >= aByteArray size and:
	[(stream next: aByteArray size)  = aByteArray])
		ifTrue: [^true].
	stream position: position.
	^false
]

{ #category : 'stream access' }
ImageReadWriter >> next [

	^stream next
]

{ #category : 'stream access' }
ImageReadWriter >> next: size [

	^stream next: size
]

{ #category : 'accessing' }
ImageReadWriter >> nextImage [
	"Dencoding an image on stream and answer the image."

	^self subclassResponsibility
]

{ #category : 'stream access' }
ImageReadWriter >> nextPut: aByte [

	^stream nextPut: aByte
]

{ #category : 'stream access' }
ImageReadWriter >> nextPutAll: aByteArray [

	^stream nextPutAll: aByteArray
]

{ #category : 'accessing' }
ImageReadWriter >> nextPutImage: anImage [
	"Encoding anImage on stream."

	^self subclassResponsibility
]

{ #category : 'stream access' }
ImageReadWriter >> nextUint32 [
	"Read a 32-bit unsigned quantity from the stream.
	Big Endian"

	^ stream nextUint32BigEndian: true
]

{ #category : 'stream access' }
ImageReadWriter >> nextUint32Put: aNumber [
	"Write out a 32-bit integer as 32 bits.
	Big Endian"

	stream nextUint32Put: aNumber bigEndian: true.
	^aNumber
]

{ #category : 'stream access' }
ImageReadWriter >> peekFor: aValue [

	^stream peekFor: aValue
]

{ #category : 'stream access' }
ImageReadWriter >> position [

	^stream position
]

{ #category : 'stream access' }
ImageReadWriter >> position: anInteger [

	^stream position: anInteger
]

{ #category : 'stream access' }
ImageReadWriter >> size [

	^stream size
]

{ #category : 'stream access' }
ImageReadWriter >> skip: anInteger [

	^stream skip: anInteger
]

{ #category : 'testing' }
ImageReadWriter >> understandsImageFormat [
	"Test to see if the image stream format is understood by this decoder.
	This should be implemented in each subclass of ImageReadWriter so that
	a proper decoder can be selected without ImageReadWriter having to know
	about all possible image file types."

	^ false
]
