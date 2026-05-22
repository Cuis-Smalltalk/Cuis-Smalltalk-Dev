"
I provide fast JPEG compression and decompression. I require the VM pluginJPEGReadWriter2Plugin, which is typically stored in same directory as the Squeak virtual machine.

JPEGReadWriter2Plugin is based on LIBJPEG library. This sentence applies to the plugin:
   "This software is based in part on the work of the Independent JPEG Group".

The LIBJPEG license allows it to be used free for any purpose so long as its origin and copyright are acknowledged. You can read more about LIBJPEG and get the complete source code at www.ijg.org.

"
Class {
	#name : 'JPEGReadWriter2',
	#superclass : 'ImageReadWriter',
	#category : 'Graphics-Files',
	#package : 'Graphics',
	#tag : 'Files'
}

{ #category : 'image reading/writing' }
JPEGReadWriter2 class >> primJPEGPluginIsPresent [
	<primitive: 'primJPEGPluginIsPresent' module: 'JPEGReadWriter2Plugin'>
	^false
]

{ #category : 'image reading/writing' }
JPEGReadWriter2 class >> putForm: aForm quality: quality progressiveJPEG: progressiveFlag onFileNamed: fileName [
	"Store the given Form as a JPEG file of the given name, overwriting any existing file of that name. Quality goes from 0 (low) to 100 (high), where -1 means default. If progressiveFlag is true, encode as a progressive JPEG."

	fileName asFileEntry forceWriteStreamDo: [ :stream |
		(self onBinaryStream: stream binary)
			nextPutImage: aForm quality: quality progressiveJPEG: progressiveFlag ]
]

{ #category : 'image reading/writing' }
JPEGReadWriter2 class >> typicalFileExtensions [
	"Answer a collection of file extensions (lowercase) which files that I can read might commonly have"
	^#('jpg' 'jpeg')
]

{ #category : 'public access' }
JPEGReadWriter2 >> compress: aForm quality: quality progressiveJPEG: progressiveFlag [
	"Encode the given Form and answer the compressed ByteArray.
	Quality goes from 0 (low) to 100 (high), where -1 means default.
	Usually progressiveFlag is false"
	"Creates buffer twice. Normally you would use a better optimization."
	
	self compress: aForm quality: quality progressiveJPEG: progressiveFlag usingBuffer: nil into: [ :buffer :byteCount |
		^ buffer copyFrom: 1 to: byteCount ]
]

{ #category : 'public access' }
JPEGReadWriter2 >> compress: aForm quality: quality progressiveJPEG: progressiveFlag usingBuffer: aByteArrayOrNil into: aBlock [
	"Encode the given Form with the given settings. Quality goes from 0 (low) to 100 (high), where -1 means default. If progressiveFlag is true, encode as a progressive JPEG.
	
	Evaluate aBlock with two arguments. The first is a ByteArray with the data. Usually bigger than needed.
	The second argument is the actual maningful bytes.
	
	We can only compress:
		* 32-bit deep Forms 
		* -32-bit deep Forms
		* 16-bit deep Forms
		* -16-bit deep Forms
		* 8-bit deep GrayForms
		* -8-bit deep GrayForms"

	| sourceForm jpegCompressStruct jpegErrorMgr2Struct buffer byteCount |
	self supportsGrayForms
		ifTrue: [
			"Newer plugin supports 32bpp, 16bpp, GrayForms"
			sourceForm := (aForm depth = 32) |  (aForm depth = 16) | (aForm is: #GrayForm)
				ifTrue: [aForm]
				ifFalse: [aForm asFormOfDepth: 16]]
		ifFalse: [
			"Original plugin supports 32bpp and even width big endian 16bpp"
			sourceForm := (aForm depth = 32) | (aForm width even & (aForm nativeDepth = 16))
				ifTrue: [aForm]
				ifFalse: [aForm asFormOfDepth: 32]].

	jpegCompressStruct := ByteArray new: self primJPEGCompressStructSize.
	jpegErrorMgr2Struct := ByteArray new: self primJPEGErrorMgr2StructSize.
	"Most likely more than needed"
	buffer := aByteArrayOrNil ifNil: [ByteArray new: sourceForm width * sourceForm height // 2+1024].
	[
		byteCount := self jpegWriteImage: jpegCompressStruct 
			onByteArray: buffer
			form: sourceForm
			quality: quality
			progressiveJPEG: progressiveFlag
			errorMgr: jpegErrorMgr2Struct.
		byteCount = 0 ] whileTrue: [
			"But if not, ask for some more"
			buffer := ByteArray new: buffer size * 14 // 10 ].
	
	aBlock value: buffer value: byteCount
]

{ #category : 'public access' }
JPEGReadWriter2 >> imageExtent: aByteArray  [
	"Answer the extent of the compressed image encoded in the given ByteArray."

	| jpegDecompressStruct jpegErrorMgr2Struct w h |
	jpegDecompressStruct := ByteArray new: self primJPEGDecompressStructSize.
	jpegErrorMgr2Struct := ByteArray new: self primJPEGErrorMgr2StructSize.
	self
		primJPEGReadHeader: jpegDecompressStruct 
		fromByteArray: aByteArray
		errorMgr: jpegErrorMgr2Struct.
	w := self primImageWidth: jpegDecompressStruct.
	h := self primImageHeight: jpegDecompressStruct.
	^ w @ h

]

{ #category : 'testing' }
JPEGReadWriter2 >> isPluginPresent [
	^self primJPEGPluginIsPresent
]

{ #category : 'primitives' }
JPEGReadWriter2 >> jpegReadImage: aJPEGDecompressStruct fromByteArray: source onForm: form doDithering: ditherFlag errorMgr: aJPEGErrorMgr2Struct [

	^ self primJPEGReadImage: aJPEGDecompressStruct fromByteArray: source onForm: form doDithering: ditherFlag errorMgr: aJPEGErrorMgr2Struct
]

{ #category : 'primitives' }
JPEGReadWriter2 >> jpegWriteImage: aJPEGCompressStruct onByteArray: destination form: form quality: quality progressiveJPEG: progressiveFlag errorMgr: aJPEGErrorMgr2Struct [

	^ self primJPEGWriteImage: aJPEGCompressStruct onByteArray: destination form: form quality: quality progressiveJPEG: progressiveFlag errorMgr: aJPEGErrorMgr2Struct
]

{ #category : 'public access' }
JPEGReadWriter2 >> nextImage [
	"Decode and answer a Form from my stream. 
	We can read RGB JPEGs into:
		* 32-bit Forms
		* -32-bit Forms
		*16-bit Forms (with or without dithering!)
		* -16-bit Forms (with or without dithering!)
	We can read grayscale JPEGs into:
		* 32-bit Forms
		* -32-bit Forms
		*16-bit Forms (with or without dithering!)
		* -16-bit Forms (with or without dithering!)
		* 8-bit GrayForms
		* -8-bit GrayForms"

	| bytes |
"	bytes := stream upToEnd."
	bytes := stream contents.
	^self uncompress: bytes into: nil
]

{ #category : 'public access' }
JPEGReadWriter2 >> nextPutImage: aForm [
	"Encode the given Form on my stream with default quality."

	^ self nextPutImage: aForm quality: -1 progressiveJPEG: false

]

{ #category : 'public access' }
JPEGReadWriter2 >> nextPutImage: aForm quality: quality progressiveJPEG: progressiveFlag [
	"Encode the given Form on my stream with the given settings. Quality goes from 0 (low) to 100 (high), where -1 means default. If progressiveFlag is true, encode as a progressive JPEG.
	We can only compress:
		* 32-bit deep Forms 
		* -32-bit deep Forms
		* 16-bit deep Forms
		* -16-bit deep Forms
		* 8-bit deep GrayForms
		* -8-bit deep GrayForms"

	self compress: aForm quality: quality progressiveJPEG: progressiveFlag usingBuffer: nil into: [ :buffer :byteCount |
		stream next: byteCount putAll: buffer startingAt: 1 ]
]

{ #category : 'primitives' }
JPEGReadWriter2 >> primImageHeight: aJPEGCompressStruct [

	<primitive: 'primImageHeight' module: 'JPEGReadWriter2Plugin'>
	self primitiveFailed

]

{ #category : 'primitives' }
JPEGReadWriter2 >> primImageNumComponents: aJPEGDecompressStruct [
	"If primitive not present, answer nil."
	<primitive: 'primImageNumComponents' module: 'JPEGReadWriter2Plugin'>
	^nil
]

{ #category : 'primitives' }
JPEGReadWriter2 >> primImageWidth: aJPEGCompressStruct [

	<primitive: 'primImageWidth' module: 'JPEGReadWriter2Plugin'>
	self primitiveFailed

]

{ #category : 'primitives' }
JPEGReadWriter2 >> primJPEGCompressStructSize [

	<primitive: 'primJPEGCompressStructSize' module: 'JPEGReadWriter2Plugin'>
	self primitiveFailed

]

{ #category : 'primitives' }
JPEGReadWriter2 >> primJPEGDecompressStructSize [

	<primitive: 'primJPEGDecompressStructSize' module: 'JPEGReadWriter2Plugin'>
	self primitiveFailed

]

{ #category : 'primitives' }
JPEGReadWriter2 >> primJPEGErrorMgr2StructSize [

	<primitive: 'primJPEGErrorMgr2StructSize' module: 'JPEGReadWriter2Plugin'>
	self primitiveFailed

]

{ #category : 'primitives' }
JPEGReadWriter2 >> primJPEGPluginIsPresent [
	<primitive: 'primJPEGPluginIsPresent' module: 'JPEGReadWriter2Plugin'>
	^false
]

{ #category : 'primitives' }
JPEGReadWriter2 >> primJPEGReadHeader: aJPEGDecompressStruct fromByteArray: source errorMgr: aJPEGErrorMgr2Struct [

	<primitive: 'primJPEGReadHeaderfromByteArrayerrorMgr' module: 'JPEGReadWriter2Plugin'>
	self primitiveFailed

]

{ #category : 'primitives' }
JPEGReadWriter2 >> primJPEGReadImage: aJPEGDecompressStruct fromByteArray: source onForm: form doDithering: ditherFlag errorMgr: aJPEGErrorMgr2Struct [

	<primitive: 'primJPEGReadImagefromByteArrayonFormdoDitheringerrorMgr' module: 'JPEGReadWriter2Plugin'>
	self primitiveFailed

]

{ #category : 'primitives' }
JPEGReadWriter2 >> primJPEGWriteImage: aJPEGCompressStruct onByteArray: destination form: form quality: quality progressiveJPEG: progressiveFlag errorMgr: aJPEGErrorMgr2Struct [

	<primitive: 'primJPEGWriteImageonByteArrayformqualityprogressiveJPEGerrorMgr' module: 'JPEGReadWriter2Plugin'>
	self primitiveFailed

]

{ #category : 'aux' }
JPEGReadWriter2 >> resultFormWidth: width height: height components: components orReuse: aFormOrNil [

	aFormOrNil ifNotNil: [
		(aFormOrNil class == (components = 1 ifTrue: [GrayForm] ifFalse: [Form])) ifTrue: [
			aFormOrNil extent = (width@height) ifTrue: [
				^ aFormOrNil ]]].

	^components
			ifNil: [ Form extent: width@height depth: 32]		"Original version of the JPEG plugin"
			ifNotNil: [ 												"Enhanced version of the JPEG plugin as of 6/2016"
				components = 3
					ifTrue: [ Form extent: width@height nativeDepthFrom: 32 ]
					ifFalse: [ GrayForm extent: width@height ]]
]

{ #category : 'testing' }
JPEGReadWriter2 >> supportsGrayForms [
	<primitive: 'primSupports8BitGrayscaleJPEGs' module: 'JPEGReadWriter2Plugin'>
	^false
]

{ #category : 'public access' }
JPEGReadWriter2 >> uncompress: aByteArray into: aFormOrNil [
	"Uncompress an image from the given ByteArray.
	If we have a post 6/2016 version of the plugin, we can read RGB JPEGs into:
		* 32-bit Forms
		* -32-bit Forms
		* 16-bit Forms (with or without dithering!)
		* -16-bit Forms (with or without dithering!)
	We can read grayscale JPEGs into:
		* 32-bit Forms
		* -32-bit Forms
		* 16-bit Forms (with or without dithering!)
		* -16-bit Forms (with or without dithering!)
		* 8-bit GrayForms
		* -8-bit GrayForms
	
	aFormOrNil might be a Form (16 or 32 bpp) for 3 RGB JPEG or a GrayForm for Gray JPEG. 
	If nil, an appropriate instance is created.
	Optional argument aFormOrNil might be useful to save memory or avoid allocation.
	"

	| width height components form jpegDecompressStruct jpegErrorMgr2Struct |
	
	jpegDecompressStruct := ByteArray new: self primJPEGDecompressStructSize.
	jpegErrorMgr2Struct := ByteArray new: self primJPEGErrorMgr2StructSize.
	self 
		primJPEGReadHeader: jpegDecompressStruct 
		fromByteArray: aByteArray
		errorMgr: jpegErrorMgr2Struct.
	
	width := self primImageWidth: jpegDecompressStruct.
	height := self primImageHeight: jpegDecompressStruct.
	components := self primImageNumComponents: jpegDecompressStruct.
	form := self resultFormWidth: width height: height components: components orReuse: aFormOrNil.

	(width = 0 or: [height = 0]) ifTrue: [^ form].
	self
		jpegReadImage: jpegDecompressStruct
		fromByteArray: aByteArray
		onForm: form
		doDithering: true
		errorMgr: jpegErrorMgr2Struct.
	
	^ form
]

{ #category : 'testing' }
JPEGReadWriter2 >> understandsImageFormat [
	"Answer true if the image stream format is understood by this decoder."
	self isPluginPresent ifFalse:[^false]. "cannot read it otherwise"
	self next = 16rFF ifFalse: [^ false].
	self next = 16rD8 ifFalse: [^ false].
	^ true

]
