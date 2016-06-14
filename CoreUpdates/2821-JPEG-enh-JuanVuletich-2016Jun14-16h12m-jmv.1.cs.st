'From Cuis 4.2 of 25 July 2013 [latest update: #2820] on 14 June 2016 at 4:22:34.976491 pm'!

!JPEGReadWriter2 methodsFor: 'public access' stamp: 'jmv 6/14/2016 15:23'!
compress: aForm quality: quality progressiveJPEG: progressiveFlag usingBuffer: aByteArrayOrNil into: aBlock
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
			sourceForm _ (aForm depth = 32) |  (aForm depth = 16) | (aForm is: #GrayForm)
				ifTrue: [aForm]
				ifFalse: [aForm asFormOfDepth: 16]]
		ifFalse: [
			"Original plugin supports 32bpp and even width big endian 16bpp"
			sourceForm _ (aForm depth = 32) | (aForm width even & (aForm nativeDepth = 16))
				ifTrue: [aForm]
				ifFalse: [aForm asFormOfDepth: 32]].

	jpegCompressStruct _ ByteArray new: self primJPEGCompressStructSize.
	jpegErrorMgr2Struct _ ByteArray new: self primJPEGErrorMgr2StructSize.
	"Most likely more than needed"
	buffer _ aByteArrayOrNil ifNil: [ByteArray new: sourceForm width * sourceForm height // 2].
	[
		byteCount _ self jpegWriteImage: jpegCompressStruct 
			onByteArray: buffer
			form: sourceForm
			quality: quality
			progressiveJPEG: progressiveFlag
			errorMgr: jpegErrorMgr2Struct.
		byteCount = 0 ] whileTrue: [
			"But if not, ask for some more"
			buffer _ ByteArray new: buffer size * 14 // 10 ].
	
	aBlock value: buffer value: byteCount! !

!JPEGReadWriter2 methodsFor: 'aux' stamp: 'jmv 6/14/2016 16:20'!
resultFormWidth: width height: height components: components orReuse: aFormOrNil

	aFormOrNil ifNotNil: [
		(aFormOrNil class == (components = 1 ifTrue: [GrayForm] ifFalse: [Form])) ifTrue: [
			aFormOrNil extent = (width@height) ifTrue: [
				^ aFormOrNil ]]].

	^components
			ifNil: [ Form extent: width@height depth: 32]		"Original version of the JPEG plugin"
			ifNotNil: [ 												"Enhanced version of the JPEG plugin as of 6/2016"
				components = 3
					ifTrue: [ Form extent: width@height nativeDepthFrom: 32 ]
					ifFalse: [ GrayForm extent: width@height ]]! !


!JPEGReadWriter2 methodsFor: 'public access' stamp: 'jmv 6/14/2016 15:51'!
compress: aForm quality: quality progressiveJPEG: progressiveFlag
	"Encode the given Form and answer the compressed ByteArray.
	Quality goes from 0 (low) to 100 (high), where -1 means default.
	Usually progressiveFlag is false"
	"Creates buffer twice. Normally you would use a better optimization."
	
	self compress: aForm quality: quality progressiveJPEG: progressiveFlag usingBuffer: nil into: [ :buffer :byteCount |
		^ buffer copyFrom: 1 to: byteCount ]! !

!JPEGReadWriter2 methodsFor: 'public access' stamp: 'jmv 6/14/2016 15:23'!
nextPutImage: aForm quality: quality progressiveJPEG: progressiveFlag
	"Encode the given Form on my stream with the given settings. Quality goes from 0 (low) to 100 (high), where -1 means default. If progressiveFlag is true, encode as a progressive JPEG.
	We can only compress:
		* 32-bit deep Forms 
		* -32-bit deep Forms
		* 16-bit deep Forms
		* -16-bit deep Forms
		* 8-bit deep GrayForms
		* -8-bit deep GrayForms"

	self compress: aForm quality: quality progressiveJPEG: progressiveFlag usingBuffer: nil into: [ :buffer :byteCount |
		stream next: byteCount putAll: buffer startingAt: 1 ]! !

!JPEGReadWriter2 methodsFor: 'public access' stamp: 'jmv 6/14/2016 16:22'!
uncompress: aByteArray into: aFormOrNil
	"Uncompress an image from the given ByteArray.
	If we have a post 6/2016 version of the plugin, we can read RGB JPEGs into:
		* 32-bit Forms
		* -32-bit Forms
		* 16-bit Forms (with or without dithering!!)
		* -16-bit Forms (with or without dithering!!)
	We can read grayscale JPEGs into:
		* 32-bit Forms
		* -32-bit Forms
		* 16-bit Forms (with or without dithering!!)
		* -16-bit Forms (with or without dithering!!)
		* 8-bit GrayForms
		* -8-bit GrayForms
	
	aFormOrNil might be a Form (16 or 32 bpp) for 3 RGB JPEG or a GrayForm for Gray JPEG. 
	If nil, an appropriate instance is created.
	Optional argument aFormOrNil might be useful to save memory or avoid allocation.
	"

	| width height components form jpegDecompressStruct jpegErrorMgr2Struct |
	
	jpegDecompressStruct _ ByteArray new: self primJPEGDecompressStructSize.
	jpegErrorMgr2Struct _ ByteArray new: self primJPEGErrorMgr2StructSize.
	self 
		primJPEGReadHeader: jpegDecompressStruct 
		fromByteArray: aByteArray
		errorMgr: jpegErrorMgr2Struct.
	
	width _ self primImageWidth: jpegDecompressStruct.
	height _ self primImageHeight: jpegDecompressStruct.
	components _ self primImageNumComponents: jpegDecompressStruct.
	form _ self resultFormWidth: width height: height components: components orReuse: aFormOrNil.

	(width = 0 or: [height = 0]) ifTrue: [^ form].
	self
		jpegReadImage: jpegDecompressStruct
		fromByteArray: aByteArray
		onForm: form
		doDithering: true
		errorMgr: jpegErrorMgr2Struct.
	
	^ form! !

!methodRemoval: JPEGReadWriter2 #compress:quality:progressiveJPEG:into:!
JPEGReadWriter2 removeSelector: #compress:quality:progressiveJPEG:into:!
!methodRemoval: JPEGReadWriter2 #resultFormWidth:height:components:!
JPEGReadWriter2 removeSelector: #resultFormWidth:height:components:!
