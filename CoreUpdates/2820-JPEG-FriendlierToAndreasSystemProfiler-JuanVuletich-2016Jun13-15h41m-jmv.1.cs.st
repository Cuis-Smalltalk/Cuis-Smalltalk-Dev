'From Cuis 4.2 of 25 July 2013 [latest update: #2818] on 13 June 2016 at 3:58:26.402463 pm'!

!JPEGReadWriter2 methodsFor: 'primitives' stamp: 'jmv 6/13/2016 15:52'!
jpegReadImage: aJPEGDecompressStruct fromByteArray: source onForm: form doDithering: ditherFlag errorMgr: aJPEGErrorMgr2Struct

	^ self primJPEGReadImage: aJPEGDecompressStruct fromByteArray: source onForm: form doDithering: ditherFlag errorMgr: aJPEGErrorMgr2Struct! !

!JPEGReadWriter2 methodsFor: 'primitives' stamp: 'jmv 6/13/2016 15:45'!
jpegWriteImage: aJPEGCompressStruct onByteArray: destination form: form quality: quality progressiveJPEG: progressiveFlag errorMgr: aJPEGErrorMgr2Struct

	^ self primJPEGWriteImage: aJPEGCompressStruct onByteArray: destination form: form quality: quality progressiveJPEG: progressiveFlag errorMgr: aJPEGErrorMgr2Struct! !

!JPEGReadWriter2 methodsFor: 'aux' stamp: 'jmv 6/13/2016 15:50'!
resultFormWidth: width height: height components: components
	^components
			ifNil: [ Form extent: width@height depth: 32]		"Original version of the JPEG plugin"
			ifNotNil: [ 												"Enhanced version of the JPEG plugin as of 6/2016"
				components = 3
					ifTrue: [ Form extent: width@height nativeDepthFrom: 32 ]
					ifFalse: [ GrayForm extent: width@height ]]! !


!JPEGReadWriter2 methodsFor: 'public access' stamp: 'jmv 6/13/2016 15:48'!
compress: aForm quality: quality progressiveJPEG: progressiveFlag into: aBlock
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
	buffer _ ByteArray new: sourceForm width * sourceForm height // 2.
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

!JPEGReadWriter2 methodsFor: 'public access' stamp: 'jmv 6/13/2016 15:53'!
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
	
	form _ aFormOrNil ifNil: [ self resultFormWidth: width height: height components: components ].

	(width = 0 or: [height = 0]) ifTrue: [^ form].
	self
		jpegReadImage: jpegDecompressStruct
		fromByteArray: aByteArray
		onForm: form
		doDithering: true
		errorMgr: jpegErrorMgr2Struct.
	
	^ form! !


!JPEGReadWriter2 reorganize!
('public access' compress:quality:progressiveJPEG: compress:quality:progressiveJPEG:into: imageExtent: nextImage nextPutImage: nextPutImage:quality:progressiveJPEG: uncompress:into:)
('testing' isPluginPresent supportsGrayForms understandsImageFormat)
('primitives' jpegReadImage:fromByteArray:onForm:doDithering:errorMgr: jpegWriteImage:onByteArray:form:quality:progressiveJPEG:errorMgr: primImageHeight: primImageNumComponents: primImageWidth: primJPEGCompressStructSize primJPEGDecompressStructSize primJPEGErrorMgr2StructSize primJPEGPluginIsPresent primJPEGReadHeader:fromByteArray:errorMgr: primJPEGReadImage:fromByteArray:onForm:doDithering:errorMgr: primJPEGWriteImage:onByteArray:form:quality:progressiveJPEG:errorMgr:)
('aux' resultFormWidth:height:components:)
!

