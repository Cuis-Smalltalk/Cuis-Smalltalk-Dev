'From Cuis 4.2 of 25 July 2013 [latest update: #2786] on 2 June 2016 at 7:53:06.522866 pm'!

!JPEGReadWriter2 methodsFor: 'testing' stamp: 'lpc 6/2/2016 19:49'!
supportsGrayForms

	| jpegDecompressStruct |
	jpegDecompressStruct _ ByteArray new: self primJPEGDecompressStructSize.
	^(self primImageNumComponents: jpegDecompressStruct) notNil! !

!JPEGReadWriter2 methodsFor: 'primitives' stamp: 'lpc 6/2/2016 19:49'!
primImageNumComponents: aJPEGDecompressStruct
	"If primitive not present, answer nil."
	<primitive: 'primImageNumComponents' module: 'JPEGReadWriter2Plugin'>
	^nil! !


!JPEGReadWriter2 methodsFor: 'public access' stamp: 'lpc 6/2/2016 19:49'!
compress: aForm quality: quality
	"Encode the given Form and answer the compressed ByteArray. Quality goes from 0 (low) to 100 (high), where -1 means default."
	"
	como
	nextPutImage: aForm quality: quality progressiveJPEG: progressiveFlag
	"

	| sourceForm jpegCompressStruct jpegErrorMgr2Struct buffer byteCount |
	"odd width images of depth 16 give problems; avoid them."
	sourceForm _ (aForm depth = 32) | (aForm width even & (aForm depth = 16))
		ifTrue: [aForm]
		ifFalse: [aForm asFormOfDepth: 32].
	jpegCompressStruct _ ByteArray new: self primJPEGCompressStructSize.
	jpegErrorMgr2Struct _ ByteArray new: self primJPEGErrorMgr2StructSize.
	buffer _ ByteArray new: sourceForm width * sourceForm height + 1024.
	byteCount _ self primJPEGWriteImage: jpegCompressStruct 
		onByteArray: buffer
		form: sourceForm
		quality: quality
		progressiveJPEG: false
		errorMgr: jpegErrorMgr2Struct.
	byteCount = 0 ifTrue: [self error: 'buffer too small for compressed data'].
	^ buffer copyFrom: 1 to: byteCount
! !

!JPEGReadWriter2 methodsFor: 'public access' stamp: 'lpc 6/2/2016 19:49'!
nextImage
	"Decode and answer a Form from my stream. 
	We can read RGB JPEGs into:
		* 32-bit Forms
		* -32-bit Forms
		*16-bit Forms (with or without dithering!!)
		* -16-bit Forms (with or without dithering!!)
	We can read grayscale JPEGs into:
		* 32-bit Forms
		* -32-bit Forms
		*16-bit Forms (with or without dithering!!)
		* -16-bit Forms (with or without dithering!!)
		* 8-bit GrayForms
		* -8-bit GrayForms"

	| bytes width height components form jpegDecompressStruct jpegErrorMgr2Struct |
	
	bytes _ stream upToEnd.
	
	jpegDecompressStruct _ ByteArray new: self primJPEGDecompressStructSize.
	jpegErrorMgr2Struct _ ByteArray new: self primJPEGErrorMgr2StructSize.
	self 
		primJPEGReadHeader: jpegDecompressStruct 
		fromByteArray: bytes
		errorMgr: jpegErrorMgr2Struct.
	
	width _ self primImageWidth: jpegDecompressStruct.
	height _ self primImageHeight: jpegDecompressStruct.
	components _ self primImageNumComponents: jpegDecompressStruct.
	
	form _ components
		ifNil: [ Form extent: width@height depth: 32]	"Original version of the JPEG plugin"
		ifNotNil: [ components = 3
			ifTrue: [ Form extent: width@height nativeDepthFrom: 32 ]
			ifFalse: [ GrayForm extent: width@height ]].

	(width = 0 or: [height = 0]) ifTrue: [^ form].
	self
		primJPEGReadImage: jpegDecompressStruct
		fromByteArray: bytes
		onForm: form
		doDithering: true
		errorMgr: jpegErrorMgr2Struct.
	
	^ form! !

!JPEGReadWriter2 methodsFor: 'public access' stamp: 'lpc 6/2/2016 19:49'!
nextPutImage: aForm quality: quality progressiveJPEG: progressiveFlag
	"Encode the given Form on my stream with the given settings. Quality goes from 0 (low) to 100 (high), where -1 means default. If progressiveFlag is true, encode as a progressive JPEG.
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
	buffer _ ByteArray new: sourceForm width * sourceForm height + 1024.
	byteCount _ self primJPEGWriteImage: jpegCompressStruct 
		onByteArray: buffer
		form: sourceForm
		quality: quality
		progressiveJPEG: progressiveFlag
		errorMgr: jpegErrorMgr2Struct.
	byteCount = 0 ifTrue: [self error: 'buffer too small for compressed data'].
	stream next: byteCount putAll: buffer startingAt: 1! !

!JPEGReadWriter2 methodsFor: 'public access' stamp: 'lpc 6/2/2016 19:49'!
uncompress: aByteArray into: aForm
	"Uncompress an image from the given ByteArray into the given Form.
	Fails if the given Form has the wrong dimensions or depth.
	If aForm has depth 16, do ordered dithering."

	^ self uncompress: aByteArray into: aForm doDithering: true! !

!JPEGReadWriter2 methodsFor: 'public access' stamp: 'lpc 6/2/2016 19:49'!
uncompress: aByteArray into: aForm doDithering: ditherFlag
	"Uncompress an image from the given ByteArray into the given Form. 
	Fails if the given Form has the wrong dimensions or depth.
	If aForm has depth 16 and ditherFlag = true, do ordered dithering."
	"
	como nextImage
	"

	| jpegDecompressStruct jpegErrorMgr2Struct w h |
	jpegDecompressStruct _ ByteArray new: self primJPEGDecompressStructSize.
	jpegErrorMgr2Struct _ ByteArray new: self primJPEGErrorMgr2StructSize.
	self 
		primJPEGReadHeader: jpegDecompressStruct 
		fromByteArray: aByteArray
		errorMgr: jpegErrorMgr2Struct.
	w _ self primImageWidth: jpegDecompressStruct.
	h _ self primImageHeight: jpegDecompressStruct.
	((aForm width = w) & (aForm height = h)) ifFalse: [
		^ self error: 'form dimensions do not match'].

	"odd width images of depth 16 give problems; avoid them"
	w odd
		ifTrue: [
			aForm depth = 32 ifFalse: [^ self error: 'must use depth 32 with odd width']]
		ifFalse: [
			((aForm depth = 16) | (aForm depth = 32)) ifFalse: [^ self error: 'must use depth 16 or 32']].

	self primJPEGReadImage: jpegDecompressStruct
		fromByteArray: aByteArray
		onForm: aForm
		doDithering: ditherFlag
		errorMgr: jpegErrorMgr2Struct.
! !

!methodRemoval: JPEGReadWriter2 #nextImageSuggestedDepth:!
JPEGReadWriter2 removeSelector: #nextImageSuggestedDepth:!
