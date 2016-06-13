'From Cuis 4.2 of 25 July 2013 [latest update: #2818] on 13 June 2016 at 3:41:24.160391 pm'!

!JPEGReadWriter2 methodsFor: 'public access' stamp: 'jmv 6/13/2016 15:39'!
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
		byteCount _ self primJPEGWriteImage: jpegCompressStruct 
			onByteArray: buffer
			form: sourceForm
			quality: quality
			progressiveJPEG: progressiveFlag
			errorMgr: jpegErrorMgr2Struct.
		byteCount = 0 ] whileTrue: [
			"But if not, ask for some more"
			buffer _ ByteArray new: buffer size * 14 // 10 ].
	
	aBlock value: buffer value: byteCount! !

!JPEGReadWriter2 methodsFor: 'public access' stamp: 'jmv 6/13/2016 12:10'!
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

	| bytes |
"	bytes _ stream upToEnd."
	bytes _ stream contents.
	^self uncompress: bytes into: nil! !

