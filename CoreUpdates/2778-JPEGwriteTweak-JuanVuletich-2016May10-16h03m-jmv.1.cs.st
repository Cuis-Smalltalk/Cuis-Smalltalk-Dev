'From Cuis 4.2 of 25 July 2013 [latest update: #2763] on 10 May 2016 at 4:53:28.818159 pm'!

!JPEGReadWriter2 methodsFor: 'public access' stamp: 'jmv 5/10/2016 16:53'!
nextPutImage: aForm quality: quality progressiveJPEG: progressiveFlag
	"Encode the given Form on my stream with the given settings. Quality goes from 0 (low) to 100 (high), where -1 means default. If progressiveFlag is true, encode as a progressive JPEG."

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
		progressiveJPEG: progressiveFlag
		errorMgr: jpegErrorMgr2Struct.
	byteCount = 0 ifTrue: [self error: 'buffer too small for compressed data'].
	stream next: byteCount putAll: buffer startingAt: 1! !

