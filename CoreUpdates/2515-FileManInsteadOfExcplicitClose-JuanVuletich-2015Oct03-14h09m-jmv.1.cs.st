'From Cuis 4.2 of 25 July 2013 [latest update: #2513] on 3 October 2015 at 2:20:51.569905 pm'!

!ImageReadWriter class methodsFor: 'image reading/writing' stamp: 'jmv 10/3/2015 14:11'!
formFromFileNamed: fileName
	"Answer a ColorForm stored on the file with the given name."
	
	^fileName asFileEntry readStream: [ :stream |
		stream useBytes.
		self formFromStream: stream ]! !

!ImageReadWriter class methodsFor: 'image reading/writing' stamp: 'jmv 10/3/2015 14:12'!
formFromStream: aBinaryStream
	"Answer a ColorForm stored on the given stream.  closes the stream"
	| reader readerClass form  featureName |

	readerClass _ self withAllSubclasses
		detect: [ :subclass | subclass understandsImageFormat: aBinaryStream reset ]
		ifNone: [
			featureName _ 'Graphics-Files-Additional'.
			^(FeatureRequirement name: featureName) isAlreadySatisfied
				ifTrue: [ self error: 'Unsupported image file format.' ]
				ifFalse: [
					self error: 'Unsupported image format. Try "', featureName, '".']].
	reader _ readerClass onBinaryStream: aBinaryStream reset.
	form _ reader nextImage.
	^ form! !

!ImageReadWriter class methodsFor: 'image reading/writing' stamp: 'jmv 10/3/2015 14:13'!
putForm: aForm onStream: aWriteStream
	"Store the given form on a file of the given name."

	(self onBinaryStream: aWriteStream)
		nextPutImage: aForm! !


!BMPReadWriter methodsFor: 'writing' stamp: 'jmv 10/3/2015 14:13'!
nextPutImage: aForm
	| bhSize rowBytes rgb data colorValues depth image ppw scanLineLen pixline |
	depth := aForm depth.
	depth := #(1 4 8 32 ) detect: [ :each | each >= depth].
	image := aForm asFormOfDepth: depth.
	bhSize := 14.  "# bytes in file header"
	biSize := 40.  "info header size in bytes"
	biWidth := image width.
	biHeight := image height.
	biClrUsed := depth = 32 ifTrue: [0] ifFalse:[1 << depth].  "No. color table entries"
	bfOffBits := biSize + bhSize + (4*biClrUsed).
	rowBytes := ((depth min: 24) * biWidth + 31 // 32) * 4.
	biSizeImage := biHeight * rowBytes.

	"Write the file header"
	stream position: 0.
	stream nextLittleEndianNumber: 2 put: 19778.  "bfType = BM"
	stream nextLittleEndianNumber: 4 put: bfOffBits + biSizeImage.  "Entire file size in bytes"
	stream nextLittleEndianNumber: 4 put: 0.  "bfReserved"
	stream nextLittleEndianNumber: 4 put: bfOffBits.  "Offset of bitmap data from start of hdr (and file)"

	"Write the bitmap info header"
	stream position: bhSize.
	stream nextLittleEndianNumber: 4 put: biSize.  "info header size in bytes"
	stream nextLittleEndianNumber: 4 put: image width.  "biWidth"
	stream nextLittleEndianNumber: 4 put: image height.  "biHeight"
	stream nextLittleEndianNumber: 2 put: 1.  "biPlanes"
	stream nextLittleEndianNumber: 2 put: (depth min: 24).  "biBitCount"
	stream nextLittleEndianNumber: 4 put: 0.  "biCompression"
	stream nextLittleEndianNumber: 4 put: biSizeImage.  "size of image section in bytes"
	stream nextLittleEndianNumber: 4 put: 2800.  "biXPelsPerMeter"
	stream nextLittleEndianNumber: 4 put: 2800.  "biYPelsPerMeter"
	stream nextLittleEndianNumber: 4 put: biClrUsed.
	stream nextLittleEndianNumber: 4 put: 0.  "biClrImportant"
	biClrUsed > 0 ifTrue: [
		"write color map; this works for ColorForms, too"
		colorValues := image colormapIfNeededForDepth: 32.
		1 to: biClrUsed do: [:i |
			rgb := colorValues at: i.
			0 to: 24 by: 8 do: [:j | stream nextPut: (rgb >> j bitAnd: 16rFF)]]].

	depth < 32 ifTrue: [
		"depth = 1, 4 or 8."
		data := image bits asByteArray.
		ppw := 32 // depth.
		scanLineLen := biWidth + ppw - 1 // ppw * 4.  "# of bytes in line"
		1 to: biHeight do: [:i |
			stream next: scanLineLen putAll: data startingAt: (biHeight-i)*scanLineLen+1.
		].
	] ifFalse: [
		data := image bits.
		pixline := ByteArray new: (((biWidth * 3 + 3) // 4) * 4).
		1 to: biHeight do:[:i |
			self store24BitBmpLine: pixline from: data startingAt: (biHeight-i)*biWidth+1 width: biWidth.
			stream nextPutAll: pixline.
		].
	].
	stream position = (bfOffBits + biSizeImage) ifFalse: [self error:'Write failure']! !


!JPEGReadWriter2 methodsFor: 'public access' stamp: 'jmv 10/3/2015 14:17'!
nextImageSuggestedDepth: depth
	"Decode and answer a Form of the given depth from my stream. Possible depths are 16-bit and 32-bit."

	| bytes width height form jpegDecompressStruct jpegErrorMgr2Struct depthToUse |
	bytes _ stream upToEnd.
	jpegDecompressStruct _ ByteArray new: self primJPEGDecompressStructSize.
	jpegErrorMgr2Struct _ ByteArray new: self primJPEGErrorMgr2StructSize.
	self 
		primJPEGReadHeader: jpegDecompressStruct 
		fromByteArray: bytes
		errorMgr: jpegErrorMgr2Struct.
	width _ self primImageWidth: jpegDecompressStruct.
	height _ self primImageHeight: jpegDecompressStruct.
	"Odd width images of depth 16 gave problems. Avoid them (or check carefully!!)"
	depthToUse _ ((depth = 32) | width odd) ifTrue: [32] ifFalse: [16].
	form _ Form extent: width@height depth: depthToUse.
	(width = 0 or: [height = 0]) ifTrue: [^ form].
	self
		primJPEGReadImage: jpegDecompressStruct
		fromByteArray: bytes
		onForm: form
		doDithering: true
		errorMgr: jpegErrorMgr2Struct.
	^ form! !

!JPEGReadWriter2 methodsFor: 'public access' stamp: 'jmv 10/3/2015 14:14'!
nextPutImage: aForm quality: quality progressiveJPEG: progressiveFlag
	"Encode the given Form on my stream with the given settings. Quality goes from 0 (low) to 100 (high), where -1 means default. If progressiveFlag is true, encode as a progressive JPEG."

	| sourceForm jpegCompressStruct jpegErrorMgr2Struct buffer byteCount |
	"odd width images of depth 16 give problems; avoid them."
	sourceForm _ (aForm depth = 32) | (aForm width even & (aForm depth = 16))
		ifTrue: [aForm]
		ifFalse: [aForm asFormOfDepth: 32].
	jpegCompressStruct _ ByteArray new: self primJPEGCompressStructSize.
	jpegErrorMgr2Struct _ ByteArray new: self primJPEGErrorMgr2StructSize.
	buffer _ ByteArray new: sourceForm width * sourceForm height.
	byteCount _ self primJPEGWriteImage: jpegCompressStruct 
		onByteArray: buffer
		form: sourceForm
		quality: quality
		progressiveJPEG: progressiveFlag
		errorMgr: jpegErrorMgr2Struct.
	byteCount = 0 ifTrue: [self error: 'buffer too small for compressed data'].
	stream next: byteCount putAll: buffer startingAt: 1! !


!JPEGReadWriter2 class methodsFor: 'image reading/writing' stamp: 'jmv 10/3/2015 14:15'!
putForm: aForm quality: quality progressiveJPEG: progressiveFlag onFileNamed: fileName
	"Store the given Form as a JPEG file of the given name, overwriting any existing file of that name. Quality goes from 0 (low) to 100 (high), where -1 means default. If progressiveFlag is true, encode as a progressive JPEG."

	fileName asFileEntry forceWriteStream: [ :stream |
		(self onBinaryStream: stream binary)
			nextPutImage: aForm quality: quality progressiveJPEG: progressiveFlag ]! !

!methodRemoval: BMPReadWriter class #displayAllFrom:!
BMPReadWriter class removeSelector: #displayAllFrom:!
!methodRemoval: BMPReadWriter class #readAllFrom:!
BMPReadWriter class removeSelector: #readAllFrom:!
!methodRemoval: ImageReadWriter #close!
ImageReadWriter removeSelector: #close!
