'From Cuis 4.2 of 25 July 2013 [latest update: #2835] on 1 July 2016 at 5:01:06.788572 pm'!

!Form methodsFor: 'accessing' stamp: 'jmv 7/1/2016 11:11'!
pixelsPerWord
	^32 // self depth! !

!Form methodsFor: 'accessing' stamp: 'jmv 7/1/2016 11:13'!
wordsPerLine
	| pixPerWord |
	pixPerWord _ self pixelsPerWord.
	^ width + pixPerWord - 1 // pixPerWord! !


!BitBlt class methodsFor: 'instance creation' stamp: 'jmv 7/1/2016 11:12'!
bitPeekerFromForm: sourceForm
	"Answer an instance to be used extract individual pixels from the given Form. The destination for a 1x1 copyBits will be the low order bits of (bits at: 1)."
	| pixPerWord answer |
	pixPerWord _ sourceForm pixelsPerWord.
	answer _ self destForm: (Form extent: pixPerWord@1 depth: sourceForm depth)
	 	sourceForm: nil 			"To ensure no colormap set"
		combinationRule: Form over
		destOrigin: (pixPerWord - 1)@0
		sourceOrigin: 0@0
		extent: 1@1
		clipRect: (0@0 extent: pixPerWord@1).
	"To ensure no colormap set"
	answer sourceForm: sourceForm.
	^ answer! !


!Form methodsFor: 'accessing' stamp: 'jmv 7/1/2016 11:14'!
bitsSize

	^ self wordsPerLine * height! !


!BMPReadWriter methodsFor: 'writing' stamp: 'jmv 7/1/2016 11:21'!
nextPutImage: aForm
	| bhSize rowBytes rgb data colorValues depth image scanLineLen pixline |
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
	stream nextUnsignedInt16Put: 19778 bigEndian: false.  "bfType = BM"
	stream nextUnsignedInt32Put: bfOffBits + biSizeImage bigEndian: false.  "Entire file size in bytes"
	stream nextUnsignedInt32Put: 0 bigEndian: false.  "bfReserved"
	stream nextUnsignedInt32Put: bfOffBits bigEndian: false.  "Offset of bitmap data from start of hdr (and file)"

	"Write the bitmap info header"
	stream position: bhSize.
	stream nextUnsignedInt32Put: biSize bigEndian: false.  "info header size in bytes"
	stream nextUnsignedInt32Put: image width bigEndian: false.  "biWidth"
	stream nextUnsignedInt32Put: image height bigEndian: false.  "biHeight"
	stream nextUnsignedInt16Put: 1 bigEndian: false.  "biPlanes"
	stream nextUnsignedInt16Put: (depth min: 24) bigEndian: false.  "biBitCount"
	stream nextUnsignedInt32Put: 0 bigEndian: false.  "biCompression"
	stream nextUnsignedInt32Put: biSizeImage bigEndian: false.  "size of image section in bytes"
	stream nextUnsignedInt32Put: 2800 bigEndian: false.  "biXPelsPerMeter"
	stream nextUnsignedInt32Put: 2800 bigEndian: false.  "biYPelsPerMeter"
	stream nextUnsignedInt32Put: biClrUsed bigEndian: false.
	stream nextUnsignedInt32Put: 0 bigEndian: false.  "biClrImportant"
	biClrUsed > 0 ifTrue: [
		"write color map; this works for ColorForms, too"
		colorValues := image colormapIfNeededForDepth: 32.
		1 to: biClrUsed do: [ :i |
			rgb := colorValues at: i.
			0 to: 24 by: 8 do: [ :j | stream nextPut: (rgb >> j bitAnd: 16rFF)]]].

	depth = 32 ifTrue: [
		"original depth 16 or 32"
		data _ image bits.
		pixline _ ByteArray new: (((biWidth * 3 + 3) // 4) * 4).
		1 to: biHeight do: [ :i |
			self store24BitBmpLine: pixline from: data startingAt: (biHeight-i)*biWidth+1 width: biWidth.
			stream nextPutAll: pixline.
		].
	] ifFalse: [
		"depth = 1, 4 or 8."
		data _ image bits asByteArray.
		scanLineLen _ image wordsPerLine * 4.  "# of bytes in line"
		1 to: biHeight do: [ :i |
			stream next: scanLineLen putAll: data startingAt: (biHeight-i)*scanLineLen+1.
		].
	].
	stream position = (bfOffBits + biSizeImage) ifFalse: [self error:'Write failure']! !

