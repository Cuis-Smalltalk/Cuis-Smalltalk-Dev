'From Cuis6.3 [latest update: #6278] on 16 March 2024 at 6:22:03 pm'!

!BMPReadWriter methodsFor: 'reading'!
read24BmpFile
	"Read 24-bit pixel data from the given a BMP stream."
	| form formBits pixelLine bitsIndex |
	form := Form extent: biWidth@biHeight depth: 32.
	pixelLine := ByteArray new: (((24 * biWidth) + 31) // 32) * 4.
	bitsIndex := form height - 1 * biWidth + 1.
	formBits := form bits.
	1 to: biHeight do: [:i | 
		stream readInto: pixelLine startingAt: 1 count: pixelLine size.
		self read24BmpLine: pixelLine into: formBits startingAt: bitsIndex width: biWidth.
		bitsIndex := bitsIndex - biWidth.
	].
	form makeAllPixelsOpaque.
	^ form
! !

