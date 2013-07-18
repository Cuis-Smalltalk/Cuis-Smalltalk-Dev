'From Cuis 4.1 of 12 December 2012 [latest update: #1559] on 15 January 2013 at 6:25:22 pm'!

!PNGReadWriter methodsFor: 'pixel copies' stamp: 'jmv 1/15/2013 18:24'!
copyPixelsGray: y
	"Handle non-interlaced grayscale color mode (colorType = 0)"

	bitsPerChannel = 16 ifTrue: [
		"Warning: This is extremely slow. Besides we are downsampling to 8 bits!!"
		| blitter |
		blitter := BitBlt bitPokerToForm: form.
		0 to: width - 1 do: [ :x |
			blitter pixelAt: x @ y put: 255 - (thisScanline at: x * 2 + 1) ].
			^self ].

	"Just copy the bits"
	"This interesting technique (By Andreas Raab) is a bit obscure, but it is so fast that we leave it active"
	bitsPerChannel = 1 ifTrue: [ ^self copyPixelsGrayWeirdBitBltHack: y ].

	"This interesting technique  (By Yoshiki Ohshima) is also instructive"
	true ifTrue: [ ^form bits copyFromByteArray2: thisScanline to: y * (form width* bitsPerChannel // 32) ].

	"This Smalltalk version might be easier to understand and is quite fast too."
	"This somewhat weird mixture of (#* and #+) with (#bitShift: and #bitOr:) 
	is to make use of faster arithmetic bytecodes, but not of slow largeintegers."
	"
	base _ y * (form width * bitsPerChannel + 31 // 32) + 1.
	bits _ form bits.
	0 to: thisScanline size // 4 - 1 do: [ :i |
		| ii |
		ii _ i * 4.
		word _
	           ((thisScanline at: ii+1) *256 + 
	           (thisScanline at: ii+2) *256 + 
	           ((thisScanline at: ii+3)) bitShift: 8) bitOr: 
	           (thisScanline at: ii+4).
		bits at: base + i put: word.].
	(bytesLeft := thisScanline size bitAnd: 3) = 0 ifFalse: [
		word := 0.
		thisScanline size - bytesLeft + 1 to: thisScanline size do: [ :ii |
			word := word * 256 + (thisScanline at: ii) ].
		word := word bitShift: 8 * (4 - bytesLeft).
		bits at: base + (thisScanline size // 4) put: word ].
	"! !

!PNGReadWriter methodsFor: 'pixel copies' stamp: 'jmv 1/15/2013 18:24'!
copyPixelsGrayWeirdBitBltHack: y 
	"Handle non-interlaced black and white color mode (colorType = 0)
	By Andreas Raab
	
	Currently enabled only for 1bpp, otherwise the comment at the bottom displays an incorrect image"

	auxBitBlt
		destX: 0 destY: (y * form width*bitsPerChannel//32) width: 1 height: (form width+31*bitsPerChannel//32);
		copyBits.
"
(Form fromBinaryStream: 
		'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
gg=='
				base64Decoded asByteArray readStream) display
"! !

