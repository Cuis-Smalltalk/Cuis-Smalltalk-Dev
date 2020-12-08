'From Cuis 5.0 [latest update: #4486] on 8 December 2020 at 4:37:02 pm'!
'Description '!
!provides: 'Graphics-Files-Additional' 1 24!
!requires: 'Compression' 1 nil nil!
SystemOrganization addCategory: 'Graphics-Files-Additional'!


!classDefinition: #PNGReadWriter category: 'Graphics-Files-Additional'!
ImageReadWriter subclass: #PNGReadWriter
	instanceVariableNames: 'chunk form width height depth backColor bitsPerChannel colorType interlaceMethod bitsPerPixel bytesPerScanline thisScanline prevScanline rowSize globalDataChunk unknownChunks palette transparentPixelValue filtersSeen cachedDecoderMap bigEndian auxBitBlt auxSource auxDest auxCMap'
	classVariableNames: 'BPP BlockHeight BlockWidth Debugging StandardSwizzleMaps'
	poolDictionaries: ''
	category: 'Graphics-Files-Additional'!
!classDefinition: 'PNGReadWriter class' category: 'Graphics-Files-Additional'!
PNGReadWriter class
	instanceVariableNames: ''!

!classDefinition: #TIFFReadWriter category: 'Graphics-Files-Additional'!
ImageReadWriter subclass: #TIFFReadWriter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Files-Additional'!
!classDefinition: 'TIFFReadWriter class' category: 'Graphics-Files-Additional'!
TIFFReadWriter class
	instanceVariableNames: ''!

!classDefinition: #TIFFField category: 'Graphics-Files-Additional'!
Object subclass: #TIFFField
	instanceVariableNames: 'tag type values'
	classVariableNames: 'DefaultFields SingleValuedTagSymbols TagSymbols ValueReaderSelectors'
	poolDictionaries: ''
	category: 'Graphics-Files-Additional'!
!classDefinition: 'TIFFField class' category: 'Graphics-Files-Additional'!
TIFFField class
	instanceVariableNames: ''!

!classDefinition: #TIFFImageFileDirectory category: 'Graphics-Files-Additional'!
Object subclass: #TIFFImageFileDirectory
	instanceVariableNames: 'fields'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Files-Additional'!
!classDefinition: 'TIFFImageFileDirectory class' category: 'Graphics-Files-Additional'!
TIFFImageFileDirectory class
	instanceVariableNames: ''!

!classDefinition: #TIFFReader category: 'Graphics-Files-Additional'!
Object subclass: #TIFFReader
	instanceVariableNames: 'stream structure'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Files-Additional'!
!classDefinition: 'TIFFReader class' category: 'Graphics-Files-Additional'!
TIFFReader class
	instanceVariableNames: ''!

!classDefinition: #TIFFStream category: 'Graphics-Files-Additional'!
Object subclass: #TIFFStream
	instanceVariableNames: 'stream bigEndian compressionType remainBitCount outCodes maxOutCodes outCount prefixTable suffixTable bitMask clearCode eoiCode freeCode codeSize maxCode finChar oldCode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Files-Additional'!
!classDefinition: 'TIFFStream class' category: 'Graphics-Files-Additional'!
TIFFStream class
	instanceVariableNames: ''!

!classDefinition: #TIFFStructure category: 'Graphics-Files-Additional'!
Object subclass: #TIFFStructure
	instanceVariableNames: 'imageFileDirectories'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Files-Additional'!
!classDefinition: 'TIFFStructure class' category: 'Graphics-Files-Additional'!
TIFFStructure class
	instanceVariableNames: ''!


!PNGReadWriter commentStamp: '<historical>' prior: 0!
I am a subclass of ImageReadWriter that decodes Portable Network Graphics
(PNG) images.

Submitted by Duane Maxwell!

!TIFFReadWriter commentStamp: 'mrm 7/27/2001 21:56' prior: 0!
For translating Forms to and from the TIFF file format. 
By Martin McClure

This work is based on TIFF revision 6.0. I got the specification from:
http://partners.adobe.com/asn/developer/PDFS/TN/TIFF6.pdf
The spec itself claims it can be obtained from:
http://www.adobe.com/Support/TechNotes.html
and from:
ftp://ftp.adobe.com/pub/adobe/DeveloperSupport/TechNotes/PDFfiles


Structure:
 stream			TIFFStream -- defined by superclass, wraps the stream that
							is being read or written

Current features:
* Reads uncompressed 24-bit RGB TIFF files with 8-bit alpha channels
  into 32-bit Forms, with the alpha channel intact.
* Reads both big-endian and little-endian TIFF files
* Reading TIFFs without alpha channels
* Reading compressed TIFFS (LZW only)

Some features not yet implemented that would be reasonable for this class:
* Writing TIFFs
* Reading TIFFS of different bit depths
* Any of the many other TIFF features

I've tried to make it reasonably easy for anyone who wants to add more of the missing features by breaking the design down into simple little chunks. The chief components are TIFFStream, which wraps a PositionableStream and translates the endian-ness of the stream, TIFFStructure and all its associated classes, which mirror the logical structure of the TIFF file, and TIFFReader (and future TIFFWriter) which interprets the structure of a TIFF and generates the appropriate Form.!

!TIFFField commentStamp: 'mrm 7/28/2001 15:55' prior: 0!
I represent a TIFF field, or tag-value pair. I'm part of a TIFFImageFileDirectory.
By Martin McClure

Structure:
 tag			Integer -- one of my tag constants that is the integer TIFF tag indicating the field meaning.
 type		Integer -- one of my type constants that is the integer TIFF type of my values
 values		Array -- my array of values. The size of size is equal to the TIFF 'count' word.
					Each value should be of a type compatible with my 'type' ivar.

ValueReaderSelectors 		Dictionary -- maps TIFF type integers to the selectors of the methods used to read field values of that type.

TagSymbols					Dictionary -- maps TIFF tag integers to tag symbols.

SingleValuedTagSymbols		Set -- contains the tag symbols of the fields types that should have a value that consists of an array of length one.

DefaultFields				Dictionary -- maps tag symbols to the default field with that tag. Only maps tag symbols for which the TIFF spec defines a default.!

!TIFFImageFileDirectory commentStamp: 'mrm 7/28/2001 13:12' prior: 0!
Mirrors the structure of a TIFF file's Image File Directory. Can be created by parsing itself from a TIFFStream or can (future) write itself to a TIFFStream.
By Martin McClure

Structure:
 fields		Dictionary -- maps tag symbols to the TIFFFields that make up this IFD.
 
See TIFFStructure for more information about where I fit.!

!TIFFReader commentStamp: 'mrm 7/27/2001 22:05' prior: 0!
I parse a TIFF file and produce a Form. I usually am created and used by a TIFFReadWriter.
By Martin McClure

Structure:
 stream		TIFFStream -- Usually over a TIFF file.
 structure		TIFFStructure -- The TIFF IFDs and fields that together make up the image characteristics, all the information but the pixels themselves.

!

!TIFFStream commentStamp: '<historical>' prior: 0!
Wraps a PositionableStream. Allows access to TIFF simple multi-byte types, remembering whether this TIFF is big-endian or little-endian.
By Martin McClure

Structure:
 stream				PositionableStream -- my underlying stream
 bigEndian			Boolean -- false if little-endian, true if big-endian
 compressionType	Integer -- currently supported: 1: no compression, 5 LZW compression

Only implements enough Stream protocol to let TIFFReadWriter and its cohorts to do their job.!

!TIFFStructure commentStamp: 'mrm 7/27/2001 21:42' prior: 0!
I mirror the logical structure of a TIFF file, including its directories and fields but not its pixel data. I can parse a TIFFStream or (future) write to a TIFFStream. I'm not too smart about what my structure means, I leave that interpretation to TIFFImage.
By Martin McClure

Structure:
 imageFileDirectories		SequenceableCollection -- each element is a TIFFImageFileDirectory

I hold the logical structure of the TIFF, but I do not preserve the physical structure (specific offsets) of the file I was parsed from (except for the offsets of the pixel data.)

See TIFFReader (and future TIFFWriter) for more information about the structure into which I fit.!

!TIFFField methodsFor: 'printing' stamp: 'mrm 7/31/2001 21:51'!
printOn: aStream

	super printOn: aStream.
	values printOn: aStream.! !

!PNGReadWriter methodsFor: 'pixel copies' stamp: 'DSM 3/26/2000 21:32'!
copyPixels: y
	"Handle non-interlaced pixels of supported colorTypes"

	| s |
	s _ #(copyPixelsGray: nil copyPixelsRGB: copyPixelsIndexed:
		  copyPixelsGrayAlpha: nil copyPixelsRGBA:) at: colorType+1.
	self perform: s asSymbol with: y
! !

!PNGReadWriter methodsFor: 'pixel copies' stamp: 'RAA 11/4/2000 16:08'!
copyPixels: y at: startX by: incX
	"Handle interlaced pixels of supported colorTypes"

	| s |
	s _ #(copyPixelsGray:at:by: nil copyPixelsRGB:at:by: copyPixelsIndexed:at:by:
		  copyPixelsGrayAlpha:at:by: nil copyPixelsRGBA:at:by:) at: colorType+1.
	self perform: s asSymbol with: y with: startX with: incX
! !

!PNGReadWriter methodsFor: 'pixel copies' stamp: 'jmv 6/7/2016 10:36'!
copyPixelsGray: y
	"Handle non-interlaced grayscale color mode (colorType = 0)"

	| blitter high low value base bits bytesLeft word |

	bitsPerChannel = 16 ifTrue: [
		"Warning: This is extremely slow. Besides we are downsampling to 8 bits!!"
		blitter := BitBlt bitPokerToForm: form.
		0 to: width - 1 do: [ :x |
 			high := thisScanline at: x * 2 + 1.
 			low := thisScanline at: x * 2 + 2.
 			value := (high * 256 + low = transparentPixelValue)
 				ifTrue: [0 "transparent"]
 				ifFalse: [high max: 1].
 			blitter pixelAt: x @ y put: value ].
  			^self ].

	"Just copy the bits"
	"This interesting technique (By Andreas Raab) is a bit obscure, but it is so fast that we leave it active
	Note that currently it only works on 1bpp PNG, building a BigEndian form."
	(bitsPerChannel = 1 and: [ form isBigEndian ]) ifTrue: [ ^self copyPixelsGrayWeirdBitBltHack: y ].

	"This interesting technique  (By Yoshiki Ohshima) is very fast, although only works with whole word image width"
	(thisScanline size bitAnd: 3) = 0 ifTrue: [
		^form copyFromByteArray2: thisScanline to: y * (form width* bitsPerChannel // 32) ].

	"This Smalltalk version might be easier to understand and is quite fast too."
	"This somewhat weird mixture of (#* and #+) with (#bitShift: and #bitOr:) 
	is to make use of faster arithmetic bytecodes, but not of slow largeintegers."
	"This works for GrayForms of any endianness and width"
	base _ y * (form width * bitsPerChannel + 31 // 32) + 1.
	bits _ form bits.
	form isBigEndian ifTrue: [
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
	] ifFalse: [
		0 to: thisScanline size // 4 - 1 do: [ :i |
			| ii |
			ii _ i * 4.
			word _
		           ((thisScanline at: ii+4) *256 + 
		           (thisScanline at: ii+3) *256 + 
		           ((thisScanline at: ii+2)) bitShift: 8) bitOr: 
		           (thisScanline at: ii+1).
			bits at: base + i put: word ].
		(bytesLeft := thisScanline size bitAnd: 3) = 0 ifFalse: [
			word := 0.
			thisScanline size to: thisScanline size - bytesLeft + 1 by: -1 do: [ :ii |
				word := word * 256 + (thisScanline at: ii) ].
			bits at: base + (thisScanline size // 4) put: word ].
	]! !

!PNGReadWriter methodsFor: 'pixel copies' stamp: 'nice 5/10/2014 20:14'!
copyPixelsGray: y at: startX by: incX
	"Handle interlaced grayscale color mode (colorType = 0)"

	| offset bits blitter pixPerByte shifts b pixel mask pixelNumber |
	bitsPerChannel = 16
		ifTrue: [
			"Warning: This is extremely slow. Besides we are downsampling to 8 bits!!!!"
			blitter := BitBlt bitPokerToForm: form.
			startX to: width-1 by: incX do: [ :x |
				| high low value |
				high := thisScanline at: x//incX<<1 + 1.
				low := thisScanline at: x//incX<<1 + 2.
				value := (high * 256 + low = transparentPixelValue)
					ifTrue: [0 "transparent"]
					ifFalse: [high max: 1].
				blitter pixelAt: x @ y put: value ].
			^self ].
	offset := y*rowSize+1.
	bits := form bits.
	bitsPerChannel = 8 ifTrue: [
		startX to: width-1 by: incX do: [ :x | | w |
			w := offset + (x>>2).
			b := 3- (x \\ 4) * 8.
			pixel := (thisScanline at: x // incX + 1)<<b.
			mask := (255<<b) bitInvert32.
			bits at: w put: (((bits at: w) bitAnd: mask) bitOr: pixel)
		].
		^ self
	].
	bitsPerChannel = 1 ifTrue: [
		pixPerByte := 8.
		mask := 1.
		shifts := #(7 6 5 4 3 2 1 0).
	].
	bitsPerChannel = 2 ifTrue: [
		pixPerByte := 4.
		mask := 3.
		shifts := #(6 4 2 0).
	].
	bitsPerChannel = 4 ifTrue: [
		pixPerByte := 2.
		mask := 15.
		shifts := #(4 0).
	].

	blitter := BitBlt bitPokerToForm: form.
	pixelNumber := 0.
	startX to: width-1 by: incX do: [ :x | | rawByte |
		rawByte := thisScanline at: (pixelNumber // pixPerByte) + 1.
		pixel := (rawByte >> (shifts at: (pixelNumber \\ pixPerByte) + 1)) bitAnd: mask.
		blitter pixelAt: (x@y) put: pixel.
		pixelNumber := pixelNumber + 1.
	].
! !

!PNGReadWriter methodsFor: 'pixel copies' stamp: 'jmv 9/23/2012 21:45'!
copyPixelsGrayAlpha: y
	"Handle non-interlaced grayscale with alpha color mode (colorType = 4)"

	| i pixel gray b |
	b _ BitBlt bitPokerToForm: form.
	bitsPerChannel = 8
		ifTrue: [
			0 to: width-1 do: [ :x |
				i _ (x << 1) + 1.
				gray _ thisScanline at: i.
				pixel _ ((thisScanline at: i+1)<<24) + (gray<<16) + (gray<<8) + gray.
				b pixelAt: x@y put: pixel.
				]
			]
		ifFalse: [
			0 to: width-1 do: [ :x |
				i _ (x << 2) + 1.
				gray _ thisScanline at: i.
				pixel _ ((thisScanline at: i+2)<<24) + (gray<<16) + (gray<<8) + gray.
				b pixelAt: x@y put: pixel.
				]
			]
! !

!PNGReadWriter methodsFor: 'pixel copies' stamp: 'jmv 9/23/2012 21:45'!
copyPixelsGrayAlpha: y at: startX by: incX
	"Handle interlaced grayscale with alpha color mode (colorType = 4)"

	| i pixel gray b |
	b _ BitBlt bitPokerToForm: form.
	bitsPerChannel = 8
		ifTrue: [
			startX to: width-1 by: incX do: [ :x |
				i _ (x // incX << 1) + 1.
				gray _ thisScanline at: i.
				pixel _ ((thisScanline at: i+1)<<24) + (gray<<16) + (gray<<8) + gray.
				b pixelAt: x@y put: pixel.
				]
			]
		ifFalse: [
			startX to: width-1 by: incX do: [ :x |
				i _ (x // incX << 2) + 1.
				gray _ thisScanline at: i.
				pixel _ ((thisScanline at: i+2)<<24) + (gray<<16) + (gray<<8) + gray.
				b pixelAt: x@y put: pixel.
				]
			]
! !

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
		'iVBORw0KGgoAAAANSUhEUgAAAGwAAACHCAAAAADWjuNQAAAAB3RJTUUH2QMFAAEAkIBc5AAA
GadJREFUaIHte2l4lUXS9t39nC052UPCZpCdAIKgEJVFBhUV3EBQB0ERFXfFccOZQUXlehVl
lEsdBUVGEEEERRREENk3gQAqWwghEAgkJCHbOTnr0/f341nOCeDMOJ8z1/dd7/SPnO7qpbqq
q6qrqp8I4j9X5H8Q13+R/cbIziUoNkydC2hUyHNO57mqjhgo7A/4hdmqD8PlOj/J3tKeZfuP
EIKugaNb2XNDGxFp3cmoi/0H/eIGewKEvm7LrnLh7dl3QJLSNZMmmkVxWvPmHqfL5XK5XG6H
lA7Msfr4RqK9StZLNvRkKnA/ldGYALj2213cO8Sa0PkLFdSNQXHIngMgEFemGQspzgcAuJo3
dwHAe9b6J1OBh63GX4GEn8y1dC7LtJYSwLuMGINsNiot0eavK0IIItscHpoC4bz99y0ytPJ5
rwFT7vDa68R2pwBhLSYP310F2ebKC1KOrlkj+PTFebTPjBCgxHWJCdKRQFeqZ8FbEEx97VZl
sPpgAXjvXwGg2YXpBd4W8dTbp18MOFOtxqRyqR6dlArguanPwD89zxpN6qQyuUqSfDURErn5
dnunB+i7+1hlTW0kyrhSlgo8ZjXGA6llZv1YBjDMPBze12zyEeo2G+WRoh6Z9l7rHp4LqNtm
pCpLLTpdsENuuiTVpYkEZvQd3T2estKtDVE6ooL748g8XA08bnBayVdeagolbMrUob8VGYQp
xbL+kBCTFG1SFb9xxS3vmWN1laXFoxVIPWnO+BpIO2wLC5UyJ4Bk1aZ1P4ZNwdueC4nMxfHc
UlGuHXdhkltCAJBocsJiY6ajkfSmWchWCXj2mZMj4di2HQAyasv7GISKL8fWaHruvJ4qzo5R
w4ABkeO1dSHBw+8Uhivzr7ckMIpsVzUIQISiYMSEt0z0B3d1NiY76uszYkuR3L7x6HFSkS9p
ELi2wtyLKTRq9ZsfvF9uCQ+A18z6ycwWyU/XHy0pKSkpKR8PJBdZrOsNdDhmyAdHt/+ScUod
JZVOsm40hNAejjBKRqPRqMXqdwAMPkqSLOwFYLnFxnQ3xlss+iOQcsRqzIZA128CJA+NBvAI
Y0otCQrg9E0bhVCuw8MjDQm6T0Rbzk42iL/hpVNyeY8+GY5o5eZawUuuoCVcYeigfW7Urdqd
Sxdqe4d07ZxYtaFOUFwWz0ZDar5C45JdZaif4jxA2CY7Y7OpNSxLi22az8RRprNySGwd7X0L
hy0HAtEzkBFANApQH7n0MuFs1iQBQOL1ay/TLcICQDg2Hw0hq65nLnnPvA6cA1eOs8dYikix
/c3EZIfuSRJwQbqEcI5KMHhEAX3nifLTp31tWnfvBFoGsWFm8HSfG0025u+QzmG2vaKAf/vP
xTyvZW6P2PZjyHjWpc3Gl8C5u5UQ5ur/uMSQGRiFAIUNjFuBIAVEI4iBT9jzGyOkuZ0YVPzX
b/wvsr9XYq5cTPQpLFGOB9pVBSGsJoXhOQphaKQtegqCBiXKcDoaIYNoqIs0cwIATwW184wp
Ws22/Lr6pPT+vZ00llOIahogRK1SKlMAIAWjglIAmr01XbktvBKgqYb2Fcm/pLc4RJKs6pI4
2oQu62DOGFhiGsGn215aQkX/I+e36LCAilTccMWAi/K6devapc+4DdZy4dFX3XaQimTDn242
fcJ4v3Ei5AGSZEUqRhnAfenQ+j7yzIO9gYERA3QrUorJw5cDrVeYU9fETkVbYq6rcoGuJxgl
Q11wI8/wGwUkNGEdpOl0LKrGtEcA4PHtnqqmZl+SG6vuPIlr5mSbllMKDm8Ld9QV+LjsjevN
NZwQe0cuagLADcsrdwDgicMXJUIICDcAwKNZkYQTqNzr0jze151QBt8lpOO1P0cx4WWnARDQ
iCf6AACOzz9cm26ctguOyLqRn6eAtOXGARD+410TAQLvZjrCynHUZzll174QfnGKRzodroue
voQmft/vVwPo6rQlzy24OhhSIe3kZrTymrKgMPzip1eNmZdAIMGWG/Lgpu+21ZAvpVjCreEu
8zg+bGUJWDPTNxsHCW1oEhI2W6fNDTFbqy2i4cfq3TCWrwncznBPjDMHOgC0O+bL81JIIZMN
dPWwLvi779xVqIuQf9GGstX3AAD8UJnv/P7VPwbGfJ9j3SsiUXMw7Efm1fdcaYm/gMDT1a/M
805tsJ1XBwAZDDkkEK51L+8UBWTF5ZVm78Lj+u29ASBzAyotqhM+uwLP7p9TeP+XphiFGZh+
a/T4DcfUkxdbJBrbmFz1/geBetiFttP6AlyFhp5lYYxB9wSgz8Kdhbs/7wKsMUB3otkJKtZ0
A54zubMO+JLk1xpaFdqa1B3jqMhxgMS9JjBOz56Hs4AkeSoDYw3gie4APEkewJ5wO5ocIcl8
L7T5huuzBlhMKr4E9PFbetbNPKlxEPbcOEOsI2LGxwmWfW6+4vH2aUFf0JP7l+lm8ByE2wGA
F70JfVy+BIAI4AOAiTdj811h8xwjCAIg3h9FWJ5y3E196ICznxcAwgfpbWugFfAdO6ajZUeP
5WUU1GjdXQCg1oYjXdoBQNmWUN8cgKJiu1CXm87mppNtLgZAUfN1TVfT0/yt3IJzOTwxmOkV
Oc4e82uLEsDZrhl4xi9+A8rUr7h//29vauqRfzzIKmdT9k+5m7HRf9+R/UfI/o2lsYCEdhfJ
nF7GRWP4GgAoziA25DLgjYjSYz7BLzOHpNJ1XSfJzy8AgF5rqUidd7X+miT5aLcvScX8G8eH
Sc4YMmhKmCTX9MwbMPCaG4c/sZeK5Imru60nqfhdv9trSHLNTTcPGzb03j8tDtrXgxl5RnWS
nwLd7hrVAcnbSRXlYMwmSQ7GB6TiJvSN8vRI4MWgIslFgNAkgKZ7SOr8A+4gqXgTniBJfmyR
87vj8cjUzm/9umJFazwVIX2DHTNJ6rwZC0iSN2AWSW7BIO7pjXbfGDExv8C1xUWHig7cgEkk
dR7NcO2m4lbZqoIkOQuD87f+sOqDPPS3aHMAEKgBKH440muKBLwzqrvTiD6+qQ2nOVlgHIMD
oU8fqnZ/2ssOZZKaBkAtFWEAEq3ue/XNj4Cp6qEmAACJFhcBwPX9NqweHHPlDsx/d31U58e2
ddZJpfMW+1xnkTp/cDrRrB1utXjyBTxpqZlNm8CzyQCUNHXu5DaZYxDGj2D6g8Pxl7h0UsdC
XiiJRBwAFGSdnk4hgDJc1EJBcEs1AF26HYGeS+ou+6zTS6QEBZHcMbK/ynPbA30MLyvn5vfe
+fAN9VgT05tGBCSE8MNlyS1JtXp5va54LAuv6WR4bIfvSJL98AVJcpBxZhtETin5GWDeYgtx
O/m9W1vEqBnDF2VmzHK1qzMpn4PbSJKfOsQ2xudBVERXitOAyx59pCfwSQyZUtGBBrJ16B5U
is8iYydJci5GUfF1eNdZORo+Bjfettg8F91fnvyn8UPdGEN1hkdMku80BYBWs0lSMc9AGuhu
5G9Xo0OAZHggcstIcjpuIclhaHnAmn84Ce1qrMZ888ATHw5YoHhzRVG2tVK1vCxDlwIUX50a
2A5kePGxmzoBOLI8ZYQbFCeX1l7VAxQFm8+/AsCpj+ovHmqtsLT44j6W+di/LES4s9IuaG8j
iEdm3RakjBkjoyEII46yHQkzrrJyFzZM/Z2kw7/LEEfoOhv4G9zU5yznvCcbAxl3m8e/RCiA
OmmOUIwNiQ23RwIANQ2MLWh2xrExVNikGVBT1NaIQ7Bydfl5119i1PPLfuelKCy51Asoob79
viqx+4gsCiB0FIgirYV1cOVLd+udRzYBiEiBty2AvUuGdomZK0UqRe4U80kuw1qSpP8OZLf1
ipcNob0V95L8s+cwSZ3j0bx7R1xQQZLFTaTURPIYM/N9oDVyctD5EEnqQ3IqSd7gPBQfef60
3Bcld+BLkitgmLrnMc0fLRuKjVQk7wLGk39yF5NkecrlYXLJw3tJ8rBnxPfrVjyDWxSpU12d
+F0kMrO9YYPWYwr5IyZaegaSavsH5STzcfl9Dz5yldhEkqEOw0iySD5Pkryl8/OYwonJJ0iq
hi7OobfdNamAOskjCX8kyXscJaTicdcEkgybq49MOswHPVYinA4AvpLE0mwAYn+lQq0d4gGA
7jbOVHe+qCYkRdwuAEj47L2dNdqCGfnNAUQNn71lNGyELk4A8JnH/sIXbz4186E2MQEkuWa2
TynmYxYVl5hsHIu54UjFMGwkSQ5v18BnkNr8JEldXzC1jnwc+SRZmvYUI/rmrJxqkuTvkrcw
OitrIUlS53PeoXaa2mTj5u8COrkDH5L8GutIkmV90Kp9CiYYJv66NgHySaSWklSchrbXd8cV
DSRZ3jShXdvW8JhZ6h+y0bYV2u8lSSqWZ+NBK8trZguMhHHypdkAmvRPAQA2/faTjSXXDhls
aOLtxQRebXbcDUBgfLdVJzs8NDIBAJy3+smknMHdDOnO2/LJIXnhSCOzIJD99rZ7Y7rcyFyR
oDQdt0Y2TknQyA6d6aXZ3rcVt0ucNdsujZFBRV0UlldIY3cwDLHdgGk3zOyWMJJWcQnVxiPj
dve/I5NqWFrSNLEEoOIIp91koxn2j2mNTYtMxNVjz5DWTIHy2tR0FwGgWiZL/RSyBaBOe5Kg
VyUmAWjwS8p0Y4cnDl3q4unNl2QByh8i6EklBSjqV+zLvqq9EgBE6KfTWR2T4h7rrFI2KjUz
4ykfScXr72bNdTmLqEhfx5lkTe6bJPmWJzklPW8ZSfJDjFHcK9aSiq97m7ds1nQGSfLIhQl5
LVJWkSSLL81qkd1pb5ye+XcndydEeETNxzl7HsTrAFCXve9WtSgPAhCVAkjS6wGgLnGmC18M
+74fBbxiPmdTSgAobbGYjr88PSIDwMS6n9v57tt+JUDxyc8bOlW9eti8YhwAXPVBAti+Kb8n
ehyb9bIHSstZtqL0HuMRlokfbosGjhtJ1oxhwNVLl/UDoGfNGJUzWghAwFk6AYk/dvYCCG+9
sR2S5hmKd7F2T7L7/G7mQTkAlNR5TrYggtIDIBw2HoBqFzcMH3CH4ec01EOFYvrSEEqCAERw
8De37DHVy5X348Lb3nUrCVebXQDWdmwBgH2Xnyw/NUWfZQsU69+eS5KVrXtuPPi+9zmSOgfe
Qv4Za6mTvrSZZDTnRZKc7H3zrdd7tjxMRU53n+YaL9aRig/mkqOzjEtnnbvXayPxHknFqZ0X
/Phl2j1xhrjm6x0kyd2XJSVmPhogqXP0A2Ro6KBqkg29PyWD17xDkh+0bH1+79t+ok5y6Wgf
ufKBQlJx+j1Blg+5vU6RiptH9R/8foSk4onbWp/fbOhJ86aOsyAUemFNuyyjGhEOAH6PBiCs
aRQRqQHQdcApzrKQFCAEAi7tLHjQF3uY/99hrv7Jwn+ZGf8Csui/jE3CspO0DWbMlT3rMx2S
cELE9585zIbaf+1fQSCiOxwgosoJGXBLwhdNE9V6E4DH/Uw+j5RQP3Xw+vde6KaoPNYzvL51
ewBLO7er/m6oC8CRd2o73ZeihMBPn9T1HuUGsOdgJNqrk5LAgrnZf/WYSMnjX+whyT2Dqrh2
cIDk3LHkG4+R1Pv2vDpvbINOBjtfr4rbnyS59pLKETeVkOSl81jQsZ5kVe/73x44JESyusvk
BcM3keQTfZ4a3aGM5IoOs7/yxzk8ifWEQNekj56YOsADILx3tb7TA0DVv9endvBnYwDVTD38
rFcASDg6qLQoiQJIdMHhAYAN3ukY1+lAd8B7weYj6akAhTudHqcCsPiBO2OvHVBHTjVEAGDi
khXHHgQAV8HU11e5AUBLQ2qiHwDdM1MeUcZb8VvX3AcBwH0QRwISQMvS9cFvmAGA1z81+sA7
ABBKS5n5P811oMdn+T9FY/dZ6btmOP+I628kyek3kU/eSTKSe/mofgMrSJ7uVcC7mp0iueZy
1vR4myS3XjSgqxGv/61XbtdFVKT+QP8b8taQ5NPT+crgCpLBx3vc7I/ZxrIfzA8wDr4eUCRZ
sIbcuplkdMmHCzf6qcjw9lo2bAtQsb5IZ8V2nVSsKyg1Tayv2ArcS4uqqUhVWc3QEQNJfcB0
U+Nto+4AKUFKKEhQmO4fIazwmYTdaYXZiKvZYbWSUEJYw0xbKmgNRuMFYEPQqCkaAc7Ko1oe
ZRyKf38Af87yK80Vf7HxSwPjB2mT4vkOKJN4BYryY8kOXeinPRAgKxNY6HEAJ0rraumhXucR
tf4EssyhHXE5Kao2l2VLiPJjR0NpQgm1Zl2zZIKy9MdWMbaTkcoKUnHPg6OHrafipBHDn/Qr
xVVXjrjmFHlojJHFfSD//sdqqTh9XH/XMir9wbUc8w2VurmQI7eRp65+8c7pVJx4y7N9N5Hq
Dw/8+doT1HXe09r6Xo8SgLZ7Jyhw6NA1cguA/U++VTtDAFNeXPiCg1AhCBCouvv7l1N08P5H
HLOHKMihswv8VwPi9ITxWx2As0VZOAqFUGi/5gWO//ze5G5rIOS2olvnWLyUgH7YGagRgBao
aFIMgO4UrQFA83U/ro8KuGq3rzwhBEJv/uGugKSYeWkP90EpcHX6nXdrAMK/G9U5CITS89JW
QaDhziu8nYDmzce/mt8LxKzzHSv2mWKoTYLwlznaaxDHNjvK+vWCWDt3cdrTXorLVy5p31cT
3L91V/scINKvf3F6c2B9B09B6/Mp0Kp2nAQYHNpFtstkQnB3kwlpFFrPQa6UdGpXnZYP5SrR
oE24MjczA/Yrk1pZeg8FQn7N7aGAHhHuM5P5jVTvzLaSMZWyc6YifmycUkeroi1/3aNIfIkq
51mq/QtF0AhiEZfKM7J7jT6DOHsvNoRmqvwX6ImfKRjXVsbXQoKksL/AUIKmkaOEEhBQkqC0
DKWEopA2IiPINiNwA2gH2NokIFJWmwpARbQwJKPaEekht5Y2l6j/6bgjiVDrDrR0AlyengRx
qKDVlkyXKP1wxwUuWfK3i3bM6auVf7attIPE0hT3960cFVPW7e7hEp8v/ambQ6z8aEPieRZl
EoBWcAgEdn4gph4X1ZNnzHZAe3/x4hXAymkLnlVCvrN0XT4gdr07E8TP969fVovQlLyk1wVK
5n318TdRlHx+6uUSQCxcfsCFihW+FDeji47tbQB2nyz6AvHISmW0nkR0wcRvJRwblrXxEpuf
bH8ACDaEKsPAnrEPZQCY6/6kSALPfLfBiYjPjSAQuvfNNp2iCGrZE88DriucfRegpSbVhiB6
NN97FKjJK7nGRqZNAiK1eiuHRElk8v4LsuoPfjgjtTVSPy5rdyFOHHBmDXKiyfRVvZrj9Ik/
5EZao7blTcE8l6vd55GHvVAtL+nXqo0m+w3J1QDktO2mhNuV7Mn1SG/ZjX0kHEP6h+xvqgUB
biy7BYDfn12d5FR1aao+FQiFkwEVpfufEGmerVzxUmhXBQEViKaeOeDfUgTjlMmQe3FGwsZq
k8IwyTBua+IXlfAXipmPIgEIS4+UoKACoJQQQukQQuhKSCodEIoUIkoKIZQCoFPRrxjUoxGg
rkQBerUCfCUno1Any8BonemZSwD69j0CQGTfvqisKN9ZIX07G3YJsW1JvtS2jJ9HrWTOoiot
MP2FIq3h05pZUv6waE5AlEz541554uHDY49KKWZFxfcrPz4I37RZh4Dv7/cDhU+O9GHrpKm7
xNEpIkaZ8JXoBPa+MnEnjj+xIB2+r8JfQXy6yo/wR4/lH0bp8rk74O3Quh2SI5ObCFzw3vEU
BE4fUAi43mAAVasqJIK7f65GrecGILoi8CmQqj+ahFM9ehyFtL4J1SYhWlBYltAMOBYZcaGr
TMtObuLd/UOnrthanNlTc+9IH+jw73H3awnhaQWcV3CHFAU9HC0Sa7vkNk9XrVMGNE0VO3u0
Qerl3bKSU8IHu3l97camZKM4JamVy1uaMcjp7ZAau2JC37kHKYk6kUxG6AgniFBdlhKBsDOR
UtdIRuG2pcb4MZ5ojPyfEgJK2HeSbqUsEXJKG2Qh0zfK/r9Ohv/JcqaYCgK6du6xv3mRAIRu
ZPwafWRDReutxQpHz4ws7TBVEdCJ2EtM/CgF3ZqpTQLqVkWyAQohlBSCQkCpqA5AqqCmM+wI
QxL+sKvspFcrPprkLtb93gpR6wnt0w60OLqkoaXcFsz0H6z1pW/7Nkf/4Hhu4IsjraPbS5MS
Ni1Kzd7+fqCjHZ+BoXIAcsP0nbJ42VeyaPqKav2liTrn/3XOz/LL8nnihyJQbNgs3n2yQDz/
P/twYkwFTs1Yrp167pWv8fP82VrgjanQ33jxANalBSo+qZU/1mwp9GxemYaSdRK7E9ebLHFQ
1O3zqEACRYH7k4uK516Hir0ru3sukR7ou0JDEXq1DyIfjLx4x+q70eaEW/W9riUwsLhL12/z
4B0d6Iy2r1Sg/I6G0+6RmRr6/aglT+uNbnt75mJEAODFPtyyoavlpChF37QodZ2b3vqZR0o3
BCvK9tfzVCWV//QppYd2hdhQVKWCVSoSCem6ooqGGYrqxqlSp/GtvVIkjYSV8cW4Tuo6GfvX
AFIoIFLY0akACdQnx+4J0lAf0wGCUIAgISBs50zQqijThCtpR13GUEA0umL+Y+X/9dzV/yfI
HGEJBU0J848QyqGUAKWSSkJpikaHQxcKAtB0mr2aoiMKIRVo9EpC0ymVAMC4XmH9R5ijOE3V
yfSA9LOJXwsohzuQ3VDrcgSTfN76VNRk+4Jaml82oGmVxwe30LNOBxKU7gl6fBnBhmaVuisp
wKDMqvL4kBhxZJRHkoKaVCKcUR/NOsUETygScWYlGNb3/wA9AhmM+SowxQAAAABJRU5ErkJg
gg=='
				base64Decoded asByteArray readStream) display
"! !

!PNGReadWriter methodsFor: 'pixel copies' stamp: 'jmv 6/30/2011 10:22'!
copyPixelsIndexed: y
	"Handle non-interlaced indexed color mode (colorType = 3)"
	| hack hackBlt swizzleHack swizzleBlt scanline hackDepth |
	scanline := ByteArray new: bytesPerScanline + 3 // 4 * 4.
	scanline replaceFrom: 1 to: thisScanline size with: thisScanline startingAt: 1.
	hackDepth := bigEndian ifTrue:[form depth] ifFalse:[form depth negated].
	hack := Form extent: width@1 depth: hackDepth bits: scanline.
	hackBlt := BitBlt toForm: form.
	hackBlt sourceForm: hack.
	hackBlt combinationRule: Form over.
	hackBlt destOrigin: 0@y.
	hackBlt width: width; height: 1.

	(form depth < 8 and:[bigEndian not]) ifTrue:[
		swizzleHack := Form new hackBits: scanline.
		swizzleBlt := BitBlt toForm: swizzleHack.
		swizzleBlt sourceForm: swizzleHack.
		swizzleBlt combinationRule: Form over.
		swizzleBlt colorMap: (StandardSwizzleMaps at: form depth).
		swizzleBlt copyBits.
	].

	hackBlt copyBits! !

!PNGReadWriter methodsFor: 'pixel copies' stamp: 'jmv 9/23/2012 21:45'!
copyPixelsIndexed: y at: startX by: incX
	"Handle interlaced indexed color mode (colorType = 3)"

	| offset bits pixPerByte shifts blitter pixel mask pixelNumber |
	offset := y*rowSize+1.
	bits := form bits.
	bitsPerChannel = 8
		ifTrue: [
			startX to: width-1 by: incX do: [ :x | | b w |
				w := offset + (x>>2).
				b := 3 - (x \\ 4) * 8.
				pixel := (thisScanline at: x // incX + 1)<<b.
				mask := (255<<b) bitInvert32.
				bits at: w put: (((bits at: w) bitAnd: mask) bitOr: pixel)].
			^ self ].
	bitsPerChannel = 1 ifTrue: [
		pixPerByte := 8.
		mask := 1.
		shifts := #(7 6 5 4 3 2 1 0).
	].
	bitsPerChannel = 2 ifTrue: [
		pixPerByte := 4.
		mask := 3.
		shifts := #(6 4 2 0).
	].
	bitsPerChannel = 4 ifTrue: [
		pixPerByte := 2.
		mask := 15.
		shifts := #(4 0).
	].

	blitter := BitBlt bitPokerToForm: form.
	pixelNumber := 0.
	startX to: width-1 by: incX do: [ :x | | rawByte |
		rawByte := thisScanline at: (pixelNumber // pixPerByte) + 1.
		pixel := (rawByte >> (shifts at: (pixelNumber \\ pixPerByte) + 1)) bitAnd: mask.
		blitter pixelAt: (x@y) put: pixel.
		pixelNumber := pixelNumber + 1.
	].
! !

!PNGReadWriter methodsFor: 'pixel copies' stamp: 'jmv 1/24/2017 11:22:33'!
copyPixelsRGB: y
	"Handle non-interlaced RGB color mode (colorType = 2)"

	| i pixel tempForm tempBits |
	tempForm _ Form extent: width @ 1 depth: 32.
	tempBits _ tempForm bits.
	pixel _ LargePositiveInteger new: 4.
	pixel at: 4 put: 255.
	bitsPerChannel = 8
		ifTrue: [
			i _ 1.
			1 to: width do: [ :x |
				pixel
					at: 3 put: (thisScanline at: i);
					at: 2 put: (thisScanline at: i + 1);
					at: 1 put: (thisScanline at: i + 2).
				tempBits at: x put: pixel normalize.
				i _ i + 3 ].
			transparentPixelValue ifNotNil: [
				1 to: width do: [ :x |
					(tempBits at: x) = transparentPixelValue ifTrue: [
						tempBits at: x put: 0 ]]]]
		ifFalse: [
			i _ 1.
			1 to: width do: [ :x |
				(transparentPixelValue == nil or: [
					(1 to: 6) anySatisfy: [ :k |
						(transparentPixelValue digitAt: k) ~= (thisScanline at: i + 6 - k) ]])
					ifTrue: [
						pixel
							at: 3 put: (thisScanline at: i);
							at: 2 put: (thisScanline at: i + 2);
							at: 1 put: (thisScanline at: i + 4).
						tempBits at: x put: pixel normalize ]
					ifFalse: [
						tempBits at: x put: 0 ].
					i _ i + 6 ]].
	tempForm displayOn: form at: 0 @ y rule: Form over! !

!PNGReadWriter methodsFor: 'pixel copies' stamp: 'jmv 1/24/2017 11:22:49'!
copyPixelsRGB: y at: startX by: incX
	"Handle interlaced RGB color mode (colorType = 2)"
	| i pixel tempForm tempBits xx loopsToDo |
	tempForm _ Form
		extent: width @ 1
		depth: 32.
	tempBits _ tempForm bits.
	pixel _ LargePositiveInteger new: 4.
	pixel
		at: 4
		put: 255.
	loopsToDo _ width - startX + incX - 1 // incX.
	bitsPerChannel = 8
		ifTrue: [
			i _ startX // incX * 3 + 1.
			xx _ startX + 1.
			1 to: loopsToDo do: [ :j |
				pixel
					at: 3 put: (thisScanline at: i);
					at: 2 put: (thisScanline at: i + 1);
					at: 1 put: (thisScanline at: i + 2).
				tempBits at: xx put: pixel normalize.
				i _ i + 3.
				xx _ xx + incX ].
			transparentPixelValue ifNotNil: [
				startX to: width - 1 by: incX do: [ :x |
					(tempBits at: x + 1) = transparentPixelValue ifTrue: [
						tempBits at: x + 1 put: 0 ]]]]
		ifFalse: [
			i _ startX // incX * 6 + 1.
			xx _ startX + 1.
			1 to: loopsToDo do: [ :j |
				(transparentPixelValue == nil or: [
					(1 to: 6) anySatisfy: [ :k |
						(transparentPixelValue digitAt: k) ~= (thisScanline at: i + 6 - k) ]])
					ifTrue: [
						pixel
							at: 3 put: (thisScanline at: i);
							at: 2 put: (thisScanline at: i + 2);
							at: 1 put: (thisScanline at: i + 4).
						tempBits at: xx put: pixel normalize ]
					ifFalse: [
						tempBits at: xx put: 0 ].
					i _ i + 6.
					xx _ xx + incX ]].
	tempForm displayOn: form at: 0 @ y rule: Form over! !

!PNGReadWriter methodsFor: 'pixel copies' stamp: 'jmv 1/24/2017 11:23:10'!
copyPixelsRGBA: y
	"Handle non-interlaced RGBA color modes (colorType = 6)"

	| i pixel tempForm tempBits ff |
	bitsPerChannel = 8 ifTrue: [
		ff := Form extent: width@1 depth: 32 bits: thisScanline.
		cachedDecoderMap 
			ifNil:[cachedDecoderMap := self rgbaDecoderMapForDepth: depth].
		(BitBlt toForm: form)
			sourceForm: ff;
			destOrigin: 0@y;
			combinationRule: Form over;
			colorMap: cachedDecoderMap;
			copyBits.
		^self.
	].
	tempForm _ Form extent: width@1 depth: 32.
	tempBits _ tempForm bits.
	pixel := LargePositiveInteger new: 4.
	i := -7.
	0 to: width-1 do: [ :x |
			i := i + 8.
			pixel at: 4 put: (thisScanline at: i+6);
				at: 3 put: (thisScanline at: i);
				at: 2 put: (thisScanline at: i+2);
				at: 1 put: (thisScanline at: i+4).
			tempBits at: x+1 put: pixel normalize.
	].
	tempForm displayOn: form at: 0@y rule: Form over.
! !

!PNGReadWriter methodsFor: 'pixel copies' stamp: 'jmv 1/24/2017 11:23:39'!
copyPixelsRGBA: y at: startX by: incX
	"Handle interlaced RGBA color modes (colorType = 6)"

	| i pixel tempForm tempBits |

	tempForm := Form extent: width@1 depth: 32.
	tempBits := tempForm bits.
	pixel := LargePositiveInteger new: 4.
	bitsPerChannel = 8 ifTrue: [
		i := (startX // incX << 2) + 1.
		startX to: width-1 by: incX do: [ :x |
			pixel at: 4 put: (thisScanline at: i+3);
				at: 3 put: (thisScanline at: i);
				at: 2 put: (thisScanline at: i+1);
				at: 1 put: (thisScanline at: i+2).
			tempBits at: x+1 put: pixel normalize.
			i := i + 4.
		]
	] ifFalse: [
		i := (startX // incX << 3) +1.
		startX to: width-1 by: incX do: [ :x |
			pixel at: 4 put: (thisScanline at: i+6);
				at: 3 put: (thisScanline at: i);
				at: 2 put: (thisScanline at: i+2);
				at: 1 put: (thisScanline at: i+4).
			tempBits at: x+1 put: pixel normalize.
			i := i + 8.
		].
	].
	tempForm displayOn: form at: 0@y rule: Form paintAlpha.

! !

!PNGReadWriter methodsFor: 'pixel copies' stamp: 'ar 2/19/2004 00:10'!
rgbaDecoderMapForDepth: decoderDepth
	bigEndian ifTrue:[
		depth = 16 ifTrue:[
			"Big endian, 32 -> 16 color mapping."
			^ColorMap
				shifts: #(-17 -14 -11 0)
				masks: #(16rF8000000 16rF80000 16rF800 16r00)
		] ifFalse:[
			"Big endian, 32 -> 32 color mapping"
			^ColorMap 
				shifts: #(-8 -8 -8 24) 
				masks: #(16rFF000000 16rFF0000 16rFF00 16rFF).
		].
	].
	depth = 16 ifTrue:[
		"Little endian, 32 -> 16 color mapping."
		^ColorMap
			shifts: #(7 -6 -19 0)
			masks: #(16rF8 16rF800 16rF80000 0)
	] ifFalse:[
		"Little endian, 32 -> 32 color mapping"
		^ColorMap 
			shifts: #(-16 0 16 0) 
			masks: #(16rFF0000 16rFF00 16rFF 16rFF000000).
	].! !

!PNGReadWriter methodsFor: 'accessing' stamp: 'RAA 11/7/2000 09:20'!
debugging

	^Debugging == true! !

!PNGReadWriter methodsFor: 'accessing' stamp: 'jmv 12/8/2020 16:33:59'!
nextImage
	bigEndian := Smalltalk isBigEndian.
	filtersSeen _ Bag new.
	globalDataChunk _ nil.
	transparentPixelValue _ nil.
	unknownChunks _ Set new.
	stream reset.
	stream skip: 8.
	[stream atEnd] whileFalse: [self processNextChunk].

	"Set up our form"
	(depth = 8 and: [ palette isNil ]) ifTrue: [
		form _ GrayForm extent: width@height
	] ifFalse: [
		(depth <= 8 and: [ palette notNil ]) ifTrue: [
			form := ColorForm extent: width@height nativeDepthFrom: depth.
			form colors: palette.
		] ifFalse:[
			form := Form extent: width@height depth: depth.
		]
	].

	backColor ifNotNil:[form fillColor: backColor].
	chunk _ globalDataChunk ifNil:[self error: 'image data is missing'].
	chunk ifNotNil: [self processIDATChunk].
	unknownChunks isEmpty ifFalse: [
		"Transcript show: ' ',unknownChunks asSortedCollection asArray printString."
	].
	self debugging ifTrue: [
		Transcript newLine; show: 'form = ',form printString.
		Transcript newLine; show: 'colorType = ',colorType printString.
		Transcript newLine; show: 'interlaceMethod = ',interlaceMethod printString.
		Transcript newLine; show: 'filters = ',filtersSeen sortedCounts asArray printString.
	].
	^ form
! !

!PNGReadWriter methodsFor: 'accessing' stamp: 'DSM 3/24/2000 01:12'!
understandsImageFormat
	#(137 80 78 71 13 10 26 10) do: [ :byte |
		stream next = byte ifFalse: [^ false]].
	^ true
! !

!PNGReadWriter methodsFor: 'miscellaneous' stamp: 'DSM 4/27/2000 13:09'!
doPass: pass
	"Certain interlace passes are skipped with certain small image
dimensions"

	pass = 1 ifTrue: [ ^ true ].
	((width = 1) and: [height = 1]) ifTrue: [ ^ false ].
	pass = 2 ifTrue: [ ^ width >= 5 ].
	pass = 3 ifTrue: [ ^ height >= 5 ].
	pass = 4 ifTrue: [ ^ (width >=3 ) or: [height >= 5] ].
	pass = 5 ifTrue: [ ^ height >=3 ].
	pass = 6 ifTrue: [ ^ width >=2 ].
	pass = 7 ifTrue: [ ^ height >=2 ].

! !

!PNGReadWriter methodsFor: 'miscellaneous' stamp: 'jmv 4/15/2010 10:19'!
grayColorsFor: d
	"return a color table for a gray image"

	palette _ Array new: 1<<d.
	d = 1 ifTrue: [
		palette at: 1 put: Color black.
		palette at: 2 put: Color white.
		^  palette
		].
	d = 2 ifTrue: [
		palette at: 1 put: Color black.
		palette at: 2 put: (Color gray: 85.0 / 255.0).
		palette at: 3 put: (Color gray: 170.0 / 255.0).
		palette at: 4 put: Color white.
		^ palette
		].
	d = 4 ifTrue: [
		0 to: 15 do: [ :g |
			palette at: g+1 put: (Color gray: (g/15) asFloat) ].
		^ palette
		].
	d = 8 ifTrue: [
		0 to: 255 do: [ :g |
			palette at: g+1 put: (Color gray: (g/255) asFloat) ].
		^ palette
		].
! !

!PNGReadWriter methodsFor: 'filtering' stamp: 'RAA 11/7/2000 09:43'!
filterAverage: count
	"Use the average of the pixel to the left and the pixel above as a predictor"

	| delta |
	delta _ bitsPerPixel // 8 max: 1.
	1 to: delta do: [:i |
		thisScanline at: i put: ((thisScanline at: i) + ((prevScanline at: i) // 2) bitAnd: 255)].
	delta + 1 to: count do: [:i |
		thisScanline at: i put:
			((thisScanline at: i)
			+ ((prevScanline at: i)
			+ (thisScanline at: i - delta) // 2) bitAnd: 255)]! !

!PNGReadWriter methodsFor: 'filtering' stamp: 'DSM 3/25/2000 17:54'!
filterHorizontal: count
	"Use the pixel to the left as a predictor"

	| delta |
	delta _ bitsPerPixel // 8 max: 1.
	delta+1 to: count do: [ :i |
		thisScanline at: i put: (((thisScanline at: i) +
(thisScanline at: i-delta)) bitAnd: 255) ]


! !

!PNGReadWriter methodsFor: 'filtering' stamp: 'DSM 3/25/2000 17:55'!
filterNone: count
! !

!PNGReadWriter methodsFor: 'filtering' stamp: 'RAA 11/7/2000 09:45'!
filterPaeth: count
	"Select one of (the pixel to the left, the pixel above and the pixel to above left) to
	predict the value of this pixel"

	| delta |
	delta _ bitsPerPixel // 8 max: 1.
	1 to: delta do: [ :i |
		thisScanline at: i put:
			(((thisScanline at: i) + (prevScanline at: i)) bitAnd: 255)].
	delta+1 to: count do: [ :i |
		thisScanline
			at: i
			put: (((thisScanline at: i) + (self
				paethPredictLeft: (thisScanline at: i-delta)
				above: (prevScanline at: i)
				aboveLeft: (prevScanline at: i-delta)))
					bitAnd: 255)]

! !

!PNGReadWriter methodsFor: 'filtering' stamp: 'eat 9/11/2000 20:08'!
filterScanline: filterType count: count

	self
		perform: (
			#(filterNone: filterHorizontal: filterVertical: filterAverage: filterPaeth:)
				at: filterType+1)
		with: count.

! !

!PNGReadWriter methodsFor: 'filtering' stamp: 'DSM 3/25/2000 17:54'!
filterVertical: count
	"Use the pixel above as a predictor"

	1 to: count do: [ :i |
		thisScanline at: i put: (((thisScanline at: i) +
(prevScanline at: i)) bitAnd: 255) ]

! !

!PNGReadWriter methodsFor: 'filtering' stamp: 'eat 9/11/2000 20:05'!
paethPredictLeft: a above: b aboveLeft: c
	"Predicts the value of a pixel based on nearby pixels, based on
Paeth (GG II, 1991)"

	| pa pb pc |
	pa _ b > c ifTrue: [b - c] ifFalse: [c - b].
	pb _ a > c ifTrue: [a - c] ifFalse: [c - a].
	pc _ a + b - c - c.
	pc < 0 ifTrue: [
		pc := pc * -1].
	((pa <= pb) and: [pa <= pc]) ifTrue: [^ a].
	(pb <= pc) ifTrue: [^ b].
	^ c
! !

!PNGReadWriter methodsFor: 'writing' stamp: 'ar 12/12/2003 16:37'!
nextPutImage: aForm
	"Write out the given form. We're keeping it simple here, no interlacing, no filters."
	^self nextPutImage: aForm interlace: 0 filter: 0. "no filtering"! !

!PNGReadWriter methodsFor: 'writing' stamp: 'jmv 5/13/2016 14:48'!
nextPutImage: aForm interlace: aMethod filter: aFilterType
	"Note: For now we keep it simple - interlace and filtering are simply ignored"
	| crcStream |
	bigEndian := Smalltalk isBigEndian.
	form := aForm.
	width := aForm width.
	height := aForm height.
	(aForm is: #GrayForm) ifTrue: [
		bitsPerChannel := aForm depth.
		colorType := 0.
		bytesPerScanline _ width * aForm depth + 7 // 8.
	] ifFalse: [
		aForm depth <= 8 ifTrue: [
			bitsPerChannel := aForm depth.
			colorType := 3.
			bytesPerScanline _ width * aForm depth + 7 // 8.
		] ifFalse: [
			bitsPerChannel := 8.
			colorType := 6.
			bytesPerScanline _ width * 4.
		]
	].

	self writeFileSignature.

	crcStream := WriteStream on: (ByteArray new: 1000).
	crcStream resetToStart.
	self writeIHDRChunkOn: crcStream.
	self writeChunk: crcStream.

	(form depth <= 8 and: [(aForm is: #GrayForm) not]) ifTrue:[
		crcStream resetToStart.
		self writePLTEChunkOn: crcStream.
		self writeChunk: crcStream.
		(form is: #ColorForm) ifTrue: [
			crcStream resetToStart.
			self writeTRNSChunkOn: crcStream.
			self writeChunk: crcStream.
		].
	].

	form depth = 16 ifTrue:[
		crcStream resetToStart.
		self writeSBITChunkOn: crcStream.
		self writeChunk: crcStream.
	].

	crcStream resetToStart.
	self writeIDATChunkOn: crcStream.
	self writeChunk: crcStream.

	crcStream resetToStart.
	self writeIENDChunkOn: crcStream.
	self writeChunk: crcStream.! !

!PNGReadWriter methodsFor: 'writing' stamp: 'nk 2/17/2004 16:51'!
updateCrc: oldCrc from: start to: stop in: aCollection
	^ZipWriteStream updateCrc: oldCrc from: start to: stop in: aCollection! !

!PNGReadWriter methodsFor: 'writing' stamp: 'jmv 2/26/2016 17:06'!
writeChunk: crcStream
	| bytes length crc debug |
	debug := self debugging.
	bytes := crcStream originalContents.
	length := crcStream position.
	crc := self updateCrc: 16rFFFFFFFF from: 1 to: length in: bytes.
	crc := crc bitXor: 16rFFFFFFFF.
	debug ifTrue: [
		Transcript newLine;
			print: stream position; space;
			nextPutAll: (bytes copyFrom: 1 to: 4) asString;
			nextPutAll: ' len='; print: length;
			nextPutAll: ' crc=0x'; nextPutAll: crc printStringHex  ].
	stream nextUnsignedInt32Put: length-4 bigEndian: true. "exclude chunk name"
	stream next: length putAll: bytes startingAt: 1.
	stream nextUnsignedInt32Put: crc bigEndian: true.
	debug ifTrue: [ Transcript nextPutAll: ' afterPos='; print: stream position ].
	crcStream resetToStart.! !

!PNGReadWriter methodsFor: 'writing' stamp: 'nice 1/18/2010 18:08'!
writeFileSignature
	stream nextPutAll: #[ 16r89 16r50 16r4E  16r47 16r0D 16r0A 16r1A 16r0A ]! !

!PNGReadWriter methodsFor: 'writing' stamp: 'jmv 3/12/2017 19:19:47'!
writeIDATChunkOn: aStream
	"Write the IDAT chunk"
	| z |
	aStream nextPutAll: 'IDAT' asByteArray.

	"Avoid growing multiple times. do a rough estimation of space needed"
	aStream growTo: bytesPerScanline * height *11//20.

	z _ ZLibWriteStream on: aStream.
	form depth <= 8 
		ifTrue: [ 
			"Same format for type 3 (indexed color) and type 0 (grayscale, GrayForm)"
			self writeType3DataOn: z ]
		ifFalse: [ self writeType6DataOn: z ].
	z close.
	self debugging ifTrue: [
		Transcript newLine;
			nextPutAll: 'compressed size=';
			print: aStream position;
			nextPutAll: ' uncompressed size=';
			print: z position ]! !

!PNGReadWriter methodsFor: 'writing' stamp: 'ar 12/12/2003 17:08'!
writeIENDChunkOn: aStream
	"Write the IEND chunk"
	aStream nextPutAll: 'IEND' asByteArray.! !

!PNGReadWriter methodsFor: 'writing' stamp: 'jmv 2/26/2016 17:06'!
writeIHDRChunkOn: aStream
	"Write the IHDR chunk"
	aStream nextPutAll: 'IHDR' asByteArray.
	aStream nextSignedInt32Put: width bigEndian: true.
	aStream nextSignedInt32Put: height bigEndian: true.
	aStream nextPut: bitsPerChannel.
	aStream nextPut: colorType.
	aStream nextPut: 0. "compression"
	aStream nextPut: 0. "filter method"
	aStream nextPut: 0. "interlace method"
! !

!PNGReadWriter methodsFor: 'writing' stamp: 'jmv 12/8/2020 16:35:46'!
writePLTEChunkOn: aStream
	"Write the PLTE chunk.
	Ignore any form of translucency."
	| r g b colors |
	aStream nextPutAll: 'PLTE' asByteArray.
	(form is: #ColorForm) 
		ifTrue: [ colors := form colors]
		ifFalse: [ colors := Color indexedColors copyFrom: 1 to: (1 bitShift: form depth)].
	colors do: [ :aColor |
		r := (aColor red * 255) truncated.
		g := (aColor green * 255) truncated.
		b := (aColor blue * 255) truncated.
		aStream nextPut: r; nextPut: g; nextPut: b.
	].! !

!PNGReadWriter methodsFor: 'writing' stamp: 'ar 12/12/2003 18:29'!
writeSBITChunkOn: aStream
	"Write the IDAT chunk"
	aStream nextPutAll: 'sBIT' asByteArray.
	form depth = 16 ifFalse:[self error: 'Unimplemented feature'].
	aStream nextPut: 5.
	aStream nextPut: 5.
	aStream nextPut: 5.
	aStream nextPut: 1.! !

!PNGReadWriter methodsFor: 'writing' stamp: 'ar 12/12/2003 17:34'!
writeTRNSChunkOn: aStream
	"Write out tRNS chunk"
	aStream nextPutAll: 'tRNS' asByteArray.
	form colors do:[:aColor|
		aStream nextPut: (aColor alpha * 255) truncated.
	].! !

!PNGReadWriter methodsFor: 'writing' stamp: 'jmv 10/3/2015 14:18'!
writeType3DataOn: zStream
	"Write color indexed data."
	| scanline hack hackBlt swizzleBlt swizzleHack hackDepth |
	scanline := ByteArray new: bytesPerScanline + 3 // 4 * 4.
	hackDepth := bigEndian ifTrue:[form depth] ifFalse:[form depth negated].
	hack := Form extent: width@1 depth: hackDepth bits: scanline.
	hackBlt := BitBlt toForm: hack.
	hackBlt sourceForm: form.
	hackBlt combinationRule: Form over.
	hackBlt destOrigin: 0@0.
	hackBlt width: width; height: 1.
	(form depth < 8 and:[bigEndian not]) ifTrue:[
		swizzleHack := Form new hackBits: scanline.
		swizzleBlt := BitBlt toForm: swizzleHack.
		swizzleBlt sourceForm: swizzleHack.
		swizzleBlt combinationRule: Form over.
		swizzleBlt colorMap: (StandardSwizzleMaps at: form depth).
	].
	0 to: height-1 do:[:i|
		hackBlt sourceOrigin: 0@i; copyBits.
		swizzleBlt ifNotNil:[swizzleBlt copyBits].
		zStream nextPut: 0. "filterType"
		zStream next: bytesPerScanline putAll: scanline startingAt: 1.
	]! !

!PNGReadWriter methodsFor: 'writing' stamp: 'jmv 10/3/2015 14:19'!
writeType6DataOn: zStream
	"Write RGBA data."
	| scanline hack hackBlt cm miscBlt |
	scanline := ByteArray new: bytesPerScanline.
	hack := Form extent: width@1 depth: 32 bits: scanline.
	form depth = 16 ifTrue:[
		"Expand 16 -> 32"
		miscBlt := BitBlt toForm: hack.
		miscBlt sourceForm: form.
		miscBlt combinationRule: Form over.
		miscBlt destOrigin: 0@0.
		miscBlt width: width; height: 1.
	].
	hackBlt := BitBlt toForm: hack.
	hackBlt sourceForm: (miscBlt ifNil:[form] ifNotNil:[hack]).
	hackBlt combinationRule: Form over.
	hackBlt destOrigin: 0@0.
	hackBlt width: width; height: 1.
	bigEndian ifTrue:[
		cm := ColorMap 
			shifts: #(8 8 8 -24) 
			masks: #(16rFF0000 16rFF00 16rFF 16rFF000000).
	] ifFalse:[
		cm := ColorMap 
			shifts: #(-16 0 16 0) 
			masks: #(16rFF0000 16rFF00 16rFF 16rFF000000).
	].
	hackBlt colorMap: cm.
	0 to: height-1 do:[:i|
		miscBlt ifNil:[
			hackBlt sourceOrigin: 0@i; copyBits.
		] ifNotNil:[
			miscBlt sourceOrigin: 0@i; copyBits.
			hack fixAlpha.
			hackBlt copyBits.
		].
		zStream nextPut: 0. "filterType"
		zStream nextPutAll: scanline.
	]! !

!PNGReadWriter methodsFor: 'chunks' stamp: 'ar 2/10/2004 23:55'!
processBackgroundChunk

	| val red green blue max |

	"Transcript show: '  BACKGROUND: ',chunk printString."
	colorType = 3 ifTrue: [
		backColor := palette at: chunk first + 1.
		^self
	].
	max _ (2 raisedTo: bitsPerChannel) - 1.
	(colorType = 0 or: [colorType = 4]) ifTrue: [
		val _ chunk unsignedShortAt: 1 bigEndian: true.
		backColor := Color gray: val / max.
		^self
	].
	(colorType = 2 or: [colorType = 6]) ifTrue: [
		red _ chunk unsignedShortAt: 1 bigEndian: true.
		green _ chunk unsignedShortAt: 3 bigEndian: true.
		blue _ chunk unsignedShortAt: 5 bigEndian: true.
		backColor := Color r: red/max g: green/max b: blue/max.
		^self
	].
"self halt."

"====
The bKGD chunk specifies a default background color to present the image against. Note that viewers are not bound to honor this chunk; a viewer can choose to use a different background. 

For color type 3 (indexed color), the bKGD chunk contains: 


   Palette index:  1 byte

The value is the palette index of the color to be used as background. 

For color types 0 and 4 (grayscale, with or without alpha), bKGD contains: 


   Gray:  2 bytes, range 0 .. (2^bitdepth)-1

(For consistency, 2 bytes are used regardless of the image bit depth.) The value is the gray level to be used as background. 

For color types 2 and 6 (truecolor, with or without alpha), bKGD contains: 


   Red:   2 bytes, range 0 .. (2^bitdepth)-1
   Green: 2 bytes, range 0 .. (2^bitdepth)-1
   Blue:  2 bytes, range 0 .. (2^bitdepth)-1

(For consistency, 2 bytes per sample are used regardless of the image bit depth.) This is the RGB color to be used as background. 

When present, the bKGD chunk must precede the first IDAT chunk, and must follow the PLTE chunk, if any. 
==="
! !

!PNGReadWriter methodsFor: 'chunks' stamp: 'RAA 11/4/2000 17:00'!
processIDATChunk

	interlaceMethod = 0
		ifTrue: [ self processNonInterlaced ]
		ifFalse: [ self processInterlaced ]
! !

!PNGReadWriter methodsFor: 'chunks' stamp: 'jmv 5/13/2016 15:04'!
processIHDRChunk
	width _ chunk longAt: 1 bigEndian: true.
	height _ chunk longAt: 5 bigEndian: true.
	bitsPerChannel _ chunk at: 9.
	colorType _ chunk at: 10.
	"compression _ chunk at: 11." "TODO - validate compression"
	"filterMethod _ chunk at: 12." "TODO - validate filterMethod"
	interlaceMethod _ chunk at: 13. "TODO - validate interlace method"
	(#(2 4 6) includes: colorType)
		ifTrue: [depth _ 32].
	(colorType = 0 and: [ bitsPerChannel = 8 ]) ifTrue: [
		depth _ 8.
		palette _ nil.		"Meaning: 'please use a GrayForm'"
	] ifFalse: [
		(#(0 3) includes: colorType) ifTrue: [
			depth _ bitsPerChannel min: 8.
			colorType = 0 ifTrue: [ "grayscale"
				palette := self grayColorsFor: depth ]]].
	bitsPerPixel _ (BPP at: colorType+1) at: bitsPerChannel highBit.
	bytesPerScanline _ width * bitsPerPixel + 7 // 8.
	rowSize _ width * depth + 31 >> 5.
! !

!PNGReadWriter methodsFor: 'chunks' stamp: 'nice 12/26/2009 19:05'!
processInterlaced
	| z startingCol colIncrement rowIncrement startingRow |
	startingCol := #(0 4 0 2 0 1 0 ).
	colIncrement := #(8 8 4 4 2 2 1 ).
	rowIncrement := #(8 8 8 4 4 2 2 ).
	startingRow := #(0 0 4 0 2 0 1 ).
	z := ZLibReadStream on: chunk from: 1 to: chunk size.
	1 to: 7 do: [:pass |
		| cx sc bytesPerPass |
		(self doPass: pass)
			ifTrue:
				[cx := colIncrement at: pass.
				sc := startingCol at: pass.
				bytesPerPass := width - sc + cx - 1 // cx * bitsPerPixel + 7 // 8.
				prevScanline := ByteArray new: bytesPerPass.
				thisScanline := ByteArray new: bytesPerScanline.
				(startingRow at: pass)
					to: height - 1
					by: (rowIncrement at: pass)
					do: [:y |
						| filter temp |
						filter := z next.
						filtersSeen add: filter.
						(filter isNil or: [(filter between: 0 and: 4) not])
							ifTrue: [^ self].
						thisScanline := z next: bytesPerPass into: thisScanline startingAt: 1.
						self filterScanline: filter count: bytesPerPass.
						self copyPixels: y at: sc by: cx.
						temp := prevScanline.
						prevScanline := thisScanline.
						thisScanline := temp.
					]
				]
	].
	z atEnd ifFalse:[self error:'Unexpected data'].! !

!PNGReadWriter methodsFor: 'chunks' stamp: 'jmv 2/29/2016 11:19'!
processNextChunk

	| length chunkType crc chunkCrc |

	length _ self nextUnsignedInt32.
	chunkType _ (self next: 4) asString.

	"If someone appended garbage at the end of the file, don't let that bite us."
	fileSize - self position < length ifTrue: [
		unknownChunks add: chunkType.
		stream setToEnd.
		^self ].

	chunk _ self next: length.
	chunkCrc := self nextUnsignedInt32 bitXor: 16rFFFFFFFF.
	crc := self updateCrc: 16rFFFFFFFF from: 1 to: 4 in: chunkType.
	crc := self updateCrc: crc from: 1 to: length in: chunk.
	crc = chunkCrc ifFalse:[
		self error: 'PNGReadWriter crc error in chunk ', chunkType.
	].

	chunkType = 'IEND' ifTrue: [^self	"*should* be the last chunk"].
	chunkType = 'sBIT' ifTrue: [^self processSBITChunk "could indicate unusual sample depth in original"].
	chunkType = 'gAMA' ifTrue: [^self 	"indicates gamma correction value"].
	chunkType = 'bKGD' ifTrue: [^self processBackgroundChunk].
	chunkType = 'pHYs' ifTrue: [^self processPhysicalPixelChunk].
	chunkType = 'tRNS' ifTrue: [^self processTransparencyChunk].

	chunkType = 'IHDR' ifTrue: [^self processIHDRChunk].
	chunkType = 'PLTE' ifTrue: [^self processPLTEChunk].
	chunkType = 'IDAT' ifTrue: [
		"---since the compressed data can span multiple
		chunks, stitch them all together first. later,
		if memory is an issue, we need to figure out how
		to do this on the fly---"
		globalDataChunk _ globalDataChunk ifNil: [chunk] ifNotNil:
			[globalDataChunk,chunk].
		^self
	].
	unknownChunks add: chunkType.
! !

!PNGReadWriter methodsFor: 'chunks' stamp: 'jmv 3/13/2012 12:42'!
processNonInterlaced
	| z filter temp copyMethod debug |
	debug := self debugging.
	copyMethod _ #(copyPixelsGray: nil copyPixelsRGB: copyPixelsIndexed:
		  copyPixelsGrayAlpha: nil copyPixelsRGBA:) at: colorType+1.
	debug ifTrue: [ Transcript newLine; nextPutAll: 'NI chunk size='; print: chunk size ].
	z _ ZLibReadStream on: chunk from: 1 to: chunk size.
	prevScanline _ ByteArray new: bytesPerScanline.
	thisScanline := ByteArray new: bytesPerScanline.	

	(colorType = 0 and: [ bitsPerChannel < 16]) ifTrue: [
		auxSource _ Form extent: 1 @ (thisScanline size // 4) depth: 32 bits: thisScanline.
		auxDest _ Form extent: 1 @ (form bits size) depth: 32 bits: form bits.
		auxCMap _ Smalltalk isLittleEndian
			ifTrue:[ColorMap 
					shifts: #(-24 -8 8 24) 
					masks: #(16rFF000000 16r00FF0000 16r0000FF00 16r000000FF)].
		auxBitBlt _ (BitBlt toForm: auxDest)
			sourceForm: auxSource;
			colorMap: auxCMap;
			combinationRule: 3 ].

	0 to: height-1 do: [ :y |
		filter _ (z next: 1) first.
		debug ifTrue:[filtersSeen add: filter].
		thisScanline _ z next: bytesPerScanline into: thisScanline startingAt: 1.
		(debug and: [ thisScanline size < bytesPerScanline ]) ifTrue: [
			Transcript nextPutAll: ('wanted {1} but only got {2}' format: { bytesPerScanline. thisScanline size }); newLine ].
		filter = 0 ifFalse:[self filterScanline: filter count: bytesPerScanline].
		self perform: copyMethod with: y.
		temp := prevScanline.
		prevScanline := thisScanline.
		thisScanline := temp.
		].
	z atEnd ifFalse:[self error:'Unexpected data'].
	debug ifTrue: [Transcript  nextPutAll: ' compressed size='; print: z position  ].
! !

!PNGReadWriter methodsFor: 'chunks' stamp: 'ar 2/11/2004 01:02'!
processPLTEChunk

	| colorCount i |

	colorCount _ chunk size // 3. "TODO - validate colorCount against depth"
	palette _ Array new: colorCount.
	0 to: colorCount-1 do: [ :index |
		i _ index * 3 + 1.
		palette at: index+1 put:
			(Color r: (chunk at: i)/255.0 g: (chunk at: i+1)/255.0 b: (chunk at: i+2)/255.0)
		].! !

!PNGReadWriter methodsFor: 'chunks' stamp: 'RAA 11/5/2000 11:24'!
processPhysicalPixelChunk

	"Transcript show: '  PHYSICAL: ',chunk printString."
! !

!PNGReadWriter methodsFor: 'chunks' stamp: 'ar 12/12/2003 18:33'!
processSBITChunk
	| rBits gBits bBits aBits |
	colorType = 6 ifFalse:[^self].
	rBits := chunk at: 1.
	gBits := chunk at: 2.
	bBits := chunk at: 3.
	aBits := chunk at: 4.
	(rBits = 5 and:[gBits = 5 and:[bBits = 5 and:[aBits = 1]]]) ifTrue:[
		depth := 16.
	].! !

!PNGReadWriter methodsFor: 'chunks' stamp: 'jmv 1/18/2017 11:52:19'!
processTransparencyChunk

	"Transcript show: '  TRANSPARENCY ',chunk printString."
	colorType = 0 ifTrue: [
		transparentPixelValue _ chunk unsignedShortAt: 1 bigEndian: true.
		bitsPerChannel <= 8
			ifTrue: [
				"If palette is nil, we are building a GrayForm from a grayscale 8bpp file. Ignore any transparency chunk!!"
				palette ifNotNil: [
					palette at: transparentPixelValue + 1 put: Color transparent ]]
			ifFalse: [
				palette at: 1 put: Color transparent ].
		^ self ].
	colorType = 2 ifTrue: [
		| red green blue |
		red _ chunk unsignedShortAt: 1 bigEndian: true.
		green _ chunk unsignedShortAt: 3 bigEndian: true.
		blue _ chunk unsignedShortAt: 5 bigEndian: true.
		transparentPixelValue _ bitsPerChannel <= 8
			ifTrue: [ 16rFF00 + red << 8 + green << 8 + blue ]
			ifFalse: [ red << 16 + green << 16 + blue ].
		^ self ].
	colorType = 3 ifTrue: [
		chunk withIndexDo: [ :alpha :index |
			palette at: index put: ((palette at: index) alpha: alpha / 255) ].
		^ self ]! !

!PNGReadWriter class methodsFor: 'as yet unclassified' stamp: 'ar 2/11/2004 00:54'!
computeSwizzleMapForDepth: depth
	"Answer a map that maps pixels in a word to their opposite location. Used for 'middle-endian' forms where the byte-order is different from the bit order (good joke, eh?)."
	| map swizzled |
	map := Bitmap new: 256.
	depth = 4 ifTrue:[
		0 to: 255 do:[:pix|
			swizzled := 0.
			swizzled := swizzled bitOr: (((pix bitShift: 0) bitAnd: 15) bitShift: 4).
			swizzled := swizzled bitOr: (((pix bitShift: -4) bitAnd: 15) bitShift: 0).
			map at: pix+1 put: swizzled.
		].
		^ColorMap colors: map
	].

	depth = 2 ifTrue:[
		0 to: 255 do:[:pix|
			swizzled := 0.
			swizzled := swizzled bitOr: (((pix bitShift: 0) bitAnd: 3) bitShift: 6).
			swizzled := swizzled bitOr: (((pix bitShift: -2) bitAnd: 3) bitShift: 4).
			swizzled := swizzled bitOr: (((pix bitShift: -4) bitAnd: 3) bitShift: 2).
			swizzled := swizzled bitOr: (((pix bitShift: -6) bitAnd: 3) bitShift: 0).
			map at: pix+1 put: swizzled.
		].
		^ColorMap colors: map
	].

	depth = 1 ifTrue:[
		0 to: 255 do:[:pix|
			swizzled := 0.
			swizzled := swizzled bitOr: (((pix bitShift: 0) bitAnd: 1) bitShift: 7).
			swizzled := swizzled bitOr: (((pix bitShift: -1) bitAnd: 1) bitShift: 6).
			swizzled := swizzled bitOr: (((pix bitShift: -2) bitAnd: 1) bitShift: 5).
			swizzled := swizzled bitOr: (((pix bitShift: -3) bitAnd: 1) bitShift: 4).
			swizzled := swizzled bitOr: (((pix bitShift: -4) bitAnd: 1) bitShift: 3).
			swizzled := swizzled bitOr: (((pix bitShift: -5) bitAnd: 1) bitShift: 2).
			swizzled := swizzled bitOr: (((pix bitShift: -6) bitAnd: 1) bitShift: 1).
			swizzled := swizzled bitOr: (((pix bitShift: -7) bitAnd: 1) bitShift: 0).
			map at: pix+1 put: swizzled.
		].
		^ColorMap colors: map
	].
	self error: 'Unrecognized depth'! !

!PNGReadWriter class methodsFor: 'as yet unclassified' stamp: 'RAA 11/7/2000 09:20'!
debugging: aBoolean

	Debugging _ aBoolean! !

!PNGReadWriter class methodsFor: 'as yet unclassified' stamp: 'jmv 12/8/2020 16:34:09'!
initialize
	"
	PNGReadWriter initialize
	"

	BPP _ {	#(1 2 4 8 16).
			#(0 0 0 0 0).
			#(0 0 0 24 48).
			#(1 2 4 8 0).
			#(0 0 0 16 32).
			#(0 0 0 0 0).
			#(0 0 0 32 64).
			#(0 0 0 0 0) }.

	BlockHeight _ #(8 8 4 4 2 2 1).
	BlockWidth _ #(8 4 4 2 2 1 1).

	StandardSwizzleMaps := Array new: 4.
	#(1 2 4) do:[:i| StandardSwizzleMaps at: i put: (self computeSwizzleMapForDepth: i)].! !

!PNGReadWriter class methodsFor: 'image reading/writing' stamp: 'nk 7/16/2003 17:57'!
typicalFileExtensions
	"Answer a collection of file extensions (lowercase) which files that I can read might commonly have"
	^#('png')! !

!TIFFReadWriter methodsFor: 'private' stamp: 'jmv 12/21/2013 09:26'!
binaryStream: aPositionableStream
	"Reset the given Stream, wrap it in a TIFFStream, and set it as my stream."

	| bigEndian |
	super binaryStream: aPositionableStream.
	bigEndian _ (aPositionableStream peek) = 16r4D. 
		"Assume 16r49 or 16r4D. Bad first byte caught later by TIFFStructure>>validateHeaderOf:"
	stream _ TIFFStream 
		on: aPositionableStream 
		bigEndian: bigEndian.! !

!TIFFReadWriter methodsFor: 'accessing' stamp: 'jmv 4/8/2015 15:39'!
nextImage
	"Decode an image on my stream and answer the image as a Form."
	^ self reader readForm! !

!TIFFReadWriter methodsFor: 'accessing' stamp: 'mrm 7/22/2001 18:46'!
nextPutImage: aForm
	"Encode aForm into TIFF form and write on my stream."

	^self error: 'Not Yet Implemented.'.! !

!TIFFReadWriter methodsFor: 'accessing' stamp: 'jmv 4/8/2015 15:39'!
reader
	| reader |
	reader _ TIFFReader onTiffStream: stream.
	^ reader! !

!TIFFReadWriter methodsFor: 'testing' stamp: 'mrm 7/22/2001 19:07'!
understandsImageFormat
	"Test to see if the image stream format is understood by this decoder."
	"Return true if this stream appears to contain a TIFF file, otherwise 
	return false."

	"Check for Big-endian header."
	(self hasMagicNumber: 
		(ByteArray with: 16r4D with: 16r4D with: 16r00 with: 16r2A) )
		ifTrue: [ ^ true].

	"Check for little-endian header."
	^ (self hasMagicNumber: 
		(ByteArray with: 16r49 with: 16r49 with: 16r2A with: 16r00) ).! !

!TIFFField methodsFor: 'private' stamp: 'mrm 7/28/2001 13:47'!
isSingleValued
	"Answer true iff my tag is one that should only have one value, and I only have one value.
	Raise an error if I should have exactly one value but I have some other number of values."
	| singleValued |
	singleValued _ SingleValuedTagSymbols includes: self tagSymbol.
	(singleValued and: [values size ~= 1])
		ifTrue: [self error: 'Field not single-valued that should be.'].
	^ singleValued.! !

!TIFFField methodsFor: 'private' stamp: 'jmv 2/29/2016 11:22'!
parseFrom: aTIFFStream
	"Fill in my state with the results from parsing the given TIFFStream from its current position. Leave the stream positioned after the field, which will be the beginning of the next field, if any. Answer nil if the field has an unknown tag, otherwise answer self."

	| selector |
	tag _ aTIFFStream nextUnsignedInt16.
	(TagSymbols includesKey: tag) 
		ifFalse: [aTIFFStream skip: 10. "Move past the type, count, and value/offset."
				^ nil].
	type _ aTIFFStream nextUnsignedInt16.
	selector _ ValueReaderSelectors 
		at: type 
		ifAbsent: "Can't read this type, ignore the field."
			[aTIFFStream skip: 8. "Move past the value/offset."
			^ nil].
	self perform: selector with: aTIFFStream.
	! !

!TIFFField methodsFor: 'private' stamp: 'mrm 7/28/2001 16:27'!
tag: tagInteger type: typeInteger values: valueArray
	"Set my state to the given values."
	tag _ tagInteger.
	type _ typeInteger.
	values _ valueArray.! !

!TIFFField methodsFor: 'private-value reading' stamp: 'jmv 9/5/2016 16:57:57'!
read: count charsFrom: aTIFFStream
	"Read count bytes from the current position of aTIFFStream and return them as an Array of Strings, using NUL as the separating character between strings. Don't worry about the ending position of aTIFFStream."
	| strings oneString size |
	strings _ OrderedCollection new.
	oneString _ OrderedCollection new.
	1 to: count do: [ :i |
		aTIFFStream peek = 0
			ifFalse: [
				oneString add: (Character numericValue: aTIFFStream nextByte) ]
			ifTrue: [ "Null, so that's the end of this string. "
				size _ oneString size.
				strings add: ((String new: size) replaceFrom: 1 to: size with: oneString) ]].
	^ strings asArray! !

!TIFFField methodsFor: 'private-value reading' stamp: 'jmv 2/29/2016 11:19'!
readAsciiValuesFrom: aTIFFStream
	"Read the count long and count number of characters from aTIFFStream at its current position according to my type. Store the result as one or more Strings in my array of values, using the NUL character to detect separation between strings. Leave aTIFFStream positioned eight bytes past its starting position."

	| count |
	count _ aTIFFStream nextUnsignedInt32.
	values _ (count <= 4) "Offset is immediate data, not pointer to data."
		ifTrue: [self readImmediate: count charsFrom: aTIFFStream.]
		ifFalse: [self readIndirect: count charsFrom: aTIFFStream.].! !

!TIFFField methodsFor: 'private-value reading' stamp: 'jmv 2/29/2016 11:19'!
readByteValuesFrom: aTIFFStream
	"Read the count long and count number of bytes from aTIFFStream at its current position according to my type. Store the result as my array of values. Leave aTIFFStream positioned eight bytes past its starting position."

	| count |
	count _ aTIFFStream nextUnsignedInt32.
	values _ Array new: count.
	(count <= 4) "Offset is immediate data, not pointer to data."
		ifTrue: [self readImmediate: count bytesFrom: aTIFFStream.]
		ifFalse: [self readIndirect: count bytesFrom: aTIFFStream.].
! !

!TIFFField methodsFor: 'private-value reading' stamp: 'jmv 4/7/2015 16:32'!
readImmediate: count bytesFrom: aTIFFStream
	"Put the next count bytes in aTIFFStream into my values array, starting from values at: 1. Leave aTIFFStream positioned four bytes past its starting position. Count must be <= 4."
	| position |
	position _ aTIFFStream position.
	1 to: count do: [ :i | values at: i put: aTIFFStream nextByte].
	aTIFFStream position: position + 4.
	! !

!TIFFField methodsFor: 'private-value reading' stamp: 'mrm 7/27/2001 16:56'!
readImmediate: count charsFrom: aTIFFStream
	"Read the next count bytes in aTIFFStream and return an Array of Strings, using NUL as the separating character between strings. Leave aTIFFStream positioned four bytes past its starting position. Count must be <= 4."
	| position result |
	position _ aTIFFStream position.
	result _ self read: count charsFrom: aTIFFStream.
	aTIFFStream position: position + 4.
	^ result.! !

!TIFFField methodsFor: 'private-value reading' stamp: 'jmv 2/29/2016 11:19'!
readImmediate: count longsFrom: aTIFFStream
	"Put the next count 32-bit longs in aTIFFStream into my values array, starting from values at: 1. Leave aTIFFStream positioned four bytes past its starting position. Count must be <= 1."
	| position |
	position _ aTIFFStream position.
	1 to: count do: [:i | values at: i put: aTIFFStream nextUnsignedInt32].
	aTIFFStream position: position + 4.
	! !

!TIFFField methodsFor: 'private-value reading' stamp: 'jmv 2/29/2016 11:21'!
readImmediate: count shortsFrom: aTIFFStream
	"Put the next count 16-bit shorts in aTIFFStream into my values array, starting from values at: 1. Leave aTIFFStream positioned four bytes past its starting position. Count must be <= 2."
	| position |
	position _ aTIFFStream position.
	1 to: count do: [:i | values at: i put: aTIFFStream nextUnsignedInt16].
	aTIFFStream position: position + 4.
	! !

!TIFFField methodsFor: 'private-value reading' stamp: 'jmv 2/29/2016 11:19'!
readIndirect: count bytesFrom: aTIFFStream
	"Read a long offset from aTIFFStream, then read count bytes from that offset in aTIFFStream into my values array, starting from values at: 1. Leave aTIFFStream positioned four bytes past its starting position."
	| offset position |
	offset _ aTIFFStream nextUnsignedInt32.
	position _ aTIFFStream position.
	aTIFFStream position: offset.
	1 to: count do: [:i | values at: i put: aTIFFStream nextByte].
	aTIFFStream position: position.! !

!TIFFField methodsFor: 'private-value reading' stamp: 'jmv 2/29/2016 11:19'!
readIndirect: count charsFrom: aTIFFStream
	"Read a long offset from aTIFFStream, then read count bytes from that offset in aTIFFStream and return them as an Array of Strings, using NUL as the separating character between strings. Leave aTIFFStream positioned four bytes past its starting position."
	| offset position result |
	offset _ aTIFFStream nextUnsignedInt32.
	position _ aTIFFStream position.
	aTIFFStream position: offset.
	result _ self read: count charsFrom: aTIFFStream.
	aTIFFStream position: position.
	^ result.! !

!TIFFField methodsFor: 'private-value reading' stamp: 'jmv 2/29/2016 11:20'!
readIndirect: count longsFrom: aTIFFStream
	"Read a 32-bit offset from aTIFFStream, then read count 32-bit longs from that offset in aTIFFStream into my values array, starting from values at: 1. Leave aTIFFStream positioned four bytes past its starting position."
	| offset position |
	offset _ aTIFFStream nextUnsignedInt32.
	position _ aTIFFStream position.
	aTIFFStream position: offset.
	1 to: count do: [:i | values at: i put: aTIFFStream nextUnsignedInt32].
	aTIFFStream position: position.! !

!TIFFField methodsFor: 'private-value reading' stamp: 'jmv 2/29/2016 11:20'!
readIndirect: count rationalsFrom: aTIFFStream
	"Read a 32-bit offset from aTIFFStream, then read count 32-bit longs from that offset in aTIFFStream into my values array, starting from values at: 1. Leave aTIFFStream positioned four bytes past its starting position."
	| offset position |
	offset _ aTIFFStream nextUnsignedInt32.
	position _ aTIFFStream position.
	aTIFFStream position: offset.
	1 to: count do: [:i | values at: i put: (aTIFFStream nextUnsignedInt32 / aTIFFStream nextUnsignedInt32)].
	aTIFFStream position: position.! !

!TIFFField methodsFor: 'private-value reading' stamp: 'jmv 2/29/2016 11:21'!
readIndirect: count shortsFrom: aTIFFStream
	"Read a long offset from aTIFFStream, then read count 16-bit shorts from that offset in aTIFFStream into my values array, starting from values at: 1. Leave aTIFFStream positioned four bytes past its starting position."
	| offset position |
	offset _ aTIFFStream nextUnsignedInt32.
	position _ aTIFFStream position.
	aTIFFStream position: offset.
	1 to: count do: [:i | values at: i put: aTIFFStream nextUnsignedInt16].
	aTIFFStream position: position.! !

!TIFFField methodsFor: 'private-value reading' stamp: 'jmv 2/29/2016 11:20'!
readLongValuesFrom: aTIFFStream
	"Read the 32-bit count and count number of 32-bit values from aTIFFStream at its current position according to my type. Store the result as my array of values. Leave aTIFFStream positioned eight bytes past its starting position."

	| count |
	count _ aTIFFStream nextUnsignedInt32.
	values _ Array new: count.
	(count <= 1) "Offset is immediate data, not pointer to data."
		ifTrue: [self readImmediate: count longsFrom: aTIFFStream.]
		ifFalse: [self readIndirect: count longsFrom: aTIFFStream.].
! !

!TIFFField methodsFor: 'private-value reading' stamp: 'jmv 2/29/2016 11:20'!
readRationalValuesFrom: aTIFFStream
	"Read the 32-bit count and count number of 64-bit rational values from aTIFFStream at its current position according to my type. Store the result as my array of values. Leave aTIFFStream positioned eight bytes past its starting position."

	| count |
	count _ aTIFFStream nextUnsignedInt32.
	values _ Array new: count.
	"Can't fit even one rational into 32 bits, so always indirect, never immediate."
	self readIndirect: count rationalsFrom: aTIFFStream.
! !

!TIFFField methodsFor: 'private-value reading' stamp: 'jmv 2/29/2016 11:20'!
readShortValuesFrom: aTIFFStream
	"Read the 32-bit count and count number of 16-bit shorts from aTIFFStream at its current position according to my type. Store the result as my array of values. Leave aTIFFStream positioned eight bytes past its starting position."

	| count |
	count _ aTIFFStream nextUnsignedInt32.
	values _ Array new: count.
	(count <= 2) "Offset is immediate data, not pointer to data."
		ifTrue: [self readImmediate: count shortsFrom: aTIFFStream.]
		ifFalse: [self readIndirect: count shortsFrom: aTIFFStream.].
! !

!TIFFField methodsFor: 'accessing' stamp: 'mrm 7/27/2001 17:50'!
tag
	"Return the tag integer that indicates what kind of TIFF field this is."
	^ tag.
! !

!TIFFField methodsFor: 'accessing' stamp: 'mrm 7/31/2001 20:12'!
tagSymbol
	"Answer the Symbol that indicates what kind of TIFF field this is.
	If it's an unknown tag, answer nil."
	^ TagSymbols at: tag ifAbsent: [nil].
! !

!TIFFField methodsFor: 'accessing' stamp: 'mrm 7/28/2001 13:38'!
value
	"Answer my value. All TIFF fields are structured with array values, but many tags never have an array with more than one value. Thus, I answer the sole value for such tags, and answer an array for tags that can have more than one value.
	I report an error if I have more than one value for a tag that should only have one value."

	^ (self isSingleValued) 
		ifTrue: [values at: 1]
		ifFalse: [values].! !

!TIFFField class methodsFor: 'defaults' stamp: 'mrm 7/28/2001 15:53'!
defaultFor: tagSymbol
		"Answer the default field for the given tag Symbol. Raise an error if there is no default field for the given tag Symbol."
	^ DefaultFields 
		at: tagSymbol
		ifAbsent: [self error: 'No default value for this tag.']. ! !

!TIFFField class methodsFor: 'initializing' stamp: 'mrm 7/28/2001 15:56'!
initialize
	"TIFFField initialize."

	self initializeValueReaderSelectors.
	self initializeTagSymbols.
	self initializeSingleValuedTagSymbols.
	self initializeDefaultFields.! !

!TIFFField class methodsFor: 'initializing' stamp: 'jmv 1/12/2011 15:58'!
initializeDefaultFields
	"DefaultFields				Dictionary -- maps tag symbols to the default field with that tag. Only maps tag symbols for which the TIFF spec defines a default."

	DefaultFields _ Dictionary new.
	DefaultFields
		at: #BitsPerSample "1-bit bi-level image."
		put: (TIFFField tag: 258 type: 3 values: (Array with: 1));

		at: #Compression "No compression"
		put: (TIFFField tag: 259 type: 3 values: (Array with: 1));

		at: #Predictor "No predictor"
		put: (TIFFField tag: 317 type: 3 values: (Array with: 1));
		
		at: #FillOrder "Lower column values in higher-order bits"
		put: (TIFFField tag: 266 type: 3 values: (Array with: 1));
		
		at: #GrayResponseUnit "hundredths"
		put: (TIFFField tag: 290 type: 3 values: (Array with: 2));
		
		"The defaults for MaxSampleValue and MinSampleValue are a function of BitsPerSample and SmaplesPerPixel, and I don't anticipate needing these fields for Squeak purposes, so defaults for them are not handled according to the spec."

		at: #NewSubfileType "Primary Image"
		put: (TIFFField tag: 254 type: 4 values: (Array with: 0));
		
		at: #Orientation "0, 0 is top left of image"
		put: (TIFFField tag: 274 type: 3 values: (Array with: 1));
		
		at: #PlanarConfiguration "Chunky"
		put: (TIFFField tag: 284 type: 3 values: (Array with: 1));
		
		at: #ResolutionUnit "Inch"
		put: (TIFFField tag: 296 type: 3 values: (Array with: 2));
		
		at: #RowsPerStrip "Effectively infinite"
		put: (TIFFField tag: 278 type: 4 values: (Array with: 16rFFFFFFFF));
		
		at: #SamplesPerPixel "One"
		put: (TIFFField tag: 277 type: 3 values: (Array with: 1));
		
		at: #Threshholding "No dithering or halftoning applied"
		put: (TIFFField tag: 263 type: 3 values: (Array with: 1)).
		
! !

!TIFFField class methodsFor: 'initializing' stamp: 'jmv 1/12/2011 15:59'!
initializeSingleValuedTagSymbols
	"SingleValuedTagSymbols		Set -- contains the tag symbols of the fields types that should have a value that consists of an array of length one."
	SingleValuedTagSymbols _ Set new.
	SingleValuedTagSymbols
		add: #CellLength ;
		add: #CellWidth ;
		add: #Compression ;
		add: #Predictor ;
		add: #DateTime ; "Should be a single String."
		add: #FillOrder ;
		add: #GrayResponseUnit ;
		add: #ImageLength ;
		add: #ImageWidth ;
		add: #NewSubfileType ;
		add: #Orientation ;
		add: #PhotometricInterpretation ;
		add: #PlanarConfiguration ;
		add: #ResolutionUnit ;
		add: #RowsPerStrip ;
		add: #SamplesPerPixel ;
		add: #SubfileType ;
		add: #Threshholding ;
		add: #XResolution ;
		add: #YResolution .
! !

!TIFFField class methodsFor: 'initializing' stamp: 'jmv 1/12/2011 15:58'!
initializeTagSymbols
	"TagSymbols					Dictionary -- maps TIFF tag integers to tag symbols."
	TagSymbols _ Dictionary new.
	TagSymbols
		at: 315 put: #Artist ;
		at: 258 put: #BitsPerSample ;
		at: 265 put: #CellLength ;
		at: 264 put: #CellWidth ;
		at: 320 put: #ColorMap ;
		at: 259 put: #Compression ;
		at: 317 put: #Predictor ;
		at: 33432 put: #Copyright ;
		at: 306 put: #DateTime ;
		at: 338 put: #ExtraSamples ;
		at: 266 put: #FillOrder ;
		at: 289 put: #FreeByteCounts ;
		at: 288 put: #FreeOffsets ;
		at: 291 put: #GrayResponseCurve ;
		at: 290 put: #GrayResponseUnit ;
		at: 316 put: #HostComputer ;
		at: 270 put: #ImageDescription ;
		at: 257 put: #ImageLength ;
		at: 256 put: #ImageWidth ;
		at: 271 put: #Make ;
		at: 281 put: #MaxSampleValue ;
		at: 280 put: #MinSampleValue ;
		at: 272 put: #Model ;
		at: 254 put: #NewSubfileType ;
		at: 274 put: #Orientation ;
		at: 262 put: #PhotometricInterpretation ;
		at: 284 put: #PlanarConfiguration ;
		at: 296 put: #ResolutionUnit ;
		at: 278 put: #RowsPerStrip ;
		at: 277 put: #SamplesPerPixel ;
		at: 305 put: #Software ;
		at: 279 put: #StripByteCounts ;
		at: 273 put: #StripOffsets ;
		at: 255 put: #SubfileType ;
		at: 263 put: #Threshholding ;
		at: 282 put: #XResolution ;
		at: 283 put: #YResolution .
! !

!TIFFField class methodsFor: 'initializing' stamp: 'mrm 7/27/2001 17:47'!
initializeValueReaderSelectors
	"ValueReaderSelectors 	Dictionary -- maps TIFF type integers to the selectors of the methods used to read field values of that type."

	ValueReaderSelectors _ Dictionary new.
	ValueReaderSelectors
		at: 1 put: #readByteValuesFrom: ;
		at: 2 put: #readAsciiValuesFrom: ;
		at: 3 put: #readShortValuesFrom: ;
		at: 4 put: #readLongValuesFrom: ;
		at: 5 put: #readRationalValuesFrom: .
! !

!TIFFField class methodsFor: 'instance creation' stamp: 'mrm 7/25/2001 22:22'!
parseFrom: aTIFFStream
	"Answer a new instance of the receiver resulting from parsing the given TIFFStream from its current position. Leave the stream positioned after the field, which will be the beginning of the next field, if any."

	^ self new parseFrom: aTIFFStream.! !

!TIFFField class methodsFor: 'private' stamp: 'mrm 7/28/2001 16:25'!
tag: tagInteger type: typeInteger values: valueArray
	"Answer a new instance with the given state."
	^ self new 
		tag: tagInteger
		type: typeInteger
		values: valueArray.! !

!TIFFImageFileDirectory methodsFor: 'accessing' stamp: 'mrm 7/28/2001 15:52'!
at: tagSymbol
	"Answer the value I have for the given tag Symbol. If I have no value for that tag, return the default value for that tag. 
	All TIFF tags are structured with array values, but many tags never have an array with more than one value. Thus, I answer the sole value for such tags, and answer an array for tags that can have more than one value.
	I report an error if I have no value for a tag that has no default or if I have more than one value for a tag that should only have one value."

	| field |
	field _ fields 
		at: tagSymbol
		ifAbsent: 
			[TIFFField defaultFor: tagSymbol]. "Raises error if no default."
	^ field value.! !

!TIFFImageFileDirectory methodsFor: 'private' stamp: 'jmv 2/29/2016 11:22'!
parseFrom: aTIFFStream
	"Blow away any former state I had and parse a new collection of fields from the given stream.
	Leave the stream positioned at the end of the IFD, just before the pointer to the next IFD."
	| fieldCount field |
	fieldCount _ aTIFFStream nextUnsignedInt16.
	fields _ (Dictionary new: fieldCount).
	fieldCount timesRepeat: 
		[field _ TIFFField parseFrom: aTIFFStream.
		field isNil ifFalse: [fields at: field tagSymbol put: field] ].
! !

!TIFFImageFileDirectory class methodsFor: 'instance creation' stamp: 'mrm 7/25/2001 22:22'!
parseFrom: aTIFFStream
	"Answer a new instance of the receiver resulting from parsing the given TIFFStream from its current position. Leave the stream positioned at the start of the next IFD, or at 0 if no more."
	^ self new parseFrom: aTIFFStream.! !

!TIFFReader methodsFor: 'accessing' stamp: 'mrm 7/28/2001 17:03'!
extent
	"Answer a Point that defines my image's size in pixels."
	^ (structure at: #ImageWidth) @ (structure at: #ImageLength).
! !

!TIFFReader methodsFor: 'private' stamp: 'jmv 1/12/2011 16:52'!
positionStreamToStripNum: anInteger
	"Position my TIFFStream at the start of the given strip, where 1 is the first strip."
	stream position: ((structure at: #StripOffsets) at: anInteger).
	stream newStrip! !

!TIFFReader methodsFor: 'private' stamp: 'jmv 7/26/2016 15:55:59'!
readGray16PixelsInto: aShortWordMatrix
	"Replace the given Form's pixel values with pixel values from my TIFFStream. aForm must have the correct extent.
	Handles 16 bit pixels on source, float as destination."

	| elements totalPixels pixelsPerStrip currentStripNum pixel | 
	elements _ aShortWordMatrix elements.
	totalPixels _ elements size.
	pixelsPerStrip _ aShortWordMatrix extent x * (structure at: #RowsPerStrip).
	currentStripNum _ 0.

	1 to: totalPixels do: [ :pixNum |
		(pixNum-1 \\ aShortWordMatrix width = 0) ifTrue: [
			(pixNum - 1 \\ pixelsPerStrip = 0) ifTrue: [
				"Time to go to next strip"
				currentStripNum _ currentStripNum + 1.
				self positionStreamToStripNum: currentStripNum ]].

		pixel _ stream nextUnsignedInt16.
		elements at: pixNum put: pixel ]! !

!TIFFReader methodsFor: 'private' stamp: 'jmv 5/12/2016 18:27'!
readGray8PixelsInto: aGrayForm horizontalPredictor: useHorizontalPredictor
	"Replace the given Form's pixel values with pixel values from my TIFFStream. aForm must have the correct extent.
	Only handles 8-bit gray pixels on both source and destination."

	| bits totalPixels pixelsPerStrip currentStripNum g gg pixel bytesPerRow row column |
	bits _ aGrayForm bits.
	totalPixels _ aGrayForm width * aGrayForm height.
	pixelsPerStrip _ aGrayForm width * (structure at: #RowsPerStrip).
	currentStripNum _ 0.
	row _ -1.
	column _ 0.
	bytesPerRow _ aGrayForm width + 3 // 4 * 4.
	1 to: totalPixels do: [ :pixNum |
		(pixNum-1 \\ aGrayForm width = 0) ifTrue: [
			row _ row + 1.
			column _ 0.
			(pixNum - 1 \\ pixelsPerStrip = 0) ifTrue: [
				"Time to go to next strip"
				currentStripNum _ currentStripNum + 1.
				self positionStreamToStripNum: currentStripNum ].
			useHorizontalPredictor ifTrue: [
				g _ 0 ]].
		column _ column + 1.

		gg _ stream nextByteInBody.

		useHorizontalPredictor
			ifTrue: [
				g _ (g + gg) bitAnd: 255 ]
			ifFalse: [
				g _ gg ].
		pixel _ g.
		bits byteAt: (row *  bytesPerRow) + column put: pixel bigEndian: aGrayForm isBigEndian ]! !

!TIFFReader methodsFor: 'private' stamp: 'jmv 4/8/2015 11:54'!
readPlanarRGB8PixelsInto: a32BitForm readAlpha: doReadAlpha horizontalPredictor: useHorizontalPredictor
	"Replace the given Form's pixel values with pixel values from my TIFFStream. aForm must have the correct extent.
	Handles 24 or 32-bit pixels on source, and destination of 32 bits per pixel."

	| bits totalPixels pixelsPerStrip currentStripNum pixel component componentByte shifts | 
	bits _ a32BitForm bits.
	totalPixels _ bits size.
	pixelsPerStrip _ a32BitForm extent x * (structure at: #RowsPerStrip).
	currentStripNum _ 0.

	shifts _ doReadAlpha
		ifTrue: [ #( 24 16 8 0 ) ]
		ifFalse: [ #( 16 8 0 ) ].

	shifts do: [ :shift |
		1 to: totalPixels do: [ :pixNum |
			(pixNum-1 \\ a32BitForm width = 0) ifTrue: [
				(pixNum - 1 \\ pixelsPerStrip = 0) ifTrue: [
					"Time to go to next strip"
					currentStripNum _ currentStripNum + 1.
					self positionStreamToStripNum: currentStripNum ].
				useHorizontalPredictor ifTrue: [
					component _ 0 ]].

			componentByte _ stream nextByteInBody.
			component _ useHorizontalPredictor
				ifTrue: [ (component + componentByte) bitAnd: 255 ]
				ifFalse: [ componentByte ].

			pixel _ component << shift + (bits at: pixNum).
			bits at: pixNum put: pixel ]].

	"Alpha set at the end, to avoid LargIntegers in the code above
	(a32BitForm came with all zeros, see senders)"
	doReadAlpha ifFalse: [
		a32BitForm makeAllPixelsOpaque ]! !

!TIFFReader methodsFor: 'private' stamp: 'jmv 4/7/2015 16:34'!
readRGB8PixelsInto: a32BitForm readAlpha: doReadAlpha horizontalPredictor: useHorizontalPredictor
	"Replace the given Form's pixel values with pixel values from my TIFFStream. aForm must have the correct extent.
	Handles 24 or 32-bit pixels on source, and destination of 32 bits per pixel."

	| bits totalPixels pixelsPerStrip currentStripNum pixel a b g r aa bb rr gg | 
	bits _ a32BitForm bits.
	totalPixels _ bits size.
	pixelsPerStrip _ a32BitForm extent x * (structure at: #RowsPerStrip).
	currentStripNum _ 0.
	a _ 255.

	1 to: totalPixels do: [ :pixNum |
		(pixNum-1 \\ a32BitForm width = 0) ifTrue: [
			(pixNum - 1 \\ pixelsPerStrip = 0) ifTrue: [
				"Time to go to next strip"
				currentStripNum _ currentStripNum + 1.
				self positionStreamToStripNum: currentStripNum ].
			useHorizontalPredictor ifTrue: [
				r _ g _ b _ 0.
				a _ doReadAlpha ifTrue: [ 0 ] ifFalse: [ 255 ] ]].

		rr _ stream nextByteInBody.
		gg _ stream nextByteInBody.
		bb _ stream nextByteInBody.
		doReadAlpha
			ifTrue: [ aa _ stream nextByteInBody ].

		useHorizontalPredictor
			ifTrue: [
				r _ (r + rr) bitAnd: 255.
				g _ (g + gg) bitAnd: 255.
				b _ (b + bb) bitAnd: 255.
				doReadAlpha
					ifTrue: [ a _ (a + aa) bitAnd: 255 ] ]
			ifFalse: [
				r _ rr.
				g _ gg.
				b _ bb.
				doReadAlpha
					ifTrue: [ a _ aa ] ].
		pixel _ (r << 16) + (g << 8) + b + (a << 24).
		bits at: pixNum put: pixel.].! !

!TIFFReader methodsFor: 'private' stamp: 'jmv 12/21/2013 09:28'!
tiffStream: aTIFFStream
	"Set the given stream as my stream, and read the TIFF's structure from that stream."
	stream _ aTIFFStream.
	structure _ TIFFStructure parseFrom: stream.
! !

!TIFFReader methodsFor: 'private' stamp: 'jmv 4/8/2015 11:13'!
validateTIFFType
	"Examine my structure to see if it represents a TIFF that I know how to read. 
	At the moment, I can read only
		Gray 8-bit
		Gray 16-bit
		RGB 24-bit 
		RGB 32-bit with an alpha channel,
	uncompressed, chunky TIFFs ."

	| bpsArray compression predictor |
	((structure at: #PhotometricInterpretation) between: 1 and: 2) "Black is zero: 1, RGB: 2"
		ifFalse: [self error: 'Cannot read, this is not an Gray or RGB TIFF.'].
	(#(1 3 4) includes: (structure at: #SamplesPerPixel))  "gray/bilevel: 1, RGB: 3, RGBA: 4"
		ifFalse: [self error: 'Cannot read, this TIFF is not a Gray, Bilevel, RGB, or RGBA TIFF.'].
	compression _ structure at: #Compression.
	(compression = 1 or: [ compression = 5 ]) "Uncompressed (1) or LZW (5)"
		ifFalse: [self error: 'Cannot read, this TIFF is compressed with an unsupported algorithm.'].
	predictor _ structure at: #Predictor.
	(predictor = 1 or: [ predictor = 2 and: [ compression = 5]])
		ifFalse: [self error: 'Cannot read, this TIFF uses an unsupported predictor.'].
	((structure at: #FillOrder) = 1) 
		ifFalse: [self error: 'Cannot read, this TIFF has a FillOrder other than 1.'].
	((structure at: #Orientation) = 1) 
		ifFalse: [self error: 'Cannot read, this TIFF has an Orientation other than 1.'].
	bpsArray _ structure at: #BitsPerSample.
	((#(1 3 4) includes: bpsArray size) and: [ (bpsArray allSatisfy: [ :d | d = 8 ]) or: [bpsArray = #(16)]])
		ifFalse: [self error: 'Cannot read, this TIFF is not a Gray, RGB or RGBA TIFF with 8bpp or Gray 16bpp.']! !

!TIFFReader methodsFor: 'reading' stamp: 'jmv 8/1/2016 10:37:24'!
readForm
	"Answer a Form created from the data in my stream and structure."
	| result bps |
	self validateTIFFType. "Make sure it's a type we can handle."
	
	(structure at: #SamplesPerPixel) >= 3 ifTrue: [
		result _ Form 
				extent: self extent
				depth: 32. "32 bits form with RGB or RGBA"
		
		((structure at: #PlanarConfiguration) = 1)
		ifTrue: [ 	"chunky" 
			self
				readRGB8PixelsInto: result
				readAlpha: (structure at: #SamplesPerPixel) = 4
				horizontalPredictor: ((structure at: #Predictor) = 2) ]
		ifFalse: [ 	"planar"
			self
				readPlanarRGB8PixelsInto: result
				readAlpha: (structure at: #SamplesPerPixel) = 4
				horizontalPredictor: (structure at: #Predictor) = 2 ]].
	
	(structure at: #SamplesPerPixel) = 1 ifTrue: [
		bps _ (structure at: #BitsPerSample) at: 1.
		bps caseOf: {
			[ 8 ] -> [
				"ColorForm set for 8bpp gray scale images"
				result _ GrayForm extent: self extent.
				self
					readGray8PixelsInto: result
					horizontalPredictor: ((structure at: #Predictor) = 2) ].

			[ 16 ] -> [
				"float image"
				result _ (Smalltalk at: #ShortWordMatrix ifAbsent: [
						self error: '16bpp TIFF requires ShortWordMatrix (LinearAlgebra package)' ])
					extent: self extent.
				self readGray16PixelsInto: result ] }].

	^ result! !

!TIFFReader class methodsFor: 'instance creation' stamp: 'jmv 12/21/2013 09:28'!
onTiffStream: aTIFFStream
	"Create and answer an instance of the receiver based on the given TIFFStream. Parse the structure at this time as well."
	^ self new tiffStream: aTIFFStream.
! !

!TIFFStream methodsFor: 'LZW uncompressions' stamp: 'jmv 1/12/2011 16:52'!
checkLZWCodeSize

	(freeCode+1 > maxCode and: [ codeSize < 12 ]) ifTrue: [
		codeSize := codeSize + 1.
		maxCode := (1 bitShift: codeSize) - 1 ]! !

!TIFFStream methodsFor: 'LZW uncompressions' stamp: 'jmv 1/12/2011 16:57'!
initializeLZWDecompression

	maxOutCodes := 4096.
	remainBitCount := 0.
	outCodes := ByteArray new: maxOutCodes + 1.
	outCount := 0.
	prefixTable := Array new: 4096.
	suffixTable := Array new: 4096.
	bitMask := (1 bitShift: 8) - 1! !

!TIFFStream methodsFor: 'LZW uncompressions' stamp: 'jmv 1/12/2011 17:02'!
nextLZWCode
	| integer readBitCount shiftCount byte |
	integer := 0.
	remainBitCount = 0 
		ifTrue: [
			readBitCount := 8.
			shiftCount := 0 ]
		ifFalse: [
			readBitCount := remainBitCount.
			shiftCount := remainBitCount - 8 ].
	[ readBitCount < codeSize ] whileTrue: [
		byte := self nextByte.
		byte == nil ifTrue: [ ^ eoiCode ].
		remainBitCount = 0 ifFalse: [
			byte _ byte bitAnd: (1 bitShift: remainBitCount)-1.
			remainBitCount _ 0 ].
		integer := (integer bitShift: 8) + byte.
		shiftCount := shiftCount + 8.
		readBitCount := readBitCount + 8 ].
	(remainBitCount := readBitCount - codeSize) = 0 
		ifTrue: [ byte := self nextByte ]
		ifFalse: [ byte := self peek ].
	byte == nil ifTrue: [ ^ eoiCode ].
	integer := (integer bitShift: 8) + byte.
	integer _ integer bitShift: remainBitCount negated.
	^integer! !

!TIFFStream methodsFor: 'LZW uncompressions' stamp: 'jmv 1/12/2011 17:04'!
nextLZWUncompressedByte
	"Answer the next 8-bit unsigned quantity from the stream."
	| answer |
	outCount = 0 ifTrue: [
		self readAdditionalLZWStuff ].
	
	answer _ outCodes at: outCount.
	outCount _ outCount - 1.
	^answer! !

!TIFFStream methodsFor: 'LZW uncompressions' stamp: 'jmv 1/12/2011 15:18'!
reInitializeLZWDecompression
	clearCode := 1 bitShift: 8.
	eoiCode := clearCode + 1.
	freeCode := clearCode + 2.
	codeSize := 9.	
	maxCode := (1 bitShift: codeSize) - 1! !

!TIFFStream methodsFor: 'LZW uncompressions' stamp: 'jmv 1/12/2011 17:13'!
readAdditionalLZWStuff
	"Answer the next 8-bit unsigned quantity from the stream."
	| code curCode inCode |
	code := self nextLZWCode.
	code = eoiCode ifTrue: [
		^nil ].

	code = clearCode 
		ifTrue: [
			self reInitializeLZWDecompression.
			curCode := oldCode := code := self nextLZWCode.
			finChar := curCode]
		ifFalse: [
			curCode := inCode := code.
			curCode >= freeCode ifTrue: [
				curCode := oldCode.
				outCodes 
					at: (outCount := outCount + 1)
					put: finChar ].
			[ curCode > bitMask ] whileTrue: [
				outCount > maxOutCodes ifTrue: [ ^ self error: 'corrupt LZW TIFF file (OutCount)' ].
				outCodes 
					at: (outCount := outCount + 1)
					put: (suffixTable at: curCode + 1).
				curCode := prefixTable at: curCode + 1 ].
			finChar := curCode.
			prefixTable 
				at: freeCode + 1
				put: oldCode.
			suffixTable 
				at: freeCode + 1
				put: finChar.
			oldCode := inCode.
			freeCode := freeCode + 1.
			self checkLZWCodeSize ].
	outCodes 
		at: (outCount := outCount + 1)
		put: finChar! !

!TIFFStream methodsFor: 'accessing' stamp: 'jmv 1/12/2011 16:05'!
compressionType: aNumber
	compressionType _ aNumber.
	aNumber = 5 ifTrue: [
		self initializeLZWDecompression ]! !

!TIFFStream methodsFor: 'accessing' stamp: 'mrm 7/22/2001 21:59'!
isBigEndian
	"Return true if I am a big-endian TIFF, false if I am little-endian."
	^ bigEndian.
! !

!TIFFStream methodsFor: 'TIFF type access' stamp: 'jmv 1/12/2011 17:02'!
newStrip
	remainBitCount _ 0.
	self reInitializeLZWDecompression! !

!TIFFStream methodsFor: 'TIFF type access' stamp: 'mrm 7/27/2001 17:49'!
nextByte
	"Answer the next 8-bit unsigned quantity from the stream."

	^ stream next.! !

!TIFFStream methodsFor: 'TIFF type access' stamp: 'jmv 1/13/2011 10:13'!
nextByteInBody
	"Answer the next 8-bit unsigned quantity from the stream.
	Uncompress data if appropriate"
	compressionType = 5 ifTrue: [
		^self nextLZWUncompressedByte ].
	^ stream next! !

!TIFFStream methodsFor: 'TIFF type access' stamp: 'jmv 2/29/2016 11:21'!
nextUnsignedInt16
	"Read a 16-bit unsigned quantity from the stream."

	^ stream nextUnsignedInt16BigEndian: bigEndian! !

!TIFFStream methodsFor: 'TIFF type access' stamp: 'jmv 2/29/2016 11:18'!
nextUnsignedInt32
	"Read a 32-bit unsigned quantity from the stream."

	^ stream nextUnsignedInt32BigEndian: bigEndian! !

!TIFFStream methodsFor: 'stream access' stamp: 'mrm 7/22/2001 20:23'!
next: anInteger

	"Answer the next anInteger elements of my stream's collection."
	^ stream next: anInteger.! !

!TIFFStream methodsFor: 'stream access' stamp: 'mrm 7/27/2001 17:35'!
peek

	"Answer the next byte of my underlying stream."
	^ stream peek.

! !

!TIFFStream methodsFor: 'stream access' stamp: 'mrm 7/22/2001 20:11'!
position

	"Answer the current position of accessing the sequence of objects."
	^ stream position.! !

!TIFFStream methodsFor: 'stream access' stamp: 'mrm 7/22/2001 20:18'!
position: anInteger 

	"Set the current position for accessing the objects to be anInteger, as long 
	as anInteger is within the bounds of the receiver's contents. If it is not, 
	create an error notification."
	stream position: anInteger.! !

!TIFFStream methodsFor: 'stream access' stamp: 'jmv 1/12/2011 10:01'!
reset
	"Goto start."
	stream reset! !

!TIFFStream methodsFor: 'stream access' stamp: 'mrm 7/22/2001 20:13'!
size

	"Answer the size of my underlying stream."
	^ stream size.

! !

!TIFFStream methodsFor: 'stream access' stamp: 'mrm 7/31/2001 21:44'!
skip: anInteger
	"Set the receiver's position to be the current position+anInteger."

	stream skip: anInteger.! !

!TIFFStream methodsFor: 'private' stamp: 'mrm 7/22/2001 20:34'!
on: aPositionableStream bigEndian: aBoolean

	"Make aPositionableStream be the stream I'm wrapping. Interpret it as big or little-endian according to aBoolean."
	stream _ aPositionableStream.
	bigEndian _ aBoolean.
! !

!TIFFStream class methodsFor: 'instance creation' stamp: 'mrm 7/22/2001 20:42'!
on: aPositionableStream bigEndian: aBoolean

	"Answer an instance of the receiver for encoding and/or decoding TIFFs on the given."

	^ self new 
		on: aPositionableStream 
		bigEndian: aBoolean.
! !

!TIFFStructure methodsFor: 'accessing' stamp: 'mrm 7/28/2001 09:33'!
at: tagSymbol
	"Answer the value I have for the given tag Symbol. If I have no value for that tag, return the default value for that tag. 
	All TIFF tags are structured with array values, but many tags never have an array with more than one value. Thus, I answer the sole value for such tags, and answer an array for tags that can have more than one value.
	I report an error if I have no value for a tag that has no default or if I have more than one value for a tag that should only have one value.
	I only look in my first image file directory for values."

	| ifd |
	ifd _ imageFileDirectories first. "Reports an error if no IFDs."
	^ ifd at: tagSymbol.! !

!TIFFStructure methodsFor: 'parsing' stamp: 'jmv 1/12/2011 16:06'!
parseFrom: aTIFFStream
	"Parse the given TIFFStream and set my internal state to match. Error if the TIFFStream does not contain a valid TIFF."

	"Wipe out any former contents."
	imageFileDirectories _ OrderedCollection new.

	self validateHeaderOf: aTIFFStream.
	self parseIFDsFrom: aTIFFStream.
	aTIFFStream compressionType: (self at: #Compression)! !

!TIFFStructure methodsFor: 'parsing' stamp: 'jmv 2/29/2016 11:20'!
parseIFDsFrom: aTIFFStream
	"Read as many image file directories as aTIFFStream contains, and put them into my state. Per the TIFF spec, there must be at least one IFD."

	| ifd ifdPosition |
	aTIFFStream position: 4. "Offset in header of pointer to first IFD."
	ifdPosition _ aTIFFStream nextUnsignedInt32.
	[ifdPosition = 0] 
		whileFalse: [
			aTIFFStream position: ifdPosition.
			ifd _ TIFFImageFileDirectory parseFrom: aTIFFStream.
			imageFileDirectories add: ifd.
			ifdPosition _ aTIFFStream nextUnsignedInt32].
! !

!TIFFStructure methodsFor: 'parsing' stamp: 'jmv 5/9/2016 17:00'!
validateHeaderOf: aTIFFStream
	"Report an error if the header of the given stream is not valid, 
	or does not match the endian-ness of the TIFFStream."

	| expectedWord douglasAdams |
	expectedWord _ aTIFFStream isBigEndian 
		ifTrue: [16r4D4D]
		ifFalse: [16r4949].
	douglasAdams _ 42.
	aTIFFStream reset.
	(aTIFFStream nextUnsignedInt16 = expectedWord) 
		ifFalse: [self error: 'TIFF Header not valid. First word not correct for expected endian-ness'].
	(aTIFFStream nextUnsignedInt16 = douglasAdams) 
		ifFalse: [self error: 'TIFF Header not valid. Answer not 42.']! !

!TIFFStructure class methodsFor: 'instance creation' stamp: 'mrm 7/25/2001 21:52'!
parseFrom: aTIFFStream
	"Answer a new instance of the receiver resulting from parsing the given TIFFStream from a position of 0. Error if the TIFFStream does not contain a valid TIFF."

	^ self new parseFrom: aTIFFStream.! !

!Form methodsFor: '*Graphics-Files-Additional' stamp: 'jmv 9/21/2018 16:57:37'!
writePNGfile: fileEntry
	"
	Display writePNGfile: 'display.png' asFileEntry
	"
	PNGReadWriter putForm: self onFile: fileEntry! !

!Form methodsFor: '*Graphics-Files-Additional' stamp: 'jmv 6/8/2013 14:08'!
writePNGfileNamed: fName
	"
	Display writePNGfileNamed: 'display.png'
	"
	PNGReadWriter putForm: self onFileNamed: fName! !
PNGReadWriter initialize!
TIFFField initialize!
