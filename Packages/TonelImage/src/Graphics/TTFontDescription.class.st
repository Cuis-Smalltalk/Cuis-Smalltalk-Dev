"
Holds a TrueType font from a .ttf file in memory.
My instances are members of a FontFamily (a Typeface), and are of a specific style.
Examples: DejaVu Sans Oblique, DejaVu Sans Book. They are menbers of the DejaVu Sans typeface (FontFamily).

See AbstractFont / TrueTypeFont.
See FontFamily.

See
https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html
https://en.wikipedia.org/wiki/Font.
"
Class {
	#name : 'TTFontDescription',
	#superclass : 'Object',
	#instVars : [
		'folderName',
		'contourDataForUtf8',
		'contourDataIndexesByUtf8',
		'contourDataIndexesByLatin1',
		'kernPairs',
		'copyright',
		'familyName',
		'fullName',
		'subfamilyName',
		'uniqueName',
		'versionName',
		'postscriptName',
		'trademark',
		'bounds',
		'unitsPerEm',
		'ascent',
		'descent',
		'lineGap',
		'letterMTopSideBearing'
	],
	#classVars : [
		'Descriptions'
	],
	#category : 'Graphics-TrueType',
	#package : 'Graphics',
	#tag : 'TrueType'
}

{ #category : 'instance creation' }
TTFontDescription class >> add: ttf to: aDictionary [
	"Add a TTFontDesctiption to aDictionary.
	If an emphasis is already found, then come up with another family name.
	In Cuis, each font family can have up to 4 fonts: Regular, Bold, Italic, Bold Italic.
	But some font families include many variations. Install them as different families.
	See senders."

	| thisFamilyDict |
	thisFamilyDict := aDictionary at: ttf familyName ifAbsentPut: [ Dictionary new].
	(thisFamilyDict includesKey: ttf emphasis) ifTrue: [
		ttf familyName: ttf familyName, '-', ttf subfamilyName subFamilyName: ''.
		thisFamilyDict := aDictionary at: ttf familyName ifAbsentPut: [ Dictionary new]].
	thisFamilyDict at: ttf emphasis put: ttf.
]

{ #category : 'class initialization' }
TTFontDescription class >> initialize [
	"
	TTFontDescription initialize.
	FontFamily releaseClassCachedState.
	TrueTypeFontFamily readAllTrueTypeFontsIn: DirectoryEntry trueTypeFontsDirectory / 'DejaVu' / 'DejaVuSans'.
	"
	Descriptions := nil.
]

{ #category : 'instance creation' }
TTFontDescription class >> readTrueTypeFontsIn: aDirectoryEntry [
	"
	TTFontDescription readTrueTypeFontsIn: DirectoryEntry trueTypeFontsDirectory
	"
	| read |
	read := OrderedDictionary new.
	aDirectoryEntry
		allFilesDo: [ :file | | ttf |
			ttf := TTFontReader readTTFFrom: file binaryContents.
			ttf folderName: file parent name.
			self add: ttf to: read ]
		matches: [ :file | '*.ttf' match: file name ].
	aDirectoryEntry
		allFilesDo: [ :file | | ttc |
			ttc := TTFontReader readTTCFrom: file binaryContents.
			ttc do: [ :eachTtf |
				eachTtf folderName: file parent name.
				self add: eachTtf to: read ]]
		matches: [ :file | '*.ttc' match: file name ].
	Descriptions ifNil: [ Descriptions := Dictionary new ].
	read keysAndValuesDo: [ :familyName :emphasisDict |
		emphasisDict keysAndValuesDo: [ :emphasis :ttf |
			(Descriptions at: familyName ifAbsentPut: [ Dictionary new]) at: emphasis put: ttf ]].
	^read keys.
]

{ #category : 'instance accessing' }
TTFontDescription class >> withFamilyName: aString [
	"
	TTFontDescription withFamilyName: 'DejaVu Sans'
	"
	^Descriptions at: aString
]

{ #category : 'private-initialization' }
TTFontDescription >> appendGlyphDataTo: stream glyphsArray: glyphsArray indexes: indexes base: baseIndex bar: aProgressBarOrNil unknown: theUnknownGlyph scaleArrowGlyphs: arrowGlyphs [

		glyphsArray withIndexDo: [ :glyphOrArrayOrNumberNil :index |
			aProgressBarOrNil ifNotNil: [ :bar | bar value: index ].
			glyphOrArrayOrNumberNil ifNotNil: [
				glyphOrArrayOrNumberNil == theUnknownGlyph
					ifTrue: [ indexes at: baseIndex + index put: 1 ] "Set to unknownGlyph. UnknownGlyph is index 1."
					ifFalse: [
						glyphOrArrayOrNumberNil isArray
							ifTrue: [
								| nextBaseIndex |
								nextBaseIndex := indexes size - 128.
								"Any incomplete array sequence will point at 1. VectorEngine will show (Character codePoint: 500001)
								as four invalid glyphs and not just one. Real solution is for VectorEngine to skip ignored bytes."
								indexes addAll: ((IntegerArray new: 64) atAllPut: 1).
								indexes at: baseIndex + index put: nextBaseIndex negated.
								self appendGlyphDataTo: stream glyphsArray: glyphOrArrayOrNumberNil indexes: indexes
									base: nextBaseIndex bar: nil unknown: theUnknownGlyph scaleArrowGlyphs: arrowGlyphs ]
							ifFalse: [
								glyphOrArrayOrNumberNil isNumber
									ifTrue: [ | glyph tx |
										glyph := arrowGlyphs at: glyphOrArrayOrNumberNil.
										tx := AffineTransformation withScale: letterMTopSideBearing asFloat / arrowGlyphs second topSideBearing * 0.7.
										indexes at: baseIndex + index put: stream position+1.
										glyph addGlyphDataTo: stream tx: tx
										]
									ifFalse: [
										indexes at: baseIndex + index put: stream position+1.
										glyphOrArrayOrNumberNil addGlyphDataTo: stream tx: nil ]]]]].
]

{ #category : 'properties' }
TTFontDescription >> ascent [
	"Ascent of the font. Relative to unitsPerEm. Usually positive."
	^ascent
]

{ #category : 'properties' }
TTFontDescription >> bounds [
	^bounds
]

{ #category : 'accessing' }
TTFontDescription >> contourDataForUtf8 [
	^contourDataForUtf8
]

{ #category : 'accessing' }
TTFontDescription >> contourDataIndexForUtf8Byte1: byte1 byte2: byte2OrNil byte3: byte3OrNil byte4: byte4OrNil [
	"Answer the position in contourDataForUtf8 for requested UTF-8 encoded code point.
	Answer 0 if we don't include it.
	Entries in contourDataIndexesByUtf8 may be:
		> 1: actual index in countourData for this CodePoint
		= 1: No contourData for this CodePoint. Use the UnknownGlyph, stored at position 1. (If byte1 > 0)
		< 0: Another UTF-8 byte is needed.
		= 0: No contourData for this CodePoint. Use the UnknownGlyph, stored at position 1."

	| i |
	i := contourDataIndexesByUtf8 at: byte1 + 1.
	byte2OrNil notNil ifTrue: [
		i < 0 ifFalse: [ ^0 ].
		i := contourDataIndexesByUtf8 at: i negated + byte2OrNil + 1.
		byte3OrNil notNil ifTrue: [
			i < 0 ifFalse: [ ^0 ].
			i := contourDataIndexesByUtf8 at: i negated + byte3OrNil + 1.
			byte4OrNil notNil ifTrue: [
				i < 0 ifFalse: [ ^0 ].
				i := contourDataIndexesByUtf8 at: i negated + byte4OrNil + 1 ]]].
	
	^i
]

{ #category : 'accessing' }
TTFontDescription >> contourDataIndexesByLatin1 [
	^contourDataIndexesByLatin1 
]

{ #category : 'accessing' }
TTFontDescription >> contourDataIndexesByUtf8 [
	^contourDataIndexesByUtf8 
]

{ #category : 'information' }
TTFontDescription >> copyright [
	^copyright
]

{ #category : 'properties' }
TTFontDescription >> descent [
	"Descender of the font. Relative to unitsPerEm. Usually negative."
	^descent
]

{ #category : 'emphasis' }
TTFontDescription >> emphasis [
	"See senders of emphasis:"
	| emphasis |
	familyName = fullName ifTrue: [ ^ 0 ].
	emphasis := 0.
	('*Bold*' match: subfamilyName) ifTrue: [ emphasis := emphasis bitOr: 1 ].
	('*Oblique*' match: subfamilyName) ifTrue: [ emphasis := emphasis bitOr: 2 ].
	('*Italic*' match: subfamilyName) ifTrue: [ emphasis := emphasis bitOr: 2 ].
	^ emphasis.
]

{ #category : 'information' }
TTFontDescription >> familyName [
	^familyName
]

{ #category : 'private-initialization' }
TTFontDescription >> familyName: aString subFamilyName: anotherString [
	familyName := aString.
	subfamilyName := anotherString.
]

{ #category : 'accessing' }
TTFontDescription >> first [

	^ self.

]

{ #category : 'accessing' }
TTFontDescription >> folderName [
	^folderName
]

{ #category : 'accessing' }
TTFontDescription >> folderName: aString [
	folderName := aString
]

{ #category : 'conversion' }
TTFontDescription >> formGlyphForUtf8Byte1: byte1 byte2: byte2OrNil byte3: byte3OrNil byte4: byte4OrNil pixelSize: pixelSize [
	"Needed only for BitBltCanvasEngine"

	| utf8ByteArray toPixelsScale formWidth formHeight leftPixelOffset topPixelOffset form canvas advanceWidth boundsLeft boundsRight boundsBottom boundsTop i deltaXToCenter xtraRoom usedRect |

	"Get the position in contourData for our glyph definition."
	i := self contourDataIndexForUtf8Byte1: byte1 byte2: byte2OrNil byte3: byte3OrNil byte4: byte4OrNil.
	
	advanceWidth := contourDataForUtf8 at: i. i := i + 1.
	boundsLeft := contourDataForUtf8 at: i. i := i + 1.
	boundsRight := contourDataForUtf8 at: i. i := i + 1.
	boundsBottom := contourDataForUtf8 at: i. i := i + 1.
	boundsTop := contourDataForUtf8 at: i. i := i + 1.

	toPixelsScale := 0.0 + pixelSize / self letterMTopSideBearing.

	"If positive, pixels we save at the left. if negative, xtra pix needed.
	Be sure to leave extra room so there's no risk of missing pixels."
	leftPixelOffset := (boundsLeft * toPixelsScale) rounded.
	formWidth := ((boundsRight - boundsLeft) * toPixelsScale) ceiling.
	xtraRoom := formWidth * 2 //10 + 3.
	leftPixelOffset := leftPixelOffset - xtraRoom.
	formWidth := formWidth + xtraRoom + xtraRoom.

	"Usually positive. Needed pixels above the baseline.
	Be sure to leave extra room so there's no risk of missing pixels."
	topPixelOffset := (boundsTop * toPixelsScale) ceiling.
	formHeight := ((boundsTop - boundsBottom) * toPixelsScale) ceiling.
	xtraRoom := formHeight * 2 //10 + 3.
	topPixelOffset := topPixelOffset + xtraRoom.
	formHeight := formHeight + xtraRoom + xtraRoom.
	
	"Draw our glyph."
	utf8ByteArray := { byte1. byte2OrNil. byte3OrNil. byte4OrNil} select: [ :b | b notNil ] :: asByteArray.
	form := Form extent: formWidth@formHeight depth: 32.
	form fillColor: Color white.
"form fillColor: (Color r: 01.0 g: 0.9 b: 0.9)." "Debug aid"
	canvas := VectorCanvas onFormWithSubPixelAntiAliasing: form.
	canvas 
		engine
			drawUtf8String: utf8ByteArray
			fromByte: 1
			toByte: utf8ByteArray size
			at: leftPixelOffset negated@topPixelOffset
			trueTypeFont: self
			color: Color black
			scale: 1.0 * pixelSize
			preventMirroring: true.

	"Now only take the pixels that are actually needed."
	usedRect := form boundingBox intersect: canvas engine morphBoundsAfterDraw.
	"usedRect := usedRect outsetBy: 1." "Debug aid"
	form := form copy: usedRect.
	leftPixelOffset := leftPixelOffset + usedRect left.
	topPixelOffset := topPixelOffset - usedRect top.

"Debug Aid"
"
f2 := form copy.
f2 fill: (form boundingBox insetBy: 1) fillColor: (f2 colorAt: 0@0).
(f2 tallyPixelValues count: [ :v | v > 0 ]) > 1 ifTrue: [
	Tota ifNil: [ Tota := OrderedCollection new ].
	Tota add: form.
	Tota2 ifNil: [ Tota2 := OrderedCollection new ].
	Tota2 add: utf8ByteArray.
].
"

	"TrueType doesn't speciy how to position diacritics wrt base character glyph.
	Horizontally center diacriticals wrt base glyph.
	See
	basicDisplayUtf8:fromByte:toByte:at:trueTypeFont:
	Apparently OpenType can do better than TrueType, as it includes the GPOS table."
	deltaXToCenter := (advanceWidth - (boundsRight+boundsLeft * 0.51)) * toPixelsScale.
	byte1 = 204 ifTrue: [
		byte2OrNil = 128 ifTrue: [ "Combining Grave Accent"
			deltaXToCenter := (advanceWidth - (boundsRight+boundsLeft * 0.43)) * toPixelsScale ].
		byte2OrNil = 129 ifTrue: [ "Combining Acute Accent"
			deltaXToCenter := (advanceWidth - (boundsRight+boundsLeft * 0.65)) * toPixelsScale ]].

	"DoubleWidth code points that combine above previous and following characters.
	Codepoints between 16r035C and 16r0363.
	Example: o͢o
	Don't center them"
	(byte1 = 205 and: [ byte2OrNil between: 156 and: 163 ]) ifTrue: [
		deltaXToCenter := 0 ].
	
	^(FormGlyph
		form: form
		leftOffset: leftPixelOffset
		topOffset: topPixelOffset
		advanceWidth: (advanceWidth * toPixelsScale))
			deltaXToCenter: deltaXToCenter.
]

{ #category : 'information' }
TTFontDescription >> fullName [
	^fullName
]

{ #category : 'private-initialization' }
TTFontDescription >> glyphsByUtf8Bytes: glyphsByUTF8 unknownGlyph: theUnknownGlyph scaleArrowGlyphs: arrowGlyphs [

	| mGlyph |
	
	mGlyph := glyphsByUTF8 at: $M asciiValue + 1.
	letterMTopSideBearing := (mGlyph ifNil: [theUnknownGlyph]) topSideBearing.

	"Any incomplete array sequence will point at 1. VectorEngine will show (Character codePoint: 500001)
	as four invalid glyphs and not just one. Real solution is for VectorEngine to skip ignored bytes."
	contourDataIndexesByUtf8 := ((IntegerArray new: 256) atAllPut: 1) as: OrderedCollection.
	contourDataForUtf8 := Float32Array streamContents: [ :stream |
		theUnknownGlyph addGlyphDataTo: stream tx: nil.		"First data is for unknownGlyph"	
		self fullName, ' - Processing glyph data' 
			displayProgressAt: 100@100
			from: 1 
			to: glyphsByUTF8 size 
			during: [ :bar |
				self appendGlyphDataTo: stream glyphsArray: glyphsByUTF8 indexes: contourDataIndexesByUtf8
					base: 0 bar: bar unknown: theUnknownGlyph scaleArrowGlyphs: arrowGlyphs ]].
	contourDataIndexesByUtf8 := contourDataIndexesByUtf8 as: IntegerArray.
	"Any incomplete array sequence will point at 1. VectorEngine will show (Character codePoint: 500001)
	as four invalid glyphs and not just one. Real solution is for VectorEngine to skip ignored bytes."
	contourDataIndexesByLatin1 := (IntegerArray new: 256)  atAllPut: 1.
	0 to: 255 do: [ :cp | | i |
		Character
			evaluateOnce: [ :byte1 :byte2OrNil :byte3OrNil :byte4OrNil |
				i := contourDataIndexesByUtf8 at: byte1 + 1.
				(byte2OrNil notNil and: [ i < 0 ]) ifTrue: [
					i := contourDataIndexesByUtf8 at: i negated + byte2OrNil + 1 ].
				contourDataIndexesByLatin1 at: cp+1 put: i ]
			withUtf8BytesOf: cp ].
]

{ #category : 'testing' }
TTFontDescription >> includesAscii [
	"Answer true if we include the basic ASCII characters.
	If not, we are not suitable, for instance, for displaying Smalltalk code."

	^(33 to: 127) allSatisfy: [ :i | (contourDataIndexesByUtf8  at: i) > 1 ].
]

{ #category : 'accessing' }
TTFontDescription >> includesContourDataForUtf8Byte1: byte1 byte2: byte2OrNil byte3: byte3OrNil byte4: byte4OrNil [
	"Answer true if we include a glyph definition for the requested UTF-8 encoded code point.
	Answer false if we don't include it.
	See comment at #contourDataIndexForUtf8Byte1:byte2:byte3:byte4:"

	"The unknown glyph is indeed included.
	See #appendGlyphDataTo:glyphsArray:indexes:base:bar:unknown:scaleArrowGlyphs:"
	byte1 = 1 ifTrue: [
		^true ].

	"But any other glyph redirected to it is not included."
	^(self contourDataIndexForUtf8Byte1: byte1 byte2: byte2OrNil byte3: byte3OrNil byte4: byte4OrNil) > 1
]

{ #category : 'testing' }
TTFontDescription >> isBaseFont [
	^ self emphasis = 0
]

{ #category : 'properties' }
TTFontDescription >> letterMTopSideBearing [
	"This number is what needs to be scaled by i.e. 12 to render a 12 pixel font"

	^letterMTopSideBearing
]

{ #category : 'properties' }
TTFontDescription >> lineGap [
	"Leading of the font. Relative to unitsPerEm."
	^lineGap
]

{ #category : 'properties' }
TTFontDescription >> lineSpacing [
	^ascent - descent + lineGap
]

{ #category : 'accessing' }
TTFontDescription >> name [

	^ self familyName copyWithout: Character space.

]

{ #category : 'information' }
TTFontDescription >> postscriptName [
	^postscriptName
]

{ #category : 'printing' }
TTFontDescription >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(.
	fullName printOn: aStream.
	aStream nextPut:$).
]

{ #category : 'private-initialization' }
TTFontDescription >> setAscent: asc descent: desc lineGap: lgap [
	ascent := asc.
	descent := desc.
	lineGap := lgap
]

{ #category : 'private-initialization' }
TTFontDescription >> setBounds: aRect unitsPerEm: aNumber [
	bounds := aRect.
	unitsPerEm := aNumber.
]

{ #category : 'private-initialization' }
TTFontDescription >> setKernPairs: array [
	kernPairs := array
]

{ #category : 'private-initialization' }
TTFontDescription >> setStrings: anArray [
	copyright := anArray at: 1.
	familyName := anArray at: 2.
	subfamilyName := anArray at: 3.
	uniqueName := anArray at: 4.
	fullName := anArray at: 5.
	versionName := anArray at: 6.
	postscriptName := anArray at: 7.
	trademark := anArray at: 8.

]

{ #category : 'information' }
TTFontDescription >> subfamilyName [
	^subfamilyName
]

{ #category : 'information' }
TTFontDescription >> trademark [
	^trademark
]

{ #category : 'information' }
TTFontDescription >> uniqueName [
	^uniqueName
]

{ #category : 'properties' }
TTFontDescription >> unitsPerEm [
	^unitsPerEm
]

{ #category : 'information' }
TTFontDescription >> versionName [
	^versionName
]

{ #category : 'glyph properties' }
TTFontDescription >> widthOf: aCharacterOrCodePoint [

	Character
		evaluateOnce: [ :byte1 :byte2OrNil :byte3OrNil :byte4OrNil |
			^self widthOfUtf8Byte1: byte1 b2: byte2OrNil b3: byte3OrNil b4: byte4OrNil ]
		withUtf8BytesOf: aCharacterOrCodePoint codePoint.
]

{ #category : 'glyph properties' }
TTFontDescription >> widthOfUtf8Byte1: byte1 b2: byte2OrNil b3: byte3OrNil b4: byte4OrNil [

	| i advanceWidth |
	
	i := self contourDataIndexForUtf8Byte1: byte1 byte2: byte2OrNil byte3: byte3OrNil byte4: byte4OrNil.
	
	"If glyph not found, take first glyph 'theUnknownGlyph'"
	i = 0 ifTrue: [ i := 1 ].
	advanceWidth := contourDataForUtf8 at: i.
	^ advanceWidth / letterMTopSideBearing.
]
