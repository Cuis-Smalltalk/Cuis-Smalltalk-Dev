"
TrueType fonts are the most widely used format for pixel independent vector Fonts.
See https://en.wikipedia.org/wiki/Font.

In Cuis what is usually called a TrueType Font is represented by TTFontDescription.

A TrueTypeFont refers to a TTFontDescription, but adds a specific pointSize, and is the object actually used in text editors. They may have an emphasis added, like bold, italic, underlined, strikeTrhough, superscript and subscript. If they  have an emphasis, then they also know their base font (without any emphasis at all).

They also hold cached FormGlyph instances, that are used to speed up text display in BitBltCanvas and HybridCanvas.

See FontFamily.
See TTFontDescription.
"
Class {
	#name : 'TrueTypeFont',
	#superclass : 'AbstractFont',
	#instVars : [
		'ttFontDescription',
		'pointSize',
		'emphasis',
		'glyphFormsByUtf8',
		'family',
		'baseFont',
		'derivativeFonts'
	],
	#category : 'Graphics-TrueType',
	#package : 'Graphics',
	#tag : 'TrueType'
}

{ #category : 'instance creation' }
TrueTypeFont class >> family: aTrueTypeFontFamily ttFontDescription: aTTFontDescription pointSize: aNumber [
	^ self new family: aTrueTypeFontFamily ttFontDescription: aTTFontDescription pointSize: aNumber
]

{ #category : 'initialization' }
TrueTypeFont >> addDerivativeFont: aTrueTypeFont [
	derivativeFonts at: aTrueTypeFont emphasis put: aTrueTypeFont
]

{ #category : 'emphasis' }
TrueTypeFont >> addEmphasis: code [
"	Set the integer code for emphasis.
	See AbstractFont, 'constants' class category."

	emphasis := code.
	(emphasis anyMask: 4+8) ifTrue: [
		"They need to be recreated"
		glyphFormsByUtf8 := Array new: 256 ].
]

{ #category : 'accessing' }
TrueTypeFont >> ascent [
	"The distance from the baseline to the highest or upper grid coordinate used to place an outline point.
	It is a positive value, due to the grid's orientation with the Y axis upwards.
	See https://freetype.org/freetype2/docs/glyphs/glyphs-3.html
	"
	^ 0.0 + ttFontDescription ascent * pointSize / ttFontDescription letterMTopSideBearing
]

{ #category : 'accessing' }
TrueTypeFont >> baseFont [
	^ baseFont ifNil: [self]
]

{ #category : 'initialization' }
TrueTypeFont >> baseFont: aFont [

	baseFont := aFont.
	baseFont ifNotNil: [
		derivativeFonts := nil ]
]

{ #category : 'accessing' }
TrueTypeFont >> cachedFormDepth [
^32
]

{ #category : 'accessing' }
TrueTypeFont >> descent [
	"The distance from the baseline to the lowest grid coordinate used to place an outline point.
	It is a positive value (although in the TTFontDescription it is negative).
	See https://freetype.org/freetype2/docs/glyphs/glyphs-3.html
	"
	^ 0.0 + ttFontDescription descent negated * pointSize / ttFontDescription letterMTopSideBearing
]

{ #category : 'emphasis' }
TrueTypeFont >> emphasis [
	^ emphasis
]

{ #category : 'emphasis' }
TrueTypeFont >> emphasized: code [
"	Argument is one of the emphases defined in AbstractFont, 'constants' class category.
	Codes can be combined by OR-ing them.
	Add the requested emphasis to those we may already have.
"
	| derivative addedEmphasis derivedFrom |
	self isBaseFont ifFalse: [ ^self baseFont emphasized: (code bitOr: emphasis) ].
	code = 0 ifTrue: [ ^ self ].

	derivativeFonts ifNil: [ derivativeFonts := Dictionary new ].
	derivative := derivativeFonts at: code ifPresent: [ :der | ^ der ].		"Already have this style"

	"Bold+Italic requested but not available.
	Handle this case before decomposing, because neither can be built synthetic.
	Answer Italic if present.
	Otherwise answer Bold if present. Or just the base font."
	code = 3 ifTrue: [
		derivativeFonts at: 2 ifPresent: [ :der | ^ der ].
		derivativeFonts at: 1 ifPresent: [ :der | ^ der ].
		^self baseFont ].
	"Bold or Italic requested but not available. Just use base font."
	code < 3 ifTrue: [
		^self baseFont ].

	"Dont have it -- derive from another with one with less emphasis"
	addedEmphasis := 1 bitShift: code highBit - 1.
	derivedFrom := self emphasized: code - addedEmphasis.

	derivative := addedEmphasis caseOf: {
		[ 4 ] -> [ derivedFrom copy ].  												"Underlined(U)"
		[ 8 ] -> [ derivedFrom copy ].  												"StruckThrough(X)"
		[ 16 ] -> [ derivedFrom syntheticSuperscript ].  												"Superscript(Sup)"
		[ 32 ] -> [ derivedFrom syntheticSubscript ].  												"Subscript(Sub)"
	}.

	derivative baseFont: self.
	derivative addEmphasis: code.
	derivativeFonts at: code put: derivative.
	^ derivative
]

{ #category : 'accessing' }
TrueTypeFont >> family [
	^ family
]

{ #category : 'initialization' }
TrueTypeFont >> family: aTrueTypeFontFamily ttFontDescription: aTTFontDescription pointSize: aNumber [
	ttFontDescription := aTTFontDescription.
	emphasis := aTTFontDescription emphasis.
	pointSize := aNumber.
	glyphFormsByUtf8 := Array new: 256.
	family := aTrueTypeFontFamily.
	baseFont := nil.
	derivativeFonts := emphasis = 0 ifTrue: [Dictionary new].
]

{ #category : 'accessing' }
TrueTypeFont >> familyName [
	^ family familyName
]

{ #category : 'accessing' }
TrueTypeFont >> fontDescription [
	^ ttFontDescription 
]

{ #category : 'glyph access' }
TrueTypeFont >> glyphAtUtf8Byte1: byte1 byte2: byte2OrNil byte3: byte3OrNil byte4: byte4OrNil [
	"Needed only for BitBltCanvasEngine"

	| lastArray lastIndex |
	
	"Avoid creating multiple cached instances for the unknown glyph. Just keep one!"
	(ttFontDescription includesContourDataForUtf8Byte1: byte1 byte2: byte2OrNil byte3: byte3OrNil byte4: byte4OrNil)
		 ifFalse: [
			^self glyphAtUtf8Byte1: 1 byte2: nil byte3: nil byte4: nil ].
	lastArray := glyphFormsByUtf8 .
	lastIndex := byte1 + 1.
	byte2OrNil notNil ifTrue: [
		(lastArray at: lastIndex) ifNil: [ lastArray at: lastIndex put: (Array new: 64) ].
		lastArray := lastArray at: lastIndex.
		lastIndex := (byte2OrNil bitAnd: 63) + 1.
		byte3OrNil notNil ifTrue: [
			(lastArray at: lastIndex) ifNil: [ lastArray at: lastIndex put: (Array new: 64) ].
			lastArray := lastArray at: lastIndex.
			lastIndex := (byte3OrNil bitAnd: 63) + 1.
			byte4OrNil notNil ifTrue: [
				(lastArray at: lastIndex) ifNil: [ lastArray at: lastIndex put: (Array new: 64) ].
				lastArray := lastArray at: lastIndex.
				lastIndex := (byte4OrNil bitAnd: 63) + 1 ]]].
	^ (lastArray at: lastIndex) ifNil: [
		| formGlyph |
		formGlyph := (ttFontDescription
				formGlyphForUtf8Byte1: byte1 byte2: byte2OrNil
				byte3: byte3OrNil byte4: byte4OrNil
				pixelSize: pointSize)
			ifNotNil: [ :theGlyph |
				self isUnderlined ifTrue: [
					self makeUnderlinedGlyph: theGlyph ].
				self isStruckThrough ifTrue: [
					self makeStruckThroughGlyph: theGlyph ].
				theGlyph].
		lastArray at: lastIndex put: formGlyph.
		formGlyph ]
]

{ #category : 'testing' }
TrueTypeFont >> isBaseFont [
	^baseFont isNil
]

{ #category : 'testing' }
TrueTypeFont >> isBold [
	^self emphasis allMask: 1
]

{ #category : 'testing' }
TrueTypeFont >> isItalic [
	^self emphasis allMask: 2
]

{ #category : 'testing' }
TrueTypeFont >> isStruckThrough [
	^emphasis allMask: 8
]

{ #category : 'testing' }
TrueTypeFont >> isSubscript [
	^emphasis allMask: 32
]

{ #category : 'testing' }
TrueTypeFont >> isSuperscript [
	^emphasis allMask: 16
]

{ #category : 'testing' }
TrueTypeFont >> isUnderlined [
	^emphasis allMask: 4
]

{ #category : 'accessing' }
TrueTypeFont >> lineGap [
	"The distance that must be placed between two lines of text. The baseline-to-baseline distance 
	should be computed as
		linespace = ascent + descent + linegap
	if you use the typographic values.
	See https://freetype.org/freetype2/docs/glyphs/glyphs-3.html
	"
	^ 0.0 + ttFontDescription lineGap * pointSize / ttFontDescription letterMTopSideBearing
]

{ #category : 'accessing' }
TrueTypeFont >> lineSpacing [
	"Answer the height of the receiver including any additional line gap."

	^self ascent + self descent + self lineGap
]

{ #category : 'synthetic derivatives' }
TrueTypeFont >> makeStruckThroughGlyph: aFormGlyph [
	| oldForm form d y |
	"Please keep consistent with
	#drawOnVectorEngine:from:to:at:font:color:
	#drawOnVectorEngine:from:to:at:font:color:borderWidth:borderColor:
	Similar to: #makeUnderlinedGlyph:"

	form := aFormGlyph form.
	form width = 0 ifTrue: [ ^self ].
	d := (self descent / 4) ceiling.
	y := (aFormGlyph topOffset - self descent - d) floor.
	true ifTrue: [
		oldForm := form.
		form := Form extent: oldForm width+2 @ oldForm height depth: oldForm depth.
		form fillWhite.
		form copyBits: oldForm boundingBox from: oldForm at: `1@0` clippingBox: form boundingBox rule: Form over.
		aFormGlyph form: form leftOffsetBy: -1].
	form fillBlack: (0 @ y extent: form width @ d).
	"For testing it:
	FontFamily releaseClassCachedState
	"
]

{ #category : 'synthetic derivatives' }
TrueTypeFont >> makeUnderlinedGlyph: aFormGlyph [
	| height y oldForm form d |
	"Please keep consistent with
	#drawOnVectorEngine:from:to:at:font:color:
	#drawOnVectorEngine:from:to:at:font:color:borderWidth:borderColor:
	Similar to #makeStruckThroughGlyph:"

	form := aFormGlyph form.
	form width = 0 ifTrue: [ ^self ].
	d := (self descent / 4) ceiling.
	y := aFormGlyph topOffset + d.
	height := aFormGlyph topOffset + (2*d).
	true "height > form height" ifTrue: [
		oldForm := form.
		form := Form extent: oldForm width+2 @ height depth: oldForm depth.
		form fillWhite.
		form copyBits: oldForm boundingBox from: oldForm at: `1@0` clippingBox: form boundingBox rule: Form over.
		aFormGlyph form: form leftOffsetBy: -1 ].
	form fillBlack: (0 @ y extent: form width @ d).
	"For testing it:
	FontFamily releaseClassCachedState
	"
]

{ #category : 'objects from disk' }
TrueTypeFont >> objectForDataStream: refStrm [

	"I am about to be written on an object file.  Write a textual reference instead.
	Warning: This saves a lot of space, but might fail if using other fonts than those in AvailableFonts"

	^ DiskProxy
		global: #FontFamily
		selector: #familyName:pointSize:
		args: (Array with: self familyName with: self pointSize)
]

{ #category : 'accessing' }
TrueTypeFont >> pointSize [
"
p.ej.
scrollbarThickness
"
"self forwarding: #pointSize printOneStack: true."
	^pointSize
]

{ #category : 'printing' }
TrueTypeFont >> printOn: aStream [
	aStream
		nextPutAll: 'TrueTypeFont(''';
		nextPutAll: self familyName;
		nextPutAll: ''' ';
		space;
		print: self pointSize;
		space;
		nextPutAll:  ttFontDescription subfamilyName;
		nextPut: $).
]

{ #category : 'caching' }
TrueTypeFont >> releaseCachedState [
	glyphFormsByUtf8 atAllPut: nil.
	derivativeFonts notNil ifTrue: [
		derivativeFonts keysAndValuesDo: [ :anEmphasis :derivedFont |
			derivedFont releaseCachedState  ]]
]

{ #category : 'synthetic derivatives' }
TrueTypeFont >> syntheticSubscript [
	"Build and answer a derivative that is Subscript."

	| derivative |
	derivative := ((family atPointSize: pointSize * 0.68) emphasized: emphasis).
	^ derivative copy

"
('Hi ', (Text string: 'there' attribute: TextEmphasis superscript), ' how ', (Text string: 'are' attribute: TextEmphasis subscript), ' you?') edit.
"
]

{ #category : 'synthetic derivatives' }
TrueTypeFont >> syntheticSuperscript [
	"Build and answer a derivative that is Subscript."

	| derivative |
	derivative := ((family atPointSize: pointSize * 0.68) emphasized: emphasis).
	^ derivative copy

"
('Hi ', (Text string: 'there' attribute: TextEmphasis superscript), ' how ', (Text string: 'are' attribute: TextEmphasis subscript), ' you?') edit.
"
]

{ #category : 'accessing' }
TrueTypeFont >> widthOf: aCharacter [

	^ (ttFontDescription widthOf: aCharacter) * pointSize
]

{ #category : 'accessing' }
TrueTypeFont >> widthOfUtf8Byte1: aByte1 b2: aByte2OrNil b3: aByte3OrNil b4: aByte4OrNil [

	^ (ttFontDescription
		widthOfUtf8Byte1: aByte1 b2: aByte2OrNil b3: aByte3OrNil b4: aByte4OrNil)
			* pointSize
]
