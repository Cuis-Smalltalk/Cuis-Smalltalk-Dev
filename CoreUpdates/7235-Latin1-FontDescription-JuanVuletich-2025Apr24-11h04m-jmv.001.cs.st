'From Cuis7.3 [latest update: #7147] on 24 April 2025 at 11:06:43 am'!
!classDefinition: #TTFontDescription category: #'Graphics-TrueType'!
Object subclass: #TTFontDescription
	instanceVariableNames: 'folderName contourDataForUtf8 contourDataIndexesByUtf8 kernPairs copyright familyName fullName subfamilyName uniqueName versionName postscriptName trademark bounds unitsPerEm ascent descent lineGap letterMTopSideBearing contourDataIndexesByLatin1 '
	classVariableNames: 'Descriptions '
	poolDictionaries: ''
	category: 'Graphics-TrueType'!

!TTFontDescription methodsFor: 'accessing' stamp: 'jmv 4/24/2025 10:15:09'!
contourDataIndexesByLatin1
	^contourDataIndexesByLatin1 ! !


!TTFontDescription methodsFor: 'private-initialization' stamp: 'jmv 4/24/2025 11:05:54'!
glyphsByUtf8Bytes: glyphsByUTF8 unknownGlyph: theUnknownGlyph scaleArrowGlyphs: arrowGlyphs

	| mGlyph |
	
	mGlyph := glyphsByUTF8 at: $M asciiValue + 1.
	letterMTopSideBearing := (mGlyph ifNil: [theUnknownGlyph]) topSideBearing.

	contourDataIndexesByUtf8 := (IntegerArray new: 256) as: OrderedCollection.
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
	contourDataIndexesByLatin1 := (IntegerArray new: 256).
	0 to: 255 do: [ :cp | | i |
		Character
			evaluateOnce: [ :byte1 :byte2OrNil :byte3OrNil :byte4OrNil |
				i := contourDataIndexesByUtf8 at: byte1 + 1.
				byte2OrNil notNil ifTrue: [
					i := contourDataIndexesByUtf8 at: i negated + byte2OrNil + 1 ].
				contourDataIndexesByLatin1 at: cp+1 put: i ]
			withUtf8BytesOf: cp ].! !


!VectorEngine methodsFor: 'text and multi paths' stamp: 'jmv 4/24/2025 11:02:07'!
drawByteString: aString from: startIndex to: stopIndex at: aPoint trueTypeFont: aTTFontDescription color: aColor scale: scale preventMirroring: doPreventMirroring
	"TrueType assumes a Math like coordinate system (Y increasing upwards).
	But Display and Form Y coordinates in Cuis increase downwards, as it is done in most computer hardware and software.
	If doPreventMirroring is true, ensure that glyphs don't look mirrored, by having a negative y scale."
"
| e f s |
'Try all FOUR of the,!!'.
e := VectorEngineWholePixel onForm: Display.
e := VectorEngineSubPixel onForm: Display.
e := VectorEngineWithPluginWholePixel onForm: Display.
e := VectorEngineWithPluginSubPixel onForm: Display.
f := 	FontFamily defaultFamilyAndPointSize fontDescription.
s := 'Hello World!! (Ascii Only in this example. Latin-1 requires an additional single byte table in the Font, that Cuis does not include.' asByteString.
e drawByteString: s from: 1 to: s size at: 10@120 trueTypeFont: f color: Color black scale: 30 preventMirroring: true.
Display forceToScreen.
"

	| nextGlyphX nextGlyphY sx sy contourData contourDataIndexes trueTypeScale |
	trueTypeScale := 1.0 / aTTFontDescription letterMTopSideBearing.	"Perhaps measures in TTFontDescription should be normalized"
	sx := scale * trueTypeScale asFloat.
	sy := scale * trueTypeScale asFloat.
	(doPreventMirroring and: [ geometryTransformation doesMirror not ]) ifTrue: [ sy := sy negated ].
	contourData := aTTFontDescription contourDataForUtf8.	
	contourDataIndexes := aTTFontDescription contourDataIndexesByLatin1.
	nextGlyphX := aPoint x asFloat.
	nextGlyphY := aPoint y asFloat.
	self fillColor: aColor do: [
		nextGlyphX := self
			displayByteString: aString from: startIndex to: stopIndex
			atx: nextGlyphX y: nextGlyphY
			scalex: sx y: sy
			contourData: contourData
			contourDataIndexes: contourDataIndexes ].
	^ nextGlyphX @ nextGlyphY.! !


!VectorEngineWithPlugin methodsFor: 'text and multi paths' stamp: 'jmv 4/24/2025 11:02:17'!
drawByteString: aString from: startIndex to: stopIndex at: aPoint trueTypeFont: aTTFontDescription color: aColor scale: scale preventMirroring: doPreventMirroring
	"TrueType assumes a Math like coordinate system (Y increasing upwards).
	But Display and Form Y coordinates in Cuis increase downwards, as it is done in most computer hardware and software.
	If doPreventMirroring is true, ensure that glyphs don't look mirrored, by having a negative y scale."
"
| e f s |
'Try all FOUR of the,!!'.
e := VectorEngineWholePixel onForm: Display.
e := VectorEngineSubPixel onForm: Display.
e := VectorEngineWithPluginWholePixel onForm: Display.
e := VectorEngineWithPluginSubPixel onForm: Display.
f := 	FontFamily defaultFamilyAndPointSize fontDescription.
s := 'Hello World!! (Ascii Only in this example. Latin-1 requires an additional single byte table in the Font, that Cuis does not include.' asByteString.
e drawByteString: s from: 1 to: s size at: 10@120 trueTypeFont: f color: Color black scale: 30 preventMirroring: true.
Display forceToScreen.
"

	| nextGlyphX nextGlyphY sx sy contourData contourDataIndexes trueTypeScale |
	trueTypeScale := 1.0 / aTTFontDescription letterMTopSideBearing.	"Perhaps measures in TTFontDescription should be normalized"
	sx := scale * trueTypeScale asFloat.
	sy := scale * trueTypeScale asFloat.
	(doPreventMirroring and: [ geometryTransformation doesMirror not ]) ifTrue: [ sy := sy negated ].
	contourData := aTTFontDescription contourDataForUtf8.
	contourDataIndexes := aTTFontDescription contourDataIndexesByLatin1.
	nextGlyphX := aPoint x asFloat.
	nextGlyphY := aPoint y asFloat.
	self fillColor: aColor do: [
		nextGlyphX := self
			primDisplayByteString: aString from: startIndex to: stopIndex
			atx: nextGlyphX y: nextGlyphY
			scalex: sx y: sy
			contourData: contourData
			contourDataIndexes: contourDataIndexes ].
	^ nextGlyphX @ nextGlyphY.! !

!classDefinition: #TTFontDescription category: #'Graphics-TrueType'!
Object subclass: #TTFontDescription
	instanceVariableNames: 'folderName contourDataForUtf8 contourDataIndexesByUtf8 contourDataIndexesByLatin1 kernPairs copyright familyName fullName subfamilyName uniqueName versionName postscriptName trademark bounds unitsPerEm ascent descent lineGap letterMTopSideBearing'
	classVariableNames: 'Descriptions'
	poolDictionaries: ''
	category: 'Graphics-TrueType'!

VectorCanvas initialize!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
	TrueTypeFontFamily readAllTrueTypeFontsIn: DirectoryEntry trueTypeFontsDirectory / 'DejaVu' / 'DejaVuSans'
!

