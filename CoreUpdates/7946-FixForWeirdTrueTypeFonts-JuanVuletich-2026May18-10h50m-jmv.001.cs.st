'From Cuis7.7 [latest update: #7945] on 18 May 2026 at 10:50:51 am'!

!TTFontDescription methodsFor: 'private-initialization' stamp: 'jmv 18/May/2026 10:47:57'!
glyphsByUtf8Bytes: glyphsByUTF8 unknownGlyph: theUnknownGlyph scaleArrowGlyphs: arrowGlyphs

	| mGlyph |
	
	mGlyph := glyphsByUTF8 at: $M asciiValue + 1.
	letterMTopSideBearing := (mGlyph ifNil: [theUnknownGlyph]) topSideBearing.
	"Workaround for fonts without M character and without a suitable Unknown Glyph"
	letterMTopSideBearing = 0 ifTrue: [ letterMTopSideBearing := (unitsPerEm * 0.729) rounded ].

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
			withUtf8BytesOf: cp ].! !

