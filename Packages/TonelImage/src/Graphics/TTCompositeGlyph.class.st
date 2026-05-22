"
This class represents a composite TrueType glyph, e.g. one which contains many simple TTGlyphs.
"
Class {
	#name : 'TTCompositeGlyph',
	#superclass : 'TTGlyph',
	#instVars : [
		'glyphs'
	],
	#category : 'Graphics-TrueType',
	#package : 'Graphics',
	#tag : 'TrueType'
}

{ #category : 'primitive format' }
TTCompositeGlyph >> addContourDataTo: stream tx: aGeometryTransformationOrNil [
	| glyph tx |
	glyphs do: [ :txAndGlyph |
		tx := txAndGlyph key.
		aGeometryTransformationOrNil notNil ifTrue: [
			tx := aGeometryTransformationOrNil composedWith: tx ].
		glyph := txAndGlyph value.
		glyph addContourDataTo: stream tx: tx ]
]

{ #category : 'accessing' }
TTCompositeGlyph >> addGlyph: aGlyph transformation: aMatrix [
	glyphs := glyphs copyWith: (aMatrix -> aGlyph)
]

{ #category : 'accessing' }
TTCompositeGlyph >> contourCount [
	| glyph count |
	count := 0.
	glyphs do: [ :txAndGlyph |
		glyph := txAndGlyph value.
		count := count + glyph contourCount.
		].
	^count
]

{ #category : 'drawing' }
TTCompositeGlyph >> drawGlyphOn: aVectorCanvas [
	glyphs do: [ :each |
		each value drawGlyphOn: aVectorCanvas geometryTransformation: each key ]
]

{ #category : 'accessing' }
TTCompositeGlyph >> glyphs [

	^glyphs collect:[:assoc| assoc value].
]

{ #category : 'initialize' }
TTCompositeGlyph >> initialize [
	glyphs := #().
]

{ #category : 'testing' }
TTCompositeGlyph >> isComposite [
	^true
]

{ #category : 'private-initialization' }
TTCompositeGlyph >> replaceIndexesByComponentGlyphsIn: anArray [
	glyphs := glyphs collect: [ :assoc | assoc key -> (anArray at: assoc value) ]
]
