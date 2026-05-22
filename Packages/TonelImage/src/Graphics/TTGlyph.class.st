"
This class represents a glyph of a TrueType font.

Instance variables:
	boundsLeft 					xMin in TrueType parlance
	boundsRight 			 	xMax in TrueType parlance
	boundsBottom 				yMin in TrueType parlance (usually close to baseline, i.e. 0)
	boundsTop 					yMax in TrueType parlance (usually positive and large, in TrueType, y axis points up)
	contours						<Array of: TTContourConstruction>
	advanceWidth				<Integer>	advance width of the glyph
	leftSideBearing  				<Integer>	left side bearing
	rightSideBearing  			<Integer>	right side bearing
"
Class {
	#name : 'TTGlyph',
	#superclass : 'Object',
	#instVars : [
		'boundsLeft',
		'boundsRight',
		'boundsTop',
		'boundsBottom',
		'contours',
		'advanceWidth',
		'leftSideBearing',
		'rightSideBearing'
	],
	#category : 'Graphics-TrueType',
	#package : 'Graphics',
	#tag : 'TrueType'
}

{ #category : 'primitive format' }
TTGlyph >> addContourDataTo: stream tx: aGeometryTransformationOrNil [

	contours do: [ :c | c addContourDataTo: stream tx: aGeometryTransformationOrNil ]
]

{ #category : 'primitive format' }
TTGlyph >> addGlyphDataTo: stream tx: aGeometryTransformationOrNil [
	| aw lb rt |
	aw := self advanceWidth.
	lb := self boundsLeft @ self boundsBottom.
	rt := self boundsRight @ self boundsTop.
	aGeometryTransformationOrNil ifNotNil: [
		aw := aGeometryTransformationOrNil externalizeScalar: aw.
		lb := aGeometryTransformationOrNil externalizePosition: lb.
		rt := aGeometryTransformationOrNil externalizePosition: rt ].
	stream nextPut: aw.
	stream nextPut: lb x. "left = xMin"
	stream nextPut: rt x. "right = xMax"
	stream nextPut: lb y. "bottom = yMin"
	stream nextPut: rt y. "top = yMax"
	stream nextPut: self contourCount.
	self addContourDataTo: stream tx: aGeometryTransformationOrNil
]

{ #category : 'accessing' }
TTGlyph >> advanceWidth [
	^advanceWidth
]

{ #category : 'accessing' }
TTGlyph >> advanceWidth: aNumber [
	advanceWidth := aNumber.
]

{ #category : 'accessing' }
TTGlyph >> boundsBottom [
	"yMin in TrueType parlance (usually close to baseline, i.e. 0)"
	^boundsBottom
]

{ #category : 'accessing' }
TTGlyph >> boundsLeft [
	"xMin in TrueType parlance"
	^boundsLeft
]

{ #category : 'accessing' }
TTGlyph >> boundsLeft: xMin boundsRight: xMax boundsTop: yMax boundsBottom: yMin [
	boundsLeft := xMin.
	boundsRight := xMax.
	boundsTop := yMax.
	boundsBottom := yMin.
]

{ #category : 'accessing' }
TTGlyph >> boundsRight [
	"xMax in TrueType parlance"
	^boundsRight
]

{ #category : 'accessing' }
TTGlyph >> boundsTop [
	"yMax in TrueType parlance (usually positive and large, in TrueType, y axis points up)"
	^boundsTop
]

{ #category : 'accessing' }
TTGlyph >> contourCount [
	^contours count: [ :each | each isEmpty not ]
]

{ #category : 'accessing' }
TTGlyph >> contours [
	^contours
]

{ #category : 'drawing' }
TTGlyph >> drawGlyphOn: aVectorCanvas [
	self drawGlyphOn: aVectorCanvas geometryTransformation: nil
]

{ #category : 'drawing' }
TTGlyph >> drawGlyphOn: aVectorCanvas geometryTransformation: txOrNil [
	| initialPathAndMove |
	contours do: [ :contour |
		contour isEmpty ifFalse: [
			aVectorCanvas moveTo: `0.0@0.0`.
			initialPathAndMove := contour initialMoveToAndPathCommands: txOrNil.
			initialPathAndMove first runOn: aVectorCanvas .
			initialPathAndMove second runOn: aVectorCanvas ]].
]

{ #category : 'initialize-release' }
TTGlyph >> initialize [

	boundsLeft := boundsRight := boundsTop := boundsBottom := 0.
	contours := #().
	advanceWidth := 0.
	leftSideBearing := 0.
	rightSideBearing := 0.
]

{ #category : 'private-initialization' }
TTGlyph >> initializeContours: numContours with: endPoints [
	"Initialize the contours for creation of the glyph."
	| startPt pts endPt |
	contours := Array new: numContours.
	startPt := -1.
	1 to: numContours do:[:i|
		endPt := endPoints at: i.
		pts := Int16PointArray new: endPt - startPt.
		contours at: i put: (TTContourConstruction on: pts).
		startPt := endPt].
]

{ #category : 'testing' }
TTGlyph >> isComposite [
	^false
]

{ #category : 'accessing' }
TTGlyph >> leftSideBearing [
	^leftSideBearing
]

{ #category : 'accessing' }
TTGlyph >> leftSideBearing: aNumber [
	leftSideBearing := aNumber.
]

{ #category : 'private-initialization' }
TTGlyph >> makePointsAbsolute [
	"Build the contours in the receiver glyph.
	The contour is constructed by converting the points
	form each contour into an absolute value and then
	compressing the contours into PointArrays."
	| contourOrigin |
	contourOrigin := 0@0.
	contours do: [ :contour | 
		contourOrigin := contour makePointsAbsolute: contourOrigin ]
]

{ #category : 'printing' }
TTGlyph >> printOn: aStream [

	aStream
		nextPutAll: self class name;
		nextPut:$(;
		print: (contours ifNil: [0] ifNotNil: [contours size]);
		nextPut:$).
]

{ #category : 'private-initialization' }
TTGlyph >> replaceIndexesByComponentGlyphsIn: dummy [
	"NOP"
]

{ #category : 'accessing' }
TTGlyph >> rightSideBearing [
	^rightSideBearing
]

{ #category : 'accessing' }
TTGlyph >> rightSideBearing: aNumber [
	rightSideBearing := aNumber.
]

{ #category : 'accessing' }
TTGlyph >> topSideBearing [
	"
	https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html
	"
	^boundsTop
]

{ #category : 'private-initialization' }
TTGlyph >> updateRightSideBearing [
	"Update the right side bearing value"
	"@@: Is the following really correct?!?!"
	rightSideBearing := advanceWidth - leftSideBearing - boundsRight + boundsLeft
]
