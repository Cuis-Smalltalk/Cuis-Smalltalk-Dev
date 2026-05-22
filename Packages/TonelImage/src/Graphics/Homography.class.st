"
An homography (or perspective transformation) for resampling images (for example). Can apply or correct for the perspective resulting from taking a photograph to a 2D object that is not perpendicular to the camera axis (for example, taking an image of the ground ahead of the camera).

http://docs.opencv.org/modules/imgproc/doc/geometric_transformations.html#warpperspective

To be of use, needs #map:to: in NumCuis/ImageProcessing.pck.st
"
Class {
	#name : 'Homography',
	#superclass : 'GeometryTransformation',
	#category : 'Graphics-Primitives',
	#package : 'Graphics',
	#tag : 'Primitives'
}

{ #category : 'instance creation' }
Homography class >> new [
	^(self basicNew: 8) initialize
]

{ #category : 'instance creation' }
Homography class >> new: s [
	self error: 'Please call Homography>>#new (without arguments).'.
	^self new
]

{ #category : 'element access' }
Homography >> a0 [
	^self at: 1
]

{ #category : 'element access' }
Homography >> a1 [
	^self at: 2
]

{ #category : 'element access' }
Homography >> a2 [
	^self at: 3
]

{ #category : 'private access' }
Homography >> at: index [
	"Used to be:
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>"
	<primitive: 238 error: ec>
	^Float fromIEEE32Bit: (self basicAt: index)
]

{ #category : 'private access' }
Homography >> at: index put: value [
	"Used to be:
	<primitive: 'primitiveAtPut' module: 'FloatArrayPlugin'>"
	<primitive: 239 error: ec>
	value isFloat 
		ifTrue:[self basicAt: index put: value asIEEE32BitWord]
		ifFalse:[self at: index put: value asFloat].
	^value
]

{ #category : 'element access' }
Homography >> b0 [
	^self at: 4
]

{ #category : 'element access' }
Homography >> b1 [
	^self at: 5
]

{ #category : 'element access' }
Homography >> b2 [
	^self at: 6
]

{ #category : 'element access' }
Homography >> c0 [
	^self at: 7
]

{ #category : 'element access' }
Homography >> c1 [
	^self at: 8
]

{ #category : 'converting coordinates' }
Homography >> map: aPoint [
	| xx yy zz |
	xx := (self a0 * aPoint x) + (self a1 * aPoint y) + self a2.
	yy := (self b0 * aPoint x) + (self b1 * aPoint y) + self b2.
	zz := (self c0 * aPoint x) + (self c1 * aPoint y) + 1.
	^(xx / zz) @ (yy / zz)
]
