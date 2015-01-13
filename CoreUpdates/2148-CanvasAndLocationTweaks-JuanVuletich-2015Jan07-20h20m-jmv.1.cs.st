'From Cuis 4.2 of 25 July 2013 [latest update: #2147] on 7 January 2015 at 8:31:29.589016 pm'!

!MatrixTransform2x3 methodsFor: 'modifying' stamp: 'jmv 1/7/2015 09:38'!
italizing
	"a little shear
	Answer the modified object. In this implementation is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	self a12: self a12  + (self scale*0.2).
	^self! !

!MatrixTransform2x3 methodsFor: 'modifying' stamp: 'jmv 1/7/2015 09:38'!
rotatedBy: radians
	"rotate the receiver by radians angle.
	Answer the modified object. In this implementation is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	| s c a11 a12 a13 a21 a22 a23|
	s _ radians sin.
	c _ radians cos.
	a11 _ self a11.
	a12 _ self a12.
	a13 _ self a13.
	a21 _ self a21.
	a22 _ self a22.
	a23 _ self a23.
	self a11: (c * a11) - (s * a21).
	self a12: (c * a12) - (s * a22).
	self a13: (c * a13) - (s * a23).
	self a21: (s * a11) + (c * a21).
	self a22: (s * a12) + (c * a22).
	self a23: (s * a13) + (c * a23).
	^ self! !

!MatrixTransform2x3 methodsFor: 'modifying' stamp: 'jmv 1/7/2015 09:39'!
scaledBy: aPoint
	"Multiply by a scale.
	Argument can be a point, applying different scaling in x and in y directions.
	Keep the transformed position of 0@0, i.e. don't change offset.

	Answer the modified object. In this implementation is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	| pt sx sy |
	pt _ aPoint asPoint.
	sx _ pt x.
	sy _ pt y.
	self a11: self a11 * sx.
	self a12: self a12 * sx.
	self a21: self a21 * sy.
	self a22: self a22 * sy.
	^ self! !

!MatrixTransform2x3 methodsFor: 'modifying' stamp: 'jmv 1/7/2015 09:39'!
scaledByNumber: aNumber rotateBy: radians
	"rotate the receiver by radians angle. Also scale by aNumber.
	Note: the scale factor is a number, not a point. Therefore, the same scale is applied in all directions.
	This means that there is no difference between  scaling then rotating and rotating then scaling.

	Answer the modified object. In this implementation is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	| s c a11 a12 a13 a21 a22 a23|
	s _ radians sin.
	c _ radians cos.
	a11 _ self a11 * aNumber.
	a12 _ self a12 * aNumber.
	a13 _ self a13 * aNumber.
	a21 _ self a21 * aNumber.
	a22 _ self a22 * aNumber.
	a23 _ self a23 * aNumber.
	self a11: (c * a11) - (s * a21).
	self a12: (c * a12) - (s * a22).
	self a13: (c * a13) - (s * a23).
	self a21: (s * a11) + (c * a21).
	self a22: (s * a12) + (c * a22).
	self a23: (s * a13) + (c * a23).
	^self! !

!MatrixTransform2x3 methodsFor: 'modifying' stamp: 'jmv 1/7/2015 09:39'!
withAddedOffset: aPoint
	"add an offset in the receiver

	Answer the modified object. In this implementation is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	| pt |
	pt _ aPoint asPoint.
	self a13: self a13 + pt x.
	self a23: self a23 + pt y.
	^ self! !

!MatrixTransform2x3 methodsFor: 'modifying' stamp: 'jmv 1/7/2015 09:39'!
withTranslation: aPoint
	"set an offset in the receiver

	Answer the modified object. In this implementation is self, but some classes of transformations,
	more restricted ones (like a possible NullTransformation or sch) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	| pt |
	pt _ aPoint asPoint.
	self a13: pt x.
	self a23: pt y.
	^ self! !

!MatrixTransform2x3 methodsFor: 'modifying' stamp: 'jmv 1/7/2015 09:39'!
withYAxisNegated
	"Swap inneer point Y sign.
	Make y increment upwards.
	This makes the any matrix transform from standard mathematical coordinates
	to standard display coordinates (in addition to the transform it was already doing)

	Answer the modified object. In this implementation is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	self a12: self a12  negated.
	self a22: self a22  negated! !

!MatrixTransform2x3 methodsFor: 'initialization' stamp: 'jmv 1/7/2015 07:51'!
initialize
	"Initialize the receiver to the identity transformation (e.g., not affecting points)"
	self
		a11: 1.0;
		a22: 1.0! !


!MorphicCanvas methodsFor: 'initialization' stamp: 'jmv 1/7/2015 20:20'!
initializeWith: aForm origin: aPoint
	self initialize.
	self setForm: aForm.

	"We currently set up these only on initialization.
	This is safe (wrt walkbacks during world redraw) because a new instance is created
	each time the world is redrawn. See #drawInvalidAreasWorld:submorphs:
	Maybe this cleanup should be in an aux method that can be called each time on an existing instance..."
	currentTransformation _ MatrixTransform2x3 withTranslation: aPoint.
	cti _ 1.
	transformations
		ifNil: [ transformations _ OrderedCollection with: currentTransformation ]
		ifNotNil: [ transformations at: cti put: currentTransformation ]! !


!BitBltCanvas methodsFor: 'initialization' stamp: 'jmv 1/7/2015 09:19'!
initializeWith: aForm origin: aPoint

	super initializeWith: aForm origin: aPoint.
	self resetGrafPort! !


!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 1/7/2015 08:29'!
internalizeDelta: aPoint
	"Internalize a distance vector. A distance is not a position. It is a magnitude with a direction.
	It is usually used as a delta to be added to a position to obtain some other position."

	| x y det a11 a12 a21 a22 detX detY |
	x _ aPoint x.
	y _ aPoint y.
	a11 _ self a11.
	a12 _ self a12.
	a21 _ self a21.
	a22 _ self a22.
	det _ (a11 * a22) - (a12 * a21).
	det = 0.0 ifTrue: [ ^0@0 ].		"So we have at least a valid result"
	det _ 1.0 / det.
	detX _ (x * a22) - (a12 * y).
	detY _ (a11 * y) - (x * a21).
	^(detX * det) @ (detY * det)! !

!MatrixTransform2x3 methodsFor: 'private' stamp: 'jmv 1/7/2015 07:56'!
setRadians: radians scale: scale
	"Set the raw rotation angle in the receiver. private"

	| s c |
	s _ radians sin * scale.
	c _ radians cos * scale.
	self a11: c.
	self a12: s negated.
	self a21: s.
	self a22: c! !

!MatrixTransform2x3 methodsFor: 'private' stamp: 'jmv 1/7/2015 08:29'!
setTranslation: aPoint
	"private. Set the raw offset in the receiver"

	| pt |
	pt := aPoint asPoint.
	self a13: pt x.
	self a23: pt y! !

!MatrixTransform2x3 methodsFor: 'private - primitives' stamp: 'jmv 1/7/2015 08:28'!
floatInverseTransform: aPoint
	"Apply the inverse transformation to aPoint, i.e. multiply our inverse by aPoint.
	Smalltalk code for Float conversion."
	| x y det a11 a12 a21 a22 detX detY |

	x _ aPoint x - self a13.
	y _ aPoint y - self a23.
	a11 _ self a11.
	a12 _ self a12.
	a21 _ self a21.
	a22 _ self a22.
	det _ (a11 * a22) - (a12 * a21).
	det = 0.0 ifTrue: [ ^0@0 ].		"So we have at least a valid result"
	det _ 1.0 / det.
	detX _ (x * a22) - (a12 * y).
	detY _ (a11 * y) - (x * a21).
	^(detX * det) @ (detY * det)! !


!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'jmv 1/7/2015 07:53'!
new
	^(self basicNew: 6) initialize! !

!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'jmv 1/7/2015 07:53'!
withPosition: aPoint
	"scale is one, angle is zero"

	^self new setTranslation: aPoint! !

!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'jmv 1/7/2015 09:32'!
withTranslation: aPoint
	"scale is one, angle is zero"

	^self new setTranslation: aPoint! !


!Morph methodsFor: 'geometry' stamp: 'jmv 1/7/2015 09:09'!
morphPosition: aPoint
	"Change the position of this morph. Argument is in owner's coordinates."

	(location isTranslation: aPoint) ifTrue: [
		^ self ].		"Null change"

	self redrawNeeded.
	location _ location withTranslation: aPoint.
	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 1/7/2015 09:09'!
morphPositionInWorld: newPositionInWorld
	"Change the position of this morph."
	"El tema es, que tipo de coordenadas tenemos?
	En un mundo relativista, no hay un marco de referencia absoluto.
	No tiene sentido hablar de coordenadas del mundo... El mundo podria estar escalado... 
		Que tienen de especial las coordenadas del mundo?
	Coordenadas 'del hardware'? No deberia saber mucho sobre el... Puede haber multiples displays, hands de diverso tipo, remotas, virtuales...
	
	En ppio, un par de coordenadas pueden ser relativas a cualquier morph. Pareciera que necesito metodos de conversion de cualquier morph hacia mi, y de mi hacia cualquier morph... Como encontrar un marco de referencia comun????
	Dejar esto para despues. En realidad, para empezar, preciso menos: Solo preciso saber si las coordenadas estan en el morph o en su owner. Nada mas. Los eventos se iran transformando apropiadamente al moverse por el arbol, o al menos, llevaran consigo una transformacion (MatrixTransform2x3) que se ira actualizando"

	| newPositionInOwner |
	self flag: #jmvVer2.
	"This method MUST die"

	newPositionInOwner _ owner
		ifNotNil: [ owner internalizeFromWorld: newPositionInWorld ]
		ifNil: [ newPositionInWorld ].

	(location isTranslation: newPositionInOwner) ifTrue: [
		^ self ].		"Null change".

	self redrawNeeded.
	location _ location withTranslation: newPositionInOwner.
	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!Morph methodsFor: 'initialization' stamp: 'jmv 1/7/2015 20:27'!
initialize
	"initialize the state of the receiver"

	owner _ nil.
	submorphs _ #().
	location _ MatrixTransform2x3 new.
	layoutNeeded _ false! !

!Morph methodsFor: 'private' stamp: 'jmv 1/7/2015 09:09'!
privateOwner: aMorph
	"Private!! Should only be used by methods that maintain the ower/submorph invariant."

	| oldGlobalPosition prevOwner |

	self flag: #jmvVer2.
	"Is this the best behavior???"
	prevOwner _ owner.
	prevOwner
		ifNotNil: [
			"Had an owner. Maintain my global position..."
			oldGlobalPosition _ self morphPositionInWorld ].
	owner _ aMorph.
	owner
		ifNil: [
			"Won't have any owner. Keep local position, as it will be maintained in my new owner later"
			]
		ifNotNil: [
			prevOwner
				ifNil: [
					"Didn't have any owner. Assume my local position is to be maintained in my new owner"
					]
				ifNotNil: [
					"Had an owner. Maintain my global position..."
					location _ location withTranslation: (owner internalizeFromWorld: oldGlobalPosition).
					self flag: #jmvVer2.
					"extent _ owner internalizeDistanceFromWorld: oldGlobalExtent" 	"or something like this!!"
					]]! !

!Morph methodsFor: 'private' stamp: 'jmv 1/7/2015 09:10'!
privatePosition: aPoint
	"Change the position of this morph. Argument is in owner's coordinates."

	(location isTranslation: aPoint) ifTrue: [
		^ self ].		"Null change"

	location _ location withTranslation: aPoint! !


!FunctionGraphMorph methodsFor: 'initialization' stamp: 'jmv 1/7/2015 09:35'!
initialize
	super initialize.
 	location _ (location
		withYAxisNegated
		withAddedOffset: 300)
		scaledBy: 40.
	functions _ OrderedCollection new.
	colors _ OrderedCollection new! !


!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 1/7/2015 09:32'!
morphPosition: newPos extent: newExtent
	"Change the position of this morph. Argument is in owner's coordinates."

	| oldBoundsInWorld someChange |

	"Ask for the old bounds before updating them, but ask for repair only if extent was really changed."
	oldBoundsInWorld _ self morphBoundsInWorld.
	someChange _ false.
	(location isTranslation: newPos) ifFalse: [
		location _ location withTranslation: newPos.
		someChange _ true ].

	extent = newExtent ifFalse: [
		(self privateExtent: newExtent) ifTrue: [
			someChange _ true ]].

	someChange ifTrue: [
		"Ask for the old bounds before updating them, but ask for repair only if extent was really changed."
		oldBoundsInWorld ifNotNil: [
			self invalidateDisplayRect: oldBoundsInWorld from: nil ].
		self someSubmorphPositionOrExtentChanged.
		owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
		self redrawNeeded ]! !


!PasteUpMorph methodsFor: 'project state' stamp: 'jmv 1/7/2015 09:31'!
viewBox: newViewBox
	self flag: #jmvVer2.
	self isWorldMorph ifTrue: [
		(self viewBox isNil or: [ self viewBox extent ~= newViewBox extent ])
			ifTrue: [ worldState clearCanvas ].
		worldState viewBox: newViewBox ].
	location _ location withTranslation: (owner
		ifNil: [ newViewBox topLeft ]
		ifNotNil: [ owner internalizeFromWorld: newViewBox topLeft ]).
	extent _ newViewBox extent! !


!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 1/7/2015 09:15'!
on: aForm over: aRectangle
	"Warning: aForm extent must equal aRectangle extent"

	^ self basicNew
		initializeWith: aForm origin: aRectangle topLeft negated! !

!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 1/7/2015 09:16'!
onForm: aForm

	^ self basicNew
		initializeWith: aForm origin: 0@0! !

!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 1/7/2015 09:16'!
withExtent: extent depth: depth

	^ self onForm: (Form extent: extent depth: depth)! !

!methodRemoval: MorphicCanvas #initialize!
MorphicCanvas removeSelector: #initialize!
!methodRemoval: MorphicCanvas #setOrigin:!
MorphicCanvas removeSelector: #setOrigin:!
!methodRemoval: MatrixTransform2x3 class #identity!
MatrixTransform2x3 class removeSelector: #identity!
!methodRemoval: MatrixTransform2x3 #addOffset:!
MatrixTransform2x3 removeSelector: #addOffset:!
!methodRemoval: MatrixTransform2x3 #italics!
MatrixTransform2x3 removeSelector: #italics!
!methodRemoval: MatrixTransform2x3 #negateYAxis!
MatrixTransform2x3 removeSelector: #negateYAxis!
!methodRemoval: MatrixTransform2x3 #rotateBy:!
MatrixTransform2x3 removeSelector: #rotateBy:!
!methodRemoval: MatrixTransform2x3 #scaleBy:!
MatrixTransform2x3 removeSelector: #scaleBy:!
!methodRemoval: MatrixTransform2x3 #scaleByNumber:rotateBy:!
MatrixTransform2x3 removeSelector: #scaleByNumber:rotateBy:!
!methodRemoval: MatrixTransform2x3 #setIdentity!
MatrixTransform2x3 removeSelector: #setIdentity!

!MatrixTransform2x3 reorganize!
('converting coordinates' externalizeDelta: externalizePosition: externalizeScalar: internalizeDelta: internalizePosition: internalizeScalar: inverseTransform: transform:)
('accessing' at: at:put: degrees inverseTransformation radians scale translation)
('comparing' = hash)
('composing' composedWith: composedWith:into: composedWithSelf:)
('element access' a11 a11: a12 a12: a13 a13: a21 a21: a22 a22: a23 a23:)
('objects from disk' byteSize bytesPerBasicElement restoreEndianness writeOn:)
('printing' matrixPrintString print printMatrixOn: printOn:)
('testing' doesNotRotate isIdentity isPureTranslation isTranslation:)
('private' setRadians:scale: setTranslation:)
('modifying' italizing rotatedBy: scaledBy: scaledByNumber:rotateBy: withAddedOffset: withTranslation: withYAxisNegated)
('transforming rects' displayBoundsOfInverseTransformOf: displayBoundsOfInverseTransformOf:into: displayBoundsOfTransformOf: displayBoundsOfTransformOf:into: primDisplayBoundsOfInverseTransformOf:into: primDisplayBoundsOfTransformOf:into:)
('inspecting' customizeExplorerContents explorerContents inspectorClass)
('private - primitives' fixRoundingInPrimitives: floatInverseTransform: floatTransform: intInverseTransform: intTransform: primInverseTransform: primTransform:)
('initialization' initialize)
!

