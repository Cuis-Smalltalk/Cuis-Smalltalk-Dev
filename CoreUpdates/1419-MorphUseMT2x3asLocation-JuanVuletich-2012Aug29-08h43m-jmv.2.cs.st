'From Cuis 4.0 of 21 April 2012 [latest update: #1418] on 29 August 2012 at 9:01:08 am'!
!classDefinition: #Morph category: #'Morphic-Kernel'!
Object subclass: #Morph
	instanceVariableNames: 'owner submorphs extension position location layoutNeeded '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!MatrixTransform2x3 methodsFor: 'accessing' stamp: 'jmv 8/29/2012 08:32'!
position
	"The position of 0@0 in the external coordinate system.
	It is the translation we apply when transforming points."
	^self a13 @ self a23! !

!MatrixTransform2x3 methodsFor: 'accessing' stamp: 'jmv 8/29/2012 08:35'!
scale
	"Answer the *scalar* scale applied by the receiver. Will not work correctly for shear (skew) transformations, or different scale in x and y.
	Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these."

	^(self a11 squared + self a21 squared) sqrt! !

!MatrixTransform2x3 methodsFor: 'printing' stamp: 'jmv 8/29/2012 08:22'!
matrixPrintString
	^String streamContents: [ :strm | self printMatrixOn: strm ]! !

!MatrixTransform2x3 methodsFor: 'printing' stamp: 'jmv 8/29/2012 08:21'!
printMatrixOn: aStream
	aStream
		newLine;
		nextPutAll: '| ';
		nextPutAll: (self a11 printPaddedLeft: 1 decimalPlaces: 3);
		space; space;
		nextPutAll: (self a12 printPaddedLeft: 1 decimalPlaces: 3);
		space; space;
		nextPutAll: (self a13 printPaddedLeft: 1 decimalPlaces: 3);
		nextPutAll: ' |';

		newLine;
		nextPutAll: '| ';
		nextPutAll: (self a21 printPaddedLeft: 1 decimalPlaces: 3);
		space; space;
		nextPutAll: (self a22 printPaddedLeft: 1 decimalPlaces: 3);
		space; space;
		nextPutAll: (self a23 printPaddedLeft: 1 decimalPlaces: 3);
		nextPutAll: ' |';
		newLine! !

!MatrixTransform2x3 methodsFor: 'private' stamp: 'jmv 8/29/2012 08:49'!
setPosition: aPoint
	"Set the raw offset in the receiver.
	We call it 'position' when using the instance as the location of a Morph."
	| pt |
	pt := aPoint asPoint.
	self a13: pt x asFloat.
	self a23: pt y asFloat.! !

!MatrixTransform2x3 methodsFor: 'private' stamp: 'jmv 8/29/2012 08:40'!
setRadians: radians scale: aNumber position: aPoint
	"Set the raw rotation angle in the receiver"
	| s c pt |
	s _ radians sin * aNumber.
	c _ radians cos * aNumber.
	self a11: c.
	self a12: s negated.
	self a21: s.
	self a22: c.
	pt _ aPoint asPoint.
	self a13: pt x asFloat.
	self a23: pt y asFloat.! !


!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'jmv 8/29/2012 08:53'!
withPosition: aPoint
	"scale is one, angle is zero"

	^self identity setPosition: aPoint! !

!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'jmv 8/29/2012 08:40'!
withRadians: radians scale: aNumber position: aPoint
	"Translation is added at the end. This means that aPoint is in the outer coordinate space.
	MatrixTransform2x3 withRadians: -3 scale: 12 translation: 4.5@3
	"
	^self new setRadians: radians scale: aNumber position: aPoint! !


!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 8/29/2012 08:34'!
externalizeScalar: aNumber
	"Externalize a distance (without a direction).
	 Only valid if we preserve aspect ratio (meaning that the scale factor is the same in all directions)."

	^aNumber * self scale! !

!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 8/29/2012 08:34'!
internalizeScalar: aNumber
	"Internalize a distance (without a direction). 
	 Only valid if we preserve aspect ratio (meaning that the scale factor is the same in all directions)."

	^aNumber / self scale! !

!MatrixTransform2x3 methodsFor: 'accessing' stamp: 'jmv 8/29/2012 08:34'!
printOn: aStream
	"Note:
	Will not work correctly for shear (skew) transformations, or different scale in x and y.
	Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these."
	aStream
		nextPutAll: self class name;
		nextPutAll: '( scale: '.
	self scale printOn: aStream.
	aStream nextPutAll: '. radians: '.
	self radians printOn: aStream.
	aStream nextPutAll: '. position: '.
	self position printOn: aStream.
	aStream nextPutAll: ') '! !

!MatrixTransform2x3 methodsFor: 'accessing' stamp: 'jmv 8/29/2012 08:26'!
radians
	"Answer the angle in radians applied by the receiver. Will not work correctly for shear (skew) transformations, or different scale in x and y.
	Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these."

	^ self a21 arcTan: self a11! !

!MatrixTransform2x3 methodsFor: 'accessing' stamp: 'jmv 8/29/2012 08:46'!
translation
	"Translation and position are the same.
	Use the word translation when thinking about coordinate transformation, but use
	the word position when thinking about morph locations"

	^self a13 @ self a23! !


!Morph methodsFor: 'geometry' stamp: 'jmv 8/29/2012 08:29'!
externalize: aPoint
	"aPoint is in own coordinates. Answer is in owner's coordinates."
	"Must include scale and rotation!!"
	self flag: #jmvVer2.
	^ location externalizePosition: aPoint! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/29/2012 08:30'!
externalizeToWorld: aPoint
	"aPoint is in own coordinates. Answer is in world coordinates.
	BUT there is no well defined World!!"
	| inOwners |
	self flag: #jmvVer2.

	inOwners _ self externalize: aPoint.
	^owner
		ifNotNil: [ owner externalizeToWorld: inOwners ]
		ifNil: [ inOwners ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/29/2012 08:38'!
internalize: aPoint
	"aPoint is in owner's coordinates. Answer is in own coordinates."
	"Must include scale and rotation!!"
	self flag: #jmvVer2.
	aPoint ifNil: [ 
		'---------- internalize: nil' print.
		thisContext printStack: 10.
		^nil ].	"sacar esto!!"
	^ location internalizePosition: aPoint! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/29/2012 08:39'!
internalizeFromWorld: aPoint
	"aPoint is in World coordinates. Answer is in own coordinates."
	| inOwners |
	self flag: #jmvVer2.
	aPoint ifNil: [ 
		'---------- internalizeFromWorld: nil' print.
		thisContext printStack: 10.
		^nil ].	"sacar esto!!"
	inOwners _ owner
		ifNotNil: [ owner internalizeFromWorld: aPoint ]
		ifNil: [ aPoint ].
	^self internalize: inOwners! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/29/2012 08:59'!
morphPositionInOwner
"
Cuando tenga resueltos los setters (o quizas al mismo tiempo),
ver que senders del getter quieren en realidad #zzpositionInOwner y quienes #zzpositionInWorld.
Espero que pocos hablen en coordenadas del world!!
"

	self flag: #jmvVer2.
	"Maybe we don't really need an owner to answer this..."
	self validateOwnerNotNil.

	^ location position! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/29/2012 09:00'!
morphPositionInOwner: newPositionInOwner
	"Change the position of this morph."
"Ojo aca. Ponemos position a nil pero es solo por el problema de bounds desenganchado de position..."

	self flag: #jmvVer2.

	location position = newPositionInOwner ifTrue: [
		^ self ].		"Null change"

	self redrawNeeded.

	"Maybe we don't really need an owner to run this method..."
	self validateOwnerNotNil.

	location setPosition: newPositionInOwner.
	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/29/2012 08:55'!
morphPositionInWorld
"
Cuando tenga resueltos los setters (o quizas al mismo tiempo),
ver que senders del getter quieren en realidad #zzpositionInOwner y quienes #zzpositionInWorld.
Espero que pocos hablen en coordenadas del world!!
"


	self flag: #jmvVer2.
	"Most likely we don't want to use global coordinates...
	In fact, we could be in many frames of reference at the same time...
	This method makes no sense at all!!"
	self validateOwnerNotNil.

	^self externalizeToWorld: 0@0! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/29/2012 09:00'!
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
	self validateOwnerNotNil.

	newPositionInOwner _ owner
		ifNotNil: [ owner internalizeFromWorld: newPositionInWorld ]
		ifNil: [ newPositionInWorld ].

	location position = newPositionInOwner ifTrue: [
		^ self ].		"Null change".

	self redrawNeeded.
	location setPosition: newPositionInOwner.
	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!Morph methodsFor: 'initialization' stamp: 'jmv 8/29/2012 09:00'!
initialize
	"initialize the state of the receiver"

	| b |
	owner _ nil.
	submorphs _ #().
	
	self flag: #jmvVer2.
	"Ir convirtiendo todos los usos (no las asignaciones!!) a las vars nuevas.
	Despues eliminar las asignaciones y las propias ivars (bounds y fullBounds)"
	b _ self defaultBounds.
	location _ MatrixTransform2x3 withPosition: b topLeft.
	layoutNeeded _ false! !

!Morph methodsFor: 'private' stamp: 'jmv 8/29/2012 08:59'!
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
					location setPosition: (owner internalizeFromWorld: oldGlobalPosition).
					self flag: #jmvVer2.
					"extent _ owner internalizeDistanceFromWorld: oldGlobalExtent" 	"or something like this!!"
					]]! !


!HandMorph methodsFor: 'initialization' stamp: 'jmv 8/29/2012 09:00'!
initialize
	super initialize.
	self initForEvents.
	keyboardFocus _ nil.
	mouseFocus _ nil.
	location setPosition: 0@0.
	extent _ CursorWithMask normal extent.
	damageRecorder _ DamageRecorder new.
	self initForEvents.! !


!MenuMorph methodsFor: 'initialization' stamp: 'jmv 8/29/2012 09:00'!
initialize
	super initialize.
	location setPosition: 0@0.
	extent _ 40@10.
	defaultTarget _ nil.
	selectedItem _ nil.
	stayUp _ false.
	popUpOwner _ nil! !


!PasteUpMorph methodsFor: 'project state' stamp: 'jmv 8/29/2012 09:00'!
viewBox: newViewBox
	self flag: #jmvVer2.
	self isWorldMorph ifTrue: [
		(self viewBox isNil or: [ self viewBox extent ~= newViewBox extent ])
			ifTrue: [ worldState canvas: nil ].
		worldState viewBox: newViewBox ].
	location setPosition: (owner
		ifNil: [ newViewBox topLeft ]
		ifNotNil: [ owner internalizeFromWorld: newViewBox topLeft ]).
	extent _ newViewBox extent! !

!classDefinition: #Morph category: #'Morphic-Kernel'!
Object subclass: #Morph
	instanceVariableNames: 'owner submorphs extension location layoutNeeded'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!
!methodRemoval: MatrixTransform2x3 class #withRadians:scale:translation:!
MatrixTransform2x3 class removeSelector: #withRadians:scale:translation:!
!methodRemoval: MatrixTransform2x3 #scalarScale!
MatrixTransform2x3 removeSelector: #scalarScale!
!methodRemoval: MatrixTransform2x3 #setRadians:scale:translation:!
MatrixTransform2x3 removeSelector: #setRadians:scale:translation:!

!MatrixTransform2x3 reorganize!
('converting coordinates' externalizeDistance: externalizePosition: externalizeScalar: internalizeDistance: internalizePosition: internalizeScalar: inverseTransform: inverseTransformPoints: transform: transformPositions:)
('accessing' at: at:put: inverseTransformation position printOn: radians scale translation)
('comparing' = hash)
('composing' composedWith: composedWith:into:)
('element access' a11 a11: a12 a12: a13 a13: a21 a21: a22 a22: a23 a23:)
('initialize' setIdentiy)
('objects from disk' byteSize bytesPerBasicElement restoreEndianness writeOn:)
('printing' matrixPrintString print printMatrixOn:)
('testing' isIdentity isPureTranslation)
('private' setPosition: setRadians: setRadians:scale:position: setScale: setTranslation:)
('modifying' addOffset: rotateBy: scaleBy: scaleByNumber:rotateBy:)
('converting' asMatrix)
('transforming rects' displayBoundsOfInverseTransformOf: displayBoundsOfInverseTransformOf:into: displayBoundsOfTransformOf: displayBoundsOfTransformOf:into:)
!

