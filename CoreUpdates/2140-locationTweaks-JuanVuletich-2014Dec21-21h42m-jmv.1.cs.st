'From Cuis 4.2 of 25 July 2013 [latest update: #2139] on 21 December 2014 at 9:57:19.492695 pm'!

!MatrixTransform2x3 methodsFor: 'printing' stamp: 'jmv 12/21/2014 21:42'!
printOn: aStream
	"Note:
	Will not work correctly for shear (skew) transformations, or different scale in x and y.
	Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these."
	aStream
		nextPutAll: self class name;
		nextPutAll: '(scale: '.
	self scale printOn: aStream.
	aStream nextPutAll: '. degrees: '.
	self degrees printOn: aStream.
	aStream nextPutAll: '. translation '.
	self translation printOn: aStream.
	aStream nextPutAll: ') '! !

!MatrixTransform2x3 methodsFor: 'inspecting' stamp: 'jmv 12/21/2014 21:42'!
explorerContents

	^{
		ObjectExplorerWrapper
			with: self scale
			name: 'scale'
			model: self. 
		ObjectExplorerWrapper
			with: self degrees
			name: 'degrees'
			model: self. 
		ObjectExplorerWrapper
			with: self translation
			name: 'translation'
			model: self }! !


!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'jmv 12/21/2014 21:55'!
withPosition: aPoint
	"scale is one, angle is zero"

	^self identity setTranslation: aPoint! !

!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'jmv 12/21/2014 21:55'!
withRadians: radians scale: aNumber position: aPoint
	"Translation is added at the end. This means that aPoint is in the outer coordinate space.
	MatrixTransform2x3 withRadians: -3 scale: 12 position: 4.5@3
	"
	^self new
		setRadians: radians scale: aNumber;
		setTranslation: aPoint! !


!Morph methodsFor: 'geometry' stamp: 'jmv 12/21/2014 21:43'!
morphPosition
	"Answer our position inside our owner, in owner's coordinates."

	^ location translation! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/21/2014 21:55'!
morphPosition: aPoint
	"Change the position of this morph. Argument is in owner's coordinates."

	location translation = aPoint ifTrue: [
		^ self ].		"Null change"

	self redrawNeeded.
	location setTranslation: aPoint.
	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/21/2014 21:55'!
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

	location translation = newPositionInOwner ifTrue: [
		^ self ].		"Null change".

	self redrawNeeded.
	location setTranslation: newPositionInOwner.
	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!Morph methodsFor: 'private' stamp: 'jmv 12/21/2014 21:55'!
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
					location setTranslation: (owner internalizeFromWorld: oldGlobalPosition).
					self flag: #jmvVer2.
					"extent _ owner internalizeDistanceFromWorld: oldGlobalExtent" 	"or something like this!!"
					]]! !

!Morph methodsFor: 'private' stamp: 'jmv 12/21/2014 21:55'!
privatePosition: aPoint
	"Change the position of this morph. Argument is in owner's coordinates."

	location translation = aPoint ifTrue: [
		^ self ].		"Null change"

	location setTranslation: aPoint! !


!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 12/21/2014 21:55'!
morphPosition: newPos extent: newExtent
	"Change the position of this morph. Argument is in owner's coordinates."

	| oldBoundsInWorld someChange |

	"Ask for the old bounds before updating them, but ask for repair only if extent was really changed."
	oldBoundsInWorld _ self morphBoundsInWorld.
	someChange _ false.
	location translation = newPos ifFalse: [
		location setTranslation: newPos.
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


!PasteUpMorph methodsFor: 'project state' stamp: 'jmv 12/21/2014 21:55'!
viewBox: newViewBox
	self flag: #jmvVer2.
	self isWorldMorph ifTrue: [
		(self viewBox isNil or: [ self viewBox extent ~= newViewBox extent ])
			ifTrue: [ worldState clearCanvas ].
		worldState viewBox: newViewBox ].
	location setTranslation: (owner
		ifNil: [ newViewBox topLeft ]
		ifNotNil: [ owner internalizeFromWorld: newViewBox topLeft ]).
	extent _ newViewBox extent! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 12/21/2014 21:50'!
makeMeFullyVisible 

	self world morphExtent > (0@0) ifFalse: [^ self].

	(self morphPosition >= (0@0) and: [ self morphPosition < (self world morphExtent-self morphExtent)]) ifTrue: [
		^ self "OK -- visible"].

	self morphPosition: (RealEstateAgent initialFrameFor: self initialExtent: self morphExtentInWorld world: self world) topLeft! !

!methodRemoval: MatrixTransform2x3 #position!
MatrixTransform2x3 removeSelector: #position!
!methodRemoval: MatrixTransform2x3 #setOffset:!
MatrixTransform2x3 removeSelector: #setOffset:!
!methodRemoval: MatrixTransform2x3 #setPosition:!
MatrixTransform2x3 removeSelector: #setPosition:!
