'From Cuis 5.0 [latest update: #4423] on 23 October 2020 at 11:01:16 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 10/23/2020 20:52:00'!
scale
	^1! !


!MovableMorph methodsFor: 'accessing' stamp: 'jmv 10/23/2020 20:34:55'!
location
	^location! !

!MovableMorph methodsFor: 'geometry' stamp: 'jmv 10/23/2020 21:51:57'!
allocHeightForFactor: aFactor

	"Morphs with LayoutSpecs may specify propottional layouts and minimum extents
	  amountToAlloc * proportion * factor >= minHeight
	so breakeven is when
	  amountToAlloc = minHeight / proportion * factor"
	| minHeight |
	minHeight := self minimumLayoutHeight.
	^(self isProportionalHeight)
		ifFalse:  [ minHeight ]
		ifTrue: [ minHeight / (aFactor * self layoutSpec privateProportionalHeight) ]! !

!MovableMorph methodsFor: 'geometry' stamp: 'jmv 10/23/2020 21:52:01'!
allocWidthForFactor: aFactor

	"Morphs with LayoutSpecs may specify propottional layouts and minimum extents
	  amountToAlloc * proportion * factor >= minWidth
	so breakeven is when
	  amountToAlloc = minWidth / proportion * factor"
	| minWidth |
	minWidth := self minimumLayoutWidth.
	^(self isProportionalWidth)
		ifFalse:  [ minWidth ]
		ifTrue: [ minWidth / (aFactor * self layoutSpec privateProportionalWidth) ]! !

!MovableMorph methodsFor: 'geometry' stamp: 'jmv 10/23/2020 20:35:29'!
externalize: aPoint
	"aPoint is in own coordinates. Answer is in owner's coordinates."
	"Must include scale and rotation!!"
	self flag: #jmvVer2.
	^ location externalizePosition: aPoint! !

!MovableMorph methodsFor: 'geometry' stamp: 'jmv 10/23/2020 20:36:02'!
externalizeDisplayBounds: r

	| inOwners |
	"All senders of #displayBoundsOfTransformOf: should be rethought..."
	self flag: #jmvVer2.

	inOwners _ location displayBoundsOfTransformOf: r.
	^owner
		ifNotNil: [ owner externalizeDisplayBounds: inOwners ]
		ifNil: [ inOwners ]! !

!MovableMorph methodsFor: 'geometry' stamp: 'jmv 10/23/2020 20:36:07'!
externalizeDistance: aPoint
	"aPoint is in own coordinates. Answer is in owner's coordinates."
	^ location externalizeDelta: aPoint! !

!MovableMorph methodsFor: 'geometry' stamp: 'jmv 10/23/2020 20:36:17'!
internalize: aPoint
	"aPoint is in owner's coordinates. Answer is in own coordinates."
	^ location internalizePosition: aPoint! !

!MovableMorph methodsFor: 'geometry' stamp: 'jmv 10/23/2020 20:36:22'!
internalizeDistance: aPoint
	"aPoint is in owner's coordinates. Answer is in own coordinates."
	^ location internalizeDelta: aPoint! !

!MovableMorph methodsFor: 'geometry' stamp: 'jmv 10/23/2020 20:38:46'!
minimumLayoutExtent
	"This returns the minimum extent that the morph may be shrunk to, 
	when resizing LayoutMorphs. 
	It is expressed in the morph own coordinates, like morphExtent."

	"This method combines the information from a Morph and its optional LayoutSpec"
	| minExtent |
	minExtent _ self minimumExtent.
	^ layoutSpec
		ifNil: [ minExtent ]
		ifNotNil: [ minExtent max: ( layoutSpec minimumSpecWidth @ layoutSpec minimumSpecHeight ) ]! !

!MovableMorph methodsFor: 'geometry' stamp: 'jmv 10/23/2020 21:59:39'!
morphAlign: aPoint with: anotherPoint
	^ self morphPosition: self morphPosition + anotherPoint - aPoint! !

!MovableMorph methodsFor: 'geometry' stamp: 'jmv 10/23/2020 20:36:27'!
morphPosition
	"Answer our position inside our owner, in owner's coordinates."

	^ location translation! !

!MovableMorph methodsFor: 'geometry' stamp: 'jmv 10/23/2020 20:36:47'!
morphPosition: aPoint
	"Change the position of this morph. Argument is in owner's coordinates."
	(location isTranslation: aPoint) ifTrue: [ "Null change"
		^ self ].
	location _ location withTranslation: aPoint.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded.! !

!MovableMorph methodsFor: 'geometry' stamp: 'jmv 10/23/2020 20:36:52'!
morphPositionInWorld: newPositionInWorld
	"Change the position of this morph."
	"El tema es, que tipo de coordenadas tenemos?
	En un mundo relativista, no hay un marco de referencia absoluto.
	No tiene sentido hablar de coordenadas del mundo... El mundo podria estar escalado... 
		Que tienen de especial las coordenadas del mundo?
	Coordenadas 'del hardware'? No deberia saber mucho sobre el... Puede haber multiples displays, hands de diverso tipo, remotas, virtuales...
	
	En ppio, un par de coordenadas pueden ser relativas a cualquier morph. Pareciera que necesito metodos de conversion de cualquier morph hacia mi, y de mi hacia cualquier morph... Como encontrar un marco de referencia comun????
	Dejar esto para despues. En realidad, para empezar, preciso menos: Solo preciso saber si las coordenadas estan en el morph o en su owner. Nada mas. Los eventos se iran transformando apropiadamente al moverse por el arbol, o al menos, llevaran consigo una transformacion (AffineTransformation) que se ira actualizando"

	| newPositionInOwner |
	self flag: #jmvVer2.
	"This method MUST die"

	newPositionInOwner _ owner
		ifNotNil: [ owner internalizeFromWorld: newPositionInWorld ]
		ifNil: [ newPositionInWorld ].

	(location isTranslation: newPositionInOwner) ifTrue: [
		^ self ].		"Null change".

	location _ location withTranslation: newPositionInOwner.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded.! !

!MovableMorph methodsFor: 'geometry' stamp: 'jmv 10/23/2020 20:36:56'!
orbitBy: radians
	"Change the scale of this morph. Arguments are an angle and a scale."
	location _ (AffineTransformation withRadians: radians) composedWith: location.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded.! !

!MovableMorph methodsFor: 'geometry' stamp: 'jmv 10/23/2020 22:12:43'!
referencePosition
	"Return the current reference position of the receiver"
	"a rather ugly way to say #center . At least, we avoid false polymorphism"
	"remove some day"
	self flag: #jmvVer2.
	^self morphExtentInWorld // 2 + self morphPositionInWorld! !

!MovableMorph methodsFor: 'geometry' stamp: 'jmv 10/23/2020 22:12:37'!
referencePosition: aPoint
	"a rather ugly way to say #center: . Just for consistency with #referencePosition"
	"remove some day"
	self flag: #jmvVer2.
	self morphPositionInWorld: aPoint - (self morphExtentInWorld // 2)! !

!MovableMorph methodsFor: 'geometry' stamp: 'jmv 10/23/2020 20:37:10'!
rotateBy: radians
	"Change the rotation of this morph. Argument is an angle (possibly negative), to be added to current rotation."

	location _ location rotatedBy: radians.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded.! !

!MovableMorph methodsFor: 'geometry' stamp: 'jmv 10/23/2020 20:36:39'!
rotation: radians
	"Change the rotation of this morph. Argument is an angle to be taken as the new rotation."

    self rotation: radians scale: location scale.! !

!MovableMorph methodsFor: 'geometry' stamp: 'jmv 10/23/2020 20:37:14'!
rotation: radians scale: scale
	"Change the rotation and scale of this morph. Arguments are an angle and a scale."
	location _ location withRotation: radians scale: scale.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded.! !

!MovableMorph methodsFor: 'geometry' stamp: 'jmv 10/23/2020 20:49:21'!
rotationDegrees: degrees
	location _ location rotatedBy: degrees degreesToRadians - location radians.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded.! !

!MovableMorph methodsFor: 'geometry' stamp: 'jmv 10/23/2020 20:51:47'!
scale
	^location scale! !

!MovableMorph methodsFor: 'geometry' stamp: 'jmv 10/23/2020 20:37:22'!
scaleBy: scaleFactor
	"Change the scale of this morph. Argument is a factor."
	location _ location scaledBy: scaleFactor.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded.! !

!MovableMorph methodsFor: 'initialization' stamp: 'jmv 10/23/2020 20:38:03'!
initialize
	"initialize the state of the receiver"

	super initialize.
	location _ MorphicTranslation new.! !

!MovableMorph methodsFor: 'initialization' stamp: 'jmv 10/23/2020 21:41:33'!
openInWorld: aWorld
	"Add this morph to the requested World."
	location isIdentity
		ifTrue: [ aWorld addMorph: self position: `50@50` ]
		ifFalse: [ aWorld addMorph: self ]! !

!MovableMorph methodsFor: 'layout' stamp: 'jmv 10/23/2020 21:54:32'!
minimumLayoutHeight
	"I combine information from a Morph and its optional LayoutSpec"
	| minHeight |

	minHeight _  self minimumExtent y. "from morph (me)."
	^ layoutSpec
		ifNil: [ minHeight ] 
		ifNotNil: [ :ls | minHeight max: (ls minimumSpecHeight )]
! !

!MovableMorph methodsFor: 'layout' stamp: 'jmv 10/23/2020 21:54:35'!
minimumLayoutWidth
	"I combine information from a Morph and its optional LayoutSpec"
	| minWidth |

	minWidth _  self minimumExtent x. "from morph (me)."
	^ layoutSpec
		ifNil: [ minWidth ] 
		ifNotNil: [ :ls | minWidth max: (ls minimumSpecWidth )]
! !

!MovableMorph methodsFor: 'layout' stamp: 'jmv 10/23/2020 20:38:51'!
minimumShrinkExtent
	"This returns the minimum extent that the morph may be shrunk to.
	It is expressed in the morph own coordinates, like morphExtent."

	| minExtent |
	minExtent _ self minimumExtent.
	^ layoutSpec
		ifNil: [ minExtent ]
		ifNotNil: [ minExtent max: layoutSpec minimumShrinkWidth @ layoutSpec minimumShrinkHeight ]! !

!MovableMorph methodsFor: 'layout' stamp: 'jmv 10/23/2020 20:38:57'!
minimumShrinkHeight
	"This returns the minimum height that the morph may be shrunk to by a LayoutAdjustingMorph.
	It is expressed in the morph own coordinates, like morphExtent."

	| minShrinkHeight |
	minShrinkHeight _ self minimumExtent y.
	^ layoutSpec
		ifNil: [ minShrinkHeight ]
		ifNotNil: [ minShrinkHeight max: layoutSpec minimumShrinkHeight ]! !

!MovableMorph methodsFor: 'layout' stamp: 'jmv 10/23/2020 20:39:01'!
minimumShrinkWidth
	"This returns the minimum width that the morph may be shrunk to by a LayoutAdjustingMorph.
	It is expressed in the morph own coordinates, like morphExtent."

	| minShrinkWidth |
	minShrinkWidth _ self minimumExtent x.
	^ layoutSpec
		ifNil: [ minShrinkWidth ]
		ifNotNil: [ minShrinkWidth max: layoutSpec minimumShrinkWidth ]! !

!MovableMorph methodsFor: 'private' stamp: 'jmv 10/23/2020 20:37:02'!
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

!MovableMorph methodsFor: 'private' stamp: 'jmv 10/23/2020 20:37:06'!
privatePosition: aPoint
	"Change the position of this morph. Argument is in owner's coordinates."

	(location isTranslation: aPoint) ifTrue: [
		^ self ].		"Null change"

	location _ location withTranslation: aPoint! !

!MovableMorph methodsFor: 'layout-properties' stamp: 'jmv 10/23/2020 20:39:08'!
layoutSpec
	"Layout specific. Return the layout spec describing where the
	receiver should appear in a proportional layout"

	layoutSpec ifNotNil: [ :ls | ^ ls ].
	layoutSpec := LayoutSpec keepMorphExtent.
	layoutSpec morph: self.

	^ layoutSpec ! !

!MovableMorph methodsFor: 'layout-properties' stamp: 'jmv 10/23/2020 20:39:14'!
layoutSpec: aLayoutSpec
	"Layout specific. Set the layout spec describing where the receiver should appear in a proportional layout"
	aLayoutSpec 
		useMorphWidth;
		useMorphHeight.
	self layoutSpec == aLayoutSpec ifTrue: [ ^self ].
	aLayoutSpec morph: self.
	layoutSpec := aLayoutSpec.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!MovableMorph methodsFor: 'testing' stamp: 'jmv 10/23/2020 21:54:25'!
isProportionalHeight
	"Answer true if I have a layoutSpec which specifies a proportional layout"

	^ layoutSpec
		ifNil: [ false ]
		ifNotNil: [ :ls | ls isProportionalHeight ]! !

!MovableMorph methodsFor: 'testing' stamp: 'jmv 10/23/2020 21:54:29'!
isProportionalWidth
	"Answer true if I have a layoutSpec which specifies a proportional layout"

	^ layoutSpec
		ifNil: [ false ]
		ifNotNil: [ :ls | ls isProportionalWidth ]! !


!Morph methodsFor: 'accessing' stamp: 'jmv 10/23/2020 21:16:48'!
location
	^nil! !

!Morph methodsFor: 'geometry' stamp: 'jmv 10/23/2020 21:18:12'!
externalize: aPoint
	"aPoint is in own coordinates. Answer is in owner's coordinates."
	"Must include scale and rotation!!"
	^ aPoint! !

!Morph methodsFor: 'geometry' stamp: 'jmv 10/23/2020 21:20:39'!
externalizeDisplayBounds: r

	"All senders of #displayBoundsOfTransformOf: should be rethought..."
	self flag: #jmvVer2.

	^owner
		ifNotNil: [ owner externalizeDisplayBounds: r ]
		ifNil: [ r encompassingIntegerRectangle ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 10/23/2020 21:22:26'!
externalizeDistance: aPoint
	"aPoint is in own coordinates. Answer is in owner's coordinates."
	^ aPoint! !

!Morph methodsFor: 'geometry' stamp: 'jmv 10/23/2020 21:22:46'!
internalize: aPoint
	"aPoint is in owner's coordinates. Answer is in own coordinates."
	^ aPoint! !

!Morph methodsFor: 'geometry' stamp: 'jmv 10/23/2020 21:23:01'!
internalizeDistance: aPoint
	"aPoint is in owner's coordinates. Answer is in own coordinates."
	^ aPoint! !

!Morph methodsFor: 'geometry' stamp: 'jmv 10/23/2020 21:21:42'!
morphExtentInWorld
	"eventually, remove."
	self flag: #jmvVer2.
	^(self externalizeDistanceToWorld: self morphExtent) ceiling! !

!Morph methodsFor: 'geometry' stamp: 'jmv 10/23/2020 22:00:55'!
morphPosition
	"Answer our position inside our owner, in owner's coordinates."
	self flag: #jmvVer2. self revisar. "useless"
	^ 0@0! !

!Morph methodsFor: 'geometry' stamp: 'jmv 10/23/2020 22:59:44'!
morphPosition: aPoint
	"Change the position of this morph. Argument is in owner's coordinates.
	Ignored by morphs that are not resizeable"! !

!Morph methodsFor: 'geometry' stamp: 'jmv 10/23/2020 23:00:38'!
morphPositionInWorld: newPositionInWorld
	"Change the position of this morph. Argument is in world coordinates.
	Ignored by morphs that are not movable"! !

!Morph methodsFor: 'geometry' stamp: 'jmv 10/23/2020 22:15:39'!
referencePosition
	"Return the current reference position of the receiver"
	"a rather ugly way to say #center . At least, we avoid false polymorphism"
	"remove some day"
	self flag: #jmvVer2. self revisar. "useless"
	^0@0! !

!Morph methodsFor: 'geometry' stamp: 'jmv 10/23/2020 22:04:37'!
rotation: radians scale: scale
	"Change the rotation and scale of this morph. Arguments are an angle and a scale.
	Ignored by us. Redefined in some subclasses."
	self flag: #jmvVer2. self revisar. "useless?"! !

!Morph methodsFor: 'initialization' stamp: 'jmv 10/23/2020 20:38:06'!
initialize
	"initialize the state of the receiver"

	owner _ nil.
	submorphs _ #().
	id _ 0.! !

!Morph methodsFor: 'initialization' stamp: 'jmv 10/23/2020 21:41:47'!
openInWorld: aWorld
	"Add this morph to the requested World."
	aWorld addMorph: self! !

!Morph methodsFor: 'layout' stamp: 'jmv 10/23/2020 21:28:05'!
minimumLayoutHeight
	"I combine information from a Morph and its optional LayoutSpec"

	^self minimumExtent y.! !

!Morph methodsFor: 'layout' stamp: 'jmv 10/23/2020 21:27:56'!
minimumLayoutWidth
	"I combine information from a Morph and its optional LayoutSpec"

	^self minimumExtent x.! !

!Morph methodsFor: 'layout' stamp: 'jmv 10/23/2020 21:46:12'!
minimumShrinkExtent
	"This returns the minimum extent that the morph may be shrunk to.
	It is expressed in the morph own coordinates, like morphExtent."

	^ self minimumExtent.! !

!Morph methodsFor: 'layout' stamp: 'jmv 10/23/2020 21:46:40'!
minimumShrinkHeight
	"This returns the minimum height that the morph may be shrunk to by a LayoutAdjustingMorph.
	It is expressed in the morph own coordinates, like morphExtent."

	^ self minimumExtent y.! !

!Morph methodsFor: 'layout' stamp: 'jmv 10/23/2020 21:46:53'!
minimumShrinkWidth
	"This returns the minimum width that the morph may be shrunk to by a LayoutAdjustingMorph.
	It is expressed in the morph own coordinates, like morphExtent."

	^ self minimumExtent x.! !

!Morph methodsFor: 'testing' stamp: 'jmv 10/23/2020 21:26:45'!
isProportionalHeight
	"Answer true if I have a layoutSpec which specifies a proportional layout"

	^false! !

!Morph methodsFor: 'testing' stamp: 'jmv 10/23/2020 21:49:21'!
isProportionalWidth
	"Answer true if I have a layoutSpec which specifies a proportional layout"

	^ false! !

!Morph methodsFor: 'private' stamp: 'jmv 10/23/2020 21:48:13'!
privateOwner: aMorph
	"Private!! Should only be used by methods that maintain the ower/submorph invariant."

	owner _ aMorph.! !

!Morph methodsFor: 'private' stamp: 'jmv 10/23/2020 23:00:44'!
privatePosition: aPoint
	"Change the position of this morph. Argument is in owner's coordinates.
	Ignored by morphs that are not movable"! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 10/23/2020 20:52:06'!
startRot: evt with: rotHandle
	"Initialize rotation of my target if it is rotatable.  Launch a command object to represent the action"

	evt hand obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	growingOrRotating _ true.

	self removeAllHandlesBut: rotHandle.  "remove all other handles"
	angleOffset _ evt eventPosition - target referencePosition.
	angleOffset _ Point
			r: angleOffset r
			degrees: angleOffset degrees - target rotationDegrees.
	scaleOffset _ target scale / (evt eventPosition - target referencePosition) rho.

! !


!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 10/23/2020 21:56:12'!
addMorphFrontFromWorldPosition: aMorph

	aMorph layoutSpec.
	self addMorphFront: aMorph.
	self layoutSubmorphs.
! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 10/23/2020 21:16:03'!
into: aMorph
	| locationOrNil previousLast |
	locationOrNil _ aMorph location.
	currentMorph _ aMorph.
	cti _ cti + 1.
	transformations size < cti
		ifTrue: [
			drawingMorphStack add: aMorph.
			currentTransformation _ locationOrNil
				ifNotNil: [ currentTransformation composedWith: locationOrNil]
				ifNil: [ currentTransformation copy ].
			transformations add: currentTransformation ]
		ifFalse: [
			drawingMorphStack at: cti put: aMorph.
			previousLast _ currentTransformation.
			locationOrNil
				ifNil: [
					"Podriamos reusar la instancia si nos sirve."
					currentTransformation _ previousLast copy.
					transformations at: cti put: currentTransformation ]
				ifNotNil: [ :location |
					currentTransformation _ transformations at: cti.
					"reuse the instance if possible"
					(previousLast class == location class and: [ previousLast class == currentTransformation class ])
						ifTrue: [
							previousLast composedWith: location into: currentTransformation ]
						ifFalse: [
							currentTransformation _ previousLast composedWith: location.
							transformations at: cti put: currentTransformation ]
				]
		]! !

!MorphicCanvas methodsFor: 'private' stamp: 'jmv 10/23/2020 21:19:51'!
externalizeDisplayBounds: r from: aMorph
	"r is a Rectangle, expressed in aMorph's coordinate system.
	Answer another rectangle, that bounds r when translated to World coordinates"

	"Should translate only to whatever world or PasteUp we are displaying.
	Fix when implementing multiple Canvases (Displays) showing different
	(potentially nested Worlds)"
	| inOwners owner |
	self flag: #jmvVer2.

	inOwners _ aMorph location
		ifNil: [ r ]
		ifNotNil: [ :tx | tx displayBoundsOfTransformOf: r ].
	owner _ aMorph owner.
	^owner
		ifNotNil: [ self externalizeDisplayBounds: inOwners from: owner]
		ifNil: [ inOwners encompassingIntegerRectangle ]! !

!methodRemoval: MovableMorph #addMorphFrontFromWorldPosition: stamp: 'jmv 10/23/2020 22:46:05'!
MovableMorph removeSelector: #addMorphFrontFromWorldPosition:!
!methodRemoval: Morph #layoutSpec: stamp: 'jmv 10/23/2020 22:44:05'!
Morph removeSelector: #layoutSpec:!
!methodRemoval: Morph #referencePosition: stamp: 'jmv 10/23/2020 22:45:24'!
Morph removeSelector: #referencePosition:!
!methodRemoval: Morph #morphAlign:with: stamp: 'jmv 10/23/2020 22:44:05'!
Morph removeSelector: #morphAlign:with:!
!methodRemoval: Morph #rotation: stamp: 'jmv 10/23/2020 22:44:05'!
Morph removeSelector: #rotation:!
!methodRemoval: Morph #rotationDegrees: stamp: 'jmv 10/23/2020 22:44:05'!
Morph removeSelector: #rotationDegrees:!
!methodRemoval: Morph #orbitBy: stamp: 'jmv 10/23/2020 22:44:05'!
Morph removeSelector: #orbitBy:!
!methodRemoval: Morph #rotateBy: stamp: 'jmv 10/23/2020 22:44:05'!
Morph removeSelector: #rotateBy:!
!methodRemoval: Morph #minimumLayoutExtent stamp: 'jmv 10/23/2020 22:44:05'!
Morph removeSelector: #minimumLayoutExtent!
!methodRemoval: Morph #scaleBy: stamp: 'jmv 10/23/2020 22:44:05'!
Morph removeSelector: #scaleBy:!
!methodRemoval: Morph #layoutSpec stamp: 'jmv 10/23/2020 22:44:05'!
Morph removeSelector: #layoutSpec!
!methodRemoval: Morph #allocHeightForFactor: stamp: 'jmv 10/23/2020 22:44:05'!
Morph removeSelector: #allocHeightForFactor:!
!methodRemoval: Morph #allocWidthForFactor: stamp: 'jmv 10/23/2020 22:44:05'!
Morph removeSelector: #allocWidthForFactor:!
!methodRemoval: Morph #layoutSpecOrNil stamp: 'jmv 10/23/2020 22:44:05'!
Morph removeSelector: #layoutSpecOrNil!

!Morph reorganize!
('accessing' adoptWidgetsColor: beSticky color location lock morphId resistsRemoval taskbar toggleStickiness unlock unlockContents)
('accessing - flags' isLayoutNeeded isRedrawNeeded isSubmorphRedrawNeeded layoutNeeded: needsRedraw: submorphNeedsRedraw: visible)
('accessing - properties' hasProperty: isLocked isSticky lock: name name: removeProperty: setProperty:toValue: sticky: valueOfProperty: valueOfProperty:ifAbsent: valueOfProperty:ifPresentDo:)
('as yet unclassified' canDiscardEdits disregardUnacceptedEdits whenUIinSafeState:)
('caching' clearId fullReleaseCachedState releaseCachedState)
('change reporting' addedMorph: invalidateDisplayRect:fromSubmorph:for: invalidateLocalRect:)
('classification' isWorldMorph)
('copying' copy copyForClipboard duplicate)
('debug and other' addDebuggingItemsTo:hand: buildDebugMenu: inspectOwnerChain ownerChain resumeAfterDrawError resumeAfterStepError)
('drawing' addPossiblyUncoveredAreasIn:to: drawOn: drawingFails drawingFailsNot hide icon imageForm: imageForm:depth: isKnownFailing refreshWorld show visible:)
('dropping/grabbing' aboutToBeGrabbedBy: aboutToGrab: acceptDroppingMorph:event: dropFiles: justDroppedInto:event: justGrabbedFrom: rejectDropMorphEvent: wantsDroppedMorph:event: wantsToBeDroppedInto:)
('e-toy support' embeddedInMorphicWindowLabeled: unlockOneSubpart wantsRecolorHandle)
('events' click:localPosition: doubleClick:localPosition: dragEvent:localPosition: keyDown: keyStroke: keyUp: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Down:localPosition: mouseButton2Up:localPosition: mouseButton3Down:localPosition: mouseButton3Up:localPosition: mouseEnter: mouseHover:localPosition: mouseLeave: mouseMove:localPosition: mouseScroll:localPosition: mouseStillDown windowEvent:)
('event handling testing' allowsFilesDrop allowsMorphDrop allowsSubmorphDrag handlesKeyboard handlesMouseDown: handlesMouseHover handlesMouseOver: handlesMouseScroll: handlesMouseStillDown:)
('event handling' mouseButton2Activity mouseStillDownStepRate mouseStillDownThreshold)
('events-alarms' addAlarm:after: addAlarm:with:after: addAlarm:withArguments:after: alarmScheduler removeAlarm:)
('events-processing' dispatchEvent:localPosition: focusKeyboardFor: handleFocusEvent: processDropFiles:localPosition: processDropMorph:localPosition: processKeyDown:localPosition: processKeyUp:localPosition: processKeystroke:localPosition: processMouseDown:localPosition: processMouseEnter:localPosition: processMouseLeave:localPosition: processMouseMove:localPosition: processMouseOver:localPosition: processMouseScroll:localPosition: processMouseStillDown processMouseUp:localPosition: processUnknownEvent:localPosition: processWindowEvent:localPosition: rejectsEvent: rejectsEventFully:)
('fileIn/out' prepareToBeSaved storeDataOn:)
('focus handling' hasKeyboardFocus hasMouseFocus keyboardFocusChange:)
('geometry' displayBounds displayBounds: displayBoundsForHalo displayBoundsOrBogus displayFullBounds extentBorder externalize: externalizeDisplayBounds: externalizeDistance: externalizeDistanceToWorld: externalizeToWorld: fontPreferenceChanged internalize: internalizeDistance: internalizeDistanceFromWorld: internalizeFromWorld: minimumExtent morphExtent morphExtent: morphExtentInWorld morphExtentInWorld: morphHeight morphLocalBounds morphPosition morphPosition: morphPositionInWorld morphPositionInWorld: morphTopLeft morphWidth referencePosition rotation:scale: scale)
('geometry testing' clipsLastSubmorph fullContainsGlobalPoint: fullContainsPoint: isOrthoRectangularMorph morphContainsPoint: requiresVectorCanvas submorphsMightProtrude)
('halos and balloon help' addHalo addHalo: addHandlesTo:box: addOptionalHandlesTo:box: balloonHelpDelayTime balloonText comeToFrontAndAddHalo deleteBalloon editBalloonHelpContent: editBalloonHelpText halo mouseDownOnHelpHandle: noHelpString okayToBrownDragEasily okayToResizeEasily okayToRotateEasily removeHalo setBalloonText: showBalloon: showBalloon:hand: transferHalo:from: wantsBalloon wantsHalo wantsHaloHandleWithSelector:inHalo:)
('initialization' inATwoWayScrollPane initialize intoWorld: openInHand openInWorld openInWorld:)
('iteration of all morphs' nextMorph nextMorphPart2 nextMorphThat: previousMorph previousMorphThat:)
('layout' layoutSubmorphs layoutSubmorphsIfNeeded minItemWidth minimumLayoutHeight minimumLayoutWidth minimumShrinkExtent minimumShrinkHeight minimumShrinkWidth someSubmorphPositionOrExtentChanged)
('macpal' flash flashWith:)
('menus' addAddHandMenuItemsForHalo:hand: addColorMenuItems:hand: addCopyItemsTo: addCustomHaloMenuItems:hand: addCustomMenuItems:hand: addExportMenuItems:hand: addHaloActionsTo: addStandardHaloMenuItemsTo:hand: addTitleForHaloMenu: addToggleItemsToHaloMenu: changeColor expand exportAsBMP exportAsJPEG lockUnlockMorph lockedString maybeAddCollapseItemTo: stickinessString)
('meta-actions' addEmbeddingMenuItemsTo:hand: buildHandleMenu: copyToClipboard: dismissMorph duplicateMorph: maybeDuplicateMorph potentialEmbeddingTargets)
('naming' label)
('object serialization' objectForDataStream:)
('player' okayToDuplicate)
('printing' printOn:)
('rotate scale and flex' rotationDegrees)
('stepping' shouldGetStepsFrom: startStepping startStepping: startStepping:in:stepTime: startStepping:stepTime: startSteppingStepTime: step stepAt: stopStepping stopStepping: wantsSteps)
('structure' allOwnersDo: allOwnersReverseDo: firstOwnerSuchThat: hasOwner: isInWorld owner owningWindow root veryLastLeaf withAllOwnersDo: withAllOwnersReverseDo: world)
('submorphs-accessing' allMorphsDo: clippedSubmorph findDeepSubmorphThat:ifAbsent: findSubmorphBinary: firstSubmorph hasSubmorphs lastSubmorph noteNewOwner: submorphBehind: submorphCount submorphInFrontOf: submorphs submorphsBehind:do: submorphsDo: submorphsDrawingOutsideReverseDo: submorphsInFrontOf:do: submorphsReverseDo: submorphsSatisfying: unclippedSubmorphsReverseDo:)
('submorphs-add/remove' addAllMorphs: addAllMorphs:after: addMorph: addMorph:behind: addMorph:inFrontOf: addMorph:position: addMorphBack: addMorphBack:position: addMorphFront: addMorphFront:position: addMorphFrontFromWorldPosition: atFront canAdd: comeToFront delete dismissViaHalo goBehind privateDelete removeAllMorphs removeAllMorphsIn: removeMorph: removedMorph: replaceSubmorph:by:)
('testing' hasModel is: isCollapsed isOwnedByHand isOwnedByWorld isProportionalHeight isProportionalWidth isReallyVisible stepTime)
('updating' invalidateBounds redrawNeeded)
('user interface' activateWindow activateWindowAndSendTopToBack: collapse showAndComeToFront toggleCollapseOrShow)
('private' privateAddAllMorphs:atIndex: privateAddMorph:atIndex: privateAddMorph:atIndex:position: privateAnyOwnerHandlesMouseScroll: privateFlagAt: privateFlagAt:put: privateMoveBackMorph: privateMoveFrontMorph: privateOwner: privatePosition: privateRemove: privateSubmorphs)
('previewing' beginPreview endPreview endPreviewAndToggleCollapseOrShow morphBehindBeforePreview morphBehindBeforePreview: previewing previewing: visibleBeforePreview visibleBeforePreview:)
!

