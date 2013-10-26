'From Cuis 4.0 of 21 April 2012 [latest update: #1356] on 6 August 2012 at 9:39:22 am'!
!classDefinition: #Morph category: #'Morphic-Kernel'!
Object subclass: #Morph
	instanceVariableNames: 'bounds owner submorphs color extension position extent layoutNeeded '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!FormCanvas class methodsFor: 'private' stamp: 'jmv 8/6/2012 09:15'!
buildArrowIn: aRectangle 
	"PRIVATE - create an arrow bounded in aRectangle"
	"
	(self buildArrowOfDirection: #up size: 20) display
	"

	| arrow vertices |
	vertices _ self verticesForSimpleArrow: aRectangle.
"	arrow _ PolygonMorph 
		vertices: vertices
		color: Color darkGray
		borderWidth: 0
		borderColor: Color black."
	"arrow bounds: (arrow bounds insetBy: (aRectangle width / 6) rounded)."
	^arrow! !


!Morph methodsFor: 'geometry' stamp: 'jmv 8/6/2012 09:19'!
basicExtent: aPoint

	self flag: #jmvVer2.
	"Lo dejo un rato porque se llama dde initialize... De ultima, es un problema? Es cierto que no se la escala aun..."
	"
	self validateOwnerNotNil.
	"
	
	"ver senders. Es en owner's o en world's ???"
	
	
	self validateExtentAndBounds.
	extent = aPoint ifTrue: [^ self].
	self redrawNeeded.
"	bounds _ bounds topLeft extent: aPoint."
	extent _ aPoint.
	self validatePositionAndBounds.
	self validateExtentAndBounds.
	self someSubmorphPositionOrExtentChanged.
	self redrawNeeded! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/6/2012 09:35'!
morphBoundsInWorld
	"Return the bounds of this morph."
	
	"WORLD absolute bounds :("

	"remove senders and implementors"
	| answer |
	self flag: #jmvVer2.
	answer _ self morphPositionInWorld extent: self morphExtentInWorld.
	"
	bounds = answer ifFalse: [
		#morphBoundsInWorld print.
		answer print.
		bounds print.
		thisContext printStack: 10 ].
	"
	^answer
	! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/6/2012 09:19'!
morphPositionInOwner: newPositionInOwner
	"Change the position of this morph."
"Ojo aca. Ponemos position a nil pero es solo por el problema de bounds desenganchado de position..."
	| newPositionInWorld |
	self flag: #jmvVer2.

	newPositionInWorld _ owner
		ifNotNil: [ owner externalizeToWorld: newPositionInOwner ]
		ifNil: [ newPositionInOwner ].

	position = newPositionInOwner ifTrue: [
		newPositionInWorld = self morphPositionInWorld ifFalse: [
			'---------feote2' print
		].
		^ self ].		"Null change"

	"May need to compute fullBounds, but I see no reason to layout submorphs!!"
	self flag: #jmvVer2.
	self layoutSubmorphsAndComputeFullBounds; redrawNeeded.

	self validateOwnerNotNil.


"This won't be needed once we remove the bounds ivar in global coordinates!!
With this, we also delete implementors of #privateFullMoveBy: and #privateMoveBy: "
self privateFullMoveBy: newPositionInWorld - self morphPositionInWorld.

	position _ newPositionInOwner.
	self validatePositionAndBounds.
	self validateExtentAndBounds.

	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/6/2012 09:18'!
morphPositionInWorld
	| answer1 |
"
Cuando tenga resueltos los setters (o quizas al mismo tiempo),
ver que senders del getter quieren en realidad #zzpositionInOwner y quienes #zzpositionInWorld.
Espero que pocos hablen en coordenadas del world!!
"


	self flag: #jmvVer2.
	self validateOwnerNotNil.
	self validatePositionAndBounds.
	self validateExtentAndBounds.
	
	answer1 _ owner
		ifNotNil: [ owner externalizeToWorld: position ]
		ifNil: [ position ].
"	answer2 _ bounds topLeft.
	{ answer1 . answer2 }."

	^answer1! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/6/2012 09:27'!
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
	self validateOwnerNotNil.

	newPositionInOwner _ owner
		ifNotNil: [ owner internalizeFromWorld: newPositionInWorld ]
		ifNil: [ newPositionInWorld ].


	position = newPositionInOwner ifTrue: [
		"Al menos en algunos casos esto pasa porque al poner owner, no reajustamos bounds...
		Es necesario hacer que los globales sigan a los locales (manteniendo locales, moviendo morph)
		o que las locales sigan a las globales (manteniendo posicion en el mundo... si es que estabamos en un mundo!!)"
		self flag: #jmvVer2.
		"
		newPositionInWorld - bounds topLeft = (0@0) ifFalse: [
			'-----feote' print.
			newPositionInWorld print.
			bounds print.
			position print.
			self print.
			owner print.
			owner morphPosition print.
			'--' print.
		].
		"
"Quizas no un null change del todo... Es posible que bounds y position se desincronicen, al cambiar el owner (o cualquier owner en el medio). Todo esto es fulerote. espero sacarlo pronto!!"
		self privateFullMoveBy: newPositionInWorld - self morphPositionInWorld.
		^ self ].		"Null change"

	"May need to compute fullBounds, but I see no reason to layout submorphs!!"
	self flag: #jmvVer2.
	self layoutSubmorphsAndComputeFullBounds; redrawNeeded.

"This won't be needed once we remove the bounds ivar in global coordinates!!
With this, we also delete implementors of #privateFullMoveBy: and #privateMoveBy: "
self privateFullMoveBy: newPositionInWorld - self morphPositionInWorld.

	position _ newPositionInOwner.
	self validatePositionAndBounds.
	self validateExtentAndBounds.

	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/6/2012 09:17'!
validateExtentAndBounds
	"To be removed. Just to check consistency"
	| |
	self flag: #jmvVer2.

"	answer1 _ owner
		ifNotNil: [ owner externalizeDistanceToWorld: extent ]
		ifNil: [ extent ]."

"
	answer1 _ extent.
	answer2 _ bounds extent.

	answer1 = answer2 rounded ifFalse: [
		#validateExtentAndBounds print.
		answer1 print.
		answer2 print.
		thisContext printStack: 10 ]
"! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/6/2012 09:17'!
validatePositionAndBounds
	"To be removed. Just to check consistency"
	| |
	self flag: #jmvVer2.
"
	answer1 _ owner
		ifNotNil: [ owner externalizeToWorld: position ]
		ifNil: [ position ].
	answer2 _ bounds topLeft.

	answer1 = answer2 ifFalse: [
		#validatePositionAndBounds print.
		answer1 print.
		answer2 print.
		thisContext printStack: 10 ]
"! !

!Morph methodsFor: 'initialization' stamp: 'jmv 8/6/2012 09:27'!
initialize
	"initialize the state of the receiver"

	| b |
	owner _ nil.
	submorphs _ #().
	
	self flag: #jmvVer2.
	"Ir convirtiendo todos los usos (no las asignaciones!!) a las vars nuevas.
	Despues eliminar las asignaciones y las propias ivars (bounds y fullBounds)"
	b _ self defaultBounds.
	position _ b topLeft.
	extent _ b extent.
	self validatePositionAndBounds.
	self validateExtentAndBounds.
	color _ self defaultColor.
	layoutNeeded _ false! !

!Morph methodsFor: 'layout' stamp: 'jmv 8/6/2012 09:34'!
computeFullBounds
	"Private. Compute the actual full bounds of the receiver"

	"Remove when removing fullBounds"
	self flag: #jmvVer2.

	(submorphs isEmpty or: [ self clipsSubmorphs ]) ifTrue: [ ^self morphBoundsInWorld ].
	^ self morphBoundsInWorld quickMerge: self submorphBounds! !

!Morph methodsFor: 'private' stamp: 'jmv 8/6/2012 09:26'!
privateMoveBy: delta
	"Private!! Use 'position:' instead."

	"All these will die soon!!"

	self flag: #jmvVer2.
	self validateOwnerNotNil.	"Maybe not needed"

"	bounds _ bounds translateBy: delta"! !

!Morph methodsFor: 'private' stamp: 'jmv 8/6/2012 09:26'!
privateOwner: aMorph
	"Private!! Should only be used by methods that maintain the ower/submorph invariant."

	| oldGlobalPosition prevOwner |
	self flag: #jmvVer2.
	"
	(aMorph notNil and: [
		bounds origin ~= self defaultBounds origin ]) ifTrue: [
			'                                ---------------- Nos mandan #privateOwner: , pero nos han mandado bounds antes (no necesariamente un problema!!!!!!!!!!)!!' print.
			thisContext printStack: 10 ].
	"

	self validatePositionAndBounds.
	self validateExtentAndBounds.
			
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
"			bounds _ position extent: extent."
			layoutNeeded _ true.
			self validatePositionAndBounds.
			self validateExtentAndBounds.
			]
		ifNotNil: [
			prevOwner
				ifNil: [
					"Didn't have any owner. Assume my local position is to be maintained in my new owner"
"					bounds _ (owner externalizeToWorld: position) extent: extent."
					layoutNeeded _ true.
					self validatePositionAndBounds.
					self validateExtentAndBounds.
					]
				ifNotNil: [
					"Had an owner. Maintain my global position..."
					position _ owner internalizeFromWorld: oldGlobalPosition.
					self flag: #jmvVer2.
					"extent _ owner internalizeDistanceFromWorld: oldGlobalExtent" 	"or something like this!!"
					self validatePositionAndBounds.
					self validateExtentAndBounds.
					]]! !


!HandMorph methodsFor: 'initialization' stamp: 'jmv 8/6/2012 09:37'!
initialize
	super initialize.
	self initForEvents.
	keyboardFocus _ nil.
	mouseFocus _ nil.
"	bounds _ 0@0 extent: CursorWithMask normal extent."
	position _ 0@0.
	extent _ CursorWithMask normal extent.
	self validatePositionAndBounds.
	self validateExtentAndBounds.
	damageRecorder _ DamageRecorder new.
	self initForEvents.! !


!MenuMorph methodsFor: 'initialization' stamp: 'jmv 8/6/2012 09:28'!
initialize
	super initialize.
	
	self flag: #jmvVer2.
"	bounds _ 0@0 corner: 40@10."

	position _ 0@0.
	extent _ 40@10.
	self validatePositionAndBounds.
	self validateExtentAndBounds.
	defaultTarget _ nil.
	selectedItem _ nil.
	stayUp _ false.
	popUpOwner _ nil
! !


!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 8/6/2012 09:38'!
morphExtent: aPoint

	self flag: #jmvVer2.
	self validatePositionAndBounds.
	self validateExtentAndBounds.
	extent = aPoint ifFalse: [
		self redrawNeeded.
"		bounds _ bounds topLeft extent: aPoint."
		extent _ aPoint.
		self validatePositionAndBounds.
		self validateExtentAndBounds.
		self buildMagnifiedBackgroundImage.
		self someSubmorphPositionOrExtentChanged.
		self redrawNeeded ].

	worldState ifNotNil: [
		worldState viewBox ifNotNil: [
			worldState viewBox = self morphBoundsInWorld ifFalse: [
				worldState canvas: nil.
				worldState viewBox: self morphBoundsInWorld ]]]! !

!PasteUpMorph methodsFor: 'project state' stamp: 'jmv 8/6/2012 09:37'!
viewBox: newViewBox
	self flag: #jmvVer2.
	self isWorldMorph ifTrue: [
		(self viewBox isNil or: [ self viewBox extent ~= newViewBox extent ])
			ifTrue: [ worldState canvas: nil ].
		worldState viewBox: newViewBox ].
"	super morphPosition: newViewBox topLeft."
"	bounds _ newViewBox."
	position _ owner
		ifNil: [ newViewBox topLeft ]
		ifNotNil: [ owner internalizeFromWorld: newViewBox topLeft ].
	extent _ newViewBox extent.
	self validatePositionAndBounds.
	self validateExtentAndBounds.! !

!classDefinition: #Morph category: #'Morphic-Kernel'!
Object subclass: #Morph
	instanceVariableNames: 'owner submorphs color extension position extent layoutNeeded'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!
!classRemoval: #CurveMorph!
Smalltalk removeClassNamed: #CurveMorph!
!classRemoval: #LineMorph!
Smalltalk removeClassNamed: #LineMorph!
!classRemoval: #PolygonMorph!
Smalltalk removeClassNamed: #PolygonMorph!
!classRemoval: #StarMorph!
Smalltalk removeClassNamed: #StarMorph!
