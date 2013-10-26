'From Cuis 4.0 of 21 April 2012 [latest update: #1416] on 28 August 2012 at 10:46:19 pm'!
!classDefinition: #Morph category: #'Morphic-Kernel'!
Object subclass: #Morph
	instanceVariableNames: 'owner submorphs extension position layoutNeeded location '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!Morph methodsFor: 'geometry' stamp: 'jmv 8/28/2012 22:44'!
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

	self redrawNeeded.

	"Maybe we don't really need an owner to run this method..."
	self validateOwnerNotNil.

	position _ newPositionInOwner.
	location setTranslation: newPositionInOwner.
	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/28/2012 22:45'!
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

	position = newPositionInOwner ifTrue: [
		^ self ].		"Null change".

	self redrawNeeded.
	position _ newPositionInOwner.
	location setTranslation: newPositionInOwner.
	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!Morph methodsFor: 'initialization' stamp: 'jmv 8/28/2012 22:43'!
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
	location _ MatrixTransform2x3 withTranslation: b topLeft.
	layoutNeeded _ false! !

!Morph methodsFor: 'private' stamp: 'jmv 8/28/2012 22:44'!
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
					position _ owner internalizeFromWorld: oldGlobalPosition.
					location setTranslation: (owner internalizeFromWorld: oldGlobalPosition).
					self flag: #jmvVer2.
					"extent _ owner internalizeDistanceFromWorld: oldGlobalExtent" 	"or something like this!!"
					]]! !


!HandMorph methodsFor: 'initialization' stamp: 'jmv 8/28/2012 22:46'!
initialize
	super initialize.
	self initForEvents.
	keyboardFocus _ nil.
	mouseFocus _ nil.
	position _ 0@0.
	location setTranslation: 0@0.
	extent _ CursorWithMask normal extent.
	damageRecorder _ DamageRecorder new.
	self initForEvents.! !


!MenuMorph methodsFor: 'initialization' stamp: 'jmv 8/28/2012 22:45'!
initialize
	super initialize.
	position _ 0@0.
	location setTranslation: 0@0.
	extent _ 40@10.
	defaultTarget _ nil.
	selectedItem _ nil.
	stayUp _ false.
	popUpOwner _ nil! !


!PasteUpMorph methodsFor: 'project state' stamp: 'jmv 8/28/2012 22:45'!
viewBox: newViewBox
	self flag: #jmvVer2.
	self isWorldMorph ifTrue: [
		(self viewBox isNil or: [ self viewBox extent ~= newViewBox extent ])
			ifTrue: [ worldState canvas: nil ].
		worldState viewBox: newViewBox ].
	position _ owner
		ifNil: [ newViewBox topLeft ]
		ifNotNil: [ owner internalizeFromWorld: newViewBox topLeft ].
	location setTranslation: (owner
		ifNil: [ newViewBox topLeft ]
		ifNotNil: [ owner internalizeFromWorld: newViewBox topLeft ]).
	extent _ newViewBox extent! !

!classDefinition: #Morph category: #'Morphic-Kernel'!
Object subclass: #Morph
	instanceVariableNames: 'owner submorphs extension position location layoutNeeded'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Morph allSubInstancesDo: [ :m | m instVarNamed: 'location' put: (MatrixTransform2x3 withTranslation: (m instVarNamed: 'position'))]!

