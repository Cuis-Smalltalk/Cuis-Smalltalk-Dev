'From Cuis 4.0 of 21 April 2012 [latest update: #1362] on 7 August 2012 at 10:54:41 pm'!

!Morph methodsFor: 'change reporting' stamp: 'jmv 8/7/2012 22:53'!
invalidRect: damageRect
	"warning. Senders are using global coordinates. Redesign!!"
	self flag: #jmvVer2.	"ok?"
	^self invalidRect: damageRect from: self! !

!Morph methodsFor: 'change reporting' stamp: 'jmv 8/7/2012 22:54'!
invalidRect: aRectangle from: aMorph
	| damageRect |
	"warning. Senders are using global coordinates. Redesign!!"
	self flag: #jmvVer2.	"ok?"
	owner ifNotNil: [
		aRectangle hasPositiveExtent ifFalse: [ ^ self ].
		damageRect _ aRectangle.
		aMorph == self ifFalse: [
			"Clip to receiver's clipping bounds if the damage came from a child"
			self clipsSubmorphs ifTrue: [
				damageRect _ aRectangle intersect: self clippingBounds ]].
		owner
			invalidRect: damageRect
			from: self ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/7/2012 22:51'!
externalizeToWorld: aPoint
	"aPoint is in own coordinates. Answer is in world coordinates.
	BUT there is no well defined World!!"
	self flag: #jmvVer2.
	^owner
		ifNotNil: [ owner externalizeToWorld: aPoint + position ]
		ifNil: [ aPoint + position ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/7/2012 22:49'!
morphPositionInOwner
"
Cuando tenga resueltos los setters (o quizas al mismo tiempo),
ver que senders del getter quieren en realidad #zzpositionInOwner y quienes #zzpositionInWorld.
Espero que pocos hablen en coordenadas del world!!
"

	self flag: #jmvVer2.
	"Maybe we don't really need an owner to answer this..."
	self validateOwnerNotNil.

	^ position! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/7/2012 22:49'!
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


	"Maybe we don't really need an owner to run this method..."
	self validateOwnerNotNil.

	position _ newPositionInOwner.
	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/7/2012 22:50'!
morphPositionInWorld
	| answer1 |
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
	
	answer1 _ owner
		ifNotNil: [ owner externalizeToWorld: position ]
		ifNil: [ position ].
"	answer2 _ bounds topLeft.
	{ answer1 . answer2 }."

	^answer1! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/7/2012 22:51'!
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
		^ self ].		"Null change"

	"May need to compute fullBounds, but I see no reason to layout submorphs!!"
	self flag: #jmvVer2.
	self layoutSubmorphsAndComputeFullBounds; redrawNeeded.

	position _ newPositionInOwner.
	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/7/2012 22:48'!
validateOwnerNotNil
	"To be removed. Just to check consistency"

	"We can activate this test. But beware. Most of the times the correct fix is NOT ensuring having an owner, but using local coordinates instead of global!!"
	self flag: #jmvVer2.
"
	owner ifNil: [
		'-----Still no owner, but this stuff kind of requires it!!-----' print.
		thisContext printStack: 10 ]
	"! !

