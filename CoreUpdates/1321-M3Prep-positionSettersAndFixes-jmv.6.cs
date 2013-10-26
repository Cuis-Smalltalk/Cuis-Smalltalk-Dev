'From Cuis 4.0 of 3 April 2012 [latest update: #1255] on 10 April 2012 at 2:48:24 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 12/15/2011 21:06'!
morphHeight
"Ensure everybody wants owner's coordinates!!"
	self flag: #jmvVer2.
	^ bounds height! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/15/2011 21:06'!
morphHeight: aNumber

"Ensure everybody wants owner's coordinates!!"
	self flag: #jmvVer2.
	self extent: bounds width@aNumber asInteger.
! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 09:02'!
morphPositionInOwner
"
Cuando tenga resueltos los setters (o quizas al mismo tiempo),
ver que senders del getter quieren en realidad #zzpositionInOwner y quienes #zzpositionInWorld.
Espero que pocos hablen en coordenadas del world!!
"

	self flag: #jmvVer2.
	self validateOwnerNotNil.
	self validatePositionAndBounds.

	^ position! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/15/2011 20:47'!
morphPositionInOwner: newPositionInOwner
	"Change the position of this morph."
"Ojo aca. Ponemos position a nil pero es solo por el problema de bounds desenganchado de position..."
	| newPositionInWorld |
	self flag: #jmvVer2.

	newPositionInWorld _ owner
		ifNotNil: [ owner externalizeToWorld: newPositionInOwner ]
		ifNil: [ newPositionInOwner ].

	position = newPositionInOwner ifTrue: [
		newPositionInWorld = bounds topLeft ifFalse: [
			'---------feote2' print
		].
		^ self ].		"Null change"

	self layoutSubmorphsAndComputeFullBounds; redrawNeeded.

owner ifNil: [
	'-----trying to set position before setting owner... bad idea!!-----' print.
	thisContext printStack: 10 ].


"This won't be needed once we remove the bounds ivar in global coordinates!!
With this, we also delete implementors of #privateFullMoveBy: and #privateMoveBy: "
self privateFullMoveBy: newPositionInWorld - bounds topLeft.

	position _ newPositionInOwner.
	self validatePositionAndBounds.

	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 09:02'!
morphPositionInWorld
	| answer1 answer2 |
"
Cuando tenga resueltos los setters (o quizas al mismo tiempo),
ver que senders del getter quieren en realidad #zzpositionInOwner y quienes #zzpositionInWorld.
Espero que pocos hablen en coordenadas del world!!
"


	self flag: #jmvVer2.
	self validateOwnerNotNil.
	self validatePositionAndBounds.
	
	answer1 _ owner
		ifNotNil: [ owner externalizeToWorld: position ]
		ifNil: [ position ].
	answer2 _ bounds topLeft.
	{ answer1 . answer2 }.

	^answer2! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/15/2011 20:47'!
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
owner ifNil: [
	'-----trying to set position before setting owner... bad idea!!-----' print.
	thisContext printStack: 10 ].

	newPositionInOwner _ owner
		ifNotNil: [ owner internalizeFromWorld: newPositionInWorld ]
		ifNil: [ newPositionInWorld ].


	position = newPositionInOwner ifTrue: [
		"Al menos en algunos casos esto pasa porque al poner owner, no reajustamos bounds...
		Es necesario hacer que los globales sigan a los locales (manteniendo locales, moviendo morph)
		o que las locales sigan a las globales (manteniendo posicion en el mundo... si es que estabamos en un mundo!!)"
		self flag: #jmvVer2.
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
"Quizas no un null change del todo... Es posible que bounds y position se desincronicen, al cambiar el owner (o cualquier owner en el medio). Todo esto es fulerote. espero sacarlo pronto!!"
		self privateFullMoveBy: newPositionInWorld - bounds topLeft.
		^ self ].		"Null change"


	self layoutSubmorphsAndComputeFullBounds; redrawNeeded.

"This won't be needed once we remove the bounds ivar in global coordinates!!
With this, we also delete implementors of #privateFullMoveBy: and #privateMoveBy: "
self privateFullMoveBy: newPositionInWorld - bounds topLeft.

	position _ newPositionInOwner.
	self validatePositionAndBounds.

	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/15/2011 21:09'!
morphWidth

"Ensure everybody wants owner's coordinates!!"
	self flag: #jmvVer2.
	^ bounds width! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/15/2011 21:09'!
morphWidth: aNumber
"Ensure everybody wants owner's coordinates!!"
	self flag: #jmvVer2.
	self extent: aNumber asInteger@bounds height.
! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 09:01'!
validateOwnerNotNil
	"To be removed. Just to check consistency"

	self flag: #jmvVer2.
	owner ifNil: [
		'-----Still no owner, but this stuff kind of requires it!!-----' print.
		thisContext printStack: 10 ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/15/2011 20:38'!
validatePositionAndBounds
	"To be removed. Just to check consistency"
	| answer1 answer2 |
	self flag: #jmvVer2.

	answer1 _ owner
		ifNotNil: [ owner externalizeToWorld: position ]
		ifNil: [ position ].
	answer2 _ bounds topLeft.

	answer1 = answer2 ifFalse: [
		#validatePositionAndBounds print.
		answer1 print.
		answer2 print.
		thisContext printStack: 10 ]! !


!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 12/15/2011 16:14'!
morphPositionInWorld

	self flag: #jmvVer2. "Solo para evitar los warning por falta de owner... pensar despues este caso"
	self isWorldMorph ifTrue: [ ^ 0@0 ].
	^super morphPositionInWorld! !


!SystemWindow methodsFor: 'resizing' stamp: 'jmv 12/13/2011 14:39'!
windowBottom: aNumber
	"aNumber is an Y coordinate in the owner's coordinate system"
	self bounds: (bounds bottom: aNumber)! !

!SystemWindow methodsFor: 'resizing' stamp: 'jmv 12/13/2011 14:40'!
windowBottomLeft: aPoint
	"aPoint is an X@Y coordinate pair in the owner's coordinate system"
	self bounds: (aPoint x @ bounds top corner: bounds right @ aPoint y)! !

!SystemWindow methodsFor: 'resizing' stamp: 'jmv 12/13/2011 14:41'!
windowBottomRight: aPoint
	"aPoint is an X@Y coordinate pair in the owner's coordinate system"
	self bounds: (bounds origin corner: aPoint)! !

!SystemWindow methodsFor: 'resizing' stamp: 'jmv 12/13/2011 14:41'!
windowLeft: aNumber
	"aNumber is an X coordinate in the owner's coordinate system"
	self bounds: (bounds left: aNumber)! !

!SystemWindow methodsFor: 'resizing' stamp: 'jmv 12/13/2011 14:42'!
windowRight: aNumber
	"aNumber is an X coordinate in the owner's coordinate system"
	self bounds: (bounds right: aNumber)! !

!SystemWindow methodsFor: 'resizing' stamp: 'jmv 12/13/2011 14:42'!
windowTop: aNumber
	"aNumber is an Y coordinate in the owner's coordinate system"
	self bounds: (bounds top: aNumber)! !

!SystemWindow methodsFor: 'resizing' stamp: 'jmv 12/13/2011 14:43'!
windowTopLeft: aPoint
	"aPoint is an X@Y coordinate pair in the owner's coordinate system"
	self bounds: (aPoint corner: bounds corner)! !

!SystemWindow methodsFor: 'resizing' stamp: 'jmv 12/13/2011 14:43'!
windowTopRight: aPoint
	"aPoint is an X@Y coordinate pair in the owner's coordinate system"
	self bounds: (bounds left @ aPoint y corner: aPoint x @ bounds bottom)! !


!WindowEdgeAdjustingMorph class methodsFor: 'instance creation' stamp: 'jmv 12/12/2011 13:57'!
forBottom
	^self new initializeBottom! !

!WindowEdgeAdjustingMorph class methodsFor: 'instance creation' stamp: 'jmv 12/12/2011 13:57'!
forBottomLeft
	^self new initializeBottomLeft! !

!WindowEdgeAdjustingMorph class methodsFor: 'instance creation' stamp: 'jmv 12/12/2011 13:57'!
forBottomRight
	^self new initializeBottomRight! !

!WindowEdgeAdjustingMorph class methodsFor: 'instance creation' stamp: 'jmv 12/12/2011 13:57'!
forLeft
	^self new initializeLeft! !

!WindowEdgeAdjustingMorph class methodsFor: 'instance creation' stamp: 'jmv 12/12/2011 13:57'!
forRight
	^self new initializeRight! !

!WindowEdgeAdjustingMorph class methodsFor: 'instance creation' stamp: 'jmv 12/12/2011 13:58'!
forTop
	^self new initializeTop! !

!WindowEdgeAdjustingMorph class methodsFor: 'instance creation' stamp: 'jmv 12/12/2011 13:58'!
forTopLeft
	^self new initializeTopLeft! !

!WindowEdgeAdjustingMorph class methodsFor: 'instance creation' stamp: 'jmv 12/12/2011 13:58'!
forTopRight
	^self new initializeTopRight! !


!CharacterScanner methodsFor: 'scanning' stamp: 'jmv 12/13/2011 23:33'!
placeEmbeddedObject: anchoredFormOrMorph
	"Place the anchoredMorph or return false if it cannot be placed.
	In any event, advance destX by its width."

	destX _ destX + anchoredFormOrMorph morphWidth.
	(destX > rightMargin and: [ lastIndex ~= line first ])
		"Won't fit, but  not at start of a line. Start a new line with it"
		ifTrue: [ ^ false].
	lastIndex _ lastIndex + 1.
	^ true! !


!CharacterBlockScanner methodsFor: 'scanning' stamp: 'jmv 12/13/2011 23:33'!
placeEmbeddedObject: anchoredFormOrMorph

	(super placeEmbeddedObject: anchoredFormOrMorph) ifFalse: [^ false].
	specialWidth _ anchoredFormOrMorph morphWidth.
	^ true! !


!CompositionScanner methodsFor: 'stop conditions' stamp: 'jmv 12/13/2011 23:35'!
placeEmbeddedObject: anchoredFormOrMorph
	| descent |

	(super placeEmbeddedObject: anchoredFormOrMorph) ifFalse: [
		line stop: lastIndex-1.
		^ false].
	descent _ lineHeight - baseline.
	baseline _ baseline max: anchoredFormOrMorph morphHeight.
	lineHeight _ baseline + descent.
	line stop: lastIndex.
	^ true! !


!LayoutSpec methodsFor: 'accessing' stamp: 'jmv 12/13/2011 23:35'!
fixedOrMorphHeight: aNumber
	"aNumber is taken as the fixed height to use.
	No proportional part."
	fixedHeight
		ifNotNil: [ fixedHeight _ aNumber ]
		ifNil: [ morph morphHeight: aNumber ].
	proportionalHeight _ nil! !

!LayoutSpec methodsFor: 'accessing' stamp: 'jmv 12/13/2011 23:34'!
fixedOrMorphWidth: aNumber
	"aNumber is taken as the fixed width to use.
	No proportional part."
	fixedWidth
		ifNotNil: [ fixedWidth _ aNumber ]
		ifNil: [ morph morphWidth: aNumber ].
	proportionalWidth _ nil! !

!LayoutSpec methodsFor: 'layout' stamp: 'jmv 12/13/2011 23:35'!
fixedHeight
	"If proportional is zero, answer stored fixed extent, or actual morph extent if undefined. (no proportional extent is computed)
	Otherwise, we do proportional layout, and the stored extent is a minimum extent, so we don't  really a fixed extent."
	proportionalHeight ifNotNil: [
		^0 ].
	^ fixedHeight ifNil: [ morph morphHeight ]! !

!LayoutSpec methodsFor: 'layout' stamp: 'jmv 12/13/2011 23:33'!
fixedWidth
	"If proportional is zero, answer stored fixed extent, or actual morph extent if undefined. (no proportional extent is computed)
	Otherwise, we do proportional layout, and the stored extent is a minimum extent, so we don't  really a fixed extent."
	proportionalWidth ifNotNil: [
		^0 ].
	^ fixedWidth ifNil: [ morph morphWidth ]! !


!Morph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 09:01'!
basicExtent: aPoint

	self flag: #jmvVer2.
	"Lo dejo un rato porque se llama dde initialize... De ultima, es un problema? Es cierto que no se la escala aun..."
	"
	self validateOwnerNotNil.
	"
	
	"ver senders. Es en owner's o en world's ???"
	
	
	bounds extent = aPoint ifTrue: [^ self].
	self redrawNeeded.
	bounds _ bounds topLeft extent: aPoint.
	extent _ aPoint.
	self validatePositionAndBounds.
	self someSubmorphPositionOrExtentChanged.
	self redrawNeeded! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/15/2011 21:10'!
bounds
	"Return the bounds of this morph."
	"Note: It is best not to override this method because many methods in Morph and its subclasses use the instance variable directly rather than 'self bounds'. Instead, subclasses should be sure that the bounds instance variable is correct."

	"remove senders and implementors"
	self flag: #jmvVer2.

	^ bounds
! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/15/2011 21:10'!
bounds: newBounds
	| oldExtent newExtent |

	"remove senders and implementors"
	self flag: #jmvVer2.
		
	oldExtent _ bounds extent.
	newExtent _ newBounds extent.
	"Moving stuff around is most likely the most common operation.
	Optimize it"
	oldExtent = newExtent ifTrue: [
		^self morphPosition: newBounds topLeft ].
	(oldExtent dotProduct: oldExtent) <= (newExtent dotProduct: newExtent) ifTrue:[
		"We're growing. First move then resize."
		self morphPosition: newBounds topLeft; extent: newExtent.
	] ifFalse: [
		"We're shrinking. First resize then move."
		self extent: newExtent; morphPosition: newBounds topLeft.
	].! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/15/2011 21:11'!
extent
"
?
'' print.
thisContext printStack: 10.
"
"Needs to be turned into extentInOwner and extentInWorld"
"or better yet into morphExtent:, and assume it is always in owner's coordinates!!"
	self flag: #jmvVer2.
	^ bounds extent! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/15/2011 21:11'!
extent: aPoint
"
?
'' print.
thisContext printStack: 10.
"
"Needs to be turned into extentInOwner: and extentInWorld:"
"or better yet into morphExtent:, and assume it is always in owner's coordinates!!"
	self flag: #jmvVer2.
	self basicExtent: aPoint! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/14/2011 16:01'!
externalizeToWorld: aPoint
	"aPoint is in own coordinates. Answer is in world coordinates."
	self flag: #jmvVer2.
	^owner
		ifNotNil: [ owner externalizeToWorld: aPoint + position ]
		ifNil: [ aPoint + position ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/15/2011 21:08'!
height
	self flag: #jmvVer2.
"False polymorphism elimination: turn senders into morphHeight"
'' print.
thisContext printStack: 10.
	^ bounds height! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/15/2011 21:07'!
height: aNumber
	self flag: #jmvVer2.
"False polymorphism elimination: turn senders into morphHeight:"
'' print.
thisContext printStack: 10.
	self extent: bounds width@aNumber asInteger.
! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/15/2011 21:07'!
innerBounds
	"Return the inner rectangle enclosed by the bounds of this morph excluding the space taken by its borders. For an unbordered morph, this is just its bounds."

"In own's coordinates!!"
	self flag: #jmvVer2.
	^ bounds! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/14/2011 16:01'!
internalizeFromWorld: aPoint
	"aPoint is in World coordinates. Answer is in own coordinates."
	self flag: #jmvVer2.
	^(owner
		ifNotNil: [ owner internalizeFromWorld: aPoint ]
		ifNil: [ aPoint ])
			- position! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/15/2011 21:06'!
minimumExtent
	| ext |
	"This returns the minimum extent that the morph may be shrunk to.  Not honored in too many places yet, but respected by the resizeToFit feature, at least.  copied up from SystemWindow 6/00"
	self flag: #jmvVer2.	"in owner's coordinates?"
	(ext _ self valueOfProperty: #minimumExtent)
		ifNotNil:
			[^ ext].
	^ 100 @ 80! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/14/2011 15:45'!
morphPosition
"
Ver que senders quieren #morphPositionInOwner y quienes #morphPosirionInWorld (espero que pocos!!)
Eventualmente eliminar los senders a este...
"
	self flag: #jmvVer2.
	^ self morphPositionInWorld! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/14/2011 15:45'!
morphPosition: newPositionInWorld
"Cuando terminemos, el arg es en coords del owner
entonces: Warning: argument is in owner's coordinate system. When creating new morphs, first add to owner, then set position."
	"Change the position of this morph."
	"
	Para morphs que estan en el mundo, da lo mismo!! Y para los que estan en otro morph, la mayoria de las veces usaremos un layout. Y las veces que no, usaremos coordenadas del owner tambien!!
	Podemos hacer que por default, #morphPosition y #morphPosition: sean en el owner, y chequear los senders...
	Quizas el camino es ir cambiando los senders a isOwner, y cuando todos sean ahi, eliminar los demas... suena razonable, no?
	Lo mismo para setter.
	Lo mismo para extent, height, width... aunque aqui, hasta que no introduzca factor de escala, no es tan urgente (i.e. no se rompe ya mismo!!) Eso quiere decir que no necesito pasar por los sufijos provisorios InOwner e InWorld!!
	Y en algun momento de todo esto, empezar a atacar los usos directos de la ivar bounds!!
	"
	
	"
Ver que senders quieren #morphPositionInOwner: y quienes #morphPosirionInWorld: (espero que pocos!!)
Eventualmente eliminar los senders a este...
NO OLVIDARSE que hay redefiniciones de este metodo... Redefinir los 2 setters que quedan?
	"
	self flag: #jmvVer2.
	^self morphPositionInWorld: newPositionInWorld! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/15/2011 21:08'!
width

	self flag: #jmvVer2.
"False polymorphism elimination: turn senders into morphWidth"
'' print.
thisContext printStack: 10.
	^ bounds width! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/15/2011 21:08'!
width: aNumber

	self flag: #jmvVer2.
"False polymorphism elimination: turn senders into morphWidth:"
'' print.
thisContext printStack: 10.
	self extent: aNumber asInteger@bounds height.
! !

!Morph methodsFor: 'geometry eToy' stamp: 'jmv 12/14/2011 15:51'!
referencePosition: aPoint
	"a rather ugly way to say #center: . Just for consistency with #referencePosition"
	self flag: #jmvVer2.
	self morphPosition: aPoint - (bounds extent // 2)! !

!Morph methodsFor: 'initialization' stamp: 'jmv 12/14/2011 16:04'!
inATwoWayScrollPane
	"Answer a two-way scroll pane that allows the user to scroll the receiver in either direction.  It will have permanent scroll bars unless you take some special action."

	| widget |
	self flag: #jmvVer2.
	widget _ PluggableScrollPane new.
	widget addToScroller: self.
	widget extent: (self morphWidth min: 300 max: 100) @ (self morphHeight min: 150 max: 100).
	widget setScrollDeltas.
	widget color: self color darker darker.
	^widget! !

!Morph methodsFor: 'initialization' stamp: 'jmv 12/15/2011 20:43'!
initialize
	"initialize the state of the receiver"

	owner _ nil.
	submorphs _ #().
	
	self flag: #jmvVer2.
	"Ir convirtiendo todos los usos (no las asignaciones!!) a las vars nuevas.
	Despues eliminar las asignaciones y las propias ivars (bounds y fullBounds)"
	bounds _ self defaultBounds.

	position _ bounds topLeft.
	extent _ bounds extent.
	self validatePositionAndBounds.
	color _ self defaultColor! !

!Morph methodsFor: 'private' stamp: 'jmv 12/16/2011 09:03'!
privateFullMoveBy: delta
	"Private!! Relocate me and all of my subMorphs by recursion. Subclasses that implement different coordinate systems may override this method."

	"All these will die soon!!"
	self flag: #jmvVer2.

	self privateMoveBy: delta.
	submorphs do: [ :m |
		m privateFullMoveBy: delta ]! !

!Morph methodsFor: 'private' stamp: 'jmv 12/16/2011 09:03'!
privateMoveBy: delta
	"Private!! Use 'position:' instead."

	"All these will die soon!!"

	self flag: #jmvVer2.
	self validateOwnerNotNil.

	bounds _ bounds translateBy: delta.
	fullBounds ifNotNil: [ fullBounds _ fullBounds translateBy: delta ].! !

!Morph methodsFor: 'private' stamp: 'jmv 12/16/2011 09:04'!
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
			bounds _ position extent: bounds extent.
			fullBounds _ nil.
			self validatePositionAndBounds
			]
		ifNotNil: [
			prevOwner
				ifNil: [
					"Didn't have any owner. Assume my local position is to be maintained in my new owner"
					bounds _ (owner externalizeToWorld: position) extent: bounds extent.
					fullBounds _ nil.
					self validatePositionAndBounds
					]
				ifNotNil: [
					"Had an owner. Maintain my global position..."
					position _ owner internalizeFromWorld: oldGlobalPosition.
					self validatePositionAndBounds
					]]! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 12/12/2011 19:56'!
addNameBeneath: outerRectangle string: aString 
	"Add a name display centered beneath the bottom of the outer rectangle. Return the handle."

	| nameMorph namePosition w |
	w := self world ifNil: [target world].
	nameMorph := StringMorph contents: aString.
	nameMorph color: Color magenta.
	namePosition := outerRectangle bottomCenter 
				- ((nameMorph morphWidth // 2) @ (self handleSize negated // 2 - 1)).
	nameMorph 
		morphPosition: (namePosition min: w viewBox bottomRight - nameMorph extent y + 5).
	self addMorph: (RectangleMorph new
		bounds: (nameMorph bounds outsetBy: 2);
		borderWidth: 0;
		color: (Color lightBlue alpha: 0.9)).
	self addMorph: nameMorph.
	^nameMorph! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 12/12/2011 19:56'!
basicBox
	| aBox minSide anExtent w |
	minSide _ 4 * self handleSize.
	anExtent _ ((self morphWidth + self handleSize + 8) max: minSide) @
				((self morphHeight + self handleSize + 8) max: minSide).
	aBox _ Rectangle center: bounds center extent: anExtent.
	w _ self world ifNil: [ target outermostWorldMorph ].
	^ w
		ifNil:
			[ aBox ]
		ifNotNil:
			[ aBox intersect: (w viewBox insetBy: 8@8) ]! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 12/14/2011 15:56'!
doGrow: evt with: growHandle
	"Called while the mouse is down in the grow handle"

	| newExtent |
self revisar.
	self flag: #jmvVer2.
	evt hand obtainHalo: self.
"Como podria andar el grow de un morph embebido en otro? andara ahora?"
newExtent _ evt eventPosition - positionOffset - target bounds topLeft.
	evt shiftPressed ifTrue: [newExtent _ (newExtent x max: newExtent y) asPoint].
	(newExtent x = 0 or: [newExtent y = 0]) ifTrue: [^ self].
	target extent: newExtent.
	growHandle morphPosition: evt eventPosition - (growHandle extent // 2).
	self someSubmorphPositionOrExtentChanged
! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 12/14/2011 15:56'!
doRot: evt with: rotHandle
	"Update the rotation of my target if it is rotatable.  Keep the relevant command object up to date."

	| degrees |
self revisar.
	self flag: #jmvVer2.
	evt hand obtainHalo: self.
	degrees _ (evt eventPosition - target referencePosition) degrees.
	degrees _ degrees - angleOffset degrees.
	degrees _ degrees detentBy: 10.0 atMultiplesOf: 90.0 snap: false.
	degrees = 0.0
		ifTrue: [rotHandle color: Color lightBlue]
		ifFalse: [rotHandle color: Color blue].
	rotHandle submorphsDo:
		[:m | m color: rotHandle color makeForegroundColor].
	self removeAllHandlesBut: rotHandle.

	target rotationDegrees: degrees.

	rotHandle morphPosition: evt eventPosition - (rotHandle extent // 2).
	self someSubmorphPositionOrExtentChanged! !


!HandMorph methodsFor: 'initialization' stamp: 'jmv 12/15/2011 20:46'!
initialize
	super initialize.
	self initForEvents.
	keyboardFocus _ nil.
	mouseFocus _ nil.
	bounds _ 0@0 extent: CursorWithMask normal extent.
	position _ 0@0.
	extent _ CursorWithMask normal extent.
	self validatePositionAndBounds.
	damageRecorder _ DamageRecorder new.
	self initForEvents.! !


!InnerHierarchicalListMorph methodsFor: 'geometry' stamp: 'jmv 12/12/2011 14:31'!
adjustExtent
	"And reposition submorphs"
	| w p0 h y |
	"make all items wide, so selection indicator is wide too"
	w _ self desiredWidth.
	p0 _ bounds topLeft..
	y _ 0.
	self submorphsDo: [ :m |
		h _ m morphHeight.
		m privateBounds: (p0 + (0@y) extent: w@h).
		y _ y + h ].
	self extent: w@y! !


!InnerTextMorph methodsFor: 'accessing' stamp: 'jmv 12/12/2011 14:19'!
model: aTextModel wrappedTo: width
	"Accept new text contents.  Lay it out, wrapping to width.
	Then fit my height to the result."
	wrapFlag _ true.
	self basicExtent: width truncated@self morphHeight.
	self model: aTextModel! !


!LayoutMorph methodsFor: 'adjust' stamp: 'jmv 12/12/2011 14:18'!
adjustHorizontallyBy: aLayoutAdjustMorph at: aPoint
	| delta l ls r rs lNewWidth rNewWidth i lCurrentWidth rCurrentWidth doNotResizeBelow |
	doNotResizeBelow _ self minPaneWidthForReframe.
	i _ submorphs indexOf: aLayoutAdjustMorph.
	l _ self submorphs at: i +1.
	ls _ l layoutSpec.
	lCurrentWidth _ l morphWidth max: 1.	"avoid division by zero"
	r _ self submorphs at: i - 1.
	rs _ r layoutSpec.
	rCurrentWidth _ r morphWidth max: 1.	"avoid division by zero"
	delta _ aPoint x - aLayoutAdjustMorph referencePosition x.
	delta _ delta max: doNotResizeBelow - lCurrentWidth.
	delta _ delta min: rCurrentWidth - doNotResizeBelow.
	delta = 0 ifTrue: [ ^self ].
	rNewWidth _ rCurrentWidth - delta.
	lNewWidth _ lCurrentWidth + delta.
	(ls isProportionalWidth and: [ rs isProportionalWidth ])
		ifTrue: [	"If both proportional, update them"
			ls setProportionalWidth: (1.0 * lNewWidth / lCurrentWidth * ls proportionalWidth).
			rs setProportionalWidth: (1.0 * rNewWidth / rCurrentWidth * rs proportionalWidth) ]
		ifFalse: ["If at least one is fixed, update only the fixed"
			ls isProportionalWidth ifFalse: [
				ls fixedOrMorphWidth: lNewWidth ].
			rs isProportionalWidth ifFalse: [
				rs fixedOrMorphWidth: rNewWidth ]].
	self layoutSubmorphs.
	fullBounds _ bounds! !

!LayoutMorph methodsFor: 'adjust' stamp: 'jmv 12/12/2011 14:18'!
adjustVerticallyBy: aLayoutAdjustMorph at: aPoint
	| delta t ts b bs tNewHeight bNewHeight i tCurrentHeight bCurrentHeight doNotResizeBelow |
	doNotResizeBelow _ self minPaneHeightForReframe.
	i _ submorphs indexOf: aLayoutAdjustMorph.
	t _ self submorphs at: i +1.
	ts _ t layoutSpec.
	tCurrentHeight _ t morphHeight max: 1.	"avoid division by zero"
	b _ self submorphs at: i - 1.
	bs _ b layoutSpec.
	bCurrentHeight _ b morphHeight max: 1.	"avoid division by zero"
	delta _ aPoint y - aLayoutAdjustMorph referencePosition y.
	delta _ delta max: doNotResizeBelow - tCurrentHeight.
	delta _ delta min: bCurrentHeight - doNotResizeBelow.
	delta = 0 ifTrue: [ ^self ].
	tNewHeight _ tCurrentHeight + delta.
	bNewHeight _ bCurrentHeight - delta.
	(ts isProportionalHeight and: [ bs isProportionalHeight ])
		ifTrue: [	"If both proportional, update them"
			ts setProportionalHeight: (1.0 * tNewHeight / tCurrentHeight * ts proportionalHeight).
			bs setProportionalHeight: (1.0 * bNewHeight / bCurrentHeight * bs proportionalHeight) ]
		ifFalse: ["If at least one is fixed, update only the fixed"
			ts isProportionalHeight ifFalse: [
				ts fixedOrMorphHeight: tNewHeight ].
			bs isProportionalHeight ifFalse: [
				bs fixedOrMorphHeight: bNewHeight ]].
	self layoutSubmorphs.
	fullBounds _ bounds! !


!MenuLineMorph methodsFor: 'initialization' stamp: 'jmv 12/12/2011 14:20'!
initialize
	super initialize.
	self morphHeight: 2! !


!MenuMorph methodsFor: 'construction' stamp: 'jmv 12/12/2011 14:27'!
addStayUpIcons
	| closeBox pinBox w |
	
	(self valueOfProperty: #hasStayUpIcons ifAbsent: [ false ])
		ifTrue: [
		 	self removeProperty: #needsStayUpIcons.
			^self ].
	titleMorph ifNil: [
		"Title not yet there. Flag ourself, so this method is called again when adding title."
		self setProperty: #needsStayUpIcons toValue: true.
		^ self].
	closeBox _ PluggableButtonMorph model: self action: #delete.
	closeBox icon: Theme current closeIcon.
	pinBox _ PluggableButtonMorph model: self action: #stayUp.
	pinBox icon: Theme current pushPinIcon.
	w _ (titleMorph hasSubmorphs ifTrue: [ titleMorph firstSubmorph morphWidth ] ifFalse: [ 0 ]) + 42.
	self addMorphFront: 
		(LayoutMorph newRow
			morphHeight: (titleMorph morphHeight max: 19);
			morphWidth: w;	"Make room for buttons"
			color: Color transparent;
			addMorph: closeBox fixedWidth: 20;
			addMorph: (RectangleMorph new borderWidth: 0; color: Color transparent) fixedWidth: 4;
			addMorph: titleMorph proportionalWidth: 1;
			addMorph: (RectangleMorph new borderWidth: 0; color: Color transparent) fixedWidth: 4;
			addMorph: pinBox fixedWidth: 20).

	self setProperty: #hasStayUpIcons toValue: true.
	self removeProperty: #needsStayUpIcons! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 3/14/2012 13:47'!
addTitle: aString
	"Add a title line at the top of this menu Make aString its initial 
	contents.  
	If aSelector is not nil, then periodically obtain fresh values for its 
	contents by sending aSelector to aTarget.."

	| s pp w |
	
	titleMorph _ RectangleMorph new.
	self setTitleParametersFor: titleMorph.
	pp _ 8@2.
	aString asString linesDo: [ :line |
		s _ StringMorph new
			contents: line;
			font: Preferences standardMenuFont bold.
		titleMorph addMorphBack: s.
		s morphPositionInOwner: pp.
		pp _ pp + (0@(s morphHeight+2)) ].
	w _ titleMorph submorphs inject: 0 into: [ :prev :each |
		prev max: each morphWidth ].
	titleMorph morphHeight: pp y; morphWidth: w + 8.
	self addMorphFront: titleMorph.
	
	(self hasProperty: #needsStayUpIcons) ifTrue: [ self addStayUpIcons ]! !

!MenuMorph methodsFor: 'initialization' stamp: 'jmv 12/15/2011 20:45'!
initialize
	super initialize.
	bounds _ 0@0 corner: 40@10.
	position _ 0@0.
	self validatePositionAndBounds.
	extent _ 40@10.
	defaultTarget _ nil.
	selectedItem _ nil.
	stayUp _ false.
	popUpOwner _ nil
! !

!MenuMorph methodsFor: 'private' stamp: 'jmv 12/12/2011 14:25'!
adjustSubmorphsLayout
	"Enlarge the width of submorphs as needed
	so all of them are have the same width, and no less than #minWidth.
	Also adjust their vertical position.
	Finally, set our own extent."
	
	| w p tl |
	
	submorphs isEmpty ifTrue: [ ^self ].
	w _ submorphs inject: 0 into: [ :prev :each |
		prev max: (
			(each respondsTo: #minItemWidth)
				ifTrue: [each minItemWidth]
				ifFalse: [each morphWidth])].

	w _ w + 4.
	tl _ bounds topLeft.
	p _ tl + 5.
	submorphs do: [ :m |
		m morphWidth: w.
		m morphPosition: p.
		p _ m bounds bottomLeft +(0@1) ].
	
	self extent: submorphs last bounds bottomRight - tl + 5! !

!MenuMorph methodsFor: 'private' stamp: 'jmv 12/12/2011 14:21'!
positionAt: aPoint relativeTo: aMenuItem inWorld: aWorld
	"Note: items may not be laid out yet (I found them all to be at 0@0),  
	so we have to add up heights of items above the selected item."

	| i yOffset sub delta |
	self adjustSubmorphsLayout.
	i _ 0.
	yOffset _ 0.
	[(sub _ self submorphs at: (i _ i + 1)) == aMenuItem]
		whileFalse: [ yOffset _ yOffset + sub morphHeight ].

	self morphPosition: aPoint - (2 @ (yOffset + 8)).

	"If it doesn't fit, show it to the left, not to the right of the hand."
	bounds right > aWorld world bounds right
		ifTrue: [
			self moveRight: aPoint x + 1].

	"Make sure that the menu fits in the world."
	delta _ bounds amountToTranslateWithin:
		(aWorld world bounds withHeight: ((aWorld world bounds height - 18) max: (ActiveHand morphPosition y) + 1)).
	delta = (0 @ 0) ifFalse: [ self morphPosition: self morphPosition + delta ]! !


!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 12/15/2011 20:46'!
extent: aPoint

	self flag: #jmvVer2.
	bounds extent = aPoint ifFalse: [
		self redrawNeeded.
		bounds _ bounds topLeft extent: aPoint.
		extent _ aPoint.
		self validatePositionAndBounds.
		self buildMagnifiedBackgroundImage.
		self someSubmorphPositionOrExtentChanged.
		self redrawNeeded ].

	worldState ifNotNil: [
		worldState viewBox ifNotNil: [
			worldState viewBox = bounds ifFalse: [
				worldState canvas: nil.
				worldState viewBox: bounds ]]]! !

!PasteUpMorph methodsFor: 'project state' stamp: 'jmv 12/15/2011 20:46'!
viewBox: newViewBox
	self flag: #jmvVer2.
	self isWorldMorph ifTrue: [
		(self viewBox isNil or: [ self viewBox extent ~= newViewBox extent ])
			ifTrue: [ worldState canvas: nil ].
		worldState viewBox: newViewBox ].
"	super morphPosition: newViewBox topLeft."
	fullBounds _ bounds _ newViewBox.
	position _ owner
		ifNil: [ bounds topLeft ]
		ifNotNil: [ owner internalizeFromWorld: bounds topLeft ].
	extent _ bounds extent.
	self validatePositionAndBounds.! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 12/13/2011 14:44'!
bringWindowsFullOnscreen
	"Make ever SystemWindow on the desktop be totally on-screen, whenever possible."
	
	(SystemWindow windowsIn: self satisfying: [ :w | true]) do:
		[ :aWindow | 
			aWindow windowRight: (aWindow bounds right min: bounds right).
			aWindow windowBottom: (aWindow bounds bottom min: bounds bottom).
			aWindow windowLeft: (aWindow bounds left max: bounds left).
			aWindow windowTop: (aWindow bounds top max: bounds top)]! !


!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 12/12/2011 14:14'!
hTotalScrollRange
	"Return the width extent of the receiver's scrollable area"
	^scroller morphWidth! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 12/12/2011 14:14'!
vTotalScrollRange
	"Return the height extent of the receiver's scrollable area"
	^scroller morphHeight! !


!HierarchicalListMorph methodsFor: 'geometry' stamp: 'jmv 12/12/2011 14:15'!
scrollDeltaHeight
	scroller hasSubmorphs ifFalse: [ ^1].
	^ scroller firstSubmorph morphHeight! !


!LimitedHeightTextMorph methodsFor: 'geometry' stamp: 'jmv 12/12/2011 14:20'!
innerHeight: aNumber
	"Adjust height and scrollbar to the new contents height."
	self morphHeight: (aNumber + 10 min: maxHeight)! !


!PluggableListMorph methodsFor: 'initialization' stamp: 'jmv 12/12/2011 14:17'!
initialize
	super initialize.
	scroller morphWidth: self morphWidth.
	scroller color: self textColor.! !


!PolygonMorph methodsFor: 'private' stamp: 'jmv 12/15/2011 20:45'!
computeBounds
	| oldBounds delta excludeHandles |

	self flag: #jmvVer2.
	vertices ifNil: [^ self].

	self redrawNeeded.
	oldBounds _ bounds.
	self releaseCachedState.
	bounds _ self curveBounds truncated.
	self arrowForms do: [ :f |
		bounds _ bounds merge: (f offset extent: f extent)].
	handles ifNotNil: [ self updateHandles ].

	"since we are directly updating bounds, see if any ordinary submorphs exist and move them accordingly"
	(oldBounds notNil and: [(delta _ bounds origin - oldBounds origin) ~= (0@0)]) ifTrue: [
		excludeHandles _ IdentitySet new.
		handles ifNotNil: [excludeHandles addAll: handles].
		self submorphsDo: [ :each |
			(excludeHandles includes: each) ifFalse: [
				each morphPosition: each morphPosition + delta ] ] ].
	self validatePositionAndBounds.
	self someSubmorphPositionOrExtentChanged.
	self redrawNeeded.! !


!ScrollBar methodsFor: 'geometry' stamp: 'jmv 12/12/2011 14:17'!
expandSlider
	"Compute the new size of the slider (use the old sliderThickness as a minimum)."
	| r |
	r _ self totalSliderArea.
	slider extent: (bounds isWide
		ifTrue: [((r width * interval) asInteger max: 7) @ slider morphHeight]
		ifFalse: [slider morphWidth @ ((r height * interval) asInteger max: 7)])! !


!SystemWindow methodsFor: 'initialization' stamp: 'jmv 12/12/2011 13:58'!
initialize
	"Initialize a system window. Add label, stripes, etc., if desired"

	super initialize.
	labelString ifNil: [ labelString _ 'Untitled Window'].
	isCollapsed _ false.
	
	self wantsLabel ifTrue: [self initializeLabelArea].
	self extent: 300 @ 200.
	updatablePanes _ #().

	adjusters _ Dictionary new.
	adjusters at: #topAdjuster put: WindowEdgeAdjustingMorph forTop.
	adjusters at: #bottomAdjuster put: WindowEdgeAdjustingMorph forBottom.
	adjusters at: #leftAdjuster put: WindowEdgeAdjustingMorph forLeft.
	adjusters at: #rightAdjuster put: WindowEdgeAdjustingMorph forRight.
	adjusters at: #topLeftAdjuster put: WindowEdgeAdjustingMorph forTopLeft.
	adjusters at: #bottomLeftAdjuster put: WindowEdgeAdjustingMorph forBottomLeft.
	adjusters at: #topRightAdjuster put: WindowEdgeAdjustingMorph forTopRight.
	adjusters at: #bottomRightAdjuster put: WindowEdgeAdjustingMorph forBottomRight.
	adjusters do: [ :m |
		self addMorph: m ].

	"by default"
	self beColumn! !


!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'jmv 12/13/2011 14:45'!
initializeBottom
	selector _ #windowBottom:.
	coordinateGetter _ #y.
	cursorGetter _ #resizeBottom! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'jmv 12/13/2011 14:45'!
initializeBottomLeft
	selector _ #windowBottomLeft:.
	coordinateGetter _ #yourself.
	cursorGetter _ #resizeBottomLeft! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'jmv 12/13/2011 14:45'!
initializeBottomRight
	selector _ #windowBottomRight:.
	coordinateGetter _ #yourself.
	cursorGetter _ #resizeBottomRight! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'jmv 12/13/2011 14:45'!
initializeLeft
	selector _ #windowLeft:.
	coordinateGetter _ #x.
	cursorGetter _ #resizeLeft! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'jmv 12/13/2011 14:45'!
initializeRight
	selector _ #windowRight:.
	coordinateGetter _ #x.
	cursorGetter _ #resizeRight! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'jmv 12/13/2011 14:45'!
initializeTop
	selector _ #windowTop:.
	coordinateGetter _ #y.
	cursorGetter _ #resizeTop! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'jmv 12/13/2011 14:45'!
initializeTopLeft
	selector _ #windowTopLeft:.
	coordinateGetter _ #yourself.
	cursorGetter _ #resizeTopLeft! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'jmv 12/13/2011 14:45'!
initializeTopRight
	selector _ #windowTopRight:.
	coordinateGetter _ #yourself.
	cursorGetter _ #resizeTopRight! !

!WindowEdgeAdjustingMorph methodsFor: 'geometry testing' stamp: 'jmv 12/13/2011 14:46'!
containsPoint: aPoint
	| sensitiveBorder |
	(bounds containsPoint: aPoint) ifFalse: [ ^false ].
	sensitiveBorder _ 4.
	selector caseOf: {
		[ #windowTopLeft: ] -> [ ^ aPoint x - bounds left < sensitiveBorder or: [ aPoint y - bounds top < sensitiveBorder ]].
		[ #windowTopRight: ] -> [ ^ bounds right - aPoint x <= sensitiveBorder or: [ aPoint y - bounds top < sensitiveBorder ]].
		[ #windowBottomLeft: ] -> [ ^ aPoint x - bounds left < sensitiveBorder or: [ bounds bottom - aPoint y <= sensitiveBorder ]].
		[ #windowBottomRight: ] -> [ ^ bounds right - aPoint x <= sensitiveBorder or: [ bounds bottom - aPoint y <= sensitiveBorder ]].
	}
	otherwise: [
		"all the morph is sensitive for horizontal and vertical (i.e. non corner) instances."
		^true ]! !

!methodRemoval: WindowEdgeAdjustingMorph class #bottom!
WindowEdgeAdjustingMorph class removeSelector: #bottom!
!methodRemoval: WindowEdgeAdjustingMorph class #bottomLeft!
WindowEdgeAdjustingMorph class removeSelector: #bottomLeft!
!methodRemoval: WindowEdgeAdjustingMorph class #bottomRight!
WindowEdgeAdjustingMorph class removeSelector: #bottomRight!
!methodRemoval: WindowEdgeAdjustingMorph class #left!
WindowEdgeAdjustingMorph class removeSelector: #left!
!methodRemoval: WindowEdgeAdjustingMorph class #right!
WindowEdgeAdjustingMorph class removeSelector: #right!
!methodRemoval: WindowEdgeAdjustingMorph class #top!
WindowEdgeAdjustingMorph class removeSelector: #top!
!methodRemoval: WindowEdgeAdjustingMorph class #topLeft!
WindowEdgeAdjustingMorph class removeSelector: #topLeft!
!methodRemoval: WindowEdgeAdjustingMorph class #topRight!
WindowEdgeAdjustingMorph class removeSelector: #topRight!
!methodRemoval: SystemWindow #bottom:!
SystemWindow removeSelector: #bottom:!
!methodRemoval: SystemWindow #bottomLeft:!
SystemWindow removeSelector: #bottomLeft:!
!methodRemoval: SystemWindow #bottomRight:!
SystemWindow removeSelector: #bottomRight:!
!methodRemoval: SystemWindow #left:!
SystemWindow removeSelector: #left:!
!methodRemoval: SystemWindow #right:!
SystemWindow removeSelector: #right:!
!methodRemoval: SystemWindow #top:!
SystemWindow removeSelector: #top:!
!methodRemoval: SystemWindow #topLeft:!
SystemWindow removeSelector: #topLeft:!
!methodRemoval: SystemWindow #topRight:!
SystemWindow removeSelector: #topRight:!
!methodRemoval: Morph #initPosExtent!
Morph removeSelector: #initPosExtent!
