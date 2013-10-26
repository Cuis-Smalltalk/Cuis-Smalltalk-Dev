'From Cuis 4.0 of 21 April 2012 [latest update: #1359] on 7 August 2012 at 7:19:50 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 8/7/2012 19:17'!
basicExtent: aPoint

	self flag: #jmvVer2.
	"Lo dejo un rato porque se llama dde initialize... De ultima, es un problema? Es cierto que no se la escala aun..."
	"
	self validateOwnerNotNil.
	"
	
	"ver senders. Es en owner's o en world's ???"
	extent = aPoint ifTrue: [^ self].
	self redrawNeeded.
	extent _ aPoint.
	self someSubmorphPositionOrExtentChanged.
	self redrawNeeded! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/7/2012 19:17'!
morphExtent
"assume it is always in owner's coordinates!!"
"Quizas eventualmente borrar este tambien? (no se usa mucho...)"
	self flag: #jmvVer2.
	^ extent! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/7/2012 19:18'!
morphHeight

"Ensure everybody wants owner's coordinates!!"
	self flag: #jmvVer2.
	^ extent y! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/7/2012 19:11'!
morphPositionInOwner
"
Cuando tenga resueltos los setters (o quizas al mismo tiempo),
ver que senders del getter quieren en realidad #zzpositionInOwner y quienes #zzpositionInWorld.
Espero que pocos hablen en coordenadas del world!!
"

	self flag: #jmvVer2.
	self validateOwnerNotNil.

	^ position! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/7/2012 19:11'!
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
	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/7/2012 19:13'!
morphPositionInWorld
	| answer1 |
"
Cuando tenga resueltos los setters (o quizas al mismo tiempo),
ver que senders del getter quieren en realidad #zzpositionInOwner y quienes #zzpositionInWorld.
Espero que pocos hablen en coordenadas del world!!
"


	self flag: #jmvVer2.
	self validateOwnerNotNil.
	
	answer1 _ owner
		ifNotNil: [ owner externalizeToWorld: position ]
		ifNil: [ position ].
"	answer2 _ bounds topLeft.
	{ answer1 . answer2 }."

	^answer1! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/7/2012 19:14'!
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
	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/7/2012 19:18'!
morphWidth

"Ensure everybody wants owner's coordinates!!"
	self flag: #jmvVer2.
	^ extent x! !

!Morph methodsFor: 'initialization' stamp: 'jmv 8/7/2012 19:10'!
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
	color _ self defaultColor.
	layoutNeeded _ false! !

!Morph methodsFor: 'private' stamp: 'jmv 8/7/2012 19:17'!
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
			layoutNeeded _ true ]
		ifNotNil: [
			prevOwner
				ifNil: [
					"Didn't have any owner. Assume my local position is to be maintained in my new owner"
					layoutNeeded _ true ]
				ifNotNil: [
					"Had an owner. Maintain my global position..."
					position _ owner internalizeFromWorld: oldGlobalPosition.
					self flag: #jmvVer2.
					"extent _ owner internalizeDistanceFromWorld: oldGlobalExtent" 	"or something like this!!"
					]]! !


!HaloHandleMorph methodsFor: 'drawing' stamp: 'jmv 8/7/2012 19:08'!
drawOn: aCanvas

	aCanvas
		image: (self class circleForm: extent)
		multipliedBy: (color alpha: 0.57)
		at: self morphPositionInWorld! !


!HaloMorph methodsFor: 'stepping' stamp: 'jmv 8/7/2012 19:09'!
step
	| newBounds |
	target
		ifNil: [^ self].
	newBounds _ target isWorldMorph
				ifTrue: [target morphBoundsInWorld]
				ifFalse: [target worldBoundsForHalo truncated].
	newBounds = self morphBoundsInWorld
		ifTrue: [^ self].
	newBounds extent = extent
		ifTrue: [^ self morphPosition: newBounds origin].
	growingOrRotating ifFalse: [
		submorphs size > 1
			ifTrue: [self addHandles]].
	"adjust halo bounds if appropriate"
	self morphBoundsInWorld: newBounds! !

!HaloMorph methodsFor: 'updating' stamp: 'jmv 8/7/2012 19:08'!
redrawNeeded
	"Quicker to invalidate handles individually if target is large (especially the world)"

	extent > (200@200)
		ifTrue: [(target notNil and: [target ~~ self world]) ifTrue: [
					"Invalidate 4 outer strips first, thus subsuming separate damage."
					(self morphFullBoundsInWorld areasOutside: target morphBoundsInWorld) do:
						[ :r | self invalidRect: r ]].
				self submorphsDo: [:m | m redrawNeeded]]
		ifFalse: [ super redrawNeeded ]! !


!HandMorph methodsFor: 'initialization' stamp: 'jmv 8/7/2012 19:09'!
initialize
	super initialize.
	self initForEvents.
	keyboardFocus _ nil.
	mouseFocus _ nil.
	position _ 0@0.
	extent _ CursorWithMask normal extent.
	damageRecorder _ DamageRecorder new.
	self initForEvents.! !


!ImageMorph methodsFor: 'accessing' stamp: 'jmv 8/7/2012 19:09'!
borderWidth: bw

	| newExtent |
	newExtent _ 2 * bw + image extent.
	extent = newExtent ifFalse: [
		self basicExtent: newExtent ]! !


!InnerTextMorph methodsFor: 'geometry' stamp: 'jmv 8/7/2012 19:09'!
morphExtent: aPoint
	| newExtent |
	"Resist changing the extent if no wordwrap.. this should be checked."
	wrapFlag ifFalse: [ ^ self ].
	newExtent _ aPoint truncated max: self minimumExtent.
	
	"No change of wrap width"
	newExtent x = extent x ifTrue: [ ^ self ].

	super morphExtent: newExtent.
	
	self resetParagraph.
	self editor recomputeSelection.	
	self updateFromParagraph.! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 8/7/2012 19:09'!
fit
	"Adjust my bounds to fit the text.
	Required after the text changes,
	or if wrapFlag is true and the user attempts to change the extent."

	| newExtent |
	newExtent _ (self paragraph extent max: 9 @ StrikeFont default height) + (0 @ 2).
	extent = newExtent ifFalse: [
		self basicExtent: newExtent ].

	self redrawNeeded.	"Too conservative: only paragraph composition
							should cause invalidation."
	owner innerHeight: newExtent y! !


!MenuMorph methodsFor: 'initialization' stamp: 'jmv 8/7/2012 19:10'!
initialize
	super initialize.
	position _ 0@0.
	extent _ 40@10.
	defaultTarget _ nil.
	selectedItem _ nil.
	stayUp _ false.
	popUpOwner _ nil! !


!MinimalStringMorph methodsFor: 'accessing' stamp: 'jmv 8/7/2012 19:17'!
fitContents

	| newExtent |
	newExtent _ self measureContents.
	extent = newExtent ifFalse: [
		self morphExtent: newExtent.
		self redrawNeeded ]! !


!OneLineEditorMorph methodsFor: 'accessing' stamp: 'jmv 8/7/2012 19:18'!
fitContents

	| newExtent |
	newExtent _ self measureContents.
	extent = newExtent ifFalse: [
		self morphExtent: newExtent.
		self redrawNeeded ]! !


!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 8/7/2012 19:15'!
morphExtent: aPoint

	self flag: #jmvVer2.
	extent = aPoint ifFalse: [
		self redrawNeeded.
		extent _ aPoint.
		self buildMagnifiedBackgroundImage.
		self someSubmorphPositionOrExtentChanged.
		self redrawNeeded ].

	worldState ifNotNil: [
		worldState viewBox ifNotNil: [
			worldState viewBox = self morphBoundsInWorld ifFalse: [
				worldState canvas: nil.
				worldState viewBox: self morphBoundsInWorld ]]]! !

!PasteUpMorph methodsFor: 'misc' stamp: 'jmv 8/7/2012 19:15'!
buildMagnifiedBackgroundImage
	| image old |
	old _ backgroundImage.
	backgroundImageData
		ifNil: [ backgroundImage _ nil ]
		ifNotNil: [ 
			image _ Form fromBinaryStream: backgroundImageData readStream.
			backgroundImage _ image magnifyTo: extent.
			self canvas ifNotNil: [ :c |
				(backgroundImage depth = 32 and: [ c depth < 32 ]) ifTrue: [
					backgroundImage _ backgroundImage orderedDither32To16 ]]
		].
	old == backgroundImage ifFalse: [
		self redrawNeeded ]! !

!PasteUpMorph methodsFor: 'project state' stamp: 'jmv 8/7/2012 19:15'!
viewBox: newViewBox
	self flag: #jmvVer2.
	self isWorldMorph ifTrue: [
		(self viewBox isNil or: [ self viewBox extent ~= newViewBox extent ])
			ifTrue: [ worldState canvas: nil ].
		worldState viewBox: newViewBox ].
	position _ owner
		ifNil: [ newViewBox topLeft ]
		ifNotNil: [ owner internalizeFromWorld: newViewBox topLeft ].
	extent _ newViewBox extent! !


!ScrollBar methodsFor: 'geometry' stamp: 'jmv 8/7/2012 19:15'!
morphExtent: newExtent

	| newExtentToUse |
	newExtent = extent ifTrue: [^ self].
	newExtentToUse _ self isHorizontal
		ifTrue: [ (newExtent x max: 14) @ newExtent y ]
		ifFalse: [ newExtent x @ (newExtent y max: 14) ].
	newExtentToUse = extent ifTrue: [^ self].
	super morphExtent: newExtentToUse.
		
	self flag: #jmv.
	"Most times it is not necessary to recreate the buttons"
	self recreateSubmorphs! !


!StringMorph methodsFor: 'accessing' stamp: 'jmv 8/7/2012 19:18'!
fitContents

	| newExtent |
	newExtent _ self measureContents.
	extent = newExtent ifFalse: [
		self morphExtent: newExtent.
		self redrawNeeded ]! !


!TranscriptMorph methodsFor: 'drawing' stamp: 'jmv 8/7/2012 19:15'!
drawOn: aCanvas
	"
	Transcript
		showOnDisplay: true;
		bounds: bounds;
		displayOn: aCanvas form.
	"
	Transcript
		showOnDisplay: true;
		morphBoundsInWorld: (0@0 extent: self morphExtentInWorld);
		displayOn: form;
		morphBoundsInWorld: self morphBoundsInWorld.
	aCanvas image: form at: self morphPositionInWorld! !

!methodRemoval: Morph #validateExtentAndBounds!
Morph removeSelector: #validateExtentAndBounds!
!methodRemoval: Morph #validatePositionAndBounds!
Morph removeSelector: #validatePositionAndBounds!
