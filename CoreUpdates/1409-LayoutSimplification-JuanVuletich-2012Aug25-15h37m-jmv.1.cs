'From Cuis 4.0 of 21 April 2012 [latest update: #1408] on 25 August 2012 at 3:41:40 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 8/25/2012 15:41'!
morphFullBoundsInWorld
	"Morphs should know nothing about absolute coordinates..."
	"Should implement in some reasonable way... including submorphs?"

	self flag: #jmvVer2.
	"IF I remove this, then layout of buttons in FileList breaks when selecting / deselecting code files. Besides, ProgressMorph example breaks too"
	self layoutSubmorphsIfNeeded.

	self flag: #jmvVer2.
	^self morphBoundsInWorld! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/25/2012 15:09'!
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
	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/25/2012 15:09'!
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
	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!Morph methodsFor: 'layout' stamp: 'jmv 8/25/2012 15:19'!
layoutSubmorphs
	"Compute a new layout of submorphs based on the given layout bounds."

	"Only specific subclasses do layout."
	layoutNeeded _ false! !

!Morph methodsFor: 'layout' stamp: 'jmv 8/25/2012 15:40'!
layoutSubmorphsIfNeeded
	"Return self. Recompute the layout if necessary."

	"Check senders. Many many not be needed. Others might be just to compute fullBounds, that we hope to elliminate!! Keep those that really need layout. of submorphs"
	self flag: #jmvVer2.

	layoutNeeded ifTrue: [
		self layoutSubmorphs ].! !

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 8/25/2012 15:16'!
removeMorph: aMorph
	"Remove the given morph from my submorphs"
	| aWorld |
	aMorph owner == self ifFalse:[^self].
	aWorld := self world.
	aWorld ifNotNil: [
		aMorph redrawNeeded ].
	self privateRemove: aMorph.
	aMorph privateOwner: nil.
	self removedMorph: aMorph.
	self someSubmorphPositionOrExtentChanged.! !

!Morph methodsFor: 'updating' stamp: 'jmv 8/25/2012 15:40'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn."

	self flag: #jmvVer2.
	"Invalidate the appropriate display rectangle... Just ours, or include submorphs if we don't clip.
	Think about it. We don't to know about a specific rectangle... How do we notify our 'observers' (i.e. the possible canvases we end drawn upon)?"

	
	self layoutSubmorphsIfNeeded.
	self invalidRect: self morphFullBoundsInWorld! !

!Morph methodsFor: 'private' stamp: 'jmv 8/25/2012 15:15'!
privateAddAllMorphs: aCollection atIndex: index
	"Private. Add aCollection of morphs to the receiver"
	| myWorld itsWorld otherSubmorphs |
	myWorld _ self world.
	otherSubmorphs _ submorphs copyWithoutAll: aCollection.
	(index between: 0 and: otherSubmorphs size)
		ifFalse: [^ self error: 'index out of range'].
	index = 0
		ifTrue:[	submorphs _ aCollection asArray, otherSubmorphs]
		ifFalse:[	index = otherSubmorphs size
			ifTrue:[	submorphs _ otherSubmorphs, aCollection]
			ifFalse:[	submorphs _ otherSubmorphs copyReplaceFrom: index + 1 to: index with: aCollection ]].
	aCollection do: [:m | | itsOwner |
		itsOwner _ m owner.
		itsOwner ifNotNil: [
			itsWorld _ m world.
			(itsWorld == myWorld) ifFalse: [
				itsWorld ifNotNil: [m redrawNeeded]].
			(itsOwner ~~ self) ifTrue: [
				m owner privateRemove: m.
				m owner removedMorph: m ]].
		m privateOwner: self.
		myWorld ifNotNil: [m redrawNeeded].
		(myWorld == itsWorld) ifFalse: [m intoWorld: myWorld].
		itsOwner == self ifFalse: [
			self addedMorph: m.
			m noteNewOwner: self ].
	].
	self someSubmorphPositionOrExtentChanged! !

!Morph methodsFor: 'private' stamp: 'jmv 8/25/2012 15:16'!
privateAddMorph: aMorph atIndex: index

	| oldIndex myWorld itsWorld oldOwner |
	((index >= 1) and: [index <= (submorphs size + 1)])
		ifFalse: [^ self error: 'index out of range'].
	myWorld _ self world.
	oldOwner _ aMorph owner.
	(oldOwner == self and: [(oldIndex _ submorphs indexOf: aMorph) > 0]) ifTrue:[
		"aMorph's position changes within in the submorph chain"
		oldIndex < index ifTrue:[
			"moving aMorph to back"
			submorphs replaceFrom: oldIndex to: index-2 with: submorphs startingAt: oldIndex+1.
			submorphs at: index-1 put: aMorph.
		] ifFalse:[
			"moving aMorph to front"
			oldIndex-1 to: index by: -1 do:[:i|
				submorphs at: i+1 put: (submorphs at: i)].
			submorphs at: index put: aMorph.
		].
	] ifFalse:[
		"adding a new morph"
		oldOwner ifNotNil:[
			itsWorld _ aMorph world.
			itsWorld ifNotNil: [aMorph redrawNeeded].
			oldOwner privateRemove: aMorph.
			oldOwner removedMorph: aMorph.
		].
		aMorph privateOwner: self.
		submorphs _ submorphs copyReplaceFrom: index to: index-1 with: (Array with: aMorph).
		(itsWorld == myWorld) ifFalse: [aMorph intoWorld: myWorld].
	].
	myWorld ifNotNil:[aMorph redrawNeeded].
	self someSubmorphPositionOrExtentChanged.
	oldOwner == self ifFalse: [
		self addedMorph: aMorph.
		aMorph noteNewOwner: self ].
! !


!SystemWindow methodsFor: 'layout' stamp: 'jmv 8/25/2012 15:19'!
layoutSubmorphs
	"Compute a new layout of submorphs based on the given layout bounds."

	| bl br h thickness tl tr w cornerExtent b |
	thickness _ 4.
	cornerExtent _ 20.
	b _ self morphBoundsInWorld.
	tl _ b topLeft.
	tr _ b topRight.
	bl _ b bottomLeft.
	br _ b bottomRight.
	w _ b width - cornerExtent - cornerExtent.
	h _ b height - cornerExtent - cornerExtent.
	(adjusters at: #topAdjuster) morphBoundsInWorld: (tl + (cornerExtent@0) extent: w@thickness).
	(adjusters at: #bottomAdjuster) morphBoundsInWorld: (bl+ (cornerExtent @ thickness negated) extent: w@thickness).
	(adjusters at: #leftAdjuster) morphBoundsInWorld: (tl+ (0@cornerExtent) extent: thickness@h).
	(adjusters at: #rightAdjuster) morphBoundsInWorld: (tr + (thickness negated@ cornerExtent) extent: thickness@h).
	(adjusters at: #topLeftAdjuster) morphBoundsInWorld: (tl extent: cornerExtent@cornerExtent).
	(adjusters at: #bottomLeftAdjuster) morphBoundsInWorld: (bl-(0@cornerExtent) extent: cornerExtent@cornerExtent).
	(adjusters at: #topRightAdjuster) morphBoundsInWorld: (tr+(cornerExtent negated@0) extent: cornerExtent@cornerExtent).
	(adjusters at: #bottomRightAdjuster) morphBoundsInWorld: (br-cornerExtent extent: cornerExtent@cornerExtent).

	layoutMorph ifNotNil: [
		layoutMorph morphBoundsInWorld: self layoutBounds ].
	
	layoutNeeded _ false! !


!WorldState methodsFor: 'drawing' stamp: 'jmv 8/25/2012 15:07'!
displayWorld: aWorld submorphs: submorphs
	"Update this world's display."

	| deferredUpdateMode worldDamageRects handsToDraw allDamage |
	self checkIfUpdateNeeded ifFalse: [ ^ self ].  "display is already up-to-date"
	deferredUpdateMode _ self doDeferredUpdatingFor: aWorld.
	deferredUpdateMode ifFalse: [ self assuredNonDisplayCanvas ].

	"repair world's damage on canvas"
	worldDamageRects _ self drawInvalidAreasWorld: aWorld submorphs: submorphs.

	handsToDraw _ self selectHandsToDrawForDamage: worldDamageRects.
	allDamage _ Array streamContents: [ :strm |
		strm nextPutAll: worldDamageRects.
		handsToDraw do: [ :h | 
			h savePatchFrom: canvas appendDamageTo: strm ]].

	"draw hands onto world canvas"
	handsToDraw reverseDo: [ :h | self drawHand: h ].

	"*make this true to flash damaged areas for testing*"
	Preferences debugShowDamage ifTrue: [ aWorld flashRects: allDamage ].

	"quickly copy altered rects of canvas to Display:"
	deferredUpdateMode
		ifTrue: [ self forceDamageToScreen: allDamage ]
		ifFalse: [ canvas showAt: aWorld viewBox origin invalidRects: allDamage ].
	"restore world canvas under hands"
	handsToDraw do: [ :h | h restoreSavedPatchOn: canvas ].
	Display deferUpdates: false; forceDisplayUpdate! !

!methodRemoval: Morph #privateInvalidateMorph:!
Morph removeSelector: #privateInvalidateMorph:!
