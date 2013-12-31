'From Cuis 4.0 of 3 April 2012 [latest update: #1261] on 10 April 2012 at 3:33:56 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 16:41'!
validateExtentAndBounds
	"To be removed. Just to check consistency"
	| answer1 answer2 |
	self flag: #jmvVer2.

"	answer1 _ owner
		ifNotNil: [ owner externalizeDistanceToWorld: extent ]
		ifNil: [ extent ]."
	answer1 _ extent.
	answer2 _ bounds extent.

	answer1 = answer2 ifFalse: [
		#validateExtentAndBounds print.
		answer1 print.
		answer2 print.
		thisContext printStack: 10 ]! !


!Morph methodsFor: 'change reporting' stamp: 'jmv 12/16/2011 15:54'!
privateInvalidateMorph: aMorph
	"Private. Invalidate the given morph after adding or removing.
	This method is private because a) we're invalidating the morph 'remotely'
	and b) it forces a fullBounds computation which should not be necessary
	for a general morph c) the morph may or may not actually invalidate
	anything (if it's not in the world nothing will happen) and d) the entire
	mechanism should be rewritten."

	"Is this needed at all?"
	self flag: #jmvVer2.
	aMorph layoutSubmorphsAndComputeFullBounds.

	aMorph redrawNeeded! !

!Morph methodsFor: 'drawing' stamp: 'jmv 12/16/2011 16:39'!
drawErrorOn: aCanvas
	"The morph (or one of its submorphs) had an error in its drawing method."
	| r w |
	w _ 10.
	r _ bounds truncated.
	aCanvas
		frameAndFillRectangle: r
		fillColor: Color red
		borderWidth: w
		borderColor: Color yellow.
	aCanvas line: r topLeft +w to: r bottomRight -w width: w color: Color yellow.
	aCanvas line: r topRight + (w negated @ w) to: r bottomLeft + (w @ w negated) width: w color: Color yellow.! !

!Morph methodsFor: 'drawing' stamp: 'jmv 12/16/2011 16:51'!
fullDrawOn: aCanvas
	"Draw the full Morphic structure on the given Canvas"

	self visible ifFalse: [^ self].
	(aCanvas isVisible: self fullBounds) ifFalse:[^self].		"Needs fullBounds 'in owner' if inside a scroller"
	self isKnownFailing ifTrue: [^self drawErrorOn: aCanvas].

	"Draw receiver itself"
	(aCanvas isVisible: bounds) ifTrue: [
		aCanvas clipBy: bounds during: [ :c | self drawOn: c ]].	"Needs bounds 'in owner' if inside a scroller"
	self drawSubmorphsOn: aCanvas.
	self drawDropHighlightOn: aCanvas.
	self drawMouseDownHighlightOn: aCanvas! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 16:54'!
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
	bounds _ bounds topLeft extent: aPoint.
	extent _ aPoint.
	self validatePositionAndBounds.
	self validateExtentAndBounds.
	self someSubmorphPositionOrExtentChanged.
	self redrawNeeded! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 16:45'!
morphExtent
"assume it is always in owner's coordinates!!"
"Quizas eventualmente borrar este tambien? (no se usa mucho...)"
	self flag: #jmvVer2.
	self validateExtentAndBounds.
	^ extent! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 16:50'!
morphHeight

"Ensure everybody wants owner's coordinates!!"
	self flag: #jmvVer2.
	self validateExtentAndBounds.
	^ extent y! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 16:47'!
morphHeight: aNumber

"Ensure everybody wants owner's coordinates!!
Besides, #asInteger???"
	self flag: #jmvVer2.
	self morphExtent: self morphWidth@aNumber asInteger! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 16:42'!
morphPositionInOwner
"
Cuando tenga resueltos los setters (o quizas al mismo tiempo),
ver que senders del getter quieren en realidad #zzpositionInOwner y quienes #zzpositionInWorld.
Espero que pocos hablen en coordenadas del world!!
"

	self flag: #jmvVer2.
	self validateOwnerNotNil.
	self validatePositionAndBounds.
	self validateExtentAndBounds.

	^ position! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 16:42'!
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

	"May need to compute fullBounds, but I see no reason to layout submorphs!!"
	self flag: #jmvVer2.
	self layoutSubmorphsAndComputeFullBounds; redrawNeeded.

	self validateOwnerNotNil.


"This won't be needed once we remove the bounds ivar in global coordinates!!
With this, we also delete implementors of #privateFullMoveBy: and #privateMoveBy: "
self privateFullMoveBy: newPositionInWorld - bounds topLeft.

	position _ newPositionInOwner.
	self validatePositionAndBounds.
	self validateExtentAndBounds.

	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 16:43'!
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
	self validateExtentAndBounds.
	
	answer1 _ owner
		ifNotNil: [ owner externalizeToWorld: position ]
		ifNil: [ position ].
	answer2 _ bounds topLeft.
	{ answer1 . answer2 }.

	^answer2! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 16:43'!
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

	"May need to compute fullBounds, but I see no reason to layout submorphs!!"
	self flag: #jmvVer2.
	self layoutSubmorphsAndComputeFullBounds; redrawNeeded.

"This won't be needed once we remove the bounds ivar in global coordinates!!
With this, we also delete implementors of #privateFullMoveBy: and #privateMoveBy: "
self privateFullMoveBy: newPositionInWorld - bounds topLeft.

	position _ newPositionInOwner.
	self validatePositionAndBounds.
	self validateExtentAndBounds.

	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 16:48'!
morphWidth

"Ensure everybody wants owner's coordinates!!"
	self flag: #jmvVer2.
	self validateExtentAndBounds.
	^ extent x! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 16:49'!
morphWidth: aNumber

"Ensure everybody wants owner's coordinates!!
Besides, #asInteger???"
	self flag: #jmvVer2.
	self morphExtent: aNumber asInteger@self morphHeight! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 16:36'!
validateNotSent
	"To be removed. Just to check consistency"

	self flag: #jmvVer2.
"	Count ifNil: [ Count _ 0 ].
	Count _ Count + 1.
	Count < 10 ifTrue: ["
		'-----False polymorphism. Should not be sent!!-----' print.
		thisContext printStack: 10 
	"]"! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 12/16/2011 16:30'!
fullContainsPoint: aPoint
"
	This alternative implementation is included in this comment because it could be useful someday.
	If we start to rely heavily on the use of #ownShadowForm in #containsPoint, this could be cheaper.
	
	| shadow |
	self clipSubmorphs
		ifTrue: [ ^self containsPoint: aPoint ]
		ifFalse: [
			(self fullBounds containsPoint: aPoint) ifFalse: [^ false].
			(self containsPoint: aPoint) ifTrue: [^ true].
			shadow _ self shadowForm.
			^(shadow pixelValueAt: aPoint - shadow offset) > 0 ]
"
	
	self flag: #jmvVer2.
	"Is the comment relevant now?"
	
	(self fullBounds containsPoint: aPoint) ifFalse: [ ^ false ].  "quick elimination"
	(self containsPoint: aPoint) ifTrue: [ ^ true ].  "quick acceptance"
	submorphs do: [:m | (m fullContainsPoint: aPoint) ifTrue: [ ^ true ]].
	^ false
! !

!Morph methodsFor: 'initialization' stamp: 'jmv 12/16/2011 16:42'!
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
	self validateExtentAndBounds.
	color _ self defaultColor! !

!Morph methodsFor: 'layout' stamp: 'jmv 12/16/2011 15:56'!
computeFullBounds
	"Private. Compute the actual full bounds of the receiver"

	"Remove when removing fullBounds"
	self flag: #jmvVer2.

	(submorphs isEmpty or: [ self clipsSubmorphs ]) ifTrue: [ ^bounds ].
	^ bounds quickMerge: self submorphBounds! !

!Morph methodsFor: 'layout' stamp: 'jmv 12/16/2011 15:56'!
layoutSubmorphsAndComputeFullBounds
	"Return self. Recompute the layout if necessary."

	"Check senders. Many many not be needed. Others might be just to compute fullBounds, that we hope to elliminate!! Keep those that really need layout. of submorphs"
	self flag: #jmvVer2.

	fullBounds ifNotNil: [ ^self ].

	"Errors at this point can be critical so make sure we catch 'em all right"
	[
		self layoutSubmorphs.
		fullBounds _ self computeFullBounds.
	] on: Exception do: [ :ex |
		"This should do it unless you don't screw up the bounds"
		fullBounds _ bounds.
		ex pass ]! !

!Morph methodsFor: 'layout' stamp: 'jmv 12/16/2011 16:31'!
submorphBounds
	"Private. Compute the actual full bounds of the receiver"

	"Remove when removing fullBounds? Reimplement?"
	self flag: #jmvVer2.

	^submorphs inject: nil into: [ :prevBox :m |
		m visible
			ifTrue: [ m fullBounds quickMerge: prevBox ]
			ifFalse: [ prevBox ] ]! !

!Morph methodsFor: 'updating' stamp: 'jmv 12/16/2011 16:00'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn."

	self flag: #jmvVer2.
	"Invalidate the appropriate display rectangle... Just ours, or include submorphs if we don't clip.
	Think about it. We don't to know about a specific rectangle... How do we notify our 'observers' (i.e. the possible canvases we end drawn upon)?"
	
	"This method is the only real use of ivar fullBounds, other than senders of #fullBounds"

	self invalidRect: (fullBounds ifNil: [ bounds ])! !

!Morph methodsFor: 'private' stamp: 'jmv 12/16/2011 17:07'!
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
			bounds _ position extent: extent.
			fullBounds _ nil.
			self validatePositionAndBounds.
			self validateExtentAndBounds.
			]
		ifNotNil: [
			prevOwner
				ifNil: [
					"Didn't have any owner. Assume my local position is to be maintained in my new owner"
					bounds _ (owner externalizeToWorld: position) extent: extent.
					fullBounds _ nil.
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


!HaloHandleMorph methodsFor: 'drawing' stamp: 'jmv 12/16/2011 17:09'!
drawOn: aCanvas
	self validatePositionAndBounds.
	self validateExtentAndBounds.
	aCanvas
		image: (self class circleForm: extent)
		multipliedBy: (color alpha: 0.57)
		at: bounds topLeft.! !


!HaloMorph methodsFor: 'stepping' stamp: 'jmv 12/16/2011 17:08'!
step
	| newBounds |
	target
		ifNil: [^ self].
	newBounds _ target isWorldMorph
				ifTrue: [target bounds]
				ifFalse: [target worldBoundsForHalo truncated].
	newBounds = bounds
		ifTrue: [^ self].
	self validatePositionAndBounds.
	self validateExtentAndBounds.
	newBounds extent = extent
		ifTrue: [^ self morphPosition: newBounds origin].
	growingOrRotating ifFalse: [
		submorphs size > 1
			ifTrue: [self addHandles]].
	"adjust halo bounds if appropriate"
	self bounds: newBounds! !

!HaloMorph methodsFor: 'updating' stamp: 'jmv 12/16/2011 17:09'!
redrawNeeded
	"Quicker to invalidate handles individually if target is large (especially the world)"

	self validatePositionAndBounds.
	self validateExtentAndBounds.
	extent > (200@200)
		ifTrue: [(target notNil and: [target ~~ self world]) ifTrue: [
					"Invalidate 4 outer strips first, thus subsuming separate damage."
					(self fullBounds areasOutside: target bounds) do:
						[ :r | self invalidRect: r ]].
				self submorphsDo: [:m | m redrawNeeded]]
		ifFalse: [ super redrawNeeded ]! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 12/16/2011 17:09'!
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
	target morphExtent: newExtent.
	growHandle morphPosition: evt eventPosition - (growHandle morphExtent // 2).
	self someSubmorphPositionOrExtentChanged
! !


!HandMorph methodsFor: 'initialization' stamp: 'jmv 12/16/2011 16:42'!
initialize
	super initialize.
	self initForEvents.
	keyboardFocus _ nil.
	mouseFocus _ nil.
	bounds _ 0@0 extent: CursorWithMask normal extent.
	position _ 0@0.
	extent _ CursorWithMask normal extent.
	self validatePositionAndBounds.
	self validateExtentAndBounds.
	damageRecorder _ DamageRecorder new.
	self initForEvents.! !


!ImageMorph methodsFor: 'accessing' stamp: 'jmv 12/16/2011 17:11'!
borderWidth: bw
	| newExtent |
	self validatePositionAndBounds.
	self validateExtentAndBounds.
	newExtent _ 2 * bw + image extent.
	extent = newExtent ifFalse: [
		self basicExtent: newExtent ]! !


!InnerTextMorph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 17:11'!
morphExtent: aPoint
	| newExtent |
	self validatePositionAndBounds.
	self validateExtentAndBounds.
	"Resist changing the extent if no wordwrap.. this should be checked."
	wrapFlag ifFalse: [ ^ self ].
	newExtent _ aPoint truncated max: self minimumExtent.
	
	"No change of wrap width"
	newExtent x = extent x ifTrue: [ ^ self ].

	super morphExtent: newExtent.
	
	self resetParagraph.
	self editor recomputeSelection.	
	self updateFromParagraph.! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 12/16/2011 17:11'!
fit
	"Adjust my bounds to fit the text.
	Required after the text changes,
	or if wrapFlag is true and the user attempts to change the extent."

	| newExtent |
	self validatePositionAndBounds.
	self validateExtentAndBounds.
	newExtent _ (self paragraph extent max: 9 @ StrikeFont default height) + (0 @ 2).
	extent = newExtent ifFalse: [
		self basicExtent: newExtent.
		self redrawNeeded.	"Too conservative: only paragraph composition
								should cause invalidation."
		].

	owner innerHeight: newExtent y! !


!MagnifierMorph methodsFor: 'menu' stamp: 'jmv 12/16/2011 17:00'!
chooseMagnification
	| result |
	result _ (SelectionMenu selections: #(1.5 2 4 8))
		startUpWithCaption: 'Choose magnification
(currently ', magnification printString, ')'.
	(result == nil or: [ result = magnification ]) ifTrue: [ ^ self ].
	magnification _ result.
	self morphExtent: extent. "round to new magnification"
	self redrawNeeded. "redraw even if extent wasn't changed"! !


!MenuMorph methodsFor: 'initialization' stamp: 'jmv 12/16/2011 16:42'!
initialize
	super initialize.
	bounds _ 0@0 corner: 40@10.
	position _ 0@0.
	extent _ 40@10.
	self validatePositionAndBounds.
	self validateExtentAndBounds.
	defaultTarget _ nil.
	selectedItem _ nil.
	stayUp _ false.
	popUpOwner _ nil
! !


!MinimalStringMorph methodsFor: 'accessing' stamp: 'jmv 12/16/2011 17:00'!
fitContents

	| newExtent |
	newExtent _ self measureContents.
	self validateExtentAndBounds.
	extent = newExtent ifFalse: [
		self morphExtent: newExtent.
		self redrawNeeded ]! !


!OneLineEditorMorph methodsFor: 'accessing' stamp: 'jmv 12/16/2011 17:11'!
fitContents

	| newExtent |
	self validatePositionAndBounds.
	self validateExtentAndBounds.
	newExtent _ self measureContents.
	self validateExtentAndBounds.
	extent = newExtent ifFalse: [
		self morphExtent: newExtent.
		self redrawNeeded ]! !


!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 17:10'!
morphExtent: aPoint

	self flag: #jmvVer2.
	self validatePositionAndBounds.
	self validateExtentAndBounds.
	extent = aPoint ifFalse: [
		self redrawNeeded.
		bounds _ bounds topLeft extent: aPoint.
		extent _ aPoint.
		self validatePositionAndBounds.
		self validateExtentAndBounds.
		self buildMagnifiedBackgroundImage.
		self someSubmorphPositionOrExtentChanged.
		self redrawNeeded ].

	worldState ifNotNil: [
		worldState viewBox ifNotNil: [
			worldState viewBox = bounds ifFalse: [
				worldState canvas: nil.
				worldState viewBox: bounds ]]]! !

!PasteUpMorph methodsFor: 'misc' stamp: 'jmv 12/16/2011 17:10'!
buildMagnifiedBackgroundImage
	| image old |
	self validatePositionAndBounds.
	self validateExtentAndBounds.
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

!PasteUpMorph methodsFor: 'project state' stamp: 'jmv 12/16/2011 16:43'!
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
	self validatePositionAndBounds.
	self validateExtentAndBounds.! !

!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 12/16/2011 15:45'!
restoreMorphicDisplay
	DisplayScreen startUp.
	self
		morphExtent: Display extent;
		viewBox: Display boundingBox;
		handsDo: [ :h | h visible: true ];
		fullRepaintNeeded.
	WorldState addDeferredUIMessage: [ Cursor normal show ]! !


!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 1/3/2012 16:22'!
draw3DLookOn: aCanvas

	| w f center x y borderStyleSymbol c availableW l labelMargin |
	borderStyleSymbol _ self isPressed ifFalse: [ #raised ] ifTrue: [ #inset ].
	c _ color.
	self mouseIsOver ifTrue: [ c _ c  lighter ].
	aCanvas
		fillRectangle: bounds
		colorOrInfiniteForm: c
		borderWidth: borderWidth
		borderStyleSymbol: borderStyleSymbol.

	f _ self fontToUse.
	center _ bounds center.

	label ifNotNil: [
		labelMargin _ 4.
		w _ f widthOfString: label.
		availableW _ bounds width-labelMargin-labelMargin-1.
		availableW >= w
			ifTrue: [
				x _ center x - (w // 2).
				l _ label ]
			ifFalse: [
				x _ bounds left + labelMargin.
				l _ label squeezedTo: (label size * availableW / w) rounded ].
		y _ center y - (f height // 2).
		self isPressed ifTrue: [
			x _ x + 1.
			y _ y + 1 ].
		aCanvas
			drawString: l
			in: (x@y extent: extent - (labelMargin*2-2@4))
			font: f
			color: Theme current buttonLabel ]! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 12/16/2011 17:03'!
drawEmbossedLabelOn: aCanvas

	| availableW center colorForLabel f l labelMargin targetSize w x y |
	label ifNotNil: [
		colorForLabel _ Theme current buttonLabel.
		self isPressed
			ifFalse: [
				self mouseIsOver
					ifFalse: [ colorForLabel _ colorForLabel adjustSaturation: -0.10 brightness: 0.10 ]]
			ifTrue: [ colorForLabel _ colorForLabel adjustSaturation: 0.0 brightness: -0.07 ].
		f _ self fontToUse.
		center _ bounds center.
		labelMargin _ 3.
		w _ f widthOfString: label.
		availableW _ bounds width-labelMargin-labelMargin.
		availableW >= w
			ifTrue: [
				l _ label ]
			ifFalse: [
				x _ bounds left + labelMargin.
				targetSize _ label size * availableW // w.
				l _ label squeezedTo: targetSize.
				(f widthOfString: l) > availableW ifTrue: [
					targetSize _ targetSize - 1.
					l _ label squeezedTo: targetSize ]].
		
		w _ f widthOfString: l.
		x _ center x - (w // 2).
		y _ center y - (f height // 2).
		aCanvas
			drawStringEmbossed: l
			in: (x@y extent: extent - (labelMargin*2-2@4))
			font: f
			color: colorForLabel ]! !

!PluggableButtonMorph methodsFor: 'private' stamp: 'jmv 12/16/2011 17:03'!
magnifiedIcon
	| b |
	magnifiedIcon ifNil: [
		magnifiedIcon _ icon.
		self isRoundButton
			ifFalse: [ ^ magnifiedIcon ].
		b _ extent x max: extent y.
		b < icon extent x ifTrue: [
			magnifiedIcon _ icon magnifyTo: b@b ].
		b /  icon extent x > 1.7
			ifTrue: [	
				b _ b * 3 // 4.
				magnifiedIcon _ icon magnifyTo: b @ b]].
	^magnifiedIcon! !


!PolygonMorph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 17:09'!
morphExtent: newExtent 
	"Not really advisable, but we can preserve most of the geometry if we don't
	shrink things too small."
	| safeExtent center |
	self validatePositionAndBounds.
	self validateExtentAndBounds.
	center _ self referencePosition.
	safeExtent _ newExtent max: 20@20.
	self setVertices: (vertices collect:
		[:p | p - center * (safeExtent asFloatPoint / (extent max: 1@1)) + center])! !

!PolygonMorph methodsFor: 'private' stamp: 'jmv 12/16/2011 16:43'!
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
	self validateExtentAndBounds.
	self someSubmorphPositionOrExtentChanged.
	self redrawNeeded.! !

!PolygonMorph methodsFor: 'private' stamp: 'jmv 4/10/2012 15:31'!
filledForm
	"Note: The filled form is actually 2 pixels bigger than bounds, and the point corresponding to this morphs' position is at 1@1 in the form.  This is due to the details of the fillig routines, at least one of which requires an extra 1-pixel margin around the outside.  Computation of the filled form is done only on demand."
	| bb origin |
	self validatePositionAndBounds.
	self validateExtentAndBounds.
	closed ifFalse: [^ filledForm _ nil].
	filledForm ifNotNil: [^ filledForm].
	filledForm _ ColorForm extent: extent+2.

	"Draw the border..."
	bb _ (BitBlt current toForm: filledForm) sourceForm: nil; fillColor: Color black;
			combinationRule: Form over; width: 1; height: 1.
	origin _ bounds topLeft asIntegerPoint-1.
	self lineSegmentsDo: [:p1 :p2 | bb drawFrom: p1 asIntegerPoint-origin
										to: p2 asIntegerPoint-origin].

	"Fill it in..."
	filledForm _ ColorForm mappingWhiteToTransparentFrom: filledForm anyShapeFill.

	borderColor mightBeTranslucent ifTrue: [
		"If border is stored as a form, then erase any overlap now."
		filledForm
			copy: self borderForm boundingBox
			from: self borderForm
			to: 1@1
			rule: Form erase ].

	^ filledForm! !


!ScrollBar methodsFor: 'geometry' stamp: 'jmv 4/10/2012 15:31'!
morphExtent: newExtent

	| newExtentToUse |
	self validatePositionAndBounds.
	self validateExtentAndBounds.
	newExtent = extent ifTrue: [^ self].
	newExtentToUse _ bounds isWide
		ifTrue: [ (newExtent x max: 14) @ newExtent y ]
		ifFalse: [ newExtent x @ (newExtent y max: 14) ].
	newExtentToUse = extent ifTrue: [^ self].
	super morphExtent: newExtentToUse.
		
	self flag: #jmv.
	"Most times it is not necessary to recreate the buttons"
	self recreateSubmorphs! !


!StringMorph methodsFor: 'accessing' stamp: 'jmv 12/16/2011 16:56'!
fitContents

	| newExtent |
	newExtent _ self measureContents.
	self validateExtentAndBounds.
	extent = newExtent ifFalse: [
		self morphExtent: newExtent.
		self redrawNeeded ]! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 12/16/2011 17:04'!
makeMeVisible 

	self world morphExtent > (0@0) ifFalse: [^ self].

	(self morphPosition >= (0@0) and: [ self morphPosition < (self world morphExtent-self labelHeight)]) ifTrue: [
		^ self "OK -- at least my top left is visible"].

	"window not on screen (probably due to reframe) -- move it now"
	self isCollapsed
		ifTrue: [ self morphPosition: (RealEstateAgent assignCollapsePointFor: self)]
		ifFalse: [ self morphPosition: (RealEstateAgent initialFrameFor: self initialExtent: extent world: self world) topLeft].

! !

!SystemWindow methodsFor: 'geometry' stamp: 'jmv 12/16/2011 17:04'!
morphExtent: aPoint 
	"Set the receiver's extent to value provided. Respect my minimumExtent."

	| newExtent |
	newExtent _ self isCollapsed
		ifTrue: [aPoint]
		ifFalse: [aPoint max: self minimumExtent].
	newExtent = extent ifTrue: [^ self].

	isCollapsed
		ifTrue: [super morphExtent: newExtent x @ (self labelHeight + 2)]
		ifFalse: [super morphExtent: newExtent].
	isCollapsed
		ifTrue: [collapsedFrame _ bounds]
		ifFalse: [fullFrame _ bounds]! !

!SystemWindow methodsFor: 'layout' stamp: 'jmv 12/16/2011 15:50'!
layoutSubmorphs
	"Compute a new layout of submorphs based on the given layout bounds."

	| bl br h thickness tl tr w cornerExtent |
	thickness _ 4.
	cornerExtent _ 20.
	tl _ bounds topLeft.
	tr _ bounds topRight.
	bl _ bounds bottomLeft.
	br _ bounds bottomRight.
	w _ bounds width - cornerExtent - cornerExtent.
	h _ bounds height - cornerExtent - cornerExtent.
	(adjusters at: #topAdjuster) bounds: (tl + (cornerExtent@0) extent: w@thickness).
	(adjusters at: #bottomAdjuster) bounds: (bl+ (cornerExtent @ thickness negated) extent: w@thickness).
	(adjusters at: #leftAdjuster) bounds: (tl+ (0@cornerExtent) extent: thickness@h).
	(adjusters at: #rightAdjuster) bounds: (tr + (thickness negated@ cornerExtent) extent: thickness@h).
	(adjusters at: #topLeftAdjuster) bounds: (tl extent: cornerExtent@cornerExtent).
	(adjusters at: #bottomLeftAdjuster) bounds: (bl-(0@cornerExtent) extent: cornerExtent@cornerExtent).
	(adjusters at: #topRightAdjuster) bounds: (tr+(cornerExtent negated@0) extent: cornerExtent@cornerExtent).
	(adjusters at: #bottomRightAdjuster) bounds: (br-cornerExtent extent: cornerExtent@cornerExtent).

	layoutMorph ifNotNil: [
		layoutMorph bounds: self layoutBounds ]! !


!TranscriptMorph methodsFor: 'drawing' stamp: 'jmv 12/16/2011 17:10'!
drawOn: aCanvas
	"
	Transcript
		showOnDisplay: true;
		bounds: bounds;
		displayOn: aCanvas form.
	"
	self validatePositionAndBounds.
	self validateExtentAndBounds.
	Transcript
		showOnDisplay: true;
		bounds: (0@0 extent: extent);
		displayOn: form;
		bounds: bounds.
	aCanvas image: form at: bounds origin! !

!methodRemoval: TranscriptMorph #extent:!
TranscriptMorph removeSelector: #extent:!
!methodRemoval: SystemWindow #extent:!
SystemWindow removeSelector: #extent:!
!methodRemoval: Sonogram #extent:!
Sonogram removeSelector: #extent:!
!methodRemoval: ScrollBar #extent:!
ScrollBar removeSelector: #extent:!
!methodRemoval: PolygonMorph #extent:!
PolygonMorph removeSelector: #extent:!
!methodRemoval: LimitedHeightTextMorph #extent:!
LimitedHeightTextMorph removeSelector: #extent:!
!methodRemoval: PluggableScrollPane #extent:!
PluggableScrollPane removeSelector: #extent:!
!methodRemoval: PluggableButtonMorph #extent:!
PluggableButtonMorph removeSelector: #extent:!
!methodRemoval: PasteUpMorph #extent:!
PasteUpMorph removeSelector: #extent:!
!methodRemoval: MagnifierMorph #extent:!
MagnifierMorph removeSelector: #extent:!
!methodRemoval: InnerTextMorph #extent:!
InnerTextMorph removeSelector: #extent:!
!methodRemoval: ImageMorph #extent:!
ImageMorph removeSelector: #extent:!
!methodRemoval: Morph #extent!
Morph removeSelector: #extent!
!methodRemoval: Morph #extent:!
Morph removeSelector: #extent:!
!methodRemoval: Morph #height!
Morph removeSelector: #height!
!methodRemoval: Morph #height:!
Morph removeSelector: #height:!
!methodRemoval: Morph #width!
Morph removeSelector: #width!
!methodRemoval: Morph #width:!
Morph removeSelector: #width:!
