'From Cuis 4.0 of 21 April 2012 [latest update: #1393] on 21 August 2012 at 6:51:51 pm'!
!classDefinition: #RectangleLikeMorph category: #'Morphic-Kernel'!
Morph subclass: #RectangleLikeMorph
	instanceVariableNames: 'xtent ccolor '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!RectangleLikeMorph methodsFor: 'accessing' stamp: 'jmv 8/21/2012 18:46'!
color: aColor
	"Set the receiver's color. "
	ccolor = aColor ifFalse: [
		color _ aColor.
		ccolor _ aColor.
		self redrawNeeded ]! !

!RectangleLikeMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 18:46'!
defaultColor
	^ Color orange! !

!RectangleLikeMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 18:49'!
initialize
	super initialize.
	xtent _ self defaultBounds extent.
	ccolor _ self defaultColor! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 8/21/2012 18:48'!
basicExtent: aPoint

	self flag: #jmvVer2.
	"Lo dejo un rato porque se llama dde initialize... De ultima, es un problema? Es cierto que no se la escala aun..."
	"
	self validateOwnerNotNil.
	"
	
	"ver senders. Es en owner's o en world's ???"
	xtent = aPoint ifTrue: [^ self].
	self redrawNeeded.
	extent _ aPoint.
	xtent _ aPoint.
	self someSubmorphPositionOrExtentChanged.
	self redrawNeeded! !


!HaloMorph methodsFor: 'event handling' stamp: 'jmv 8/21/2012 18:42'!
transferHalo: event  localPosition: localEventPosition
	"Transfer the halo to the next likely recipient"
	target ifNil: [ ^self delete ].
	target transferHalo: event from: target.! !


!Morph methodsFor: 'accessing' stamp: 'jmv 8/21/2012 18:47'!
color: aColor

"borrarlo!!"

	"Set the receiver's color. "
	color = aColor ifFalse: [
		color _ aColor.
		self redrawNeeded ]! !

!Morph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 18:49'!
defaultBounds
"answer the default bounds for the receiver"

	"Maybe shouldn't exist... INDEED!!"
	self flag: #jmvVer2.
	"Anyway, many morphs redefine #initialize asigning an extent... Unify in some style.
	Maybe #defaultExtent?"
	^ 0 @ 0 corner: 50 @ 40! !

!Morph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 18:47'!
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
	color _ self defaultColor.		"borrar este!!"
	layoutNeeded _ false! !


!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 18:51'!
initialize

	super initialize.
	extent _  271@121.
	xtent _  271@121.
	responseUponCancel _ ''
	! !


!HaloMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 18:42'!
mouseButton3Down: aMouseButtonEvent localPosition: localEventPosition
	"Transfer the halo to the next likely recipient"
	target ifNil:[^self delete].
	aMouseButtonEvent hand obtainHalo: self.
	positionOffset _ aMouseButtonEvent eventPosition - target morphPosition.
	"wait for drags or transfer"
	aMouseButtonEvent hand 
		waitForClicksOrDrag: self 
		event: aMouseButtonEvent
		clkSel: #transferHalo:localPosition:
		dblClkSel: nil! !


!HandMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 18:51'!
initialize
	super initialize.
	self initForEvents.
	keyboardFocus _ nil.
	mouseFocus _ nil.
	position _ 0@0.
	extent _ CursorWithMask normal extent.
	xtent _ CursorWithMask normal extent.
	damageRecorder _ DamageRecorder new.
	self initForEvents.! !


!HandleMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 18:49'!
initialize
	"initialize the state of the receiver"
	super initialize.
	extent _ 12 @ 12.
	xtent _ 12@12! !


!InnerTextMorph methodsFor: 'accessing' stamp: 'jmv 8/21/2012 18:47'!
textColor: aColor

	ccolor = aColor ifTrue: [^ self].
	color _ aColor.
	ccolor _ aColor.
	self redrawNeeded! !


!LimitedHeightTextMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 18:50'!
initialize
	super initialize.
	extent _  200 @ 120.
	xtent _  200 @ 120! !


!MagnifierMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 18:50'!
initialize
	super initialize.
	trackPointer _ true.
	magnification _ 2.
	lastPos _ self sourcePoint.
	extent _ 128@128.
	xtent _ 128@128! !


!MenuLineMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 18:51'!
initialize
	super initialize.
	extent _ extent x @ 2.
	xtent _ xtent x @ 2! !


!MenuMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 18:49'!
initialize
	super initialize.
	position _ 0@0.
	extent _ 40@10.
	xtent _ 40@10.
	defaultTarget _ nil.
	selectedItem _ nil.
	stayUp _ false.
	popUpOwner _ nil! !


!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 8/21/2012 18:50'!
morphExtent: aPoint

	self flag: #jmvVer2.
	xtent = aPoint ifFalse: [
		self redrawNeeded.
		extent _ aPoint.
		xtent _ aPoint.
		self buildMagnifiedBackgroundImage.
		self someSubmorphPositionOrExtentChanged.
		self redrawNeeded ].

	worldState ifNotNil: [
		worldState viewBox ifNotNil: [
			worldState viewBox = self morphBoundsInWorld ifFalse: [
				worldState canvas: nil.
				worldState viewBox: self morphBoundsInWorld ]]]! !

!PasteUpMorph methodsFor: 'project state' stamp: 'jmv 8/21/2012 18:50'!
viewBox: newViewBox
	self flag: #jmvVer2.
	self isWorldMorph ifTrue: [
		(self viewBox isNil or: [ self viewBox extent ~= newViewBox extent ])
			ifTrue: [ worldState canvas: nil ].
		worldState viewBox: newViewBox ].
	position _ owner
		ifNil: [ newViewBox topLeft ]
		ifNotNil: [ owner internalizeFromWorld: newViewBox topLeft ].
	extent _ newViewBox extent.
	xtent _ newViewBox extent! !


!PluggableButtonMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 18:50'!
initialize
	"initialize the state of the receiver"
	super initialize.

	roundButtonStyle _ nil.	"nil: honor Theme. true: draw as round button. false: draw as classic 3d border square button"
	model _ nil.
	getStateSelector _ nil.
	actionSelector _ nil.
	isPressed _ false.
	mouseIsOver _ false.
	actWhen _ #buttonUp.
	extent _  20 @ 15.
	xtent _  20 @ 15! !


!SystemWindow methodsFor: 'initialization' stamp: 'jmv 8/21/2012 18:50'!
initialize
	"Initialize a system window. Add label, stripes, etc., if desired"

	super initialize.
	labelString ifNil: [ labelString _ 'Untitled Window'].
	isCollapsed _ false.
	
	self wantsLabel ifTrue: [self initializeLabelArea].
	extent _ 300 @ 200.
	xtent _ 300 @ 200.
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

!methodRemoval: HaloMorph #transferHalo:!
HaloMorph removeSelector: #transferHalo:!
!classDefinition: #RectangleLikeMorph category: #'Morphic-Kernel'!
Morph subclass: #RectangleLikeMorph
	instanceVariableNames: 'xtent ccolor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!RectangleLikeMorph reorganize!
('accessing' color:)
('initialization' defaultColor initialize)
('geometry' basicExtent:)
!