'From Cuis 4.0 of 21 April 2012 [latest update: #1400] on 21 August 2012 at 10:01:21 pm'!

!RectangleLikeMorph methodsFor: 'accessing' stamp: 'jmv 8/21/2012 21:59'!
color: aColor
	"Set the receiver's color. "
	color = aColor ifFalse: [
		color _ aColor.
		self redrawNeeded ]! !

!RectangleLikeMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 22:00'!
initialize
	super initialize.
	extent _ self defaultBounds extent.
	color _ self defaultColor! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 8/21/2012 21:58'!
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

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 8/21/2012 22:01'!
morphExtent
"assume it is always in owner's coordinates!!"
"Quizas eventualmente borrar este tambien? (no se usa mucho...)"
	self flag: #jmvVer2.
	^ extent! !


!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 21:59'!
initialize

	super initialize.
	extent _  271@121.
	responseUponCancel _ ''
	! !


!HandMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 21:59'!
initialize
	super initialize.
	self initForEvents.
	keyboardFocus _ nil.
	mouseFocus _ nil.
	position _ 0@0.
	extent _ CursorWithMask normal extent.
	damageRecorder _ DamageRecorder new.
	self initForEvents.! !


!HandleMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 21:59'!
initialize
	"initialize the state of the receiver"
	super initialize.
	extent _ 12@12! !


!InnerTextMorph methodsFor: 'accessing' stamp: 'jmv 8/21/2012 22:00'!
textColor: aColor

	color = aColor ifTrue: [^ self].
	color _ aColor.
	self redrawNeeded! !


!LimitedHeightTextMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 21:59'!
initialize
	super initialize.
	extent _  200 @ 120! !


!MagnifierMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 21:59'!
initialize
	super initialize.
	trackPointer _ true.
	magnification _ 2.
	lastPos _ self sourcePoint.
	extent _ 128@128! !


!MenuLineMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 21:58'!
initialize
	super initialize.
	extent _ extent x @ 2! !


!MenuMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 21:59'!
initialize
	super initialize.
	position _ 0@0.
	extent _ 40@10.
	defaultTarget _ nil.
	selectedItem _ nil.
	stayUp _ false.
	popUpOwner _ nil! !


!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 8/21/2012 21:59'!
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

!PasteUpMorph methodsFor: 'project state' stamp: 'jmv 8/21/2012 21:59'!
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


!PluggableButtonMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 21:59'!
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
	extent _  20 @ 15! !


!SystemWindow methodsFor: 'initialization' stamp: 'jmv 8/21/2012 21:59'!
initialize
	"Initialize a system window. Add label, stripes, etc., if desired"

	super initialize.
	labelString ifNil: [ labelString _ 'Untitled Window'].
	isCollapsed _ false.
	
	self wantsLabel ifTrue: [self initializeLabelArea].
	extent _ 300 @ 200.
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

