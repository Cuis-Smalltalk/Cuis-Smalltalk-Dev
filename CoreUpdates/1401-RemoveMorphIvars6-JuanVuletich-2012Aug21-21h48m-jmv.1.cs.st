'From Cuis 4.0 of 21 April 2012 [latest update: #1400] on 21 August 2012 at 9:51:59 pm'!

!RectangleLikeMorph methodsFor: 'accessing' stamp: 'jmv 8/21/2012 21:51'!
color: aColor
	"Set the receiver's color. "
	color = aColor ifFalse: [
		ccolor _ aColor.
		color _ aColor.
		self redrawNeeded ]! !

!RectangleLikeMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 21:49'!
initialize
	super initialize.
	xtent _ self defaultBounds extent.
	extent _ self defaultBounds extent.
	ccolor _ self defaultColor.
	color _ self defaultColor! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 8/21/2012 21:49'!
basicExtent: aPoint

	self flag: #jmvVer2.
	"Lo dejo un rato porque se llama dde initialize... De ultima, es un problema? Es cierto que no se la escala aun..."
	"
	self validateOwnerNotNil.
	"
	
	"ver senders. Es en owner's o en world's ???"
	extent = aPoint ifTrue: [^ self].
	self redrawNeeded.
	xtent _ aPoint.
	extent _ aPoint.
	self someSubmorphPositionOrExtentChanged.
	self redrawNeeded! !


!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 21:50'!
initialize

	super initialize.
	xtent _  271@121.
	extent _  271@121.
	responseUponCancel _ ''
	! !


!HandMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 21:51'!
initialize
	super initialize.
	self initForEvents.
	keyboardFocus _ nil.
	mouseFocus _ nil.
	position _ 0@0.
	xtent _ CursorWithMask normal extent.
	extent _ CursorWithMask normal extent.
	damageRecorder _ DamageRecorder new.
	self initForEvents.! !


!HandleMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 21:49'!
initialize
	"initialize the state of the receiver"
	super initialize.
	xtent _ 12@12.
	extent _ 12@12! !


!InnerTextMorph methodsFor: 'accessing' stamp: 'jmv 8/21/2012 21:51'!
textColor: aColor

	color = aColor ifTrue: [^ self].
	ccolor _ aColor.
	color _ aColor.
	self redrawNeeded! !


!LimitedHeightTextMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 21:50'!
initialize
	super initialize.
	xtent _  200 @ 120.
	extent _  200 @ 120! !


!MagnifierMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 21:50'!
initialize
	super initialize.
	trackPointer _ true.
	magnification _ 2.
	lastPos _ self sourcePoint.
	xtent _ 128@128.
	extent _ 128@128! !


!MenuLineMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 21:50'!
initialize
	super initialize.
	xtent _ xtent x @ 2.
	extent _ xtent x @ 2! !


!MenuMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 21:49'!
initialize
	super initialize.
	position _ 0@0.
	xtent _ 40@10.
	extent _ 40@10.
	defaultTarget _ nil.
	selectedItem _ nil.
	stayUp _ false.
	popUpOwner _ nil! !


!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 8/21/2012 21:50'!
morphExtent: aPoint

	self flag: #jmvVer2.
	extent = aPoint ifFalse: [
		self redrawNeeded.
		xtent _ aPoint.
		extent _ aPoint.
		self buildMagnifiedBackgroundImage.
		self someSubmorphPositionOrExtentChanged.
		self redrawNeeded ].

	worldState ifNotNil: [
		worldState viewBox ifNotNil: [
			worldState viewBox = self morphBoundsInWorld ifFalse: [
				worldState canvas: nil.
				worldState viewBox: self morphBoundsInWorld ]]]! !

!PasteUpMorph methodsFor: 'project state' stamp: 'jmv 8/21/2012 21:50'!
viewBox: newViewBox
	self flag: #jmvVer2.
	self isWorldMorph ifTrue: [
		(self viewBox isNil or: [ self viewBox extent ~= newViewBox extent ])
			ifTrue: [ worldState canvas: nil ].
		worldState viewBox: newViewBox ].
	position _ owner
		ifNil: [ newViewBox topLeft ]
		ifNotNil: [ owner internalizeFromWorld: newViewBox topLeft ].
	xtent _ newViewBox extent.
	extent _ newViewBox extent! !


!PluggableButtonMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 21:50'!
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
	xtent _  20 @ 15.
	extent _  20 @ 15! !


!SystemWindow methodsFor: 'initialization' stamp: 'jmv 8/21/2012 21:50'!
initialize
	"Initialize a system window. Add label, stripes, etc., if desired"

	super initialize.
	labelString ifNil: [ labelString _ 'Untitled Window'].
	isCollapsed _ false.
	
	self wantsLabel ifTrue: [self initializeLabelArea].
	xtent _ 300 @ 200.
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

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
RectangleLikeMorph allSubInstancesDo: [ :m |
	m instVarNamed: 'color' put: (m instVarNamed: 'ccolor').
	m instVarNamed: 'extent' put: (m instVarNamed: 'xtent') ]!

