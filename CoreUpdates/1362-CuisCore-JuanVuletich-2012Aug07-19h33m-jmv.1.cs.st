'From Cuis 4.0 of 21 April 2012 [latest update: #1361] on 7 August 2012 at 7:40:03 pm'!

!Morph methodsFor: 'initialization' stamp: 'jmv 8/7/2012 19:39'!
defaultBounds
"answer the default bounds for the receiver"

	"Maybe shouldn't exist"
	self flag: #jmvVer2.
	"Anyway, many morphs redefine #initialize asigning an extent... Unify in some style.
	Maybe #defaultExtent?"
	^ 0 @ 0 corner: 50 @ 40! !


!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 8/7/2012 19:35'!
initialize

	super initialize.
	extent _  271@121.
	responseUponCancel := ''
	! !


!HandleMorph methodsFor: 'initialization' stamp: 'jmv 8/7/2012 19:33'!
initialize
	"initialize the state of the receiver"
	super initialize.
	extent _ 12 @ 12! !


!LimitedHeightTextMorph methodsFor: 'initialization' stamp: 'jmv 8/7/2012 19:35'!
initialize
	super initialize.
	extent _  200 @ 120.! !


!MagnifierMorph methodsFor: 'initialization' stamp: 'jmv 8/7/2012 19:34'!
initialize
	super initialize.
	trackPointer _ true.
	magnification _ 2.
	lastPos _ self sourcePoint.
	extent _ 128@128! !


!MenuLineMorph methodsFor: 'initialization' stamp: 'jmv 8/7/2012 19:36'!
initialize
	super initialize.
	extent _ extent x @ 2! !


!PluggableButtonMorph methodsFor: 'initialization' stamp: 'jmv 8/7/2012 19:34'!
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


!SystemWindow methodsFor: 'initialization' stamp: 'jmv 8/7/2012 19:37'!
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

