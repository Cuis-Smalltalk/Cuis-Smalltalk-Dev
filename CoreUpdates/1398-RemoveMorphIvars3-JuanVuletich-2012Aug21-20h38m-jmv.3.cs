'From Cuis 4.0 of 21 April 2012 [latest update: #1396] on 21 August 2012 at 8:43:07 pm'!

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 8/21/2012 20:42'!
morphExtent: aPoint
"assume it is always in owner's coordinates!!"
	self flag: #jmvVer2.
	self basicExtent: aPoint! !


!Morph methodsFor: 'accessing' stamp: 'jmv 8/21/2012 20:40'!
color

	^ Color blue! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/21/2012 20:41'!
morphExtent
"assume it is always in owner's coordinates!!"
"Quizas eventualmente borrar este tambien? (no se usa mucho...)"
	self flag: #jmvVer2.
	^ 50 @ 40! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/21/2012 20:41'!
morphExtentInOwner
"assume it is always in owner's coordinates!!"
"Quizas eventualmente borrar este tambien? (no se usa mucho...)"
	self flag: #jmvVer2.
	^ 50 @ 40! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/21/2012 20:41'!
morphHeight

"Ensure everybody wants owner's coordinates!!"
	self flag: #jmvVer2.
	^ 40! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/21/2012 20:41'!
morphWidth

"Ensure everybody wants owner's coordinates!!"
	self flag: #jmvVer2.
	^ 50! !

!Morph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 20:42'!
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
	layoutNeeded _ false! !


!RectangleLikeMorph methodsFor: 'accessing' stamp: 'jmv 8/21/2012 20:39'!
color: aColor
	"Set the receiver's color. "
	ccolor = aColor ifFalse: [
		ccolor _ aColor.
		self redrawNeeded ]! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 8/21/2012 20:38'!
basicExtent: aPoint

	self flag: #jmvVer2.
	"Lo dejo un rato porque se llama dde initialize... De ultima, es un problema? Es cierto que no se la escala aun..."
	"
	self validateOwnerNotNil.
	"
	
	"ver senders. Es en owner's o en world's ???"
	xtent = aPoint ifTrue: [^ self].
	self redrawNeeded.
	xtent _ aPoint.
	self someSubmorphPositionOrExtentChanged.
	self redrawNeeded! !


!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 20:39'!
initialize

	super initialize.
	xtent _  271@121.
	responseUponCancel _ ''
	! !


!HandMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 20:39'!
initialize
	super initialize.
	self initForEvents.
	keyboardFocus _ nil.
	mouseFocus _ nil.
	position _ 0@0.
	xtent _ CursorWithMask normal extent.
	damageRecorder _ DamageRecorder new.
	self initForEvents.! !


!HandleMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 20:39'!
initialize
	"initialize the state of the receiver"
	super initialize.
	xtent _ 12@12! !


!InnerTextMorph methodsFor: 'accessing' stamp: 'jmv 8/21/2012 20:40'!
textColor: aColor

	ccolor = aColor ifTrue: [^ self].
	ccolor _ aColor.
	self redrawNeeded! !


!LimitedHeightTextMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 20:39'!
initialize
	super initialize.
	xtent _  200 @ 120! !


!MagnifierMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 20:39'!
initialize
	super initialize.
	trackPointer _ true.
	magnification _ 2.
	lastPos _ self sourcePoint.
	xtent _ 128@128! !


!MenuLineMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 20:39'!
initialize
	super initialize.
	xtent _ xtent x @ 2! !


!MenuMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 20:39'!
initialize
	super initialize.
	position _ 0@0.
	xtent _ 40@10.
	defaultTarget _ nil.
	selectedItem _ nil.
	stayUp _ false.
	popUpOwner _ nil! !


!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 8/21/2012 20:39'!
morphExtent: aPoint

	self flag: #jmvVer2.
	xtent = aPoint ifFalse: [
		self redrawNeeded.
		xtent _ aPoint.
		self buildMagnifiedBackgroundImage.
		self someSubmorphPositionOrExtentChanged.
		self redrawNeeded ].

	worldState ifNotNil: [
		worldState viewBox ifNotNil: [
			worldState viewBox = self morphBoundsInWorld ifFalse: [
				worldState canvas: nil.
				worldState viewBox: self morphBoundsInWorld ]]]! !

!PasteUpMorph methodsFor: 'project state' stamp: 'jmv 8/21/2012 20:39'!
viewBox: newViewBox
	self flag: #jmvVer2.
	self isWorldMorph ifTrue: [
		(self viewBox isNil or: [ self viewBox extent ~= newViewBox extent ])
			ifTrue: [ worldState canvas: nil ].
		worldState viewBox: newViewBox ].
	position _ owner
		ifNil: [ newViewBox topLeft ]
		ifNotNil: [ owner internalizeFromWorld: newViewBox topLeft ].
	xtent _ newViewBox extent! !


!PluggableButtonMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 20:39'!
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
	xtent _  20 @ 15! !


!SystemWindow methodsFor: 'initialization' stamp: 'jmv 8/21/2012 20:39'!
initialize
	"Initialize a system window. Add label, stripes, etc., if desired"

	super initialize.
	labelString ifNil: [ labelString _ 'Untitled Window'].
	isCollapsed _ false.
	
	self wantsLabel ifTrue: [self initializeLabelArea].
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

!methodRemoval: Morph #basicExtent:!
Morph removeSelector: #basicExtent:!
!methodRemoval: Morph #color:!
Morph removeSelector: #color:!
!methodRemoval: Morph #defaultColor!
Morph removeSelector: #defaultColor!
!methodRemoval: Morph #morphExtent:!
Morph removeSelector: #morphExtent:!