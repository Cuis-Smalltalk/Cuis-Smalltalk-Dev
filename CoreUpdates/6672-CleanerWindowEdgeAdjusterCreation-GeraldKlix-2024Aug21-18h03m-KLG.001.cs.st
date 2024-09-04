'From Haver7.0 [latest update: #6454] on 21 August 2024 at 6:10:52 pm'!

!SystemWindow methodsFor: 'initialization' stamp: 'KLG 8/21/2024 18:08:30'!
initializeAdjusters

	| adjusterClass |
	adjusterClass := self windowEdgeAdjustingMorphClass.
	adjusters := Dictionary new.
	adjusters at: #topAdjuster put: adjusterClass forTop.
	adjusters at: #bottomAdjuster put: adjusterClass forBottom.
	adjusters at: #leftAdjuster put: adjusterClass forLeft.
	adjusters at: #rightAdjuster put: adjusterClass forRight.
	adjusters at: #topLeftAdjuster put: adjusterClass forTopLeft.
	adjusters at: #bottomLeftAdjuster put: adjusterClass forBottomLeft.
	adjusters at: #topRightAdjuster put: adjusterClass forTopRight.
	adjusters at: #bottomRightAdjuster put: adjusterClass forBottomRight.
	adjusters do: [ :m |
		self addMorphFront: m ]! !

!SystemWindow methodsFor: 'initialization' stamp: 'KLG 8/21/2024 18:09:10'!
windowEdgeAdjustingMorphClass
	"Answer the adjusting morph class."

	^ self class windowEdgeAdjustingMorphClass! !


!SystemWindow class methodsFor: 'instance creation' stamp: 'KLG 8/21/2024 18:09:16'!
windowEdgeAdjustingMorphClass
	"Answer the adjusting morph class."

	^ WindowEdgeAdjustingMorph! !


!SystemWindow methodsFor: 'initialization' stamp: 'KLG 8/21/2024 18:04:54'!
initialize
	"Initialize a system window. Add label, stripes, etc., if desired"

	super initialize.
	labelString ifNil: [ labelString := 'Untitled Window'].
	
	self initializeLabelArea.
	extent := `300 @ 200`.

	self initializeAdjusters.

	"by default"
	self beColumn! !

