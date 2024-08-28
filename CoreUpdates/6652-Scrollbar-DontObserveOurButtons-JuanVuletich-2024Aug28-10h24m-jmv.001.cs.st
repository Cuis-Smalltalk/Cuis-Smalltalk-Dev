'From Cuis7.1 [latest update: #6646] on 28 August 2024 at 11:06:04 am'!

!ScrollBar methodsFor: 'initialization' stamp: 'jmv 8/28/2024 11:03:45'!
initializeDownButton
	"Initialize the receiver's downButton"

	| e |
	e := self buttonExtent.
	downButton := PluggableButtonMorph new.
	downButton actionTarget: self.
	downButton morphExtent: e@e.
	self addMorph: downButton position: extent - borderWidth - e.
	downButton
		actWhen: #buttonStillDown.		"to enable multiple action if held down".
	self isHorizontal
		ifTrue: [ self updateRightButton: downButton ]
		ifFalse: [ self updateDownButton: downButton ].
	downButton visible: Theme current scrollbarShowButtons! !

!ScrollBar methodsFor: 'initialization' stamp: 'jmv 8/28/2024 11:03:51'!
initializeSlider
	"initialize the receiver's slider"

	sliderShadow := BoxedMorph new noBorder.
	self addMorph: sliderShadow.
	sliderShadow hide.
		
	slider := DraggableButtonMorph new.
	slider actionTarget: self.
	slider grabSelector: #sliderGrabbedAt:.
	slider dragSelector: #scrollTo:.
	slider action: #sliderReleased.
	self addMorph: slider.

	self computeSlider! !

!ScrollBar methodsFor: 'initialization' stamp: 'jmv 8/28/2024 11:03:55'!
initializeUpButton
	"Initialize the receiver's upButton"

	| e |
	e := self buttonExtent.
	upButton := PluggableButtonMorph new.
	upButton actionTarget: self.
	upButton morphExtent: e@e.
	self addMorph: upButton position: borderWidth@borderWidth.
	upButton
		actWhen: #buttonStillDown.		"to enable multiple action if held down"
	self isHorizontal
		ifTrue: [ self updateLeftButton: upButton ]
		ifFalse: [ self updateUpButton: upButton ].
	upButton visible: Theme current scrollbarShowButtons! !

