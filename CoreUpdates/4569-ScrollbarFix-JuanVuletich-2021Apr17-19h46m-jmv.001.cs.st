'From Cuis 5.0 [latest update: #4568] on 17 April 2021 at 7:46:45 pm'!

!ScrollBar methodsFor: 'initialization' stamp: 'jmv 4/17/2021 19:44:08'!
initializeUpButton
	"initialize the receiver's upButton"

	| e |
	e _ self buttonExtent.
	upButton _ self buttonClass new.
	upButton model: self.
	upButton morphExtent: e@e.
	Theme current scrollbarShowButtons ifFalse: [^ self].
	self addMorph: upButton position: borderWidth@borderWidth.
	upButton
		actWhen: #buttonStillDown.		"to enable multiple action if held down"
	self isHorizontal
		ifTrue: [ self updateLeftButton: upButton ]
		ifFalse: [ self updateUpButton: upButton ].
! !

