'From Cuis 6.0 [latest update: #5822] on 29 May 2023 at 2:12:05 pm'!

!ScrollBar methodsFor: 'initialization' stamp: 'len 5/25/2023 21:55:51'!
initializeDownButton
	"Initialize the receiver's downButton"

	| e |
	e := self buttonExtent.
	downButton := PluggableButtonMorph new.
	downButton model: self.
	downButton morphExtent: e@e.
	self addMorph: downButton position: extent - borderWidth - e.
	downButton
		actWhen: #buttonStillDown.		"to enable multiple action if held down".
	self isHorizontal
		ifTrue: [ self updateRightButton: downButton ]
		ifFalse: [ self updateDownButton: downButton ].
	downButton visible: Theme current scrollbarShowButtons! !

!ScrollBar methodsFor: 'initialization' stamp: 'len 5/25/2023 21:55:28'!
initializeUpButton
	"Initialize the receiver's upButton"

	| e |
	e := self buttonExtent.
	upButton := PluggableButtonMorph new.
	upButton model: self.
	upButton morphExtent: e@e.
	self addMorph: upButton position: borderWidth@borderWidth.
	upButton
		actWhen: #buttonStillDown.		"to enable multiple action if held down"
	self isHorizontal
		ifTrue: [ self updateLeftButton: upButton ]
		ifFalse: [ self updateUpButton: upButton ].
	upButton visible: Theme current scrollbarShowButtons! !

