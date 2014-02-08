'From Cuis 4.2 of 25 July 2013 [latest update: #1947] on 2 February 2014 at 1:00:03.660312 am'!

!PluggableButtonMorph methodsFor: 'scrollbar button' stamp: 'jmv 2/2/2014 00:56'!
updateDownButtonImage
	"update the receiver's as a downButton.  put a new image inside"

	icon _ FormCanvas arrowOfDirection: #down size: ScrollBar scrollbarThickness.
	actionSelector _ #scrollDown.
	self
		roundButtonStyle: false;
		redrawNeeded! !

!PluggableButtonMorph methodsFor: 'scrollbar button' stamp: 'jmv 2/2/2014 00:56'!
updateLeftButtonImage
	"update the receiver's as a downButton.  put a new image inside"

	icon _ FormCanvas arrowOfDirection: #left size: ScrollBar scrollbarThickness.
	actionSelector _ #scrollUp.
	self
		roundButtonStyle: false;
		redrawNeeded! !

!PluggableButtonMorph methodsFor: 'scrollbar button' stamp: 'jmv 2/2/2014 00:56'!
updateRightButtonImage
	"update the receiver's as a downButton.  put a new image inside"

	icon _ FormCanvas arrowOfDirection: #right size: ScrollBar scrollbarThickness.
	actionSelector _ #scrollDown.
	self
		roundButtonStyle: false;
		redrawNeeded! !

!PluggableButtonMorph methodsFor: 'scrollbar button' stamp: 'jmv 2/2/2014 00:56'!
updateUpButtonImage
	"update the receiver's as a upButton. put a new image inside"

	icon _ FormCanvas arrowOfDirection: #up size: ScrollBar scrollbarThickness.
	actionSelector _ #scrollUp.
	self
		roundButtonStyle: false;
		redrawNeeded! !


!ScrollBar methodsFor: 'initialization' stamp: 'jmv 2/2/2014 00:54'!
initializeDownButton
	"initialize the receiver's downButton"

	| e |
	e _ self buttonExtent.
	downButton _ self buttonClass new.
	downButton model: self.
	self addMorph: downButton.
	downButton
		morphPosition: extent - borderWidth - e;
		morphExtent: e@e;
		actWhen: #buttonStillDown.		"to enable multiple action if held down".
	self isHorizontal
		ifTrue: [ downButton updateRightButtonImage ]
		ifFalse: [ downButton updateDownButtonImage ]! !

!ScrollBar methodsFor: 'initialization' stamp: 'jmv 2/2/2014 00:55'!
initializeUpButton
	"initialize the receiver's upButton"

	| e |
	e _ self buttonExtent.
	upButton _ self buttonClass new.
	upButton model: self.
	self addMorph: upButton.
	upButton
		morphPosition: borderWidth@borderWidth;
		morphExtent: e@e;
		actWhen: #buttonStillDown.		"to enable multiple action if held down"
	self isHorizontal
		ifTrue: [ upButton updateLeftButtonImage ]
		ifFalse: [ upButton updateUpButtonImage ].! !

