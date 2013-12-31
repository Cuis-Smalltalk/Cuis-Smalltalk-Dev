'From Cuis 4.0 of 3 April 2012 [latest update: #4211] on 12 April 2012 at 10:41:12 am'!

!ScrollBar methodsFor: 'geometry' stamp: 'jmv 4/12/2012 10:38'!
freeSliderRoom
	"Answer the length or height of the free slider area, i.e. substract the slider itself"

	^ (self isHorizontal
		ifTrue: [ self morphExtent x - slider morphExtent x]
		ifFalse: [ self morphExtent y - slider morphExtent y])
			- (borderWidth * 2) - (self buttonExtent * 2).! !

!ScrollBar methodsFor: 'geometry' stamp: 'jmv 4/12/2012 10:38'!
totalSliderRoom
	"Answer the length or height of the slider area"

	^ (self isHorizontal
		ifTrue: [ self morphExtent x ]
		ifFalse: [ self morphExtent y ])
			- (borderWidth * 2) - (self buttonExtent * 2).! !

!ScrollBar methodsFor: 'testing' stamp: 'jmv 4/12/2012 09:51'!
isHorizontal
	| e |
	e _ self morphExtent.
	^e x > e y! !


!ScrollBar methodsFor: 'geometry' stamp: 'jmv 4/12/2012 10:27'!
computeSlider

	| delta |
	delta _ self buttonExtent + (self freeSliderRoom * value) asInteger.
	self isHorizontal
		ifTrue: [
			slider morphPositionInOwner: borderWidth +  delta @ borderWidth ]
		ifFalse: [
			slider morphPositionInOwner: borderWidth @ (borderWidth + delta) ] ! !

!ScrollBar methodsFor: 'geometry' stamp: 'jmv 4/12/2012 10:20'!
expandSlider
	"Compute the new size of the slider (use the old sliderThickness as a minimum)."

	| e |
	e _ (self totalSliderRoom * interval) asInteger max: 7.
	slider morphExtent: (self isHorizontal
		ifTrue: [ e @ self buttonExtent ]
		ifFalse: [ self buttonExtent @ e ])! !

!ScrollBar methodsFor: 'geometry' stamp: 'jmv 4/12/2012 09:52'!
morphExtent: newExtent

	| newExtentToUse |
	self validatePositionAndBounds.
	self validateExtentAndBounds.
	newExtent = extent ifTrue: [^ self].
	newExtentToUse _ self isHorizontal
		ifTrue: [ (newExtent x max: 14) @ newExtent y ]
		ifFalse: [ newExtent x @ (newExtent y max: 14) ].
	newExtentToUse = extent ifTrue: [^ self].
	super morphExtent: newExtentToUse.
		
	self flag: #jmv.
	"Most times it is not necessary to recreate the buttons"
	self recreateSubmorphs! !

!ScrollBar methodsFor: 'initialize' stamp: 'jmv 4/12/2012 09:57'!
initializeDownButton
	"initialize the receiver's downButton"

	| e |
	e _ self buttonExtent.
	downButton _ self buttonClass new.
	downButton model: self.
	self addMorph: downButton.
	downButton
		morphPositionInOwner: self morphExtent - borderWidth - e;
		morphExtent: e.
	self isHorizontal
		ifTrue: [ downButton updateRightButtonImage ]
		ifFalse: [ downButton updateDownButtonImage ]! !

!ScrollBar methodsFor: 'initialize' stamp: 'jmv 4/12/2012 10:02'!
initializeSlider
	"initialize the receiver's slider"

	sliderShadow _ RectangleMorph new.
	sliderShadow borderWidth: 0.
	self addMorph: sliderShadow.
	sliderShadow hide.
		
	slider _ self sliderClass new.
	slider model: self.
	slider grabSelector: #sliderGrabbed.
	slider dragSelector: #scrollAbsolute:.
	slider action: #sliderReleased.
	self addMorph: slider.

	self computeSlider! !

!ScrollBar methodsFor: 'initialize' stamp: 'jmv 4/12/2012 09:56'!
initializeUpButton
	"initialize the receiver's upButton"

	upButton _ self buttonClass new.
	upButton model: self.
	self addMorph: upButton.
	upButton
		morphPositionInOwner: borderWidth@borderWidth;
		morphExtent: self buttonExtent.
	self isHorizontal
		ifTrue: [ upButton updateLeftButtonImage ]
		ifFalse: [ upButton updateUpButtonImage ].! !

!ScrollBar methodsFor: 'scrolling' stamp: 'jmv 4/12/2012 10:38'!
scrollAbsolute: aPoint
	| relativePoint v |
	relativePoint _ aPoint - bounds topLeft.
	v _ (self isHorizontal
		ifTrue: [ relativePoint x ]
		ifFalse: [ relativePoint y ])
			- borderWidth - self buttonExtent * 1.0
				/ self freeSliderRoom.
	self setValue: v! !

!ScrollBar methodsFor: 'scrolling' stamp: 'jmv 4/12/2012 09:53'!
setNextDirectionFromEvent: event

	nextPageDirection _ self isHorizontal
		ifTrue: [ event eventPosition x >= slider referencePosition x ]
		ifFalse: [ event eventPosition y >= slider referencePosition y ]! !

!ScrollBar methodsFor: 'scrolling' stamp: 'jmv 4/12/2012 09:30'!
sliderGrabbed

	| delta e |
	sliderShadow
		morphPositionInOwner: slider morphPositionInOwner;
		morphExtent: slider morphExtent;
		show! !

!methodRemoval: ScrollBar #freeSliderSpace!
ScrollBar removeSelector: #freeSliderSpace!
!methodRemoval: ScrollBar #roomToMove!
ScrollBar removeSelector: #roomToMove!
!methodRemoval: ScrollBar #sliderExtent!
ScrollBar removeSelector: #sliderExtent!
!methodRemoval: ScrollBar #totalSliderArea!
ScrollBar removeSelector: #totalSliderArea!
!methodRemoval: ScrollBar #totalSliderHeight!
ScrollBar removeSelector: #totalSliderHeight!
!methodRemoval: ScrollBar #totalSliderSpace!
ScrollBar removeSelector: #totalSliderSpace!
!methodRemoval: ScrollBar #totalSliderWidth!
ScrollBar removeSelector: #totalSliderWidth!

!ScrollBar reorganize!
('access' color: interval: scrollDelta:pageDelta: value)
('accessing' adoptWidgetsColor: model:setValueSelector:)
('geometry' buttonExtent computeSlider expandSlider freeSliderRoom morphExtent: totalSliderRoom)
('initialize' initialize initializeDownButton initializeSlider initializeUpButton recreateSubmorphs)
('model access' setValue: value:)
('scrolling' scrollAbsolute: scrollByPage scrollDown scrollDown: scrollUp scrollUp: setNextDirectionFromEvent: sliderGrabbed sliderReleased)
('initialization' defaultBounds defaultColor)
('drawing' drawOn:)
('event handling' handlesMouseDown: handlesMouseStillDown: mouseDown: mouseStillDown:)
('testing' isHorizontal)
!

