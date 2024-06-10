'From Cuis7.1 [latest update: #6456] on 10 June 2024 at 9:19:02 am'!
Smalltalk renameClassNamed: #DraggeableButtonMorph as: #DraggableButtonMorph!

!ScrollBar methodsFor: 'initialization' stamp: 'jmv 6/10/2024 09:18:25'!
initializeSlider
	"initialize the receiver's slider"

	sliderShadow := BoxedMorph new noBorder.
	self addMorph: sliderShadow.
	sliderShadow hide.
		
	slider := DraggableButtonMorph new.
	slider model: self.
	slider grabSelector: #sliderGrabbedAt:.
	slider dragSelector: #scrollTo:.
	slider action: #sliderReleased.
	self addMorph: slider.

	self computeSlider! !


!ScrollBar reorganize!
('access' color: interval: scrollDelta:pageDelta: scrollValue)
('accessing' adoptWidgetsColor: model:setValueSelector:)
('drawing' drawOn:)
('events' mouseButton1Down:localPosition: mouseStillDown)
('event handling testing' handlesMouseDown: handlesMouseStillDown:)
('geometry' buttonExtent computeSlider expandSlider extentChanged: freeSliderRoom minimumExtent sliderSlack totalSliderRoom)
('initialization' initialize initializeDownButton initializeSlider initializeUpButton recreateSubmorphs updateDownButton: updateLeftButton: updateRightButton: updateUpButton:)
('model access' internalScrollValue: scrollValue:)
('scrolling' scrollByPage scrollDown scrollDown: scrollTo: scrollUp scrollUp: setNextDirectionFromEvent: sliderGrabbedAt: sliderReleased)
('testing' isHorizontal)
!

