'From Cuis7.5 [latest update: #7502] on 3 September 2025 at 8:42:46 pm'!

!Morph methodsFor: 'event handling testing' stamp: 'jmv 9/3/2025 20:40:16'!
handlesMouseOver: aMouseEvent
	"Do I want to receive mouseEnter: and mouseLeave: when the button is up and the hand is empty?  The default response is false." 

	"Use a property test to allow individual instances to specify this."
	^ self hasProperty: #'handlesMouseOver:'! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/3/2025 20:40:49'!
processMouseEnter: aMouseEnterEvent localPosition: localEventPosition
	"System level event handling."
	aMouseEnterEvent isDraggingEvent ifTrue: [
		^self].
	self wantsBalloon ifTrue: [
		aMouseEnterEvent hand triggerBalloonFor: self after: self balloonHelpDelayTime].
	(self handlesMouseOver: aMouseEnterEvent) ifTrue: [
		aMouseEnterEvent wasHandled: true.
		self mouseEnter: aMouseEnterEvent ]! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/3/2025 20:41:24'!
processMouseLeave: aMouseLeaveEvent localPosition: localEventPosition
	"System level event handling."
	aMouseLeaveEvent hand removePendingBalloonFor: self.
	aMouseLeaveEvent isDraggingEvent ifTrue: [
		^self].
	(self handlesMouseOver: aMouseLeaveEvent) ifTrue: [
		aMouseLeaveEvent wasHandled: true.
		self mouseLeave: aMouseLeaveEvent ]! !


!MenuItemMorph methodsFor: 'events-processing' stamp: 'jmv 9/3/2025 20:40:58'!
processMouseEnter: aMouseEnterEvent localPosition: localEventPosition
	"System level event handling."
	"Do #mouseEnter: even if button down (others, like LayoutAdjustingMorph need the default behavior)"
	self wantsBalloon ifTrue: [
		aMouseEnterEvent hand triggerBalloonFor: self after: self balloonHelpDelayTime].
	(self handlesMouseOver: aMouseEnterEvent) ifTrue:[
		aMouseEnterEvent wasHandled: true.
		self mouseEnter: aMouseEnterEvent ]! !

!MenuItemMorph methodsFor: 'events-processing' stamp: 'jmv 9/3/2025 20:41:33'!
processMouseLeave: aMouseLeaveEvent localPosition: localEventPosition
	"System level event handling."

	"Do #mouseLeave: even if button down (others, like LayoutAdjustingMorph need the default behavior)"
	aMouseLeaveEvent hand removePendingBalloonFor: self.
	(self handlesMouseOver: aMouseLeaveEvent) ifTrue: [
		aMouseLeaveEvent wasHandled: true.
		self mouseLeave: aMouseLeaveEvent ]! !

