'From Cuis7.5 [latest update: #7584] on 18 September 2025 at 12:25:08 pm'!

!MenuMorph methodsFor: 'events' stamp: 'jmv 9/18/2025 12:23:36'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse down event."

	"Grab the menu and drag it to some other place
	This is reimplemented here because we handle the event, and if the following lines are commented,
	a menu can't be grabbed with the hand. This is not nice and shouldn't be needed"
	self isSticky ifTrue: [ ^self ].
	aMouseButtonEvent hand grabMorph: self.! !

!MenuMorph methodsFor: 'events' stamp: 'jmv 9/18/2025 12:23:22'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse up event."

	stayUp ifFalse: [
		"Still in pop-up transition; keep focus"
		aMouseButtonEvent hand newMouseFocus: self ].! !

