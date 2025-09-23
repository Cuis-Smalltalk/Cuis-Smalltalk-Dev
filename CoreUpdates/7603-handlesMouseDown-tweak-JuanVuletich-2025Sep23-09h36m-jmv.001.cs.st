'From Cuis7.5 [latest update: #7602] on 23 September 2025 at 9:39:50 am'!

!HaloHandleMorph methodsFor: 'event handling testing' stamp: 'jmv 9/23/2025 09:37:36'!
handlesMouseDown: aMouseButtonEvent
	"Do I want to receive mouseDown events (mouseDown:, mouseUp:)?"
	
	^ mouseDownSelector notNil | mouseUpSelector notNil! !


!ScrollBar methodsFor: 'event handling testing' stamp: 'jmv 9/23/2025 09:38:51'!
handlesMouseDown: aMouseButtonEvent
	"Do I want to receive mouseDown events (mouseDown:, mouseUp:)?"
	^true! !

