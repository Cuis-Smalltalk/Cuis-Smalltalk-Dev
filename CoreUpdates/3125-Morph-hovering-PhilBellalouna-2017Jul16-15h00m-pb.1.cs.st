'From Cuis 5.0 of 7 November 2016 [latest update: #3121] on 16 July 2017 at 3:33:18 pm'!

!Morph methodsFor: 'events' stamp: 'pb 7/16/2017 15:06:53'!
mouseHover: aMouseMoveEvent localPosition: localEventPosition
	"Handle a mouse move event.
	This message will only be sent to Morphs that answer true to #handlesMouseHover for events that have not been previously handled.
	We can query aMouseMoveEvent to know about pressed mouse buttons."
	"Allow instances to dynamically use properties for handling common events."
	self
		valueOfProperty: #mouseHover:localPosition:
		ifPresentDo: [ :handler |
			handler
				value: aMouseMoveEvent
				value: localEventPosition ].! !

!Morph methodsFor: 'event handling testing' stamp: 'pb 7/16/2017 15:00:51'!
handlesMouseHover
	"Do I want to receive unhandled mouseMove events when the button is up and the hand is empty?  The default response is false."
	"Use a property test to allow individual instances to specify this."
	^ self hasProperty: #handlesMouseHover.! !


!Morph methodsFor: 'events-processing' stamp: 'pb 7/16/2017 15:31:38'!
processMouseOver: aMouseEvent localPosition: localEventPosition
	"System level event handling."
	 self hasMouseFocus ifTrue: [
		"Got this directly through #handleFocusEvent: so check explicitly"
		(self containsPoint: localEventPosition event: aMouseEvent) ifFalse: [
			^self ]].
	aMouseEvent hand noticeMouseOver: self event: aMouseEvent.
	"Open question: should any unhandled mouse move events be filtered out? (i.e. should mouseHover:localPosition: be called when a mouse button is pressed but the morph doesn't have mouse button handlers?  Essentially, what are the limits of what is considered 'hovering'?"
	(self handlesMouseHover and: [aMouseEvent wasHandled not]) ifTrue: [
		self
			mouseHover: aMouseEvent
			localPosition: localEventPosition ].! !

