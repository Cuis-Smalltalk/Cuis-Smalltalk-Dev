'From Cuis7.7 [latest update: #7810] on 26 January 2026 at 12:57:22 pm'!

!PluggableScrollPane methodsFor: 'events' stamp: 'jmv 26/Jan/2026 12:43:02'!
keyStroke: aKeyboardEvent

	super keyStroke: aKeyboardEvent.
	aKeyboardEvent wasHandled ifTrue: [ ^self ].

	(scroller notNil and: [scroller isLocked not]) ifTrue: [
		scroller keyStroke: aKeyboardEvent ].! !

!PluggableScrollPane methodsFor: 'events' stamp: 'jmv 26/Jan/2026 12:23:38'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| eventPositionLocalToScroller |
	(scroller notNil and: [scroller isLocked not]) ifTrue: [
		eventPositionLocalToScroller := localEventPosition - scroller morphPosition.
		scroller mouseButton1Down: aMouseButtonEvent localPosition: eventPositionLocalToScroller ].
	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: nil
		dblClkSel: #doubleClick:localPosition:
		dblClkNHalfSel: #doubleClickAndHalf:localPosition:
		tripleClkSel: nil.! !

!PluggableScrollPane methodsFor: 'events' stamp: 'jmv 26/Jan/2026 12:37:47'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

	| eventPositionLocalToScroller |
	super mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition.
	(scroller notNil and: [scroller isLocked not]) ifTrue: [
		eventPositionLocalToScroller := localEventPosition - scroller morphPosition.
		scroller mouseButton1Up: aMouseButtonEvent localPosition: eventPositionLocalToScroller ].! !

!PluggableScrollPane methodsFor: 'events' stamp: 'jmv 26/Jan/2026 12:41:16'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition

	| eventPositionLocalToScroller |
	(scroller notNil and: [scroller isLocked not]) ifTrue: [
		eventPositionLocalToScroller := localEventPosition - scroller morphPosition.
		scroller mouseMove: aMouseMoveEvent localPosition: eventPositionLocalToScroller ].! !

