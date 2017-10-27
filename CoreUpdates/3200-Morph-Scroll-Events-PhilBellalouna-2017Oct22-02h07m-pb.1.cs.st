'From Cuis 5.0 of 7 November 2016 [latest update: #3195] on 22 October 2017 at 4:02:34 pm'!
!classDefinition: #MouseScrollEvent category: #'Morphic-Events'!
MouseEvent subclass: #MouseScrollEvent
	instanceVariableNames: 'direction eventHandler'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Events'!

!MouseScrollEvent commentStamp: '<historical>' prior: 0!
A MouseScrollEvent can be any type of secondary pointer movement (typically via a scroll wheel on a traditional mouse or a gesture on a trackpad).  Currently, events are extracted from KeyboardEvents (which is how the VM currently communicates things like scroll wheel events via ctl+arrow up/down)!


!Morph methodsFor: 'events' stamp: 'pb 10/22/2017 02:55:55'!
mouseScroll: aMouseScrollEvent localPosition: localEventPosition
	"Handle a mouse scroll event.
	This message will only be sent to Morphs that answer true to #handlesMouseScroll:
	We can query aMouseScrollEvent to know about pressed mouse buttons."
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #mouseScroll:localPosition: 
		ifPresentDo: [ :handler | handler value: aMouseScrollEvent value: localEventPosition ]! !

!Morph methodsFor: 'event handling testing' stamp: 'pb 10/22/2017 02:53:48'!
handlesMouseScroll: aMouseScrollEvent
	^ self hasProperty: #'handlesMouseScroll:'! !

!Morph methodsFor: 'events-processing' stamp: 'pb 10/22/2017 15:49:56'!
processMouseScroll: aMouseEvent localPosition: localEventPosition
	((self handlesMouseScroll: aMouseEvent) and: [ aMouseEvent wasHandled not ]) ifTrue: [
		self
			mouseScroll: aMouseEvent
			localPosition: localEventPosition.
		aMouseEvent wasHandled: true ].! !

!Morph methodsFor: 'private' stamp: 'pb 10/22/2017 05:19:27'!
privateAnyOwnerHandlesMouseScroll: aMouseScrollEvent
	| foundHandler |
	foundHandler _ false.
	(self ownerChain allButFirst anySatisfy: [ :anOwner |
		anOwner isWorldMorph not and: [ anOwner handlesMouseScroll: aMouseScrollEvent ]]) ifTrue: [ foundHandler _ true ].
	^ foundHandler.! !


!PluggableScrollPane methodsFor: 'events' stamp: 'pb 10/22/2017 15:52:25'!
mouseScroll: aMouseEvent localPosition: localEventPosition
	aMouseEvent direction = #up
		ifTrue: [ scrollBar scrollUp: 1 ]
		ifFalse: [ scrollBar scrollDown: 1 ].! !

!PluggableScrollPane methodsFor: 'event handling testing' stamp: 'pb 10/22/2017 16:00:50'!
handlesMouseScroll: aMouseScrollEvent
	"Only accept if we can actually do something useful with the event (i.e. not scrolling up when already at the top or down when already at the bottom) or if my owner chain doesn't want it"
	| canUse |
	canUse _ (aMouseScrollEvent direction = #up and: [ scrollBar scrollValue > 0 ]) or: [
		aMouseScrollEvent direction = #down and: [ scrollBar scrollValue < 1 ]].
	"Even if I don't want it, one of my owners might. (i.e. nested scroll panes)  If my owners don't want it, accept the event to make sure that morphs behind me doesn't get the event."
	canUse ifFalse: [
		(self privateAnyOwnerHandlesMouseScroll: aMouseScrollEvent) ifFalse: [ canUse _ true ]].
	^ canUse.! !


!MouseEvent methodsFor: 'testing' stamp: 'pb 10/22/2017 02:19:31'!
isMouseScroll
	^ type == #mouseScroll! !


!MouseScrollEvent methodsFor: 'private' stamp: 'pb 10/22/2017 02:17:18'!
setType: evtType position: evtPos direction: evtDir buttons: evtButtons hand: evtHand stamp: stamp
	type _ evtType.
	position _ evtPos.
	buttons _ evtButtons.
	source _ evtHand.
	wasHandled _ false.
	direction _ evtDir.
	timeStamp _ stamp.! !

!MouseScrollEvent methodsFor: 'comparing' stamp: 'pb 10/22/2017 02:18:29'!
hash
	^ position hash + buttons hash + direction hash! !

!MouseScrollEvent methodsFor: 'dispatching' stamp: 'pb 10/22/2017 15:51:28'!
dispatchWith: aMorph localPosition: positionInAMorph
	"Find the appropriate receiver for the event and let it handle it. Default rules:
	* The top-most chain of visible, unlocked morphs containing the event position will get a chance to handle the event.
	* When travelling down the hierarchy a prospective handler for the event is installed. This prospective handler can be used by submorphs wishing to handle the mouse down for negotiating who the receiver is.
	* When travelling up, the prospective handler is always executed. The handler needs to check if the event was handled before as well as checking if somebody else's handler has been installed.
	* If another handler has been installed but the event was not handled it means that somebody up in the hierarchy wants to handle the event.
	"
	"Try to get out quickly"
	| aMorphHandlesIt handledByInner eventPositionInChild focus|
	focus := self hand keyboardFocus.
	"FIXME - this works in all tested cases but one: when the window directly under the mouse doesn't have keyboard focus (i.e. a Transcript window)"
	((aMorph fullContainsPoint: positionInAMorph) and: [(aMorph = focus) or: [focus notNil and: [aMorph notNil and: [focus hasOwner: aMorph]]]]) ifFalse: [ ^ #rejected ].
	"Install the prospective handler for the receiver"
	aMorphHandlesIt _ false.
	(aMorph handlesMouseScroll: self) ifTrue: [
		eventHandler _ aMorph.
		aMorphHandlesIt _ true ].
	"Now give submorphs a chance to handle the event"
	handledByInner _ false.
	aMorph submorphsDo: [ :eachChild |
		handledByInner ifFalse: [
			eventPositionInChild _ eachChild internalize: positionInAMorph.
			(eachChild
				dispatchEvent: self
				localPosition: eventPositionInChild) == #rejected ifFalse: [ "Some child did contain the point so aMorph is part of the top-most chain."
				handledByInner _ true ]]].
	(handledByInner or: [
		aMorph
			containsPoint: positionInAMorph
			event: self ]) ifTrue: [
		"aMorph is in the top-most unlocked, visible morph in the chain."
		aMorphHandlesIt ifTrue: [ ^ self
				sentTo: aMorph
				localPosition: positionInAMorph ]].
	handledByInner ifTrue: [ ^ self ].
	"Mouse was not on aMorph nor any of its children"
	^ #rejected.! !

!MouseScrollEvent methodsFor: 'dispatching' stamp: 'pb 10/22/2017 03:04:54'!
sentTo: aMorph localPosition: positionInAMorph
	"Dispatch the receiver into anObject"
	^ aMorph
		processMouseScroll: self
		localPosition: positionInAMorph.! !

!MouseScrollEvent methodsFor: 'accessing' stamp: 'pb 10/22/2017 03:15:50'!
direction
	^ direction ! !


!HandMorph methodsFor: 'events-processing' stamp: 'pb 10/22/2017 14:44:34'!
startMouseDispatch: aMouseEvent

	aMouseEvent isMouseOver ifTrue: [
		^self mouseFocus
			ifNotNil: [ mouseFocus handleFocusEvent: aMouseEvent ]
			ifNil: [ owner dispatchEvent: aMouseEvent localPosition: aMouseEvent eventPosition ]].

	"any mouse event but mouseOver"
	lastMouseEvent _ aMouseEvent.	
	lastMouseEventTime _ Time localMillisecondClock.

	"Check for pending drag or double click operations."
	mouseClickState ifNotNil: [
		(mouseClickState handleEvent: aMouseEvent from: self) ifTrue: [
			"Possibly dispatched #click: or something. Do not further process this event."
			^self mouseOverHandler processMouseOver: lastMouseEvent  ]].

	aMouseEvent isMove
		ifTrue: [
			self morphPosition: aMouseEvent eventPosition.
			self mouseFocus
				ifNotNil: [ mouseFocus handleFocusEvent: aMouseEvent ]
				ifNil: [ owner dispatchEvent: aMouseEvent localPosition: aMouseEvent eventPosition ]
		] ifFalse: [
		aMouseEvent isMouseScroll ifTrue: [
			owner dispatchEvent: aMouseEvent localPosition: aMouseEvent eventPosition] ifFalse: [
			"Issue a synthetic move event if we're not at the position of the event"
			aMouseEvent eventPosition = self morphPosition ifFalse: [
				"Issue a mouse move event to make the receiver appear at the given position"
				self startMouseDispatch: (MouseMoveEvent new
					setType: #mouseMove
					position: aMouseEvent eventPosition
					buttons: aMouseEvent buttons
					hand: self
					stamp: aMouseEvent timeStamp) ].
			"Drop submorphs on button events"
			self hasSubmorphs
				ifTrue: [
					"Not if we are grabbing them"
					mouseClickState ifNil: [self dropMorphs: aMouseEvent ]]
				ifFalse: [
					self mouseFocus
						ifNotNil: [ mouseFocus handleFocusEvent: aMouseEvent ]
						ifNil: [ owner dispatchEvent: aMouseEvent localPosition: aMouseEvent eventPosition ]]]].
		self mouseOverHandler processMouseOver: self lastMouseEvent! !

!HandMorph methodsFor: 'private events' stamp: 'pb 10/22/2017 14:48:02'!
generateKeyboardEvent: evtBuf
	"Generate the appropriate mouse event for the given raw event buffer"
	| buttons modifiers type keyValue pressType stamp |
	stamp _ evtBuf second.
	stamp = 0 ifTrue: [ stamp _ Time localMillisecondClock ].
	(evtBuf sixth <= 0 or: [ (keyValue _ Character iso8859s15CodeForUnicodeCodePoint: evtBuf sixth) isNil ]) ifTrue: [ keyValue _ Character macRomanToLatin1: evtBuf third ].
	Sensor peekEvent ifNotNil: [ :nxt |
		"start: Combining diacritical marks (i.e. accents in the Linux VM)"
		(nxt fourth = EventSensor eventKeyDown and: [ nxt third > 255 ]) ifTrue: [
			keyValue _ ((Character numericValue: keyValue) withDiacriticalMark: nxt third) iso8859s15Code.
			Sensor
				nextEvent;
				nextEvent;
				nextEvent ].
		"end: Combining diacritical marks (i.e. accents in the Linux VM)"
		"start: Spurious LF after CR on Ctrl-Enter on Windows VM"
		((evtBuf fourth = EventSensor eventKeyChar and: [ evtBuf third = 13 ]) and: [
			nxt fourth = EventSensor eventKeyChar and: [ nxt third = 10 ]]) ifTrue: [ Sensor nextEvent
			"print " ]].
	modifiers _ evtBuf fifth.
	pressType _ evtBuf fourth.
	pressType = EventSensor eventKeyDown ifTrue: [
		type _ #keyDown.
		lastKeyDownValue _ keyValue ].
	pressType = EventSensor eventKeyUp ifTrue: [ type _ #keyUp ].
	pressType = EventSensor eventKeyChar ifTrue: [
		type _ #keystroke.
		"If Control key pressed, and the VM answers a code below 27,
		 it means it did the translation, convert it back to regular character:
		We want to handle the meaning of ctrl ourselves."
		(modifiers anyMask: 2) ifTrue: [
			"Control key pressed"
			keyValue < 27 ifTrue: [
				"But we don't want to do it for Home/End/PgUp/PgDn, just for alphabetic keys"
				lastKeyDownValue = keyValue ifFalse: [
					"If equal, real Home/End/PgUp/PgDn in Windows => don't translate"
					(keyValue + 64 = lastKeyDownValue or: [ "If Equal, Ctrl-alphabetic in Windows => do translate"
						lastKeyDownValue < 47 ]) ifTrue: [
						"Not on windows. If less (not sure about the bound, but do not translate 48: tab on Mac), alphabetic on Mac => do translate"
						keyValue _ (modifiers anyMask: 1)
							ifTrue: [ keyValue + 64 ]
							ifFalse: [ keyValue + 96
								"shift not pressed: conver to lowercase letter" ]]]].
			"Act as if command/alt was pressed for some usual Windows ctrl-key combinations"
			(self shouldControlEmulateAltFor: keyValue) ifTrue: [ modifiers _ modifiers bitOr: 8 ]]].
	buttons _ modifiers bitShift: 3.
	(modifiers = 2 and: [
		keyValue = 30 or: [ keyValue = 31 ]])
		ifTrue: [ "We're hijacking ctl-up/down arrow since that is what the VM sends us for scroll events.  We're going to convert these to MouseScrollEvent and throw away the keyboard event"
			^ MouseScrollEvent new
				setType: #mouseScroll
				position: self morphPosition
				direction:
					(keyValue caseOf: {
						[ "Implementing as a caseof to allow for easy adding of left/right/other events should they become available"
						30 ] -> [ #up ].
						[ 31 ] -> [ #down ]})
				buttons: buttons
				hand: self
				stamp: stamp ]
		ifFalse: [ ^ KeyboardEvent new
				setType: type
				buttons: buttons
				position: self morphPosition
				keyValue: keyValue
				hand: self
				stamp: stamp ].! !

!PluggableScrollPane reorganize!
('access' addToScroller: adoptWidgetsColor: drawKeyboardFocusIndicator: verticalScrollBar)
('access options' hideScrollBarsIndefinitely)
('drawing' clipsLastSubmorph)
('events' doubleClickAndHalf:localPosition: keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseMove:localPosition: mouseScroll:localPosition: scrollByKeyboard: scroller)
('event handling testing' handlesMouseDown: handlesMouseOver: handlesMouseScroll:)
('geometry' borderWidth: focusIndicatorExtent focusIndicatorRectangle hLeftoverScrollRange hScrollBarWidth hSetScrollDelta hTotalScrollRange mightNeedHorizontalScrollBar minimumExtent privateExtent: scrollDeltaHeight scrollDeltaWidth scrollerOffset scrollerOffset: setScrollDeltas someSubmorphPositionOrExtentChanged updateScrollBarsBounds vLeftoverScrollRange vScrollBarHeight vSetScrollDelta vTotalScrollRange viewableExtent viewableHeight viewableWidth xtraBorder)
('geometry testing' hIsScrollbarShowing vIsScrollbarShowing)
('initialization' initialize innerMorphClass scrollBarClass)
('scroll bar events' mouseButton2Activity)
('scrolling' hHideScrollBar hIsScrollbarNeeded hScrollBarValue: hShowScrollBar hideOrShowScrollBars scrollBy: scrollSelectionIntoView scrollToShow: vHideScrollBar vIsScrollbarNeeded vScrollBarValue: vShowScrollBar)
('testing' is:)
('events-processing')
!


!MouseScrollEvent reorganize!
('private' setType:position:direction:buttons:hand:stamp:)
('comparing' hash)
('dispatching' dispatchWith:localPosition: sentTo:localPosition:)
('accessing' direction)
!

