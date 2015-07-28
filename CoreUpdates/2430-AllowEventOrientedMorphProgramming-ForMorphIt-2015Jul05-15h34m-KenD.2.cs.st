'From Cuis 4.2 of 25 July 2013 [latest update: #2400] on 5 July 2015 at 3:40:40.107948 pm'!

!Morph methodsFor: 'events' stamp: 'KenD 5/13/2015 11:33'!
mouseStillDown: evt
	"Handle a mouse move event."
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #'mouseStillDown:' 
		ifPresentDo: [ :handler | handler value: evt ]! !


!Morph methodsFor: 'events' stamp: 'KenD 5/13/2015 11:32'!
click: aMouseButtonEvent localPosition: localEventPosition
	"Handle a single-click event. This message is only sent to clients that request it by sending one of the #waitForClicksOrDrag:... messages to the initiating hand in their mouseDown: method. This default implementation does nothing."
	
	self 
		valueOfProperty: #'click:localPosition:'
		ifPresentDo: [ :handler | handler value:  aMouseButtonEvent value: localEventPosition ]! !

!Morph methodsFor: 'events' stamp: 'KenD 5/13/2015 11:32'!
doubleClick: aMouseButtonEvent localPosition: localEventPosition
	"Handle a double-click event. This message is only sent to clients that request it by sending one of the #waitForClicksOrDrag:... messages to the initiating hand in their mouseDown: method. This default implementation does nothing."
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #'doubleClick:localPosition:'
		ifPresentDo: [ :handler | handler value:  aMouseButtonEvent value: localEventPosition ]! !

!Morph methodsFor: 'events' stamp: 'jmv 2/6/2015 23:06'!
dragEvent: aMouseEvent localPosition: aPoint

	aMouseEvent hand grabMorph: self! !

!Morph methodsFor: 'events' stamp: 'KenD 5/13/2015 11:32'!
keyDown: aMorphicEvent
	"Handle a key down event. The default response is to do nothing."
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #'keyDown:' 
		ifPresentDo: [ :handler | handler value: aMorphicEvent ]! !

!Morph methodsFor: 'events' stamp: 'KenD 5/13/2015 11:32'!
keyStroke: aKeyboardEvent
	"Handle a keystroke event."

	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
	(self closeWindowFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
		
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #'keyStroke:'
		ifPresentDo: [ :handler | handler value: aKeyboardEvent ]! !

!Morph methodsFor: 'events' stamp: 'KenD 5/13/2015 11:32'!
keyUp: anEvent
	"Handle a key up event. The default response is to do nothing."
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #'keyUp:'
		ifPresentDo: [ :handler | handler value: anEvent ]! !

!Morph methodsFor: 'events' stamp: 'KenD 5/13/2015 11:33'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse down event.
	This message will only be sent to Morphs that answer true to #handlesMouseDown:"
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #'mouseButton1Down:localPosition:' 
		ifPresentDo: [ :handler | handler value: aMouseButtonEvent value: localEventPosition ]! !

!Morph methodsFor: 'events' stamp: 'KenD 5/13/2015 11:33'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse button 1 up event.
	This message will only be sent to Morphs that answer true to #handlesMouseDown:"
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #'mouseButton1Up:localPosition:' 
		ifPresentDo: [ :handler | handler value: aMouseButtonEvent value: localEventPosition ]! !

!Morph methodsFor: 'events' stamp: 'jmv 1/17/2013 17:20'!
mouseButton2Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse button 2 down event.
	This message will only be sent to Morphs that answer true to #handlesMouseDown:"
	self mouseButton2Activity! !

!Morph methodsFor: 'events' stamp: 'jmv 1/17/2013 17:20'!
mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse button 2 up event.
	This message will only be sent to Morphs that answer true to #handlesMouseDown:"! !

!Morph methodsFor: 'events' stamp: 'jmv 1/14/2013 22:04'!
mouseButton3Down: aMouseButtonEvent localPosition: localEventPosition
	"Special gestures (cmd-mouse on the Macintosh; Alt-mouse on Windows and Unix) allow a mouse-sensitive morph to be moved or bring up a halo for the morph."
	| h doNotDrag |
	h _ aMouseButtonEvent hand halo.
	"Prevent wrap around halo transfers originating from throwing the event back in"
	doNotDrag _ false.
	h ifNotNil:[
		(h target == self) ifTrue: [ doNotDrag _ true].
		(h target hasOwner: self) ifTrue: [ doNotDrag _ true].
		(self hasOwner: h target) ifTrue: [ doNotDrag _ true]].

	"cmd-drag on flexed morphs works better this way"
	h _ self addHalo: aMouseButtonEvent.
	doNotDrag ifTrue: [ ^self ].
	"Initiate drag transition if requested"
	"good gesture. implement it"
	aMouseButtonEvent hand 
		waitForClicksOrDrag: h
		event: aMouseButtonEvent
		clkSel: nil
		dblClkSel: nil.
	"Pass focus explicitly here"
	aMouseButtonEvent hand newMouseFocus: h.! !

!Morph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:25'!
mouseButton3Up: aMouseButtonEvent localPosition: localEventPosition
	"Ignored. Theoretically we should never get here since control is transferred to the halo on #mouseButton3Down: but subclasses may implement this differently."! !

!Morph methodsFor: 'events' stamp: 'KenD 1/12/2014 15:11'!
mouseEnter: evt
	"Handle a mouseEnter event, meaning the mouse just entered my bounds with no button pressed."
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #mouseEnter: 
		ifPresentDo: [ :handler | handler value: evt ]! !

!Morph methodsFor: 'events' stamp: 'KenD 1/12/2014 15:11'!
mouseLeave: evt
	"Handle a mouseLeave event, meaning the mouse just left my bounds with no button pressed."
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #mouseLeave: 
		ifPresentDo: [ :handler | handler value: evt ]! !

!Morph methodsFor: 'events' stamp: 'KenD 1/12/2014 15:12'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Handle a mouse move event.
	This message will only be sent to Morphs that answer true to #handlesMouseDown:
	We can query aMouseMoveEvent to know about pressed mouse buttons."
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #mouseMove:localPosition: 
		ifPresentDo: [ :handler | handler value: aMouseMoveEvent value: localEventPosition ]! !

!Morph methodsFor: 'events' stamp: 'KenD 5/13/2015 11:33'!
mouseStillDown
	"Called from the stepping mechanism for morphs wanting continuously repeated 'yes the mouse is still down, yes it is still down, yes it has not changed yet, no the mouse is still not up, yes the button is down' etc messages"
	
	self "Allow instances to use this"
		valueOfProperty: #'mouseStillDown
'		ifPresentDo: [ :handler | handler value ]! !

!Morph methodsFor: 'events' stamp: 'KenD 5/13/2015 11:34'!
windowEvent: anEvent
	"Host window event"
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #'windowEvent:' 
		ifPresentDo: [ :handler | handler value: anEvent ]! !

!Morph methodsFor: 'event handling testing' stamp: 'KenD 5/13/2015 11:34'!
allowsMorphDrop
	"Answer whether we accept dropping morphs. By default answer false."

	"Use a property test to allow individual instances to specify this."
	^ self hasProperty: #'allowsMorphDrop'! !

!Morph methodsFor: 'event handling testing' stamp: 'KenD 5/13/2015 11:34'!
allowsSubmorphDrag
	"Answer whether our morphs can just be grabbed with the hand, instead of requiring the use of the halo. By default answer false."

	"Use a property test to allow individual instances to specify this."
	^ self hasProperty: #'allowsSubmorphDrag'! !

!Morph methodsFor: 'event handling testing' stamp: 'KenD 5/13/2015 11:34'!
handlesKeyboard
	"Return true if the receiver wishes to handle keyboard events"

	"Use a property test to allow individual instances to specify this."
	^ self hasProperty: #'handlesKeyboard'! !

!Morph methodsFor: 'event handling testing' stamp: 'KenD 5/13/2015 11:34'!
handlesMouseDown: aMouseButtonEvent
	"Do I want to receive mouseButton messages ?
	- #mouseButton1Down:localPosition:
	- #mouseButton1Up:localPosition:
	- #mouseButton2Down:localPosition:
	- #mouseButton2Up:localPosition:
	- #mouseButton3Down:localPosition:
	- #mouseButton3Up:localPosition:
	- #mouseMove:localPosition:
	- #mouseButton2Activity
	
	NOTE: The default response is false. Subclasses that implement these messages directly should override this one to return true.
	
	Implementors could query the argument, and only answer true for (for example) button 2 up only."

	"Use a property test to allow individual instances to dynamically specify this."
	^ self hasProperty: #'handlesMouseDown:'! !

!Morph methodsFor: 'event handling testing' stamp: 'KenD 5/13/2015 11:31'!
handlesMouseOver: aMorphicEvent
	"Do I want to receive mouseEnter: and mouseLeave: when the button is up and the hand is empty?  The default response is false." 

	"Use a property test to allow individual instances to specify this."
	^ self hasProperty: #'handlesMouseOver:'! !

!Morph methodsFor: 'event handling testing' stamp: 'KenD 5/13/2015 11:34'!
handlesMouseStillDown: evt
	"Return true if the receiver wants to get repeated #mouseStillDown: messages between #mouseDown: and #mouseUp"

	"Use a property test to allow individual instances to specify this."
	^ self hasProperty: #'handlesMouseStillDown:'! !

