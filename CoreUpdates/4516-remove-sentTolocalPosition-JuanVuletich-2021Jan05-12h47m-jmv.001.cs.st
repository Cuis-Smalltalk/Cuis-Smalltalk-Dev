'From Cuis 5.0 [latest update: #4515] on 5 January 2021 at 12:53:31 pm'!

!Morph methodsFor: 'events-processing' stamp: 'jmv 1/5/2021 11:51:09'!
processDropFiles: aDropFilesEvent
	"Handle a dropping file."

	aDropFilesEvent wasHandled ifTrue: [ ^self ]. 
	
	aDropFilesEvent wasHandled: true.
	self dropFiles: aDropFilesEvent! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 1/5/2021 11:50:19'!
processDropMorph: aDropEvent
	"Handle a dropping morph."
	| aMorph |
	
	aDropEvent wasHandled ifTrue: [ ^self ]. "Do it just once, for one drop destination"
	
	aMorph _ aDropEvent contents.
	aDropEvent wasHandled: true.
	self acceptDroppingMorph: aMorph event: aDropEvent.
	aMorph justDroppedInto: self event: aDropEvent! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 1/5/2021 11:52:40'!
processKeyDown: aKeyboardEvent
	"System level event handling."

	aKeyboardEvent wasHandled ifTrue: [^self].
	self handlesKeyboard ifFalse: [^self].
	aKeyboardEvent wasHandled: true.
	^self keyDown: aKeyboardEvent! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 1/5/2021 11:52:57'!
processKeyUp: aKeyboardEvent
	"System level event handling."

	aKeyboardEvent wasHandled ifTrue: [^self].
	self handlesKeyboard ifFalse: [^self].
	aKeyboardEvent wasHandled: true.
	^self keyUp: aKeyboardEvent! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 1/5/2021 11:51:54'!
processKeystroke: aKeyboardEvent
	"System level event handling."

	aKeyboardEvent wasHandled ifTrue: [^self].
	self handlesKeyboard ifFalse: [^self].
	aKeyboardEvent wasHandled: true.
	^self keyStroke: aKeyboardEvent! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 1/5/2021 11:53:33'!
processUnknownEvent: aMorphicEvent
	"An event of an unknown type was sent to the receiver. What shall we do?!!"

	Smalltalk beep. 
	aMorphicEvent printString displayAt: `0@0`.
	aMorphicEvent wasHandled: true! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 1/5/2021 12:03:21'!
processWindowEvent: aWindowEvent
	"Handle an event concerning our host window"

	aWindowEvent wasHandled ifTrue: [^self]. "not interested"
	(self wantsWindowEvent: aWindowEvent) ifFalse: [^self].
	aWindowEvent wasHandled: true.
	self windowEvent: aWindowEvent.
! !


!InnerTextMorph methodsFor: 'events-processing' stamp: 'jmv 1/5/2021 11:51:44'!
processKeystroke: aKeyboardEvent
	"System level event handling."

	aKeyboardEvent wasHandled ifTrue:[^self].
	self handlesKeyboard ifFalse: [^ self].
	aKeyboardEvent wasHandled: true.
	self keyStroke: aKeyboardEvent! !


!MorphicEvent methodsFor: 'dispatching' stamp: 'jmv 1/5/2021 11:53:47'!
sendEventTo: aMorph
	"Dispatch the receiver into aMorph"

	^ aMorph processUnknownEvent: self! !


!DropEvent methodsFor: 'dispatching' stamp: 'jmv 1/5/2021 11:50:38'!
sendEventTo: aMorph
	"Dispatch the receiver into aMorph"

	^aMorph processDropMorph: self! !


!DropFilesEvent methodsFor: 'dispatching' stamp: 'jmv 1/5/2021 11:51:15'!
sendEventTo: aMorph
	"Dispatch the receiver into aMorph"

	^aMorph processDropFiles: self! !


!KeyboardEvent methodsFor: 'dispatching' stamp: 'jmv 1/5/2021 11:53:19'!
sendEventTo: aMorph
	"Dispatch the receiver into anObject"
	type == #keystroke ifTrue: [
		self isFindClassShortcut
			ifTrue: [ ^ BrowserWindow findClass].
		self isCloseWindowShortcut
			ifTrue: [ ^ self closeCurrentWindowOf: aMorph ].
		^ aMorph processKeystroke: self ].
	type == #keyDown ifTrue: [
		^ aMorph processKeyDown: self ].
	type == #keyUp ifTrue: [ 
		^ aMorph processKeyUp: self ].
	^ super sendEventTo: aMorph.! !


!MouseEvent methodsFor: 'dispatching' stamp: 'jmv 1/5/2021 12:02:08'!
sendEventTo: aMorph
	"Dispatch the receiver into aMorph"

	type == #mouseOver ifTrue: [
		^aMorph processMouseOver: self localPosition: (aMorph internalizeFromWorld: position) ].
	type == #mouseEnter ifTrue: [
		^ aMorph processMouseEnter: self localPosition: (aMorph internalizeFromWorld: position) ].
	type == #mouseLeave ifTrue: [
		^aMorph processMouseLeave: self localPosition: (aMorph internalizeFromWorld: position) ].
	^ super sendEventTo: aMorph! !


!MouseButtonEvent methodsFor: 'dispatching' stamp: 'jmv 1/5/2021 12:00:01'!
sendEventTo: aMorph
	"Dispatch the receiver into anObject"

	type == #mouseDown ifTrue: [
		^aMorph processMouseDown: self localPosition: (aMorph internalizeFromWorld: position) ].
	type == #mouseUp ifTrue: [
		^aMorph processMouseUp: self localPosition: (aMorph internalizeFromWorld: position) ].
	^super sendEventTo: aMorph! !


!MouseMoveEvent methodsFor: 'dispatching' stamp: 'jmv 1/5/2021 12:02:43'!
sendEventTo: aMorph
	"Dispatch the receiver into anObject"

	type == #mouseMove ifTrue: [
		^aMorph processMouseMove: self localPosition: (aMorph internalizeFromWorld: position) ].
	^ super sendEventTo: aMorph! !


!MouseScrollEvent methodsFor: 'dispatching' stamp: 'jmv 1/5/2021 12:03:02'!
sendEventTo: aMorph
	"Dispatch the receiver into anObject"
	^ aMorph
		processMouseScroll: self
		localPosition: (aMorph internalizeFromWorld: position).! !


!WindowEvent methodsFor: 'dispatching' stamp: 'jmv 1/5/2021 12:03:28'!
sendEventTo: aMorph
	"Dispatch the receiver into anObject"

	^ aMorph processWindowEvent: self! !


!Morph methodsFor: 'events-processing' stamp: 'jmv 1/5/2021 12:04:44'!
handleFocusEvent: aMorphicEvent
	"Handle the given event. This message is sent if the receiver currently has the focus and is therefore receiving events directly from some hand."

	^aMorphicEvent sendEventTo: self! !


!MenuMorph methodsFor: 'events-processing' stamp: 'jmv 1/5/2021 12:04:29'!
handleFocusEvent: aMorphicEvent
	"Handle focus events. Valid menu transitions are determined based on the menu currently holding the focus after the mouse went down on one of its children."
	| eventPositionInOurCoordinates |
	eventPositionInOurCoordinates _ self internalizeFromWorld: aMorphicEvent eventPosition.

	self dispatchEvent: aMorphicEvent localPosition: eventPositionInOurCoordinates.

	"Need to handle keyboard input if we have the focus."
	aMorphicEvent isKeyboard ifTrue: [ ^ aMorphicEvent sendEventTo: self ].

	"We need to handle button clicks outside and transitions to local popUps so throw away everything else"
	(aMorphicEvent isMouseOver or: [aMorphicEvent isMouse not]) ifTrue: [ ^self ].
	"What remains are mouse buttons and moves"
	aMorphicEvent isMove ifFalse: [ ^ aMorphicEvent sendEventTo: self ]. "handle clicks outside by regular means"
	"Now it's getting tricky. On #mouseMove we might transfer control to *either* the currently active submenu or the pop up owner, if any. Since the active sub menu is always displayed upfront check it first."
	selectedItem ifNotNil:[(selectedItem activateSubmenu: aMorphicEvent) ifTrue: [^self]].
	"Note: The following does not traverse upwards but it's the best I can do for now"
	popUpOwner ifNotNil:[(popUpOwner activateOwnerMenu: aMorphicEvent) ifTrue: [^self]].! !


!MorphicEvent methodsFor: 'dispatching' stamp: 'jmv 1/5/2021 12:04:55'!
dispatchWith: aMorph localPosition: positionInAMorph
	"Dispatch me. The event will be passed to the front-most visible submorph that contains the position wrt. to the event."
	| handledByInner eventPositionInChild |

	"Try to get out quickly"
	(aMorph fullContainsGlobalPoint: self eventPosition)
		ifFalse: [ ^#rejected ].

	"Now give submorphs a chance to handle the event"
	handledByInner _ false.
	aMorph submorphsDo: [ :eachChild |
		handledByInner ifFalse: [
			eventPositionInChild _ eachChild internalize: positionInAMorph.
			(eachChild dispatchEvent: self localPosition: eventPositionInChild) == #rejected ifFalse: [
				"Some child did contain the point so aMorph is part of the top-most chain."
				handledByInner _ true ]]].

	"Check for being inside the receiver"
	(handledByInner or: [ (aMorph rejectsEvent: self) not and: [aMorph fullContainsGlobalPoint: self eventPosition] ])
		ifTrue: [ ^ self sendEventTo: aMorph ].

	^ #rejected! !


!DropEvent methodsFor: 'dispatching' stamp: 'jmv 1/5/2021 12:03:56'!
dispatchWith: aMorph localPosition: positionInAMorph
	"Drop is done on the innermost target that accepts it."
	| eventPositionInChild dropped |

	"Try to get out quickly"
	(aMorph fullContainsGlobalPoint: position)
		ifFalse: [ ^#rejected ].

	"Go looking if any of our submorphs wants it"
	aMorph submorphsDo: [ :eachChild |
		eventPositionInChild _ eachChild internalize: positionInAMorph.
		(eachChild dispatchEvent: self localPosition: eventPositionInChild) == #rejected ifFalse: [
			^self ]].

	(aMorph allowsMorphDrop and: [ (aMorph rejectsEvent: self) not and: [aMorph fullContainsGlobalPoint: position] ])
		ifTrue: [
			"Do a symmetric check if both morphs like each other"
			dropped _ self contents.
			((aMorph wantsDroppedMorph: dropped event: self)	"I want her"
				and: [dropped wantsToBeDroppedInto: aMorph])		"she wants me"
					ifTrue: [
						^ self sendEventTo: aMorph ]].
	^#rejected! !


!DropFilesEvent methodsFor: 'dispatching' stamp: 'jmv 1/5/2021 12:04:05'!
dispatchWith: aMorph localPosition: positionInAMorph
	"Drop is done on the innermost target that accepts it."

	| eventPositionInChild |

	"Try to get out quickly"
	(aMorph fullContainsGlobalPoint: position) ifFalse: [ ^#rejected ].

	"Go looking if any of our submorphs wants it"
	aMorph submorphsDo: [ :eachChild |
		eventPositionInChild _ eachChild internalize: positionInAMorph.
		(eachChild dispatchEvent: self localPosition: eventPositionInChild) == #rejected ifFalse: [ ^self ]].

	(aMorph allowsFilesDrop and: [ (aMorph rejectsEvent: self) not and: [aMorph fullContainsGlobalPoint: position] ])
		ifTrue: [^ self sendEventTo: aMorph ].
	
	^#rejected! !


!MouseButtonEvent methodsFor: 'dispatching' stamp: 'jmv 1/5/2021 12:05:04'!
dispatchWith: aMorph localPosition: positionInAMorph
	"Find the appropriate receiver for the event and let it handle it. Default rules:
	* The top-most chain of visible, unlocked morphs containing the event position will get a chance to handle the event.
	* When travelling down the hierarchy a prospective handler for the event is installed. This prospective handler can be used by submorphs wishing to handle the mouse down for negotiating who the receiver is.
	* When travelling up, the prospective handler is always executed. The handler needs to check if the event was handled before as well as checking if somebody else's handler has been installed.
	* If another handler has been installed but the event was not handled it means that somebody up in the hierarchy wants to handle the event.
	"
	| aMorphHandlesIt grabAMorph handledByInner eventPositionInChild |
	"Only for MouseDown"
	self isMouseDown ifFalse: [
		^super dispatchWith: aMorph localPosition: positionInAMorph ].

	"Try to get out quickly"
	(aMorph fullContainsGlobalPoint: position)
		ifFalse: [ ^#rejected ].

	"Install the prospective handler for the receiver"
	aMorphHandlesIt _ false.
	grabAMorph _ false.
	self mouseButton3Pressed
		ifTrue: [
			(eventHandler isNil or: [ eventHandler isWorldMorph or: [
					self shiftPressed or: [ aMorph is: #HaloMorph ]]])
				ifTrue: [
					eventHandler _ aMorph.
					aMorphHandlesIt _ true ]]
		ifFalse: [
			(aMorph handlesMouseDown: self) ifTrue: [
				eventHandler _ aMorph.
				aMorphHandlesIt _ true ].
			"If button 1, and both aMorph and the owner allows grabbing with the hand (to initiate drag & drop), so be it."
			self mouseButton1Pressed ifTrue: [
				aMorph owner ifNotNil: [ :o |
					(o allowsSubmorphDrag and: [ aMorph isSticky not ]) ifTrue: [
						grabAMorph _ true ]]]].

	"Now give submorphs a chance to handle the event"
	handledByInner _ false.
	aMorph submorphsDo: [ :eachChild |
		handledByInner ifFalse: [
			eventPositionInChild _ eachChild internalize: positionInAMorph.
			(eachChild dispatchEvent: self localPosition: eventPositionInChild) == #rejected ifFalse: [
				"Some child did contain the point so aMorph is part of the top-most chain."
				handledByInner _ true ]]].

	(handledByInner or: [ (aMorph rejectsEvent: self) not and: [aMorph fullContainsGlobalPoint: position] ]) ifTrue: [
		"aMorph is in the top-most unlocked, visible morph in the chain."
		aMorphHandlesIt
			ifTrue: [ ^self sendEventTo: aMorph ]
			ifFalse: [
				(grabAMorph and: [ handledByInner not ]) ifTrue: [
					self hand
						waitForClicksOrDrag: aMorph event: self
						dragSel: (Preferences clickGrabsMorphs ifFalse: [#dragEvent:localPosition:])
						clkSel: (Preferences clickGrabsMorphs ifTrue: [#dragEvent:localPosition:]).
					"false ifTrue: [ self hand grabMorph: aMorph ]."
					Preferences clickGrabsMorphs ifFalse: [aMorph activateWindow].
					self wasHandled: true.
					^self ]]].

	handledByInner ifTrue: [ ^self ].
	"Mouse was not on aMorph nor any of its children"
	^ #rejected! !


!MouseScrollEvent methodsFor: 'dispatching' stamp: 'jmv 1/5/2021 12:06:25'!
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
	((aMorph fullContainsGlobalPoint: position) and: [(aMorph = focus) or: [focus notNil and: [aMorph notNil and: [focus hasOwner: aMorph]]]]) ifFalse: [ ^ #rejected ].
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
		 (aMorph rejectsEvent: self) not and: [aMorph fullContainsGlobalPoint: position]]) ifTrue: [
		"aMorph is in the top-most unlocked, visible morph in the chain."
		aMorphHandlesIt ifTrue: [ ^ self sendEventTo: aMorph ]].
	handledByInner ifTrue: [ ^ self ].
	"Mouse was not on aMorph nor any of its children"
	^ #rejected.! !


!WindowEvent methodsFor: 'dispatching' stamp: 'jmv 1/5/2021 12:06:35'!
dispatchWith: aMorph localPosition: positionInAMorph
	"Host window events do not have a position and are only dispatched to the World"

	aMorph isWorldMorph ifFalse: [ ^#rejected ].
	self wasHandled ifTrue: [ ^self ].
	^ self sendEventTo: aMorph! !


!MouseOverHandler methodsFor: 'event handling' stamp: 'jmv 1/5/2021 12:06:07'!
processMouseOver: aMouseEvent 
	"Re-establish the z-order for all morphs wrt the given event"

	| hand focus evt |
	hand := aMouseEvent hand.
	leftMorphs := mouseOverMorphs asIdentitySet.
	"Assume some coherence for the number of objects in over list"
	overMorphs := WriteStream on: (Array new: leftMorphs size).
	enteredMorphs := WriteStream on: #().
	"Now go looking for eventual mouse overs"
	hand startEventDispatch: aMouseEvent asMouseOver.
	"Get out early if there's no change"
	(leftMorphs isNil or: [			"Should never happen, but it could if you halt during layout."
		(leftMorphs isEmpty and: [enteredMorphs position = 0])])
		ifTrue: [^leftMorphs := enteredMorphs := overMorphs := nil].
	focus := hand mouseFocus.
	"Send #mouseLeave as appropriate"
	evt := aMouseEvent asMouseLeave.
	"Keep the order of the left morphs by recreating it from the mouseOverMorphs"
	leftMorphs size > 1 
		ifTrue: [leftMorphs := mouseOverMorphs select: [:m | leftMorphs includes: m]].
	leftMorphs do: [ :m | 
		(m == focus or: [m hasOwner: focus]) 
			ifTrue: [
				evt sendEventTo: m ]
			ifFalse: [overMorphs nextPut: m]].
	"Send #mouseEnter as appropriate"
	evt := aMouseEvent asMouseEnter.
	enteredMorphs ifNil: [
		"inform: was called in handleEvent:"
		^ leftMorphs := enteredMorphs := overMorphs := nil].
	enteredMorphs := enteredMorphs contents.
	enteredMorphs reverseDo: [ :m | 
		(m == focus or: [m hasOwner: focus]) 
			ifTrue: [
				evt sendEventTo: m ]].
	"And remember the over list"
	overMorphs ifNil: [
			"inform: was called in handleEvent:"
			^leftMorphs := enteredMorphs := overMorphs := nil].
	mouseOverMorphs := overMorphs contents.
	leftMorphs := enteredMorphs := overMorphs := nil! !

!methodRemoval: WindowEvent #sentTo:localPosition: stamp: 'jmv 1/5/2021 12:52:18'!
WindowEvent removeSelector: #sentTo:localPosition:!
!methodRemoval: MouseScrollEvent #sentTo:localPosition: stamp: 'jmv 1/5/2021 12:52:10'!
MouseScrollEvent removeSelector: #sentTo:localPosition:!
!methodRemoval: MouseMoveEvent #sentTo:localPosition: stamp: 'jmv 1/5/2021 12:52:02'!
MouseMoveEvent removeSelector: #sentTo:localPosition:!
!methodRemoval: MouseButtonEvent #sentTo:localPosition: stamp: 'jmv 1/5/2021 12:51:18'!
MouseButtonEvent removeSelector: #sentTo:localPosition:!
!methodRemoval: MouseEvent #sentTo:localPosition: stamp: 'jmv 1/5/2021 12:51:52'!
MouseEvent removeSelector: #sentTo:localPosition:!
!methodRemoval: KeyboardEvent #sentTo:localPosition: stamp: 'jmv 1/5/2021 12:50:49'!
KeyboardEvent removeSelector: #sentTo:localPosition:!
!methodRemoval: DropFilesEvent #sentTo:localPosition: stamp: 'jmv 1/5/2021 12:50:18'!
DropFilesEvent removeSelector: #sentTo:localPosition:!
!methodRemoval: DropEvent #sentTo:localPosition: stamp: 'jmv 1/5/2021 12:50:06'!
DropEvent removeSelector: #sentTo:localPosition:!
!methodRemoval: MorphicEvent #sentTo:localPosition: stamp: 'jmv 1/5/2021 12:51:04'!
MorphicEvent removeSelector: #sentTo:localPosition:!
!methodRemoval: InnerTextMorph #processKeystroke:localPosition: stamp: 'jmv 1/5/2021 12:50:51'!
InnerTextMorph removeSelector: #processKeystroke:localPosition:!
!methodRemoval: Morph #processWindowEvent:localPosition: stamp: 'jmv 1/5/2021 12:52:23'!
Morph removeSelector: #processWindowEvent:localPosition:!
!methodRemoval: Morph #processKeyDown:localPosition: stamp: 'jmv 1/5/2021 12:50:56'!
Morph removeSelector: #processKeyDown:localPosition:!
!methodRemoval: Morph #processDropFiles:localPosition: stamp: 'jmv 1/5/2021 12:50:19'!
Morph removeSelector: #processDropFiles:localPosition:!
!methodRemoval: Morph #processDropMorph:localPosition: stamp: 'jmv 1/5/2021 12:50:07'!
Morph removeSelector: #processDropMorph:localPosition:!
!methodRemoval: Morph #processKeystroke:localPosition: stamp: 'jmv 1/5/2021 12:50:52'!
Morph removeSelector: #processKeystroke:localPosition:!
!methodRemoval: Morph #processKeyUp:localPosition: stamp: 'jmv 1/5/2021 12:50:54'!
Morph removeSelector: #processKeyUp:localPosition:!
!methodRemoval: Morph #processUnknownEvent:localPosition: stamp: 'jmv 1/5/2021 12:51:05'!
Morph removeSelector: #processUnknownEvent:localPosition:!
