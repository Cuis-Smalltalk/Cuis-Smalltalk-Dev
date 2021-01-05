'From Cuis 5.0 [latest update: #4513] on 5 January 2021 at 12:15:13 pm'!

!Morph methodsFor: 'events-processing' stamp: 'jmv 1/5/2021 12:08:39'!
dispatchEvent: aMorphicEvent
	"This is the central entry for dispatching events in morphic. Given some event, find the right receiver and let him handle it."

	^ (self rejectsEventFully: aMorphicEvent)
		ifTrue: [ #rejected ]
		ifFalse: [ aMorphicEvent dispatchWith: self  ]! !


!MorphicEvent methodsFor: 'dispatching' stamp: 'jmv 1/5/2021 12:09:46'!
dispatchWith: aMorph
	"Dispatch me. The event will be passed to the front-most visible submorph that contains the position wrt. to the event."
	| handledByInner |

	"Try to get out quickly"
	(aMorph fullContainsGlobalPoint: self eventPosition)
		ifFalse: [ ^#rejected ].

	"Now give submorphs a chance to handle the event"
	handledByInner _ false.
	aMorph submorphsDo: [ :eachChild |
		handledByInner ifFalse: [
			(eachChild dispatchEvent: self) == #rejected ifFalse: [
				"Some child did contain the point so aMorph is part of the top-most chain."
				handledByInner _ true ]]].

	"Check for being inside the receiver"
	(handledByInner or: [ (aMorph rejectsEvent: self) not and: [aMorph fullContainsGlobalPoint: self eventPosition] ])
		ifTrue: [ ^ self sendEventTo: aMorph ].

	^ #rejected! !


!DropEvent methodsFor: 'dispatching' stamp: 'jmv 1/5/2021 12:08:46'!
dispatchWith: aMorph
	"Drop is done on the innermost target that accepts it."
	| dropped |

	"Try to get out quickly"
	(aMorph fullContainsGlobalPoint: position)
		ifFalse: [ ^#rejected ].

	"Go looking if any of our submorphs wants it"
	aMorph submorphsDo: [ :eachChild |
		(eachChild dispatchEvent: self) == #rejected ifFalse: [
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


!DropFilesEvent methodsFor: 'dispatching' stamp: 'jmv 1/5/2021 12:09:18'!
dispatchWith: aMorph
	"Drop is done on the innermost target that accepts it."

	"Try to get out quickly"
	(aMorph fullContainsGlobalPoint: position) ifFalse: [ ^#rejected ].

	"Go looking if any of our submorphs wants it"
	aMorph submorphsDo: [ :eachChild |
		(eachChild dispatchEvent: self) == #rejected ifFalse: [ ^self ]].

	(aMorph allowsFilesDrop and: [ (aMorph rejectsEvent: self) not and: [aMorph fullContainsGlobalPoint: position] ])
		ifTrue: [^ self sendEventTo: aMorph ].
	
	^#rejected! !


!MouseButtonEvent methodsFor: 'dispatching' stamp: 'jmv 1/5/2021 12:10:27'!
dispatchWith: aMorph
	"Find the appropriate receiver for the event and let it handle it. Default rules:
	* The top-most chain of visible, unlocked morphs containing the event position will get a chance to handle the event.
	* When travelling down the hierarchy a prospective handler for the event is installed. This prospective handler can be used by submorphs wishing to handle the mouse down for negotiating who the receiver is.
	* When travelling up, the prospective handler is always executed. The handler needs to check if the event was handled before as well as checking if somebody else's handler has been installed.
	* If another handler has been installed but the event was not handled it means that somebody up in the hierarchy wants to handle the event.
	"
	| aMorphHandlesIt grabAMorph handledByInner |
	"Only for MouseDown"
	self isMouseDown ifFalse: [
		^super dispatchWith: aMorph ].

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
			(eachChild dispatchEvent: self) == #rejected ifFalse: [
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


!MouseScrollEvent methodsFor: 'dispatching' stamp: 'jmv 1/5/2021 12:10:57'!
dispatchWith: aMorph
	"Find the appropriate receiver for the event and let it handle it. Default rules:
	* The top-most chain of visible, unlocked morphs containing the event position will get a chance to handle the event.
	* When travelling down the hierarchy a prospective handler for the event is installed. This prospective handler can be used by submorphs wishing to handle the mouse down for negotiating who the receiver is.
	* When travelling up, the prospective handler is always executed. The handler needs to check if the event was handled before as well as checking if somebody else's handler has been installed.
	* If another handler has been installed but the event was not handled it means that somebody up in the hierarchy wants to handle the event.
	"
	"Try to get out quickly"
	| aMorphHandlesIt handledByInner focus|
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
			(eachChild dispatchEvent: self) == #rejected ifFalse: [
				"Some child did contain the point so aMorph is part of the top-most chain."
				handledByInner _ true ]]].
	(handledByInner or: [
		 (aMorph rejectsEvent: self) not and: [aMorph fullContainsGlobalPoint: position]]) ifTrue: [
		"aMorph is in the top-most unlocked, visible morph in the chain."
		aMorphHandlesIt ifTrue: [ ^ self sendEventTo: aMorph ]].
	handledByInner ifTrue: [ ^ self ].
	"Mouse was not on aMorph nor any of its children"
	^ #rejected.! !


!WindowEvent methodsFor: 'dispatching' stamp: 'jmv 1/5/2021 12:11:04'!
dispatchWith: aMorph
	"Host window events do not have a position and are only dispatched to the World"

	aMorph isWorldMorph ifFalse: [ ^#rejected ].
	self wasHandled ifTrue: [ ^self ].
	^ self sendEventTo: aMorph! !


!HandMorph methodsFor: 'events-processing' stamp: 'jmv 1/5/2021 12:12:30'!
startDropEventDispatch: aDropEvent

	owner dispatchEvent: aDropEvent.
	self mouseOverHandler processMouseOver: lastMouseEvent! !

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 1/5/2021 12:12:36'!
startDropFilesEventDispatch: aDropFilesEvent

	owner dispatchEvent: aDropFilesEvent.
	self mouseOverHandler processMouseOver: lastMouseEvent! !

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 1/5/2021 12:13:52'!
startMouseDispatch: aMouseEvent

	aMouseEvent isMouseOver ifTrue: [
		^self mouseFocus
			ifNotNil: [ mouseFocus handleFocusEvent: aMouseEvent ]
			ifNil: [ owner dispatchEvent: aMouseEvent ]].

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
				ifNil: [ owner dispatchEvent: aMouseEvent ]
		] ifFalse: [
		aMouseEvent isMouseScroll ifTrue: [
			owner dispatchEvent: aMouseEvent
		] ifFalse: [
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
					mouseClickState ifNil: [
						"Want to drop on mouseUp, NOT mouseDown"
						aMouseEvent isMouseUp ifTrue: [
							self dropMorphs: aMouseEvent ]
					]
				] ifFalse: [
					self mouseFocus
						ifNotNil: [ mouseFocus handleFocusEvent: aMouseEvent ]
						ifNil: [ owner dispatchEvent: aMouseEvent ]]]].
		self mouseOverHandler processMouseOver: self lastMouseEvent! !

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 1/5/2021 12:12:53'!
startWindowEventDispatch: aWindowEvent

	owner dispatchEvent: aWindowEvent.
	self mouseOverHandler processMouseOver: lastMouseEvent! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 1/5/2021 12:12:23'!
dropMorph: aMorph event: aMouseEvent
	"Drop the given morph which was carried by the hand"
	| morphData dropEvent |
	morphData := self grabMorphDataFor: aMorph.
	dropEvent _ DropEvent new 
			setPosition: self morphPosition 
			contents: aMorph 
			hand: self
			formerOwner: (morphData at: 1)
			formerPosition: (morphData at: 2).
	owner dispatchEvent: dropEvent.
	dropEvent wasHandled ifFalse: [ aMorph rejectDropMorphEvent: dropEvent ].
	self forgetGrabMorphDataFor: aMorph.
	self mouseOverHandler processMouseOver: aMouseEvent! !


!MenuMorph methodsFor: 'events-processing' stamp: 'jmv 1/5/2021 12:13:16'!
handleFocusEvent: aMorphicEvent
	"Handle focus events. Valid menu transitions are determined based on the menu currently holding the focus after the mouse went down on one of its children."

	self dispatchEvent: aMorphicEvent.

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

!methodRemoval: WindowEvent #dispatchWith:localPosition: stamp: 'jmv 1/5/2021 12:11:37'!
WindowEvent removeSelector: #dispatchWith:localPosition:!
!methodRemoval: MouseScrollEvent #dispatchWith:localPosition: stamp: 'jmv 1/5/2021 12:11:36'!
MouseScrollEvent removeSelector: #dispatchWith:localPosition:!
!methodRemoval: MouseButtonEvent #dispatchWith:localPosition: stamp: 'jmv 1/5/2021 12:11:31'!
MouseButtonEvent removeSelector: #dispatchWith:localPosition:!
!methodRemoval: DropFilesEvent #dispatchWith:localPosition: stamp: 'jmv 1/5/2021 12:11:35'!
DropFilesEvent removeSelector: #dispatchWith:localPosition:!
!methodRemoval: DropEvent #dispatchWith:localPosition: stamp: 'jmv 1/5/2021 12:11:35'!
DropEvent removeSelector: #dispatchWith:localPosition:!
!methodRemoval: MorphicEvent #dispatchWith:localPosition: stamp: 'jmv 1/5/2021 12:11:36'!
MorphicEvent removeSelector: #dispatchWith:localPosition:!
!methodRemoval: Morph #dispatchEvent:localPosition: stamp: 'jmv 1/5/2021 12:13:56'!
Morph removeSelector: #dispatchEvent:localPosition:!
