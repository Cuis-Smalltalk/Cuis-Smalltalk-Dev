'From Cuis7.5 [latest update: #7486] on 2 September 2025 at 4:50:11 pm'!

!MouseOverHandler methodsFor: 'event handling' stamp: 'jmv 9/2/2025 16:26:30'!
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
		ifTrue: [
			leftMorphs := enteredMorphs := overMorphs := nil.
			^self].
	focus := hand mouseFocus.
	"Send #mouseLeave as appropriate"
	evt := aMouseEvent asMouseLeave.
	"Keep the order of the left morphs by recreating it from the mouseOverMorphs"
	leftMorphs size > 1 
		ifTrue: [leftMorphs := mouseOverMorphs select: [:m | leftMorphs includes: m]].
	leftMorphs do: [ :m |
		m isInWorld and: [
			(m == focus or: [m hasOwner: focus]) 
				ifTrue: [evt sendEventTo: m ]
				ifFalse: [overMorphs nextPut: m]]].
	"Send #mouseEnter as appropriate"
	evt := aMouseEvent asMouseEnter.
	enteredMorphs ifNil: [
		"inform: was called in handleEvent:"
		leftMorphs := enteredMorphs := overMorphs := nil.
		^self].
	enteredMorphs := enteredMorphs contents.
	enteredMorphs reverseDo: [ :m |
		m isInWorld and: [
			(m == focus or: [m hasOwner: focus]) 
				ifTrue: [evt sendEventTo: m ]]].
	"And remember the over list"
	overMorphs ifNil: [
			"inform: was called in handleEvent:"
			leftMorphs := enteredMorphs := overMorphs := nil.
			^self].
	mouseOverMorphs := overMorphs contents.
	leftMorphs := enteredMorphs := overMorphs := nil! !


!MenuMorph methodsFor: 'events-processing' stamp: 'jmv 9/2/2025 16:48:34'!
handleKeyboardFocusEvent: aMorphicEvent
	"Handle focus events. Valid menu transitions are determined based on the menu currently holding the focus after the mouse went down on one of its children."

	aMorphicEvent dispatchIn: self.

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

!MenuMorph methodsFor: 'events-processing' stamp: 'jmv 9/2/2025 16:48:43'!
handleMouseFocusEvent: aMorphicEvent
	"Handle focus events. Valid menu transitions are determined based on the menu currently holding the focus after the mouse went down on one of its children."

	aMorphicEvent dispatchIn: self.

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


!HandMorph methodsFor: 'events-processing' stamp: 'jmv 9/2/2025 15:00:45'!
startDropEventDispatch: aDropEvent

	aDropEvent dispatchIn: owner..
	self mouseOverHandler processMouseOver: lastMouseEvent! !

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 9/2/2025 15:00:54'!
startDropFilesEventDispatch: aDropFilesEvent

	aDropFilesEvent dispatchIn: owner.
	self mouseOverHandler processMouseOver: lastMouseEvent! !

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 9/2/2025 16:32:00'!
startMouseDispatch: aMouseEvent

	aMouseEvent isMouseOver ifTrue: [
		self mouseFocus
			ifNotNil: [ mouseFocus handleMouseFocusEvent: aMouseEvent ]
			ifNil: [ aMouseEvent dispatchIn: owner ].
		^self].

	"any mouse event but mouseOver"
	lastMouseEvent := aMouseEvent.	
	lastMouseEventTime := Time localMillisecondClock.

	"Check for pending drag or double click operations."
	mouseClickState ifNotNil: [
		(mouseClickState handleEvent: aMouseEvent from: self) ifTrue: [
			"Possibly dispatched #click: or something. Do not further process this event."
			self mouseOverHandler processMouseOver: lastMouseEvent.
			^self ]].

	aMouseEvent isMove
		ifTrue: [
			self morphPosition: aMouseEvent eventPosition.
			self mouseFocus
				ifNotNil: [ mouseFocus handleMouseFocusEvent: aMouseEvent ]
				ifNil: [ aMouseEvent dispatchIn: owner ]
		] ifFalse: [
		aMouseEvent isMouseScroll ifTrue: [
			aMouseEvent dispatchIn: owner
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
						ifNotNil: [ mouseFocus handleMouseFocusEvent: aMouseEvent ]
						ifNil: [ aMouseEvent dispatchIn: owner ]]]].
		self mouseOverHandler processMouseOver: lastMouseEvent! !

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 9/2/2025 15:01:16'!
startWindowEventDispatch: aWindowEvent

	aWindowEvent dispatchIn: owner.
	self mouseOverHandler processMouseOver: lastMouseEvent! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 9/2/2025 15:00:34'!
dropMorph: aMorph event: aMouseEvent
	"Drop the given morph which was carried by the hand"
	| morphData dropEvent |
	morphData := self grabMorphDataFor: aMorph.
	dropEvent := DropEvent new 
			setPosition: self morphPosition 
			contents: aMorph 
			hand: self
			formerOwner: (morphData at: 1)
			formerPosition: (morphData at: 2).
	dropEvent dispatchIn: owner.
	dropEvent wasHandled ifFalse: [ aMorph rejectDropMorphEvent: dropEvent ].
	self forgetGrabMorphDataFor: aMorph.
	self mouseOverHandler processMouseOver: aMouseEvent! !

!methodRemoval: HaloMorph #rejectsEventFully: stamp: 'jmv 9/2/2025 16:49:01'!
HaloMorph removeSelector: #rejectsEventFully:!
!methodRemoval: Morph #dispatchEvent: stamp: 'jmv 9/2/2025 16:48:58'!
Morph removeSelector: #dispatchEvent:!
!methodRemoval: Morph #rejectsEventFully: stamp: 'jmv 9/2/2025 16:49:02'!
Morph removeSelector: #rejectsEventFully:!
