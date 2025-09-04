'From Cuis7.5 [latest update: #7507] on 4 September 2025 at 12:01:58 pm'!

!MorphicEvent methodsFor: 'dispatching' stamp: 'jmv 9/4/2025 11:53:04'!
dispatchIn: aMorph
	"Dispatch me.
	Different subclasses will use a specific strategy to determine who will handle each event.
	Argument is usually the WorldMorph, but not always. See subclasses."

	self subclassResponsibility! !

!MorphicEvent methodsFor: 'dispatching' stamp: 'jmv 9/4/2025 11:41:18'!
sendEventTo: aMorph
	"Event dispatch finished. Deliver us straight to aMorph for execution."

	aMorph processUnknownEvent: self! !


!DropEvent methodsFor: 'dispatching' stamp: 'jmv 9/4/2025 11:41:22'!
sendEventTo: aMorph
	"Event dispatch finished. Deliver us straight to aMorph for execution."

	aMorph processDropMorph: self! !


!DropFilesEvent methodsFor: 'dispatching' stamp: 'jmv 9/4/2025 11:41:25'!
sendEventTo: aMorph
	"Event dispatch finished. Deliver us straight to aMorph for execution."

	aMorph processDropFiles: self! !


!UserInputEvent methodsFor: 'dispatching' stamp: 'jmv 9/4/2025 11:52:47'!
dispatchIn: aMorph
	"Dispatch me.
	Different subclasses will use a specific strategy to determine who will handle each event.
	See argument name in subclasses for the specific morph passed in."

	self subclassResponsibility! !


!KeyboardEvent methodsFor: 'dispatching' stamp: 'jmv 9/4/2025 11:41:29'!
sendEventTo: aMorph
	"Event dispatch finished. Deliver us straight to aMorph for execution."
"
Revisar senders de
#definesKeyboardShortcuts.
"
	keyEventType == #keystroke ifTrue: [
		self isFindClassShortcut ifTrue: [ 
			(Preferences at: #classFinder) value.
			^self ].
		self isCloseWindowShortcut ifTrue: [
			self closeCurrentWindowOf: aMorph.
			^self ].
		aMorph processKeystroke: self.
		^self ].
	keyEventType == #keyDown ifTrue: [
		aMorph processKeyDown: self.
		^self ].
	keyEventType == #keyUp ifTrue: [ 
		aMorph processKeyUp: self.
		^self ].
	super sendEventTo: aMorph.! !


!MouseEvent methodsFor: 'dispatching' stamp: 'jmv 9/4/2025 12:00:33'!
dispatchIn: aWorldMorphOrFocusedMorph
	"Dispatch me.
	The event will be passed to the front-most visible submorph that contains the event position.
	Search usually starts with the WorldMorph, but it could be another morph if it is the hand's #mouseFocus.
	If #rejectsEvent:, or #wasHandled not by any reason, try with the owner."

	| handlerMorph |
	handlerMorph := aWorldMorphOrFocusedMorph morphVisibleAt: position.
	[ handlerMorph notNil and: [self wasHandled not]] whileTrue: [
		((handlerMorph rejectsEvent: self) not and: [handlerMorph includesPixel: position]) ifTrue: [
			
			self sendEventTo: handlerMorph.
			
		].
		handlerMorph := handlerMorph owner.
	].! !


!MouseButtonEvent methodsFor: 'dispatching' stamp: 'jmv 9/4/2025 12:00:40'!
dispatchIn: aWorldMorphOrFocusedMorph
	"Dispatch me.
	The event will be passed to the front-most visible submorph that contains the event position.
	Search usually starts with the WorldMorph, but it could be another morph if it is the hand's #mouseFocus.
	If #rejectsEvent:, or #wasHandled not by any reason, try with the owner."
	
	| handlerMorph grabTheMorph |
	
	"Only for MouseDown"
	self isMouseDown ifFalse: [
		^super dispatchIn: aWorldMorphOrFocusedMorph ].

	handlerMorph := aWorldMorphOrFocusedMorph morphVisibleAt: position.
	[ handlerMorph notNil and: [self wasHandled not]] whileTrue: [
		((handlerMorph rejectsEvent: self) not and: [handlerMorph includesPixel: position]) ifTrue: [
			
			"aMorph is in the top-most unlocked, visible morph in the chain."
			(self mouseButton3Pressed or: [handlerMorph handlesMouseDown: self])
				ifTrue: [ self sendEventTo: handlerMorph ]
				ifFalse: [
					grabTheMorph := false.
					"If button 1, and both aMorph and the owner allows grabbing with the hand (to initiate drag & drop), so be it."
					self mouseButton1Pressed ifTrue: [
						handlerMorph owner ifNotNil: [ :o |
							(o allowsSubmorphDrag and: [ handlerMorph isSticky not ]) ifTrue: [
								grabTheMorph := true ]]].
					grabTheMorph ifTrue: [
						self hand
							waitForClicksOrDrag: handlerMorph event: self
							dragSel: ((Preferences at: #clickGrabsMorphs) ifFalse: [#dragEvent:localPosition:])
							clkSel: ((Preferences at: #clickGrabsMorphs) ifTrue: [#dragEvent:localPosition:]).
						"false ifTrue: [ self hand grabMorph: aMorph ]."
						(Preferences at: #clickGrabsMorphs) ifFalse: [
							self shiftPressed
								ifTrue: [ (handlerMorph is: #SystemWindow) ifTrue: [ handlerMorph sendToBack ] ]
								ifFalse: [ handlerMorph activateWindow ] ].
						self wasHandled: true ]]
			
		].
		handlerMorph := handlerMorph owner.
	].! !

!MouseButtonEvent methodsFor: 'dispatching' stamp: 'jmv 9/4/2025 11:41:36'!
sendEventTo: aMorph
	"Event dispatch finished. Deliver us straight to aMorph for execution."

	self isMouseDown ifTrue: [
		aMorph processMouseDown: self localPosition: (aMorph internalizeFromWorld: position).
		^self ].
	self isMouseUp ifTrue: [
		aMorph processMouseUp: self localPosition: (aMorph internalizeFromWorld: position).
		^self ].! !


!MouseEnterEvent methodsFor: 'dispatching' stamp: 'jmv 9/4/2025 11:41:40'!
sendEventTo: aMorph
	"Event dispatch finished. Deliver us straight to aMorph for execution."

	aMorph processMouseEnter: self localPosition: (aMorph internalizeFromWorld: position).! !


!MouseLeaveEvent methodsFor: 'dispatching' stamp: 'jmv 9/4/2025 11:41:44'!
sendEventTo: aMorph
	"Event dispatch finished. Deliver us straight to aMorph for execution."

	aMorph processMouseLeave: self localPosition: (aMorph internalizeFromWorld: position).! !


!MouseMoveEvent methodsFor: 'dispatching' stamp: 'jmv 9/4/2025 11:41:48'!
sendEventTo: aMorph
	"Event dispatch finished. Deliver us straight to aMorph for execution."

	aMorph processMouseMove: self localPosition: (aMorph internalizeFromWorld: position).! !


!MouseOverEvent methodsFor: 'dispatching' stamp: 'jmv 9/4/2025 11:41:51'!
sendEventTo: aMorph
	"Event dispatch finished. Deliver us straight to aMorph for execution."

	aMorph processMouseOver: self localPosition: (aMorph internalizeFromWorld: position).! !


!MouseScrollEvent methodsFor: 'dispatching' stamp: 'jmv 9/4/2025 12:00:49'!
dispatchIn: aWorldMorphOrFocusedMorph
	"Dispatch me.
	The event will be passed to the front-most visible submorph that contains the event position.
	Search usually starts with the WorldMorph, but it could be another morph if it is the hand's #mouseFocus.
	If #rejectsEvent:, or #wasHandled not by any reason, try with the owner."

	| handlerMorph |
	handlerMorph := aWorldMorphOrFocusedMorph morphVisibleAt: position.
	[ handlerMorph notNil and: [self wasHandled not]] whileTrue: [
		((handlerMorph rejectsEvent: self) not and: [handlerMorph includesPixel: position]) ifTrue: [
			
			(handlerMorph handlesMouseScroll: self) ifTrue: [
				self sendEventTo: handlerMorph ].
			
		].
		handlerMorph := handlerMorph owner.
	].! !

!MouseScrollEvent methodsFor: 'dispatching' stamp: 'jmv 9/4/2025 11:41:55'!
sendEventTo: aMorph
	"Event dispatch finished. Deliver us straight to aMorph for execution."

	aMorph processMouseScroll: self localPosition: (aMorph internalizeFromWorld: position).! !


!WindowEvent methodsFor: 'dispatching' stamp: 'jmv 9/4/2025 11:41:59'!
sendEventTo: aMorph
	"Event dispatch finished. Deliver us straight to aMorph for execution."

	aMorph processWindowEvent: self.! !


!Morph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 11:46:45'!
handleMouseFocusEvent: aMouseEvent
	"Handle the given event.
	This message is sent if the receiver currently has the mouse focus
	and is therefore receiving events directly from some hand,
	without going through #dispatchIn:."

	aMouseEvent sendEventTo: self.! !


!MenuMorph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 11:47:37'!
handleMouseFocusEvent: aMouseEvent
	"Handle the given event.
		This message is sent if the receiver currently has the mouse focus
	and is therefore receiving events directly from some hand,
	without going through #dispatchIn:.
		Valid menu transitions are determined based on the menu currently holding the focus 
	after the mouse went down on one of its children."

	"#dispatchIn: and not #sendEventTo: so it is handled by the appropriate menu item."
	aMouseEvent dispatchIn: self.
	
	"Handle clicks outside us, so we are closed. This will only be done if the event wasn't handled above,
	meaning that it is a click outside of us."
	aMouseEvent isMouseButton ifTrue: [
		aMouseEvent sendEventTo: self.
		^self ].
	
	"On MouseMove maybe activate submenu, or owner."
	aMouseEvent isMouseMove ifTrue: [
		"On #mouseMove we might transfer control to *either* the currently active submenu
		or the owner menu, if any.
		Since the active sub menu is always displayed upfront check it first."
		selectedItem ifNotNil: [
			(selectedItem activateSubmenu: aMouseEvent) ifTrue: [
				^self]].
		"Note: The following does not traverse upwards but it's the best I can do for now"
		popUpOwner ifNotNil: [
			(popUpOwner activateOwnerMenu: aMouseEvent) ifTrue: [
				^self]]].! !

