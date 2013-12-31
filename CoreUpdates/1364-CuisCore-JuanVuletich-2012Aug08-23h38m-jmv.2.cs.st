'From Cuis 4.0 of 21 April 2012 [latest update: #1363] on 8 August 2012 at 11:44:53 pm'!
!classDefinition: #HandMorph category: #'Morphic-Kernel'!
Morph subclass: #HandMorph
	instanceVariableNames: 'mouseFocus keyboardFocus eventListeners mouseListeners keyboardListeners mouseClickState mouseOverHandler lastMouseEvent targetOffset damageRecorder hasChanged savedPatch lastEventBuffer lastKeyDownValue lastMouseEventTime prevBounds prevFullBounds '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!Morph methodsFor: 'fileIn/out' stamp: 'jmv 8/8/2012 23:07'!
storeDataOn: aDataStream
	"Let all Morphs be written out.  All owners are weak references.  They only go out if the owner is in the tree being written."
	| cntInstVars cntIndexedVars ti localInstVars |

	"block my owner unless he is written out by someone else"
	cntInstVars _ self class instSize.
	cntIndexedVars _ self basicSize.
	localInstVars _ Morph instVarNames.
	ti _ 1.  
	((localInstVars at: ti) = 'owner') & (Morph superclass == Object) ifFalse:
			[self error: 'this method is out of date'].
	aDataStream
		beginInstance: self class
		size: cntInstVars + cntIndexedVars.
	1 to: ti-1 do:
		[:i | aDataStream nextPut: (self instVarAt: i)].
	aDataStream nextPutWeak: owner.	"owner only written if in our tree"
	ti+1 to: cntInstVars do:
		[:i | aDataStream nextPut: (self instVarAt: i)].
	1 to: cntIndexedVars do:
		[:i | aDataStream nextPut: (self basicAt: i)]! !


!HaloMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 8/8/2012 23:39'!
startDrag: evt with: dragHandle
	"Drag my target without removing it from its owner."

	evt hand obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	positionOffset _ dragHandle referencePosition - target morphPosition! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/8/2012 23:39'!
doDebug: evt with: menuHandle
	"Ask hand to invoke the a debugging menu for my inner target.  If shift key is down, immediately put up an inspector on the inner target"

	| menu |
	evt hand obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	self world displayWorld.
	evt shiftPressed ifTrue: [
		self delete.
		^ innerTarget inspect].

	menu _ innerTarget buildDebugMenu: evt hand.
	menu addTitle: (innerTarget printStringLimitedTo: 40).
	menu popUpInWorld: self world! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/8/2012 23:41'!
doDup: evt with: dupHandle 
	"Ask hand to duplicate my target."

	evt hand obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	self setTarget: (target duplicateMorph: evt).
	evt hand grabMorph: target.
	self step.	"update position if necessary"! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/8/2012 23:41'!
doGrab: evt with: grabHandle
	"Ask hand to grab my target."

	evt hand obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	evt hand grabMorph: target.
	self step. "update position if necessary"! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/8/2012 23:39'!
doMenu: evt with: menuHandle
	"Ask hand to invoke the halo menu for my inner target."

	| menu |
	evt hand obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	self world displayWorld.
	menu _ innerTarget buildHandleMenu: evt hand.
	innerTarget addTitleForHaloMenu: menu.
	menu popUpInWorld: self world.
! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/8/2012 23:39'!
mouseDownInCollapseHandle: evt with: collapseHandle
	"The mouse went down in the collapse handle; collapse the morph"

	evt hand obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	self setDismissColor: evt with: collapseHandle! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/8/2012 23:39'!
startGrow: evt with: growHandle
	"Initialize resizing of my target.  Launch a command representing it, to support Undo"

	| botRt |
	evt hand obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	self removeAllHandlesBut: growHandle.  "remove all other handles"
	botRt _ target morphPositionInWorld + target morphExtentInWorld.
	positionOffset _ (self world viewBox containsPoint: botRt)
		ifTrue: [evt eventPosition - botRt]
		ifFalse: [0@0]! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/8/2012 23:39'!
startRot: evt with: rotHandle
	"Initialize rotation of my target if it is rotatable.  Launch a command object to represent the action"

	evt hand obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	growingOrRotating _ true.

	self removeAllHandlesBut: rotHandle.  "remove all other handles"
	angleOffset _ evt eventPosition - target referencePosition.
	angleOffset _ Point
			r: angleOffset r
			degrees: angleOffset degrees - target rotationDegrees

! !


!HandMorph methodsFor: 'events-processing' stamp: 'jmv 8/8/2012 23:27'!
handleEvent: anEvent
	| evt |
	owner ifNil: [ ^ self ].
	evt _ anEvent.
	evt isMouseOver ifTrue: [ ^ self sendMouseEvent: evt ].
	
	evt isWindowEvent ifTrue: [
		self sendEvent: evt.
		^ self mouseOverHandler processMouseOver: lastMouseEvent ].

	evt isKeyboard ifTrue: [
		self sendKeyboardEvent: evt.
		^ self mouseOverHandler processMouseOver: lastMouseEvent].

	evt isDropEvent ifTrue: [
		self sendEvent: evt.
		^ self mouseOverHandler processMouseOver: lastMouseEvent].

	evt isMouse ifTrue: [
		lastMouseEvent _ evt.	
		lastMouseEventTime _ Time millisecondClockValue].

	"Check for pending drag or double click operations."
	mouseClickState ifNotNil: [
		(mouseClickState handleEvent: evt from: self) ifTrue: [
			"Possibly dispatched #click: or something. Do not further process this event."
			^self mouseOverHandler processMouseOver: lastMouseEvent  ]].

	evt isMove ifTrue: [
		self morphPosition: evt eventPosition.
		self sendMouseEvent: evt.
	] ifFalse: [
		"Issue a synthetic move event if we're not at the position of the event"
		evt eventPosition = self morphPosition ifFalse: [ self moveToEvent: evt ].
		"Drop submorphs on button events"
		self hasSubmorphs
			ifTrue: [ self dropMorphs: evt ]
			ifFalse: [ self sendMouseEvent: evt ].
	].
	self mouseOverHandler processMouseOver: lastMouseEvent! !

!methodRemoval: SystemWindow #handleListenEvent:!
SystemWindow removeSelector: #handleListenEvent:!
!methodRemoval: PasteUpMorph #unhideHiddenObjects!
PasteUpMorph removeSelector: #unhideHiddenObjects!
!methodRemoval: HandMorph #addEventListener:!
HandMorph removeSelector: #addEventListener:!
!methodRemoval: HandMorph #addKeyboardListener:!
HandMorph removeSelector: #addKeyboardListener:!
!methodRemoval: HandMorph #addListener:to:!
HandMorph removeSelector: #addListener:to:!
!methodRemoval: HandMorph #addMouseListener:!
HandMorph removeSelector: #addMouseListener:!
!methodRemoval: HandMorph #eventListeners!
HandMorph removeSelector: #eventListeners!
!methodRemoval: HandMorph #eventListeners:!
HandMorph removeSelector: #eventListeners:!
!methodRemoval: HandMorph #keyboardListeners!
HandMorph removeSelector: #keyboardListeners!
!methodRemoval: HandMorph #keyboardListeners:!
HandMorph removeSelector: #keyboardListeners:!
!methodRemoval: HandMorph #mouseListeners!
HandMorph removeSelector: #mouseListeners!
!methodRemoval: HandMorph #mouseListeners:!
HandMorph removeSelector: #mouseListeners:!
!methodRemoval: HandMorph #removeEventListener:!
HandMorph removeSelector: #removeEventListener:!
!methodRemoval: HandMorph #removeKeyboardListener:!
HandMorph removeSelector: #removeKeyboardListener:!
!methodRemoval: HandMorph #removeListener:from:!
HandMorph removeSelector: #removeListener:from:!
!methodRemoval: HandMorph #removeMouseListener:!
HandMorph removeSelector: #removeMouseListener:!
!methodRemoval: HandMorph #sendListenEvent:to:!
HandMorph removeSelector: #sendListenEvent:to:!
!classDefinition: #HandMorph category: #'Morphic-Kernel'!
Morph subclass: #HandMorph
	instanceVariableNames: 'mouseFocus keyboardFocus mouseClickState mouseOverHandler lastMouseEvent targetOffset damageRecorder hasChanged savedPatch lastEventBuffer lastKeyDownValue lastMouseEventTime prevBounds prevFullBounds'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!
!methodRemoval: HaloMorph #handleListenEvent:!
HaloMorph removeSelector: #handleListenEvent:!
!methodRemoval: HaloMorph #obtainHaloForEvent:andRemoveAllHandlesBut:!
HaloMorph removeSelector: #obtainHaloForEvent:andRemoveAllHandlesBut:!
!methodRemoval: Morph #handleListenEvent:!
Morph removeSelector: #handleListenEvent:!
