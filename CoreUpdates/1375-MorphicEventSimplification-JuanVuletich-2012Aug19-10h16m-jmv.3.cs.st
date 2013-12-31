'From Cuis 4.0 of 21 April 2012 [latest update: #1374] on 19 August 2012 at 11:34:39 am'!
!classDefinition: #MouseMoveEvent category: #'Morphic-Events'!
MouseEvent subclass: #MouseMoveEvent
	instanceVariableNames: 'startPoint trail '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Events'!

!MouseEvent methodsFor: 'accessing' stamp: 'jmv 8/19/2012 11:13'!
eventType
	^type! !

!MouseEvent methodsFor: 'testing' stamp: 'jmv 8/19/2012 11:12'!
isMouseOver
	^type == #mouseOver! !


!MouseMoveEvent methodsFor: 'private' stamp: 'jmv 8/19/2012 10:45'!
setType: evtType position: evtEnd buttons: evtButtons hand: evtHand stamp: stamp

	type _ evtType.
	position _ evtEnd.
	buttons _ evtButtons.
	source _ evtHand.
	wasHandled _ false.
	timeStamp _ stamp! !


!WindowEvent methodsFor: 'accessing' stamp: 'jmv 8/19/2012 10:58'!
windowAction: aValue

	action _ aValue! !

!WindowEvent methodsFor: 'accessing' stamp: 'jmv 8/19/2012 11:09'!
windowEventType
	"This should match the definitions in sq.h"
	^#(
		windowMetricChange
		windowClose
		windowIconise
		windowActivated
		windowPaint
	) at: action ifAbsent: [#windowEventUnknown]! !


!AutoCompleter methodsFor: 'keyboard' stamp: 'jmv 8/19/2012 11:29'!
autoCompletionAround: aBlock keyStroke: aKeyboardEvent

	(self handleKeystrokeBefore: aKeyboardEvent)
		ifTrue: [^ self].
	aBlock value.
	"Narrow the completion with any of the keys"
	self handleKeystrokeAfter: aKeyboardEvent! !


!ChangeList methodsFor: 'menu actions' stamp: 'jmv 8/19/2012 11:02'!
selectEquivalentMethods
	"Selects all method definitions for which there is already an equivalent method in the current image, 
	(meaning that the difference is cosmetic and not in behavior)"
	Cursor wait showWhile: [
		1 to: changeList size do: [ :i | 
			| change class |
			change _ changeList at: i.
			listSelections at: i put:
				((change type == #method and: [
					(class _ change methodClass) notNil]) and: [
						(class includesSelector: change methodSelector) and: [
							| cmWithNode |
							cmWithNode _ [class basicCompile: change string notifying: nil trailer: class defaultMethodTrailer ifFail: nil] 
								on: SyntaxErrorNotification do: [ :ex | ex return ].
							(cmWithNode notNil and: [
								| current inChange |
								current _ (class compiledMethodAt: change methodSelector) copyWithTrailerBytes: #(0).
								inChange _ cmWithNode method copyWithTrailerBytes: #(0).
								current = inChange or: [
									| currentCmWithNode |
									currentCmWithNode _ [class basicCompile: (class decompilerClass new decompile: change methodSelector in: class) decompileString
											notifying: nil trailer: class defaultMethodTrailer ifFail: nil] on: SyntaxErrorNotification do: [ :ex | ex return ].
									(currentCmWithNode notNil and: [
										current _ currentCmWithNode method copyWithTrailerBytes: #(0).
										current = inChange])
								]
							])
						]]
				)]].
	self changed: #allSelections.
	self changed: #annotation! !


!Morph methodsFor: 'event handling' stamp: 'jmv 8/19/2012 11:32'!
handlesMouseDown: aMouseButtonEvent
	"Do I want to receive mouseDown events (mouseDown:, mouseMove:, mouseUp:)?"
	"NOTE: The default response is false. Subclasses that implement these messages directly should override this one to return true." 

	^ false
! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 11:29'!
handleKeystroke: aKeyboardEvent
	"System level event handling."

	aKeyboardEvent wasHandled ifTrue:[^self].
	self handlesKeyboard ifFalse:[^self].
	aKeyboardEvent wasHandled: true.
	^self keyStroke: aKeyboardEvent! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 11:26'!
handleMouseOver: aMorphicEvent
	"System level event handling."
	aMorphicEvent hand mouseFocus == self ifTrue:[
		"Got this directly through #handleFocusEvent: so check explicitly"
		(self containsPoint: aMorphicEvent eventPosition event: aMorphicEvent) ifFalse:[
			^self]].
	aMorphicEvent hand noticeMouseOver: self event: aMorphicEvent! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 11:27'!
rejectDropEvent: aMorphicEvent
	"This hook allows the receiver to repel a drop operation currently executed. The method is called prior to checking children so the receiver must validate that the event was really designated for it.
	Note that the ordering of the tests below is designed to avoid a (possibly expensive) #fullContainsPoint: test. If the receiver doesn't want to repel the morph anyways we don't need to check after all."
	(self repelsMorph: aMorphicEvent contents event: aMorphicEvent) ifFalse:[^self]. "not repelled"
	(self fullContainsPoint: aMorphicEvent eventPosition) ifFalse:[^self]. "not for me"
	"Throw it away"
	aMorphicEvent wasHandled: true.
	aMorphicEvent contents rejectDropMorphEvent: aMorphicEvent! !


!AutoCompleterMorph methodsFor: 'event handling' stamp: 'jmv 8/19/2012 11:31'!
handlesMouseDown: aMouseButtonEvent

	^ true! !


!FillInTheBlankMorph methodsFor: 'event handling' stamp: 'jmv 8/19/2012 11:31'!
handlesMouseDown: aMouseButtonEvent
	^true! !


!HaloHandleMorph methodsFor: 'event handling' stamp: 'jmv 8/19/2012 11:31'!
handlesMouseDown: aMouseButtonEvent
	"Do I want to receive mouseDown events (mouseDown:, mouseMove:, mouseUp:)?"
	^(super handlesMouseDown: aMouseButtonEvent) | 
		mouseDownSelector notNil | mouseMoveSelector notNil | mouseUpSelector notNil! !


!HaloMorph methodsFor: 'events' stamp: 'jmv 8/19/2012 11:25'!
popUpFor: aMorph event: aMorphicEvent
	"This message is sent by morphs that explicitly request the halo on a button click. Note: anEvent is in aMorphs coordinate frame."

	| hand anEvent |
	self flag: #workAround.	"We should really have some event/hand here..."
	anEvent _ aMorphicEvent
				ifNil: [
					hand _ aMorph world activeHand.
					hand ifNil: [ hand _ aMorph world firstHand ]. 
					hand lastEvent ]
				ifNotNil: [
					hand _ aMorphicEvent hand.
					aMorphicEvent ].
	hand halo: self.
	hand world addMorphFront: self.
	self target: aMorph.
	positionOffset _ anEvent eventPosition - aMorph morphPosition.
	self startStepping! !

!HaloMorph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 11:25'!
containsPoint: aPoint event: aMorphicEvent
	"mouseButton3 events are handled by the halo"

	(aMorphicEvent isMouse and: [
		aMorphicEvent isMouseDown and: [ aMorphicEvent mouseButton3Pressed ]])
	ifTrue: [
		^ self morphFullBoundsInWorld containsPoint: aMorphicEvent eventPosition ].

	^ super containsPoint: aPoint event: aMorphicEvent! !


!HandMorph methodsFor: 'private events' stamp: 'jmv 8/19/2012 10:45'!
generateMouseEvent: evtBuf 
	"Generate the appropriate mouse event for the given raw event buffer"

	| pos buttons modifiers type trail stamp oldButtons |
	stamp := evtBuf second.
	stamp = 0 ifTrue: [ stamp := Time millisecondClockValue ].
	pos := evtBuf third @ evtBuf fourth.
	buttons := evtBuf fifth.
	modifiers := evtBuf sixth.
	type := buttons = 0 
		ifTrue: [
			lastEventBuffer fifth = 0 ifTrue: [#mouseMove] ifFalse: [#mouseUp]]
		ifFalse: [
			lastEventBuffer fifth = 0 
						ifTrue: [#mouseDown]
						ifFalse: [#mouseMove]].
	buttons := buttons bitOr: (modifiers bitShift: 3).
	oldButtons := lastEventBuffer fifth 
				bitOr: (lastEventBuffer sixth bitShift: 3).
	lastEventBuffer := evtBuf.
	type == #mouseMove 
		ifTrue: [
			trail := self mouseTrailFrom: evtBuf.
			^MouseMoveEvent new 
				setType: type
				position: trail last
				buttons: buttons
				hand: self
				stamp: stamp].
	^MouseButtonEvent new 
		setType: type
		position: pos
		which: (oldButtons bitXor: buttons)
		buttons: buttons
		hand: self
		stamp: stamp! !

!HandMorph methodsFor: 'private events' stamp: 'jmv 8/19/2012 10:58'!
generateWindowEvent: evtBuf 
	"Generate the appropriate window event for the given raw event buffer"

	| evt |
	evt := WindowEvent new.
	evt setTimeStamp: evtBuf second.
	evt timeStamp = 0 ifTrue: [evt setTimeStamp: Time millisecondClockValue].
	evt windowAction: evtBuf third.
	evt rectangle: (Rectangle origin: evtBuf fourth @ evtBuf fifth corner: evtBuf sixth @ evtBuf seventh ).
	
	^evt! !

!HandMorph methodsFor: 'private events' stamp: 'jmv 8/19/2012 11:26'!
moveToEvent: aMorphicEvent
	"Issue a mouse move event to make the receiver appear at the given position"

	self handleEvent: (MouseMoveEvent new
		setType: #mouseMove
		position: aMorphicEvent eventPosition
		buttons: aMorphicEvent buttons
		hand: self
		stamp: aMorphicEvent timeStamp)! !


!InnerTextMorph methodsFor: 'event handling' stamp: 'jmv 8/19/2012 11:31'!
handlesMouseDown: aMouseButtonEvent
	^ true! !

!InnerTextMorph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 11:28'!
handleKeystroke: aKeyboardEvent
	"System level event handling."

	aKeyboardEvent wasHandled ifTrue:[^self].
	self handlesKeyboard ifFalse:	[^ self].
	aKeyboardEvent wasHandled: true.
	self keyStroke: aKeyboardEvent! !


!LayoutAdjustingMorph methodsFor: 'event handling' stamp: 'jmv 8/19/2012 11:31'!
handlesMouseDown: aMouseButtonEvent

	^ true! !


!MagnifierMorph methodsFor: 'events' stamp: 'jmv 8/19/2012 11:32'!
handlesMouseDown: aMouseButtonEvent
	^aMouseButtonEvent mouseButton2Pressed
		or: [super handlesMouseDown: aMouseButtonEvent]! !


!MenuItemMorph methodsFor: 'events' stamp: 'jmv 8/19/2012 11:32'!
handlesMouseDown: aMouseButtonEvent

	^ true
! !


!MenuMorph methodsFor: 'events' stamp: 'jmv 8/19/2012 11:32'!
handlesMouseDown: aMouseButtonEvent
	^true! !


!MorphicEvent methodsFor: 'comparing' stamp: 'jmv 8/19/2012 11:07'!
= aMorphicEvent
	^self class = aMorphicEvent class! !

!MorphicEvent methodsFor: 'comparing' stamp: 'jmv 8/19/2012 11:07'!
hash
	^super hash! !

!MorphicEvent methodsFor: 'dispatching' stamp: 'jmv 8/19/2012 11:21'!
sentTo: aMorph
	"Dispatch the receiver into aMorph"
	^aMorph handleUnknownEvent: self! !

!MorphicEvent methodsFor: 'testing' stamp: 'jmv 8/19/2012 11:12'!
isMouseOver
	^false! !


!DropEvent methodsFor: 'dispatching' stamp: 'jmv 8/19/2012 11:20'!
sentTo: aMorph
	"Dispatch the receiver into aMorph"
	^aMorph handleDropMorph: self! !


!OneLineEditorMorph methodsFor: 'event handling' stamp: 'jmv 8/19/2012 11:32'!
handlesMouseDown: aMouseButtonEvent
	^ true! !


!PasteUpMorph methodsFor: 'event handling' stamp: 'jmv 8/19/2012 11:32'!
handlesMouseDown: aMouseButtonEvent
	^true! !

!PasteUpMorph methodsFor: 'event handling' stamp: 'jmv 8/19/2012 11:09'!
windowEvent: aMorphicEvent
	self windowEventHandler
		ifNotNil: [^self windowEventHandler windowEvent: aMorphicEvent].

	aMorphicEvent windowEventType == #windowClose
		ifTrue: [
			^TheWorldMenu basicNew quitSession]
! !


!PluggableButtonMorph methodsFor: 'event handling' stamp: 'jmv 8/19/2012 11:32'!
handlesMouseDown: aMouseButtonEvent
	"Do I want to receive mouseDown events (mouseDown:, mouseMove:, mouseUp:)?"
	^true! !


!PluggableScrollPane methodsFor: 'pane events' stamp: 'jmv 8/19/2012 11:32'!
handlesMouseDown: aMouseButtonEvent
	^ true! !


!ScrollBar methodsFor: 'event handling' stamp: 'jmv 8/19/2012 11:33'!
handlesMouseDown: aMouseButtonEvent
	"Do I want to receive mouseDown events (mouseDown:, mouseMove:, mouseUp:)?"
	^true! !


!TranscriptMorph methodsFor: 'menus' stamp: 'jmv 8/19/2012 11:33'!
handlesMouseDown: aMouseButtonEvent
	^ true! !


!KeyboardEvent methodsFor: 'dispatching' stamp: 'jmv 8/19/2012 11:21'!
sentTo: aMorph
	"Dispatch the receiver into anObject"
	type == #keystroke ifTrue:[^aMorph handleKeystroke: self].
	type == #keyDown ifTrue:[^aMorph handleKeyDown: self].
	type == #keyUp ifTrue:[^aMorph handleKeyUp: self].
	^super sentTo: aMorph! !


!MouseEvent methodsFor: 'comparing' stamp: 'jmv 8/19/2012 11:13'!
= aMorphicEvent
	super = aMorphicEvent ifFalse:[^false].
	type = aMorphicEvent eventType ifFalse: [ ^false ].
	position = aMorphicEvent eventPosition ifFalse: [^ false].
	buttons = aMorphicEvent buttons ifFalse: [^ false].
	^ true
! !

!MouseEvent methodsFor: 'comparing' stamp: 'jmv 8/19/2012 11:14'!
hash
	^ type hash + position hash + buttons hash! !

!MouseEvent methodsFor: 'converting' stamp: 'jmv 8/19/2012 10:46'!
asMouseMove: deltaTime
	"Convert the receiver into a mouse move. adjust timestamp by the provided delta"

	^ MouseMoveEvent new
		setType: #mouseMove
		position: position
		buttons: buttons
		hand: source
		stamp: timeStamp + deltaTime! !

!MouseEvent methodsFor: 'dispatching' stamp: 'jmv 8/19/2012 11:22'!
sentTo: aMorph
	"Dispatch the receiver into aMorph"
	type == #mouseOver ifTrue:[^aMorph handleMouseOver: self].
	type == #mouseEnter ifTrue:[^aMorph handleMouseEnter: self].
	type == #mouseLeave ifTrue:[^aMorph handleMouseLeave: self].
	^super sentTo: aMorph! !


!MouseButtonEvent methodsFor: 'dispatching' stamp: 'jmv 8/19/2012 11:22'!
sentTo: aMorph
	"Dispatch the receiver into anObject"
	type == #mouseDown ifTrue:[^aMorph handleMouseDown: self].
	type == #mouseUp ifTrue:[^aMorph handleMouseUp: self].
	^super sentTo: aMorph! !


!MouseMoveEvent methodsFor: 'comparing' stamp: 'jmv 8/19/2012 10:42'!
= aMorphicEvent
	super = aMorphicEvent ifFalse:[^false].
	position = aMorphicEvent eventPosition ifFalse: [^ false].
	buttons = aMorphicEvent buttons ifFalse: [^ false].
	^ true
! !

!MouseMoveEvent methodsFor: 'comparing' stamp: 'jmv 8/19/2012 10:44'!
hash
	^ position hash + buttons hash! !

!MouseMoveEvent methodsFor: 'dispatching' stamp: 'jmv 8/19/2012 11:22'!
sentTo: aMorph
	"Dispatch the receiver into anObject"
	type == #mouseMove ifTrue:[^aMorph handleMouseMove: self].
	^super sentTo: aMorph! !


!WindowEvent methodsFor: 'dispatching' stamp: 'jmv 8/19/2012 11:22'!
sentTo: aMorph
	"Dispatch the receiver into anObject"
	^aMorph handleWindowEvent: self! !

!methodRemoval: WindowEvent #action!
WindowEvent removeSelector: #action!
!methodRemoval: WindowEvent #action:!
WindowEvent removeSelector: #action:!
!methodRemoval: WindowEvent #eventType!
WindowEvent removeSelector: #eventType!
!methodRemoval: WindowEvent #type!
WindowEvent removeSelector: #type!
!methodRemoval: MouseMoveEvent #endPoint!
MouseMoveEvent removeSelector: #endPoint!
!methodRemoval: MouseMoveEvent #printOn:!
MouseMoveEvent removeSelector: #printOn:!
!methodRemoval: MouseMoveEvent #setType:startPoint:endPoint:buttons:hand:stamp:!
MouseMoveEvent removeSelector: #setType:startPoint:endPoint:buttons:hand:stamp:!
!methodRemoval: MouseMoveEvent #setType:startPoint:endPoint:trail:buttons:hand:stamp:!
MouseMoveEvent removeSelector: #setType:startPoint:endPoint:trail:buttons:hand:stamp:!
!methodRemoval: MouseMoveEvent #startPoint!
MouseMoveEvent removeSelector: #startPoint!
!methodRemoval: MouseMoveEvent #storeOn:!
MouseMoveEvent removeSelector: #storeOn:!
!methodRemoval: MouseMoveEvent #trail!
MouseMoveEvent removeSelector: #trail!
!methodRemoval: MouseMoveEvent #type:readFrom:!
MouseMoveEvent removeSelector: #type:readFrom:!
!methodRemoval: MouseMoveEvent #ztranslateBy:!
MouseMoveEvent removeSelector: #ztranslateBy:!
!classDefinition: #MouseMoveEvent category: #'Morphic-Events'!
MouseEvent subclass: #MouseMoveEvent
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Events'!
!methodRemoval: MouseButtonEvent #storeOn:!
MouseButtonEvent removeSelector: #storeOn:!
!methodRemoval: MouseButtonEvent #type:readFrom:!
MouseButtonEvent removeSelector: #type:readFrom:!
!methodRemoval: MouseEvent #printOn:!
MouseEvent removeSelector: #printOn:!
!methodRemoval: MouseEvent #storeOn:!
MouseEvent removeSelector: #storeOn:!
!methodRemoval: MouseEvent #type:readFrom:!
MouseEvent removeSelector: #type:readFrom:!
!methodRemoval: KeyboardEvent #printOn:!
KeyboardEvent removeSelector: #printOn:!
!methodRemoval: KeyboardEvent #storeOn:!
KeyboardEvent removeSelector: #storeOn:!
!methodRemoval: KeyboardEvent #type:readFrom:!
KeyboardEvent removeSelector: #type:readFrom:!
!methodRemoval: UserInputEvent #buttonString!
UserInputEvent removeSelector: #buttonString!
!methodRemoval: UserInputEvent #eventType!
UserInputEvent removeSelector: #eventType!
!methodRemoval: UserInputEvent #modifierString!
UserInputEvent removeSelector: #modifierString!
!methodRemoval: UserInputEvent #type!
UserInputEvent removeSelector: #type!
!methodRemoval: MorphicEvent class #readFrom:!
MorphicEvent class removeSelector: #readFrom:!
!methodRemoval: MorphicEvent class #type:readFrom:!
MorphicEvent class removeSelector: #type:readFrom:!

!MorphicEvent class reorganize!
('instance protocol testing' gatherProtocols)
!

!methodRemoval: DropEvent #eventType!
DropEvent removeSelector: #eventType!
!methodRemoval: DropEvent #printOn:!
DropEvent removeSelector: #printOn:!
!methodRemoval: DropEvent #type!
DropEvent removeSelector: #type!
!methodRemoval: MorphicEvent #eventType!
MorphicEvent removeSelector: #eventType!
!methodRemoval: MorphicEvent #printOn:!
MorphicEvent removeSelector: #printOn:!
!methodRemoval: MorphicEvent #storeOn:!
MorphicEvent removeSelector: #storeOn:!
!methodRemoval: MorphicEvent #type!
MorphicEvent removeSelector: #type!
!methodRemoval: MorphicEvent #type:readFrom:!
MorphicEvent removeSelector: #type:readFrom:!

!MorphicEvent reorganize!
('accessing' hand timeStamp wasHandled wasHandled:)
('comparing' = hash)
('dispatching' dispatchWith: sentTo:)
('initialize' resetHandlerFields)
('testing' isDraggingEvent isDropEvent isKeyboard isKeystroke isMouse isMouseOver isWindowEvent)
('private' setTimeStamp:)
!

!methodRemoval: Morph #click!
Morph removeSelector: #click!
