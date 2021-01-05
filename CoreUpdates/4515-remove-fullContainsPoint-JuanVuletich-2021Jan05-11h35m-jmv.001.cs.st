'From Cuis 5.0 [latest update: #4513] on 5 January 2021 at 11:46:24 am'!

!MorphicEvent methodsFor: 'accessing' stamp: 'jmv 1/5/2021 11:42:09'!
eventPosition
	self subclassResponsibility! !


!Morph methodsFor: 'events-processing' stamp: 'jmv 1/5/2021 11:44:31'!
processMouseOver: aMouseEvent localPosition: localEventPosition
	"System level event handling."
	 self hasMouseFocus ifTrue: [
		"Got this directly through #handleFocusEvent: so check explicitly"
		((self rejectsEvent: aMouseEvent) not and: [self fullContainsGlobalPoint: aMouseEvent eventPosition]) ifFalse: [
			^self ]].
	aMouseEvent hand noticeMouseOver: self event: aMouseEvent.
	"Open question: should any unhandled mouse move events be filtered out? (i.e. should mouseHover:localPosition: be called when a mouse button is pressed but the morph doesn't have mouse button handlers?  Essentially, what are the limits of what is considered 'hovering'?"
	(self handlesMouseHover and: [aMouseEvent wasHandled not]) ifTrue: [
		self
			mouseHover: aMouseEvent
			localPosition: localEventPosition ].! !


!MenuMorph methodsFor: 'events' stamp: 'jmv 1/5/2021 11:43:53'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse down event."
	(stayUp or: [ self fullContainsGlobalPoint: aMouseButtonEvent eventPosition ]) 
		ifFalse: [ ^self deleteIfPopUp: aMouseButtonEvent ]. "click outside"

	"Grab the menu and drag it to some other place
	This is reimplemented here because we handle the event, and if the following lines are commented, a menu can't be grabbed with the hand. This is not nice and shouldn't be needed"
	self isSticky ifTrue: [ ^self ].
	aMouseButtonEvent hand grabMorph: self! !

!MenuMorph methodsFor: 'events' stamp: 'jmv 1/5/2021 11:44:12'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse up event.
	Note: This might be sent from a modal shell."
	(self fullContainsGlobalPoint: aMouseButtonEvent eventPosition) ifFalse:[
		"Mouse up outside. Release eventual focus and delete if pop up."
		aMouseButtonEvent hand ifNotNil: [ :h | h releaseMouseFocus: self ].
		^ self deleteIfPopUp: aMouseButtonEvent ].
	stayUp ifFalse: [
		"Still in pop-up transition; keep focus"
		aMouseButtonEvent hand newMouseFocus: self ]! !


!MorphicEvent methodsFor: 'dispatching' stamp: 'jmv 1/5/2021 11:44:50'!
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
		ifTrue: [ ^ self sentTo: aMorph localPosition: positionInAMorph ].

	^ #rejected! !


!DropEvent methodsFor: 'dispatching' stamp: 'jmv 1/5/2021 11:39:39'!
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
						^ self sentTo: aMorph localPosition: positionInAMorph ]].
	^#rejected! !


!DropFilesEvent methodsFor: 'dispatching' stamp: 'jmv 1/5/2021 11:40:08'!
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
		ifTrue: [^ self sentTo: aMorph localPosition: positionInAMorph ].
	
	^#rejected! !


!KeyboardEvent methodsFor: 'actions' stamp: 'jmv 1/5/2021 11:39:47'!
closeCurrentWindowOf: aMorph

	aMorph owningWindow ifNotNil: [ :w |
		(w containsGlobalPoint: position)
			ifTrue: [ w delete ] ].! !


!MouseButtonEvent methodsFor: 'dispatching' stamp: 'jmv 1/5/2021 11:45:08'!
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
			ifTrue: [ ^self sentTo: aMorph localPosition: positionInAMorph ]
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


!MouseScrollEvent methodsFor: 'dispatching' stamp: 'jmv 1/5/2021 11:45:22'!
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
		aMorphHandlesIt ifTrue: [ ^ self
				sentTo: aMorph
				localPosition: positionInAMorph ]].
	handledByInner ifTrue: [ ^ self ].
	"Mouse was not on aMorph nor any of its children"
	^ #rejected.! !

!methodRemoval: Morph #fullContainsPoint: stamp: 'jmv 1/5/2021 11:45:41'!
Morph removeSelector: #fullContainsPoint:!
