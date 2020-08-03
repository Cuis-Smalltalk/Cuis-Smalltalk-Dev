'From Cuis 5.0 [latest update: #4258] on 17 July 2020 at 5:15:13 pm'!

!Morph methodsFor: 'events-processing' stamp: 'jmv 7/13/2020 16:20:20'!
rejectsEvent: aMorphicEvent
	"Return true to reject the given event. Submorphs might still handle it."

	^ false! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 7/13/2020 16:07:54'!
rejectsEventFully: aMorphicEvent
	"Return true to reject the given event, for thereceiver and all submorphs."

	^ self isLocked or: [ self visible not ]! !


!HaloMorph methodsFor: 'events-processing' stamp: 'jmv 7/13/2020 16:20:15'!
rejectsEvent: aMorphicEvent
	"Return true to reject the given event. Submorphs might still handle it."

	"Only mouseButton3 events are handled by the halo itself"
	(aMorphicEvent isMouse and: [
		aMorphicEvent isMouseDown and: [ aMorphicEvent mouseButton3Pressed ]])
	ifTrue: [
		^ false ].
	^true! !

!HaloMorph methodsFor: 'events-processing' stamp: 'jmv 7/13/2020 16:08:07'!
rejectsEventFully: anEvent
	"Return true to reject the given event, for thereceiver and all submorphs."

	(super rejectsEventFully: anEvent) ifTrue: [^true].
	anEvent isDropEvent ifTrue: [^true]. "never attempt to drop on halos"
	^false! !


!Morph methodsFor: 'events-processing' stamp: 'jmv 7/13/2020 16:08:20'!
dispatchEvent: aMorphicEvent localPosition: localPosition
	"This is the central entry for dispatching events in morphic. Given some event, find the right receiver and let him handle it.
	localPosition is in our coordinates."

	^ (self rejectsEventFully: aMorphicEvent)
		ifTrue: [ #rejected ]
		ifFalse: [ aMorphicEvent dispatchWith: self localPosition: localPosition ]! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 7/13/2020 16:22:57'!
processMouseOver: aMouseEvent localPosition: localEventPosition
	"System level event handling."
	 self hasMouseFocus ifTrue: [
		"Got this directly through #handleFocusEvent: so check explicitly"
		((self rejectsEvent: aMouseEvent) not and: [self fullContainsPoint: localEventPosition]) ifFalse: [
			^self ]].
	aMouseEvent hand noticeMouseOver: self event: aMouseEvent.
	"Open question: should any unhandled mouse move events be filtered out? (i.e. should mouseHover:localPosition: be called when a mouse button is pressed but the morph doesn't have mouse button handlers?  Essentially, what are the limits of what is considered 'hovering'?"
	(self handlesMouseHover and: [aMouseEvent wasHandled not]) ifTrue: [
		self
			mouseHover: aMouseEvent
			localPosition: localEventPosition ].! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 7/13/2020 12:06:02'!
fullContainsPoint: aLocalPoint

	"If not visible, won't contain any point at all."
	self visible ifFalse: [ ^false ].

	(self morphContainsPoint: aLocalPoint) ifTrue: [ ^ true ].  "quick acceptance"
	self submorphsDrawingOutsideReverseDo: [ :m |
		(m fullContainsPoint: (m internalize: aLocalPoint)) ifTrue: [ ^ true ]].
	^ false! !


!MorphicEvent methodsFor: 'dispatching' stamp: 'jmv 7/13/2020 16:23:09'!
dispatchWith: aMorph localPosition: positionInAMorph
	"Dispatch me. The event will be passed to the front-most visible submorph that contains the position wrt. to the event."
	| handledByInner eventPositionInChild |

	"Try to get out quickly"
	(aMorph fullContainsPoint: positionInAMorph)
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
	(handledByInner or: [ (aMorph rejectsEvent: self) not and: [aMorph fullContainsPoint: positionInAMorph] ])
		ifTrue: [ ^ self sentTo: aMorph localPosition: positionInAMorph ].

	^ #rejected! !


!DropEvent methodsFor: 'dispatching' stamp: 'jmv 7/13/2020 16:21:40'!
dispatchWith: aMorph localPosition: positionInAMorph
	"Drop is done on the innermost target that accepts it."
	| eventPositionInChild dropped |

	"Try to get out quickly"
	(aMorph fullContainsPoint: positionInAMorph)
		ifFalse: [ ^#rejected ].

	"Go looking if any of our submorphs wants it"
	aMorph submorphsDo: [ :eachChild |
		eventPositionInChild _ eachChild internalize: positionInAMorph.
		(eachChild dispatchEvent: self localPosition: eventPositionInChild) == #rejected ifFalse: [
			^self ]].

	(aMorph allowsMorphDrop and: [ (aMorph rejectsEvent: self) not and: [aMorph fullContainsPoint: positionInAMorph] ])
		ifTrue: [
			"Do a symmetric check if both morphs like each other"
			dropped _ self contents.
			((aMorph wantsDroppedMorph: dropped event: self)	"I want her"
				and: [dropped wantsToBeDroppedInto: aMorph])		"she wants me"
					ifTrue: [
						^ self sentTo: aMorph localPosition: positionInAMorph ]].
	^#rejected! !


!DropFilesEvent methodsFor: 'dispatching' stamp: 'jmv 7/13/2020 16:22:06'!
dispatchWith: aMorph localPosition: positionInAMorph
	"Drop is done on the innermost target that accepts it."

	| eventPositionInChild |

	"Try to get out quickly"
	(aMorph fullContainsPoint: positionInAMorph) ifFalse: [ ^#rejected ].

	"Go looking if any of our submorphs wants it"
	aMorph submorphsDo: [ :eachChild |
		eventPositionInChild _ eachChild internalize: positionInAMorph.
		(eachChild dispatchEvent: self localPosition: eventPositionInChild) == #rejected ifFalse: [ ^self ]].

	(aMorph allowsFilesDrop and: [ (aMorph rejectsEvent: self) not and: [aMorph fullContainsPoint: positionInAMorph] ])
		ifTrue: [^ self sentTo: aMorph localPosition: positionInAMorph ].
	
	^#rejected! !


!MouseButtonEvent methodsFor: 'dispatching' stamp: 'jmv 7/13/2020 16:23:25'!
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
	(aMorph fullContainsPoint: positionInAMorph)
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

	(handledByInner or: [ (aMorph rejectsEvent: self) not and: [aMorph fullContainsPoint: positionInAMorph] ]) ifTrue: [
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


!MouseScrollEvent methodsFor: 'dispatching' stamp: 'jmv 7/13/2020 16:23:48'!
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
		 (aMorph rejectsEvent: self) not and: [aMorph fullContainsPoint: positionInAMorph]]) ifTrue: [
		"aMorph is in the top-most unlocked, visible morph in the chain."
		aMorphHandlesIt ifTrue: [ ^ self
				sentTo: aMorph
				localPosition: positionInAMorph ]].
	handledByInner ifTrue: [ ^ self ].
	"Mouse was not on aMorph nor any of its children"
	^ #rejected.! !

!methodRemoval: HaloMorph #containsPoint:event: stamp: 'jmv 7/17/2020 17:14:45'!
HaloMorph removeSelector: #containsPoint:event:!
!methodRemoval: Morph #containsPoint:event: stamp: 'jmv 7/17/2020 17:14:47'!
Morph removeSelector: #containsPoint:event:!
