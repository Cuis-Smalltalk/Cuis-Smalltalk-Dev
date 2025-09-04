'From Cuis7.5 [latest update: #7472] on 1 September 2025 at 2:48:08 pm'!
!classDefinition: #MouseButtonEvent category: #'Morphic-Events'!
MouseEvent subclass: #MouseButtonEvent
	instanceVariableNames: 'whichButton eventHandler '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Events'!

!MouseButtonEvent methodsFor: 'dispatching' stamp: 'jmv 9/1/2025 13:00:02'!
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
	(aMorph fullIncludesPixel: position)
		ifFalse: [ ^ #rejected ].

	"Install the prospective handler for the receiver"
	aMorphHandlesIt := self mouseButton3Pressed or: [aMorph handlesMouseDown: self].
	grabAMorph := false.
	"If button 1, and both aMorph and the owner allows grabbing with the hand (to initiate drag & drop), so be it."
	self mouseButton1Pressed ifTrue: [
		aMorph owner ifNotNil: [ :o |
			(o allowsSubmorphDrag and: [ aMorph isSticky not ]) ifTrue: [
				grabAMorph := true ]]].

	"Now give submorphs a chance to handle the event"
	handledByInner := false.
	aMorph submorphsDo: [ :eachChild |
		handledByInner ifFalse: [
			(eachChild dispatchEvent: self) == #rejected ifFalse: [
				"Some child did contain the point so aMorph is part of the top-most chain."
				handledByInner := true ]]].

	(handledByInner or: [ (aMorph rejectsEvent: self) not and: [ aMorph fullIncludesPixel: position ] ]) ifTrue: [
		"aMorph is in the top-most unlocked, visible morph in the chain."
		aMorphHandlesIt
			ifTrue: [ ^self sendEventTo: aMorph ]
			ifFalse: [
				(grabAMorph and: [ handledByInner not ]) ifTrue: [
					self hand
						waitForClicksOrDrag: aMorph event: self
						dragSel: ((Preferences at: #clickGrabsMorphs) ifFalse: [#dragEvent:localPosition:])
						clkSel: ((Preferences at: #clickGrabsMorphs) ifTrue: [#dragEvent:localPosition:]).
					"false ifTrue: [ self hand grabMorph: aMorph ]."
					(Preferences at: #clickGrabsMorphs) ifFalse: [
						self shiftPressed
							ifTrue: [ (aMorph is: #SystemWindow) ifTrue: [ aMorph sendToBack ] ]
							ifFalse: [ aMorph activateWindow ] ].
					self wasHandled: true.
					^ self ]]].

	handledByInner ifTrue: [ ^ self ].
	"Mouse was not on aMorph nor any of its children"
	^ #rejected! !


!Morph methodsFor: 'events' stamp: 'jmv 9/1/2025 12:53:42'!
mouseButton3Down: aMouseButtonEvent localPosition: localEventPosition

	self root addHalo: aMouseButtonEvent.! !

!classDefinition: #MouseButtonEvent category: #'Morphic-Events'!
MouseEvent subclass: #MouseButtonEvent
	instanceVariableNames: 'whichButton'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Events'!
