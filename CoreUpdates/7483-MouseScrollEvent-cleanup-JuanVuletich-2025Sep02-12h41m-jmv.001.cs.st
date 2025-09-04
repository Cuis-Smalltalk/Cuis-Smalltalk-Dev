'From Cuis7.5 [latest update: #7478] on 2 September 2025 at 12:45:45 pm'!
!classDefinition: #MouseScrollEvent category: #'Morphic-Events'!
MouseEvent subclass: #MouseScrollEvent
	instanceVariableNames: 'direction eventHandler '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Events'!

!MouseScrollEvent methodsFor: 'dispatching' stamp: 'jmv 9/2/2025 12:44:22'!
dispatchWith: aMorph
	"Find the appropriate receiver for the event and let it handle it. Default rules:
	* The top-most chain of visible, unlocked morphs containing the event position will get a chance to handle the event.
	* When travelling down the hierarchy a prospective handler for the event is installed. This prospective handler can be used by submorphs wishing to handle the mouse down for negotiating who the receiver is.
	* When travelling up, the prospective handler is always executed. The handler needs to check if the event was handled before as well as checking if somebody else's handler has been installed.
	* If another handler has been installed but the event was not handled it means that somebody up in the hierarchy wants to handle the event.
	"
	"Try to get out quickly"
	| aMorphHandlesIt handledByInner |
	"FIXME - this works in all tested cases but one: when the window directly under the mouse doesn't have keyboard focus (i.e. a Transcript window)"
	(aMorph fullIncludesPixel: position) ifFalse: [ ^ #rejected ].
	"Install the prospective handler for the receiver"
	aMorphHandlesIt := false.
	(aMorph handlesMouseScroll: self) ifTrue: [
		aMorphHandlesIt := true ].
	"Now give submorphs a chance to handle the event"
	handledByInner := false.
	aMorph submorphsDo: [ :eachChild |
		handledByInner ifFalse: [
			(eachChild dispatchEvent: self) == #rejected ifFalse: [
				"Some child did contain the point so aMorph is part of the top-most chain."
				handledByInner := true ]]].
	(handledByInner or: [
		 (aMorph rejectsEvent: self) not ]) ifTrue: [
		"aMorph is in the top-most unlocked, visible morph in the chain."
		aMorphHandlesIt ifTrue: [ ^ self sendEventTo: aMorph ]].
	handledByInner ifTrue: [ ^ self ].
	"Mouse was not on aMorph nor any of its children"
	^ #rejected.! !

!classDefinition: #MouseScrollEvent category: #'Morphic-Events'!
MouseEvent subclass: #MouseScrollEvent
	instanceVariableNames: 'direction'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Events'!
