'From Cuis7.5 [latest update: #7522] on 5 September 2025 at 12:30:01 pm'!

!Morph methodsFor: 'submorphs-accessing' stamp: 'jmv 9/5/2025 11:55:18'!
topMorphAt: worldPoint
	"Find, if any a (possibly nested) submorph that #includesPixel: worldPoint,
	and is not covered by any other morph. Answer could also be self.
	Answer nil if neither us nor any submorph #includesPixel: worldPoint.
	Note: do not include not #visible morphs (see #includesPixel:)."

	| doWeIncludePixel |
	doWeIncludePixel := self includesPixel: worldPoint.
	(doWeIncludePixel or: [ self submorphsMightProtrude and: [self clipsSubmorphs not]]) ifTrue: [
		submorphs do: [ :m |
			(m topMorphAt: worldPoint) ifNotNil: [ :sm | ^sm ]]].
	doWeIncludePixel ifTrue: [ ^self ].
	^nil! !

!Morph methodsFor: 'submorphs-accessing' stamp: 'jmv 9/5/2025 12:27:08'!
topUnlockedMorphAt: worldPoint
	"Find, if any a (possibly nested) submorph that #includesPixel: worldPoint,
	and is not covered by any other morph. Answer could also be self.
	Answer nil if neither us nor any submorph #includesPixel: worldPoint.
	Note: do not include not #visible morphs (see #includesPixel:)."

	| doWeIncludePixel |
	self isLocked ifTrue: [ ^nil ].
	doWeIncludePixel := self includesPixel: worldPoint.
	(doWeIncludePixel or: [ self submorphsMightProtrude and: [self clipsSubmorphs not]]) ifTrue: [
		submorphs do: [ :m |
			(m topUnlockedMorphAt: worldPoint) ifNotNil: [ :sm | ^sm ]]].
	doWeIncludePixel ifTrue: [ ^self ].
	^nil! !


!DropEvent methodsFor: 'dispatching' stamp: 'jmv 9/5/2025 12:27:36'!
dispatchIn: aWorldMorph
	"Drop is done on the innermost target that accepts it.
	If #rejectsEvent:, any party doesn't want the drop,or #wasHandled not by any reason, try with the owner."

	| handlerMorph dropped |
	handlerMorph := aWorldMorph topUnlockedMorphAt: position.
	[ handlerMorph notNil and: [wasHandled not]] whileTrue: [
		((handlerMorph rejectsEvent: self) not and: [handlerMorph includesPixel: position]) ifTrue: [
			
			handlerMorph allowsMorphDrop ifTrue: [
				"Do a symmetric check if both morphs like each other"
				dropped := self contents.
				((handlerMorph wantsDroppedMorph: dropped event: self)		"I want her"
					and: [dropped wantsToBeDroppedInto: handlerMorph])		"she wants me"
						ifTrue: [ self sendEventTo: handlerMorph ]].
			wasHandled
				ifFalse: [
					handlerMorph invalidDrop: self.
					(self contents is: #DraggingGuideMorph) ifTrue: [
						self wasHandled: true.
						self contents delete ]
					]
			
		].
		handlerMorph := handlerMorph owner.
	].! !


!DropFilesEvent methodsFor: 'dispatching' stamp: 'jmv 9/5/2025 12:27:44'!
dispatchIn: aWorldMorph
	"Drop is done on the innermost target that accepts it."

	| handlerMorph |
	handlerMorph := aWorldMorph topUnlockedMorphAt: position.
	[ handlerMorph notNil and: [wasHandled not]] whileTrue: [
		((handlerMorph rejectsEvent: self) not and: [handlerMorph includesPixel: position]) ifTrue: [
			
			handlerMorph allowsFilesDrop ifTrue: [
				self sendEventTo: handlerMorph ].
			
		].
		handlerMorph := handlerMorph owner.
	].! !


!MouseEvent methodsFor: 'dispatching' stamp: 'jmv 9/5/2025 12:28:07'!
dispatchIn: aWorldMorphOrFocusedMorph
	"Dispatch me.
	The event will be passed to the front-most visible submorph that contains the event position.
	Search usually starts with the WorldMorph, but it could be another morph if it is the hand's #mouseFocus.
	If #rejectsEvent:, or #wasHandled not by any reason, try with the owner."

	| handlerMorph |
	handlerMorph := aWorldMorphOrFocusedMorph topUnlockedMorphAt: position.
	[ handlerMorph notNil and: [self wasHandled not]] whileTrue: [
		((handlerMorph rejectsEvent: self) not and: [handlerMorph includesPixel: position]) ifTrue: [
			
			self sendEventTo: handlerMorph.
			
		].
		handlerMorph := handlerMorph owner.
	].! !


!MouseButtonEvent methodsFor: 'dispatching' stamp: 'jmv 9/5/2025 12:28:11'!
dispatchIn: aWorldMorphOrFocusedMorph
	"Dispatch me.
	The event will be passed to the front-most visible submorph that contains the event position.
	Search usually starts with the WorldMorph, but it could be another morph if it is the hand's #mouseFocus.
	If #rejectsEvent:, or #wasHandled not by any reason, try with the owner."
	
	| handlerMorph grabTheMorph |
	
	"Only for MouseDown"
	self isMouseDown ifFalse: [
		^super dispatchIn: aWorldMorphOrFocusedMorph ].

	handlerMorph := aWorldMorphOrFocusedMorph topUnlockedMorphAt: position.
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


!MouseScrollEvent methodsFor: 'dispatching' stamp: 'jmv 9/5/2025 12:28:14'!
dispatchIn: aWorldMorphOrFocusedMorph
	"Dispatch me.
	The event will be passed to the front-most visible submorph that contains the event position.
	Search usually starts with the WorldMorph, but it could be another morph if it is the hand's #mouseFocus.
	If #rejectsEvent:, or #wasHandled not by any reason, try with the owner."

	| handlerMorph |
	handlerMorph := aWorldMorphOrFocusedMorph topUnlockedMorphAt: position.
	[ handlerMorph notNil and: [self wasHandled not]] whileTrue: [
		((handlerMorph rejectsEvent: self) not and: [handlerMorph includesPixel: position]) ifTrue: [
			
			(handlerMorph handlesMouseScroll: self) ifTrue: [
				self sendEventTo: handlerMorph ].
			
		].
		handlerMorph := handlerMorph owner.
	].! !

!methodRemoval: Morph #morphVisibleAt: stamp: 'jmv 9/5/2025 11:53:03'!
Morph removeSelector: #morphVisibleAt:!
