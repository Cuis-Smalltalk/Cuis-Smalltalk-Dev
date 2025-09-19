'From Cuis7.5 [latest update: #7589] on 19 September 2025 at 2:23:10 pm'!

!DropEvent methodsFor: 'dispatching' stamp: 'jmv 9/19/2025 14:22:36'!
dispatchIn: aWorldMorph
	"Drop is done on the innermost target that accepts it.
	If #rejectsEvent:, any party doesn't want the drop,or #wasHandled not by any reason, try with the owner."

	| topUnlockedMorph handlerMorph dropped |
	topUnlockedMorph := aWorldMorph topUnlockedMorphAt: position.
	handlerMorph := topUnlockedMorph.
	[ handlerMorph notNil and: [wasHandled not]] whileTrue: [
		((handlerMorph rejectsEvent: self) not and: [handlerMorph fullIncludesPixel: position]) ifTrue: [
			handlerMorph allowsMorphDrop ifTrue: [
				"Do a symmetric check if both morphs like each other"
				dropped := self contents.
				((handlerMorph wantsDroppedMorph: dropped event: self)		"I want her"
					and: [dropped wantsToBeDroppedInto: handlerMorph])		"she wants me"
						ifTrue: [ self sendEventTo: handlerMorph ]].
		].
		handlerMorph := handlerMorph owner.
	].
	wasHandled
		ifFalse: [
			topUnlockedMorph invalidDrop: self.
			(self contents is: #DraggingGuideMorph) ifTrue: [
				self wasHandled: true.
				self contents delete ]
			].! !


!DropFilesEvent methodsFor: 'dispatching' stamp: 'jmv 9/19/2025 14:22:32'!
dispatchIn: aWorldMorph
	"Drop is done on the innermost target that accepts it."

	| handlerMorph |
	handlerMorph := aWorldMorph topUnlockedMorphAt: position.
	[ handlerMorph notNil and: [wasHandled not]] whileTrue: [
		((handlerMorph rejectsEvent: self) not and: [handlerMorph fullIncludesPixel: position]) ifTrue: [
			
			handlerMorph allowsFilesDrop ifTrue: [
				self sendEventTo: handlerMorph ].
			
		].
		handlerMorph := handlerMorph owner.
	].! !


!MouseEvent methodsFor: 'dispatching' stamp: 'jmv 9/19/2025 14:22:20'!
dispatchIn: aWorldMorphOrFocusedMorph
	"Dispatch me.
	The event will be passed to the front-most visible submorph that contains the event position.
	Search usually starts with the WorldMorph, but it could be another morph if it is the hand's #mouseFocus.
	If #rejectsEvent:, or #wasHandled not by any reason, try with the owner."

	| handlerMorph |
	handlerMorph := aWorldMorphOrFocusedMorph topUnlockedMorphAt: position.
	[ handlerMorph notNil and: [self wasHandled not]] whileTrue: [
		((handlerMorph rejectsEvent: self) not and: [handlerMorph fullIncludesPixel: position]) ifTrue: [
			self sendEventTo: handlerMorph ].
		handlerMorph := handlerMorph owner ].! !


!MouseButtonEvent methodsFor: 'dispatching' stamp: 'jmv 9/19/2025 14:22:16'!
dispatchIn: aWorldMorphOrFocusedMorph
	"Dispatch me.
	The event will be passed to the front-most visible submorph that contains the event position.
	Search usually starts with the WorldMorph, but it could be another morph if it is the hand's #mouseFocus.
	If #rejectsEvent:, or #wasHandled not by any reason, try with the owner."
	
	| topUnlockedMorph handlerMorph grabTheMorph |
	
	"Only for Button 1 and Button 2 MouseDown"
	self isMouseDown ifFalse: [
		^super dispatchIn: aWorldMorphOrFocusedMorph ].
	self mouseButton3Pressed ifTrue: [
		^super dispatchIn: aWorldMorphOrFocusedMorph ].

	topUnlockedMorph := aWorldMorphOrFocusedMorph topUnlockedMorphAt: position.
	handlerMorph := topUnlockedMorph.
	[ handlerMorph notNil and: [self wasHandled not]] whileTrue: [
		((handlerMorph rejectsEvent: self) not and: [handlerMorph fullIncludesPixel: position]) ifTrue: [
			
			"handlerMorph is in the owner chain of th top-most unlocked, visible morph."
			(handlerMorph handlesMouseDown: self)
				ifTrue: [
					self mouseButton2Pressed ifTrue: [
						"Don't propagate mouse button 2 up to the world.
						Getting the World menu when you right click on a morph is something you never want."
						(handlerMorph isWorldMorph and: [topUnlockedMorph isWorldMorph not]) ifTrue: [^self]].
					self sendEventTo: handlerMorph ]
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


!MouseOverEvent methodsFor: 'dispatching' stamp: 'jmv 9/19/2025 14:22:09'!
dispatchIn: aWorldMorphOrFocusedMorph
	"Dispatch me.
	The event will be passed to all morphs at event position, in strict front to back order.
	This event is not marked with `wasHandled: true`, so it is passed to multiple recipients
	across the owner chain.."

	| handlerMorph |
	handlerMorph := aWorldMorphOrFocusedMorph topUnlockedMorphAt: position.
	[ handlerMorph notNil ] whileTrue: [
		((handlerMorph rejectsEvent: self) not and: [handlerMorph fullIncludesPixel: position]) ifTrue: [
			self sendEventTo: handlerMorph ].
		"If we call #morphBehindAt: instead, MouseEnterEvent and MouseLeaveEvent are sent to morphs
		unrelated to the #topUnlockedMorphAt:, but that happen to be behind it."
		"handlerMorph := handlerMorph morphBehindAt: position."
		handlerMorph := handlerMorph owner ].! !


!Morph methodsFor: 'geometry services' stamp: 'jmv 9/19/2025 14:12:44'!
includesPixel: worldPoint
	"Answer true if the grabbing from the pixel argument pixel with the mouse should grab us:

	- Answer true if we own the pixel, i.e. we are the last morph drawn at worldPoint.

	- Some implementations (BoxMorph) may also answer true if we cover but don't own the pixel,
	meaning that some other morph was drawn later, covering us.

	- For other morphs, not BoxMorph, (i.e. they run this implementation), if VectorGraphics is not active,
	answer true for any point that lies within our bounds. If this is the case, consider using VectorGraphics: In addition to
	many other advantages, this method will always answer a strictly correct answer.

	Therefore, senders need to be aware that the answer is meaningful only when it is known that no other morph
	covers us. A common case is to find the topmost morph at some position. In that case, iterating front to back ensures
	that if any other morph covers us, it is found first.

	Note: This method is redefined by some subclasses. This implementation is only used for morphs drawn by
	VectorCanvas, either because the main canvas is a VectorCanvas, or because it is a HybridCanvas and we answer
	true to #requiresVectorCanvas.
	(See other implementors)

	Note: Also see #fullIncludesPixel: #ownsPixel: and #coversPixel:"

	"Check if #ownsPixel:, if possible."
	self mainCanvas ifNotNil: [ :mainCanvas |
		mainCanvas canDoVectorGraphics ifTrue: [
			^ (mainCanvas morphIdAt: worldPoint) = self morphId ]].

	^ false.! !

!Morph methodsFor: 'submorphs-accessing' stamp: 'jmv 9/19/2025 14:21:57'!
morphBehindAt: worldPoint
	"Answer the morph that
		- is behind us
		- covers worldPoint
		- is not locked, and no owner in the chain is
		- is visible, and all owners in the chain are
		- there's no other morph covering that point in between.
	Answer could be a sibling, a child of a sibling, our owner, some other ancestor, or some unrelated morph.
	Answer is nil only for the World."
	| top |
	owner ifNil: [
		^nil ].
	owner submorphsBehind: self do: [ :sibling |
		top := sibling topUnlockedMorphAt: worldPoint.
		top ifNotNil: [
			^top ]].
	(owner fullIncludesPixel: worldPoint)
		ifTrue: [
			^owner ].
	^owner morphBehindAt: worldPoint! !

!Morph methodsFor: 'submorphs-accessing' stamp: 'jmv 9/19/2025 14:20:50'!
topMorphAt: worldPoint
	"Find, if any a (possibly nested) submorph that #includesPixel: worldPoint,
	and is not covered by any other morph. Answer could also be self.
	Answer nil if neither us nor any submorph #includesPixel: worldPoint.
	Note: do not include not #visible morphs (see #includesPixel:)."

	| doWeIncludePixel |
	doWeIncludePixel := self fullIncludesPixel: worldPoint.
	(doWeIncludePixel or: [ self submorphsMightProtrude and: [self clipsSubmorphs not]]) ifTrue: [
		submorphs do: [ :m |
			(m topMorphAt: worldPoint) ifNotNil: [ :sm | ^sm ]]].
	doWeIncludePixel ifTrue: [ ^self ].
	^nil! !

!Morph methodsFor: 'submorphs-accessing' stamp: 'jmv 9/19/2025 14:19:14'!
topUnlockedMorphAt: worldPoint
	"Find, if any a (possibly nested) submorph that #includesPixel: worldPoint,
	and is not covered by any other morph. Answer could also be self.
	Answer nil if neither us nor any submorph #includesPixel: worldPoint.
	Note: do not include not #visible morphs (see #includesPixel:)."

	| doWeIncludePixel |
	self isLocked ifTrue: [ ^nil ].
	doWeIncludePixel := self fullIncludesPixel: worldPoint.
	(doWeIncludePixel or: [ self submorphsMightProtrude and: [self clipsSubmorphs not]]) ifTrue: [
		submorphs do: [ :m |
			(m topUnlockedMorphAt: worldPoint) ifNotNil: [ :sm | ^sm ]]].
	doWeIncludePixel ifTrue: [ ^self ].
	^nil! !

