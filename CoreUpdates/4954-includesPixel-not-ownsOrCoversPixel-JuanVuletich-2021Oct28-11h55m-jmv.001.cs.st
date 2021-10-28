'From Cuis 5.0 [latest update: #4953] on 28 October 2021 at 12:20:10 pm'!

!Morph methodsFor: 'geometry services' stamp: 'jmv 10/28/2021 12:00:05'!
fullIncludesPixel: worldPoint
	"Answer true if worldPoint is in some submorph, even if not inside our shape.
	See comment at #includesPixel: for important notes on behavior."

	(self includesPixel: worldPoint) ifTrue: [ ^ true ].
	self submorphsMightProtrude ifTrue: [
		self submorphsDo: [ :m |
			(m fullIncludesPixel: worldPoint) ifTrue: [ ^ true ]]].
	^ false.! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 10/28/2021 12:18:50'!
includesPixel: worldPoint
	"Answer true if we own the pixel, i.e. we are the last morph drawn at worldPoint.

	Some implementations (KernelMorph and WidgetMorph) may also answer true if we cover but don't own the pixel,
	meaning that some other morph was drawn later, covering us.

	For other morphs, not KernelMorph or WidgetMorph, (i.e. they run this implementation), if VectorGraphics is not active,
	answer true for any point that lies within our bounds. If this is the case, consider using VectorGraphics: In addition to
	many other advantages, this method will always answer a strictly correct answer.

	Therefore, senders need to be aware that the answer is meaningful only when it is known that no other morph
	covers us. A common case is to find the topmost morph at some position. In that case, iterating front to back ensures
	that if any other morph covers us, it is found first.

	Note: This method is redefined by some subclasses. This implementation is only used for morphs drawn by VectorCanvas,
	either because the main canvas is a VectorCanvas, or because it is a HybridCanvas and we answer true to #requiresVectorCanvas.
	(See other implementors)

	Note: Also see #ownsPixel: and #coversPixel:"

	"Check if #ownsPixel:, if possible."
	self canvas ifNotNil: [ :canvas |
		canvas canDoVectorGraphics ifTrue: [
			^ (canvas morphIdAt: worldPoint) = self morphId ]].

	"This is cheap and OK for unrotated Morphs with a rectangular shape.
	In general, the answer is not strictly correct. (There are false positives).
	WidgetMorph redefines this method with an equally cheap implementation that can also handle
	rotated morphs (but assuming they are of rectangular shape)."
	self visible ifTrue: [
		self basicDisplayBounds ifNotNil: [ :r |
			^r containsPoint: worldPoint ]].

	^ false.! !


!KernelMorph methodsFor: 'geometry services' stamp: 'jmv 10/28/2021 11:57:23'!
includesPixel: worldPoint
	"Answer true if we own the pixel, i.e. we are the last morph drawn at worldPoint. (See implementation at Morph).
	This implementation also answer true if we cover but don't own the pixel,
	meaning that some other morph was drawn later, covering us.
	Therefore, senders need to be aware that the answer is meaningful only when it is known that no other morph covers us.
	Note: This implementation is only used for morphs with a cheap #coversPixel:.
	(See other implementors)
	Note: Also see #ownsPixel: and #coversPixel:"

	^ self visible and: [self coversPixel: worldPoint].! !


!WidgetMorph methodsFor: 'geometry services' stamp: 'jmv 10/28/2021 11:57:27'!
includesPixel: worldPoint
	"Answer true if we own the pixel, i.e. we are the last morph drawn at worldPoint. (See implementation at Morph).
	This implementation also answer true if we cover but don't own the pixel,
	meaning that some other morph was drawn later, covering us.
	Therefore, senders need to be aware that the answer is meaningful only when it is known that no other morph covers us.
	Note: This implementation is only used for morphs with a cheap #coversPixel:.
	(See other implementors)
	Note: Also see #ownsPixel: and #coversPixel:"

	^ self visible and: [self coversPixel: worldPoint].! !


!Morph methodsFor: 'events-processing' stamp: 'jmv 10/28/2021 11:58:38'!
processMouseOver: aMouseEvent localPosition: localEventPosition
	"System level event handling."
	 self hasMouseFocus ifTrue: [
		"Got this directly through #handleFocusEvent: so check explicitly"
		((self rejectsEvent: aMouseEvent) not and: [self fullIncludesPixel: aMouseEvent eventPosition]) ifFalse: [
			^self ]].
	aMouseEvent hand noticeMouseOver: self event: aMouseEvent.
	"Open question: should any unhandled mouse move events be filtered out? (i.e. should mouseHover:localPosition: be called when a mouse button is pressed but the morph doesn't have mouse button handlers?  Essentially, what are the limits of what is considered 'hovering'?"
	(self handlesMouseHover and: [aMouseEvent wasHandled not]) ifTrue: [
		self
			mouseHover: aMouseEvent
			localPosition: localEventPosition ].! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 10/28/2021 12:17:08'!
coversAnyPixelCloserThan: maxDistance to: worldPoint
	"Answer true if our closest point to worldPoint is less than aNumber pixels away.
	In target surface (i.e. Display) coordinates.
	See #bitMask.
	Remember to do
	self removeProperty: #bitMask.
	when appropriate!! (i.e. when we change in such a way to make the bitMask invalid).

	Note: Requires VectorGraphics. Meant to be used only when needed.
	Note: Prefer #isCloserThan:toPixel:, that doesn't use #bitMask, and doesn't require maintenance."

	| center maxDistanceSquared |
	self visible ifFalse: [
		^false ].
	privateDisplayBounds ifNil: [
		^false ].
	center _ privateDisplayBounds center.
	"Quick checks: If not even within aNumber distance to display bounds, fail"
	(center y - worldPoint y) abs < (privateDisplayBounds height  // 2 + maxDistance) ifFalse: [
		^false ].
	(center x - worldPoint x) abs < (privateDisplayBounds width // 2 + maxDistance) ifFalse: [
		^false ].
	"Precise check with bitMask. If unavailable, just answer false. See #coversPixel:"
	(self coversPixel: worldPoint)
		ifNil: [ ^false ]
		ifNotNil: [ :coversIt |
			coversIt ifTrue: [ ^true ]].
	maxDistanceSquared _ maxDistance squared.
	maxDistance negated to: maxDistance do: [ :dy |
		maxDistance negated to: maxDistance do: [ :dx |
			dx squared + dy squared <= maxDistanceSquared ifTrue: [
				(self coversPixel: worldPoint + (dx@dy)) ifTrue: [ ^true ]]]].
	^false.! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 10/28/2021 12:15:19'!
coversPixel: worldPoint
	"Answer true if pixel worldPoint is covered by us, even if we are not visible a it because of some
	other morph above us also covers it.

	WARNING: Might answer nil when we simply don't know. It is generally advisable to call this method only for
	morphs where the answer is not nil:
		- WidgetMorphs. No sepecial care needed. (Senders in the base Cuis image are of this kind).
		- KernelMorphs. No special care needed. (Senders in the base Cuis image are of this kind).
		- Morphs drawn by VectorCanvas (i.e. they #requiresVectorCanvas). See notes below.
	
	Note: If your morph #requiresVectorCanvas, and depends on this general implementation, remember to 
	`removeProperty: #bitMask` whenever it becomes invalid due to changes in your morphs. You may consider
	using #ownsPixel: if appropriate, that doesn't require any maintenance and is cheaper (in cpu and memory).
	See #bitMask.

	See #ownsPixel:
	See #includesPixel:"

	self visible ifTrue: [
		"Expensive way for morphs with arbitrary shape in VectorGraphics.
		The base Cuis System doesn't use this.
		Use this in applications if the expense of maintaining #bitMask is worth it."
		self canvas ifNotNil: [ :canvas |
			canvas canDoVectorGraphics ifTrue: [
				^(self bitMask pixelValueAt: worldPoint - self displayFullBounds topLeft) = 1 ]].

		"This is cheap and OK for unrotated Morphs with a rectangular shape.
		In general, the answer is not strictly correct.
		WidgetMorph redefines this method with an equally cheap implementation that can also handle
		rotated morphs (but assuming they are of rectangular shape)."
		self basicDisplayBounds ifNotNil: [ :r |
			(r containsPoint: worldPoint) ifFalse: [ ^false ] ]].

	"If we don't know."
	^ nil.! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 10/28/2021 12:00:12'!
ownsPixel: worldPoint
	"Answer true if we own the pixel, i.e. we are the last morph drawn at worldPoint.
	Requires VectorGraphics, as we need VectorCanvas' morphIdAt: service.

	Valid if running on VectorCanvas.

	In case of running on HybridCanvas, this method is only valid for morphs that
	are drawn by VectorCanvas (i.e. they answer true to #requiresVectorCanvas).

	See #coversPixel:
	See #includesPixel:"

	self canvas ifNotNil: [ :canvas |
		canvas canDoVectorGraphics ifTrue: [
			^ (canvas morphIdAt: worldPoint) = self morphId ]].
	^ false.! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 10/28/2021 11:58:42'!
transferHalo: event from: formerHaloOwner
	"Progressively transfer the halo to the next likely recipient"

	formerHaloOwner == self
		ifFalse: [
			self addHalo: event.
			^self ].

	event shiftPressed ifTrue: [
		"Pass it outwards"
		owner ifNotNil: [
			owner transferHalo: event from: formerHaloOwner.
			^self ].
		"We're at the top level; just keep it on ourselves"
		^self ].

	self submorphsDo: [ :m |
		(m wantsHalo and: [ m fullIncludesPixel: event eventPosition ])
			ifTrue: [
				m transferHalo: event from: formerHaloOwner.
				^self ]].
	"We're at the bottom most level; just keep halo on ourselves"! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 10/28/2021 12:03:20'!
doRecolor: event with: aHandle
	"The mouse went down in the 'recolor' halo handle.  Allow the user to change the color of the innerTarget"

	event hand obtainHalo: self.
	(aHandle includesPixel: event eventPosition)
		ifFalse: [  "only do it if mouse still in handle on mouse up"
			self delete.
			target addHalo: event]
		ifTrue: [
			target changeColor]! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 10/28/2021 12:03:24'!
maybeCollapse: event with: aHandle 
	"Ask hand to collapse my target if mouse comes up in it."

	event hand obtainHalo: self.
	(aHandle includesPixel: event eventPosition)
		ifTrue: [
			target collapse ].
	self delete.! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 10/28/2021 12:03:27'!
maybeDismiss: event with: aHandle
	"Ask hand to dismiss my target if mouse comes up in it."

	event hand obtainHalo: self.
	(aHandle includesPixel: event eventPosition)
		ifFalse: [
			self delete.
			target addHalo: event]
		ifTrue: [
			target resistsRemoval ifTrue: [
				(PopUpMenu
					confirm: 'Really throw this away'
					trueChoice: 'Yes'
					falseChoice: 'Um, no, let me reconsider') ifFalse: [^ self]].

			self delete.
			target dismissViaHalo]! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 10/28/2021 12:03:31'!
setDismissColor: event with: aHandle
	"Called on mouseStillDown in the dismiss handle; set the color appropriately."

	| colorToUse |
	event hand obtainHalo: self.
	colorToUse _  (aHandle includesPixel: event eventPosition)
		ifFalse: [ `Color red muchLighter` ]
		ifTrue: [ `Color lightGray` ].
	aHandle color: colorToUse! !


!MenuItemMorph methodsFor: 'events' stamp: 'jmv 10/28/2021 12:03:36'!
activateOwnerMenu: evt
	"Activate our owner menu; e.g., pass control to it"
	owner ifNil: [ ^false ]. "not applicable"
	(owner includesPixel: evt eventPosition)
		ifFalse: [ ^false ].
	owner activate: evt.
	^true! !

!MenuItemMorph methodsFor: 'events' stamp: 'jmv 10/28/2021 12:03:40'!
activateSubmenu: event
	"Activate our submenu; e.g., pass control to it"

	subMenu ifNil: [ ^false ]. "not applicable"
	subMenu isInWorld ifFalse: [ ^false ].	
	(subMenu includesPixel: event eventPosition) ifFalse: [^false].
	subMenu activate: event.
	^true! !


!PluggableButtonMorph methodsFor: 'events' stamp: 'jmv 10/28/2021 12:02:45'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

	isPressed _ false.
	mouseIsOver _ false.
	(actWhen == #buttonUp and: [ 
		self includesPixel: aMouseButtonEvent eventPosition ])
		ifTrue: [ self performAction ].
	self redrawNeeded! !


!MenuMorph methodsFor: 'events' stamp: 'jmv 10/28/2021 12:02:50'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse down event."
	(stayUp or: [ self includesPixel: aMouseButtonEvent eventPosition ]) 
		ifFalse: [
			self deleteIfPopUp: aMouseButtonEvent.
			self activeHand 
				newKeyboardFocus: prevKbdFocus;
				newMouseFocus: prevMouseFocus.
			^ self ]. "click outside"

	"Grab the menu and drag it to some other place
	This is reimplemented here because we handle the event, and if the following lines are commented, a menu can't be grabbed with the hand. This is not nice and shouldn't be needed"
	self isSticky ifTrue: [ ^self ].
	aMouseButtonEvent hand grabMorph: self.! !

!MenuMorph methodsFor: 'events' stamp: 'jmv 10/28/2021 12:02:53'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse up event.
	Note: This might be sent from a modal shell."
	(self includesPixel: aMouseButtonEvent eventPosition) ifFalse:[
		"Mouse up outside. Release eventual focus and delete if pop up."
		aMouseButtonEvent hand ifNotNil: [ :h | h releaseMouseFocus: self ].
		self deleteIfPopUp: aMouseButtonEvent.
		self activeHand 
				newKeyboardFocus: prevKbdFocus;
				newMouseFocus: prevMouseFocus.
		^ self].
	stayUp ifFalse: [
		"Still in pop-up transition; keep focus"
		aMouseButtonEvent hand newMouseFocus: self ].! !


!AutoCompleterMorph methodsFor: 'events' stamp: 'jmv 10/28/2021 12:02:56'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

	(self includesPixel: aMouseButtonEvent eventPosition)
		ifTrue: [
			((self upButtonPosition extent: ScrollBar scrollbarThickness) containsPoint: localEventPosition)
				ifTrue: [ ^self stillActive; goUp ].
			((self downButtonPosition extent: ScrollBar scrollbarThickness) containsPoint: localEventPosition)
				ifTrue: [ ^self stillActive; goDown ].
			self selected: (localEventPosition y // self itemHeight) +  self firstVisible.
			completer insertSelected ]
		ifFalse: [ self delete. completer menuClosed ]! !


!MorphicEvent methodsFor: 'dispatching' stamp: 'jmv 10/28/2021 11:58:51'!
dispatchWith: aMorph
	"Dispatch me. The event will be passed to the front-most visible submorph that contains the position wrt. to the event."
	| handledByInner |

	"Try to get out quickly"
	(aMorph fullIncludesPixel: self eventPosition)
		ifFalse: [ ^#rejected ].

	"Now give submorphs a chance to handle the event"
	handledByInner _ false.
	aMorph submorphsDo: [ :eachChild |
		handledByInner ifFalse: [
			(eachChild dispatchEvent: self) == #rejected ifFalse: [
				"Some child did contain the point so aMorph is part of the top-most chain."
				handledByInner _ true ]]].

	"Check for being inside the receiver"
	(handledByInner or: [ (aMorph rejectsEvent: self) not and: [aMorph fullIncludesPixel: self eventPosition] ])
		ifTrue: [ ^ self sendEventTo: aMorph ].

	^ #rejected! !


!DropEvent methodsFor: 'dispatching' stamp: 'jmv 10/28/2021 11:58:57'!
dispatchWith: aMorph
	"Drop is done on the innermost target that accepts it."
	| dropped |

	"Try to get out quickly"
	(aMorph fullIncludesPixel: position)
		ifFalse: [ ^#rejected ].

	"Go looking if any of our submorphs wants it"
	aMorph submorphsDo: [ :eachChild |
		(eachChild dispatchEvent: self) == #rejected ifFalse: [
			^self ]].

	(aMorph allowsMorphDrop and: [ (aMorph rejectsEvent: self) not and: [aMorph fullIncludesPixel: position] ])
		ifTrue: [
			"Do a symmetric check if both morphs like each other"
			dropped _ self contents.
			((aMorph wantsDroppedMorph: dropped event: self)	"I want her"
				and: [dropped wantsToBeDroppedInto: aMorph])		"she wants me"
					ifTrue: [
						^ self sendEventTo: aMorph ]].
	^#rejected! !


!DropFilesEvent methodsFor: 'dispatching' stamp: 'jmv 10/28/2021 11:59:02'!
dispatchWith: aMorph
	"Drop is done on the innermost target that accepts it."

	"Try to get out quickly"
	(aMorph fullIncludesPixel: position) ifFalse: [ ^#rejected ].

	"Go looking if any of our submorphs wants it"
	aMorph submorphsDo: [ :eachChild |
		(eachChild dispatchEvent: self) == #rejected ifFalse: [ ^self ]].

	(aMorph allowsFilesDrop and: [ (aMorph rejectsEvent: self) not and: [aMorph fullIncludesPixel: position] ])
		ifTrue: [^ self sendEventTo: aMorph ].
	
	^#rejected! !


!KeyboardEvent methodsFor: 'actions' stamp: 'jmv 10/28/2021 12:03:50'!
closeCurrentWindowOf: aMorph

	aMorph owningWindow ifNotNil: [ :w |
		(w includesPixel: position)
			ifTrue: [ w delete ] ].! !


!MouseButtonEvent methodsFor: 'dispatching' stamp: 'jmv 10/28/2021 11:59:20'!
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
			(eachChild dispatchEvent: self) == #rejected ifFalse: [
				"Some child did contain the point so aMorph is part of the top-most chain."
				handledByInner _ true ]]].

	(handledByInner or: [ (aMorph rejectsEvent: self) not and: [aMorph fullIncludesPixel: position] ]) ifTrue: [
		"aMorph is in the top-most unlocked, visible morph in the chain."
		aMorphHandlesIt
			ifTrue: [ ^self sendEventTo: aMorph ]
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


!MouseScrollEvent methodsFor: 'dispatching' stamp: 'jmv 10/28/2021 11:59:12'!
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
	aMorph fullIncludesPixel: position :: ifFalse: [ ^ #rejected ].
	"Install the prospective handler for the receiver"
	aMorphHandlesIt _ false.
	(aMorph handlesMouseScroll: self) ifTrue: [
		eventHandler _ aMorph.
		aMorphHandlesIt _ true ].
	"Now give submorphs a chance to handle the event"
	handledByInner _ false.
	aMorph submorphsDo: [ :eachChild |
		handledByInner ifFalse: [
			(eachChild dispatchEvent: self) == #rejected ifFalse: [
				"Some child did contain the point so aMorph is part of the top-most chain."
				handledByInner _ true ]]].
	(handledByInner or: [
		 (aMorph rejectsEvent: self) not and: [aMorph fullIncludesPixel: position]]) ifTrue: [
		"aMorph is in the top-most unlocked, visible morph in the chain."
		aMorphHandlesIt ifTrue: [ ^ self sendEventTo: aMorph ]].
	handledByInner ifTrue: [ ^ self ].
	"Mouse was not on aMorph nor any of its children"
	^ #rejected.! !

!methodRemoval: WidgetMorph #ownsOrCoversPixel: stamp: 'jmv 10/28/2021 12:01:06'!
WidgetMorph removeSelector: #ownsOrCoversPixel:!
!methodRemoval: KernelMorph #ownsOrCoversPixel: stamp: 'jmv 10/28/2021 12:01:05'!
KernelMorph removeSelector: #ownsOrCoversPixel:!
!methodRemoval: Morph #ownsOrCoversPixel: stamp: 'jmv 10/28/2021 12:01:05'!
Morph removeSelector: #ownsOrCoversPixel:!
!methodRemoval: Morph #fullOwnsOrCoversPixel: stamp: 'jmv 10/28/2021 11:59:33'!
Morph removeSelector: #fullOwnsOrCoversPixel:!
