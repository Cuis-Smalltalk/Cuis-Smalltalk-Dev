'From Cuis 5.0 [latest update: #4869] on 21 September 2021 at 9:53:48 am'!

!Morph methodsFor: 'geometry services' stamp: 'jmv 9/21/2021 09:31:32'!
fullOwnsOrCoversPixel: worldPoint
	"Answer true if worldPoint is in some submorph, even if not inside our shape.
	See comment at #ownsOrCoversPixel: for important notes on behavior."

	(self ownsOrCoversPixel: worldPoint) ifTrue: [ ^true ].
	self submorphsDo: [ :m |
		(m fullOwnsOrCoversPixel: worldPoint) ifTrue: [ ^true ]].
	^ false.! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 9/21/2021 09:50:59'!
isCloserThan: maxDistance toPixel: worldPoint
	"Answer true if our closest point to aPoint is less than aNumber pixels away.
	In target surface (i.e. Display) coordinates.
	Uses precise testing of the morph contour if available:
	Morphs not in the WidgetMorph hierarchy should answer #true to wantsContour.
	Note: Considers only the external border. Any inner pixel is considered 'inside' regardless of us being transparent there.
	Note: Cheaper than #coversAnyPixelCloserThan:to: . Doesn't use #bitMask. Doesn't require maintenance."

	| center contourTop contourBottom |
	privateDisplayBounds ifNil: [
		^false ].
	center _ privateDisplayBounds center.
	"Quick checks: If not even within aNumber distance to display bounds, fail"
	(center y - worldPoint y) abs < (privateDisplayBounds height  // 2 + maxDistance) ifFalse: [
		^false ].
	(center x - worldPoint x) abs < (privateDisplayBounds width // 2 + maxDistance) ifFalse: [
		^false ].
	"Precise check with contour, if available"
	(self valueOfProperty: #contour) ifNotNil: [ :contour | | y0 y1 x0 x1 |
		contourTop _ self valueOfProperty: #contourY0.
		contourBottom _ self valueOfProperty: #contourY1.
		"Contour rows to consider are those within requested distance."
		y0 _ worldPoint y - maxDistance max: contourTop.
		y1 _ worldPoint y + maxDistance min: contourBottom.
		y0 to: y1 do: [ :y |
			x0 _ (contour at: (y - contourTop) * 2 + 1) - maxDistance.
			x1 _ (contour at: (y - contourTop) * 2 + 2) + maxDistance.
			"If a vertical line of 2*aNumber height centered on aPoint is inside the contour, quick exit"
			(worldPoint x between: x0 and: x1) ifTrue: [ ^true ].
			"Check if aPoint is close enough to contour"
			(x0@y - worldPoint) r < maxDistance ifTrue: [ ^true ].
			(x1@y - worldPoint) r < maxDistance ifTrue: [ ^true ]].
		"Not inside, not close enough to contour"
		^ false ].
	"If contour is not available, and aPoint is close enough to displayBounds, answer true, as it is the best we can know."
	^ true! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 9/21/2021 09:33:07'!
ownsOrCoversPixel: worldPoint
	"Answer true if we own the pixel, i.e. we are the last morph drawn at worldPoint.
	Some implementations (KernelMorph and WidgetMorph) may also answer true if we cover but don't own the pixel,
	meaning that some other morph was drawn later, covering us.
	Therefore, senders need to be aware that the answer is meaningful only when it is known that no other morph covers us.
	Note: This implementation is only used for VectorGraphics based morphs (i.e. morphs that answer true to #requiresVectorCanvas).
	(See other implementors)
	Note: Also see #ownsPixel: and #coversPixel:"

	^ self ownsPixel: worldPoint.! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 9/21/2021 09:12:00'!
ownsPixel: worldPoint
	"Answer true if we own the pixel, i.e. we are the last morph drawn at worldPoint.
	Requires VectorGraphics.
	Only valid for morphs that answer true to #requiresVectorCanvas"

	self topmostWorld ifNotNil: [ :w |
		w canvas ifNotNil: [ :canvas |
			^ (canvas morphIdAt: worldPoint) = self morphId ]].
	^ false.! !


!KernelMorph methodsFor: 'geometry services' stamp: 'jmv 9/20/2021 11:25:55'!
coversLocalPoint: aLocalPoint
	"Answer true as long as aLocalPoint is inside our shape even if:
		- a submorph (above us) also covers it
		- a sibling that is above us or one of their submorphs also covers it."

	"If not visible, won't cover any point at all."
	self visible ifFalse: [ ^false ].

	"We know our local bounds, and completely fill them."
	^ self morphLocalBounds containsPoint: aLocalPoint! !

!KernelMorph methodsFor: 'geometry services' stamp: 'jmv 9/21/2021 09:36:45'!
coversPixel: worldPoint
	"Answer true as long as worldPoint is inside our shape even if:
		- a submorph (above us) also covers it
		- a sibling that is above us or one of their submorphs also covers it.
	This implementation is cheap, we are a rectangular shape."

	^ self coversLocalPoint:
		(self internalizeFromWorld: worldPoint)! !

!KernelMorph methodsFor: 'geometry services' stamp: 'jmv 9/21/2021 09:31:36'!
fullOwnsOrCoversPixel: worldPoint
	"Answer true if worldPoint is in some submorph, even if not inside our shape.
	See comment at #ownsOrCoversPixel: for important notes on behavior."

	(self ownsOrCoversPixel: worldPoint) ifTrue: [ ^ true ].
	self submorphsMightProtrude ifTrue: [
		self submorphsDo: [ :m |
			(m fullOwnsOrCoversPixel: worldPoint) ifTrue: [ ^ true ]]].
	^ false.! !

!KernelMorph methodsFor: 'geometry services' stamp: 'jmv 9/21/2021 09:35:38'!
ownsOrCoversPixel: worldPoint
	"Answer true if we own the pixel, i.e. we are the last morph drawn at worldPoint.
	This implementation also answer true if we cover but don't own the pixel,
	meaning that some other morph was drawn later, covering us.
	Therefore, senders need to be aware that the answer is meaningful only when it is known that no other morph covers us.
	Note: This implementation is only used for morphs with a cheap #coversPixel:.
	(See other implementors)
	Note: Also see #ownsPixel: and #coversPixel:"

	^ self visible and: [self coversPixel: worldPoint].! !


!WidgetMorph methodsFor: 'geometry services' stamp: 'jmv 9/20/2021 11:26:19'!
coversLocalPoint: aLocalPoint
	"Answer true as long as aLocalPoint is inside our shape even if:
		- a submorph (above us) also covers it
		- a sibling that is above us or one of their submorphs also covers it."

	"If not visible, won't cover any point at all."
	self visible ifFalse: [ ^false ].

	"We know our local bounds, and completely fill them."
	^ self morphLocalBounds containsPoint: aLocalPoint! !

!WidgetMorph methodsFor: 'geometry services' stamp: 'jmv 9/21/2021 09:36:52'!
coversPixel: worldPoint
	"Answer true as long as worldPoint is inside our shape even if:
		- a submorph (above us) also covers it
		- a sibling that is above us or one of their submorphs also covers it.
	This implementation is cheap, we are a rectangular shape."

	^ self coversLocalPoint:
		(self internalizeFromWorld: worldPoint)! !

!WidgetMorph methodsFor: 'geometry services' stamp: 'jmv 9/21/2021 09:31:41'!
fullOwnsOrCoversPixel: worldPoint
	"Answer true if worldPoint is in some submorph, even if not inside our shape.
	See comment at #ownsOrCoversPixel: for important notes on behavior."

	(self ownsOrCoversPixel: worldPoint) ifTrue: [ ^ true ].
	self submorphsMightProtrude ifTrue: [
		self submorphsDo: [ :m |
			(m fullOwnsOrCoversPixel: worldPoint) ifTrue: [ ^ true ]]].
	^ false.! !

!WidgetMorph methodsFor: 'geometry services' stamp: 'jmv 9/21/2021 09:35:48'!
ownsOrCoversPixel: worldPoint
	"Answer true if we own the pixel, i.e. we are the last morph drawn at worldPoint.
	This implementation also answer true if we cover but don't own the pixel,
	meaning that some other morph was drawn later, covering us.
	Therefore, senders need to be aware that the answer is meaningful only when it is known that no other morph covers us.
	Note: This implementation is only used for morphs with a cheap #coversPixel:.
	(See other implementors)
	Note: Also see #ownsPixel: and #coversPixel:"

	^ self visible and: [self coversPixel: worldPoint].! !


!WindowEdgeAdjustingMorph methodsFor: 'geometry services' stamp: 'jmv 9/20/2021 11:30:16'!
coversLocalPoint: aLocalPoint
	"We don't completely cover our bounds. Account for that."

	| sensitiveBorder |
	( self morphLocalBounds containsPoint: aLocalPoint) ifFalse: [ ^false ].
	sensitiveBorder _ owner borderWidth.
	selector caseOf: {
		[ #windowTopLeft: ] -> [ ^ aLocalPoint x < sensitiveBorder or: [ aLocalPoint y < sensitiveBorder ]].
		[ #windowTopRight: ] -> [ ^ extent x - aLocalPoint x <= sensitiveBorder or: [ aLocalPoint y < sensitiveBorder ]].
		[ #windowBottomLeft: ] -> [ ^ aLocalPoint x < sensitiveBorder or: [ extent y- aLocalPoint y <= sensitiveBorder ]].
		[ #windowBottomRight: ] -> [ ^ extent x - aLocalPoint x <= sensitiveBorder or: [ extent y - aLocalPoint y <= sensitiveBorder ]].
	}
	otherwise: [
		"all the morph is sensitive for horizontal and vertical (i.e. non corner) instances."
		^true ]! !


!Morph methodsFor: 'events-processing' stamp: 'jmv 9/20/2021 12:13:48'!
processMouseOver: aMouseEvent localPosition: localEventPosition
	"System level event handling."
	 self hasMouseFocus ifTrue: [
		"Got this directly through #handleFocusEvent: so check explicitly"
		((self rejectsEvent: aMouseEvent) not and: [self fullOwnsOrCoversPixel: aMouseEvent eventPosition]) ifFalse: [
			^self ]].
	aMouseEvent hand noticeMouseOver: self event: aMouseEvent.
	"Open question: should any unhandled mouse move events be filtered out? (i.e. should mouseHover:localPosition: be called when a mouse button is pressed but the morph doesn't have mouse button handlers?  Essentially, what are the limits of what is considered 'hovering'?"
	(self handlesMouseHover and: [aMouseEvent wasHandled not]) ifTrue: [
		self
			mouseHover: aMouseEvent
			localPosition: localEventPosition ].! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 9/21/2021 09:53:20'!
contourIntersects: aContourArray top: aContourTop bottom: aContourBottom
	"Check if contours intersect.
	If contour is not available, use displayBounds.
	Not to be called directly. Pefer a higher level service. See senders."

	| contour contourTop contourBottom x0Own x1Own x0Arg x1Arg |
	contour _ self valueOfProperty: #contour.	
	contourTop _ (self valueOfProperty: #contourY0) ifNil: [aContourTop].
	contourBottom _ (self valueOfProperty: #contourY1) ifNil: [aContourBottom].

	(contourTop max: aContourTop) to: (contourBottom min: aContourBottom) do: [ :y |
		x0Own _ contour ifNil: [privateDisplayBounds left] ifNotNil: [ contour at: (y - contourTop) * 2 + 1 ].
		x1Own _ contour ifNil: [privateDisplayBounds right-1] ifNotNil: [ contour at: (y - contourTop) * 2 + 2 ].
		x0Arg _ aContourArray at: (y - aContourTop) * 2 + 1.
		x1Arg _ aContourArray at: (y - aContourTop) * 2 + 2.
		(x0Own <= x1Arg and: [ x0Arg <= x1Own ])
			ifTrue: [ ^true ]].

	^false! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 9/21/2021 09:51:06'!
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
	privateDisplayBounds ifNil: [
		^false ].
	center _ privateDisplayBounds center.
	"Quick checks: If not even within aNumber distance to display bounds, fail"
	(center y - worldPoint y) abs < (privateDisplayBounds height  // 2 + maxDistance) ifFalse: [
		^false ].
	(center x - worldPoint x) abs < (privateDisplayBounds width // 2 + maxDistance) ifFalse: [
		^false ].
	"Precise check with bitMask"
	(self coversPixel: worldPoint) ifTrue: [ ^true ].
	maxDistanceSquared _ maxDistance squared.
	maxDistance negated to: maxDistance do: [ :dy |
		maxDistance negated to: maxDistance do: [ :dx |
			dx squared + dy squared <= maxDistanceSquared ifTrue: [
				(self coversPixel: worldPoint + (dx@dy)) ifTrue: [ ^true ]]]].
	^false.! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 9/21/2021 09:38:06'!
coversPixel: worldPoint
	"Answer true if pixel worldPoint is covered by us, even if we are not visible a it because of some
	other morph above us also covers it.
	See #bitMask.
	Remember to do
	self removeProperty: #bitMask.
	when appropriate!! (i.e. when we change in such a way to make the bitMask invalid).

	Note: Subclasses such as KernelMorph and WidgetMorph redefine this method with an optimized
	implementation that doesn't require computing and invalidating the #bitMask. Senders in the base image
	and framework actually only use this optimized implementation. That's why general morphs don't care about
	invalidting #bitMask.
	
	Note: If your morph #requiresVectorCanvas, and depends on this general implementation, remember to 
	`removeProperty: #bitMask` whenever it becomes invalid due to changes in your morphs. You may consider
	using #ownsPixel: if appropriate, that doesn't require any maintenance and is cheaper (in cpu and memory).

	Note: This implementation requires VectorGraphics."

	self visible ifTrue: [
		^(self bitMask pixelValueAt: worldPoint - self displayFullBounds topLeft) = 1 ].
	^ false! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 9/20/2021 12:13:54'!
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
		(m wantsHalo and: [ m fullOwnsOrCoversPixel: event eventPosition ])
			ifTrue: [
				m transferHalo: event from: formerHaloOwner.
				^self ]].
	"We're at the bottom most level; just keep halo on ourselves"! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 9/20/2021 11:32:25'!
doRecolor: event with: aHandle
	"The mouse went down in the 'recolor' halo handle.  Allow the user to change the color of the innerTarget"

	event hand obtainHalo: self.
	(aHandle coversPixel: event eventPosition)
		ifFalse: [  "only do it if mouse still in handle on mouse up"
			self delete.
			target addHalo: event]
		ifTrue: [
			target changeColor]! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 9/20/2021 11:32:30'!
maybeCollapse: event with: aHandle 
	"Ask hand to collapse my target if mouse comes up in it."

	event hand obtainHalo: self.
	(aHandle coversPixel: event eventPosition)
		ifTrue: [
			target collapse ].
	self delete.! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 9/20/2021 11:32:34'!
maybeDismiss: event with: aHandle
	"Ask hand to dismiss my target if mouse comes up in it."

	event hand obtainHalo: self.
	(aHandle coversPixel: event eventPosition)
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

!HaloMorph methodsFor: 'private' stamp: 'jmv 9/20/2021 11:32:38'!
setDismissColor: event with: aHandle
	"Called on mouseStillDown in the dismiss handle; set the color appropriately."

	| colorToUse |
	event hand obtainHalo: self.
	colorToUse _  (aHandle coversPixel: event eventPosition)
		ifFalse: [ `Color red muchLighter` ]
		ifTrue: [ `Color lightGray` ].
	aHandle color: colorToUse! !


!MenuItemMorph methodsFor: 'events' stamp: 'jmv 9/20/2021 11:32:44'!
activateOwnerMenu: evt
	"Activate our owner menu; e.g., pass control to it"
	owner ifNil: [ ^false ]. "not applicable"
	(owner coversPixel: evt eventPosition)
		ifFalse: [ ^false ].
	owner activate: evt.
	^true! !

!MenuItemMorph methodsFor: 'events' stamp: 'jmv 9/20/2021 11:32:47'!
activateSubmenu: event
	"Activate our submenu; e.g., pass control to it"

	subMenu ifNil: [ ^false ]. "not applicable"
	subMenu isInWorld ifFalse: [ ^false ].	
	(subMenu coversPixel: event eventPosition) ifFalse: [^false].
	subMenu activate: event.
	^true! !


!PluggableButtonMorph methodsFor: 'events' stamp: 'jmv 9/20/2021 11:32:53'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

	isPressed _ false.
	mouseIsOver _ false.
	(actWhen == #buttonUp and: [ 
		self coversPixel: aMouseButtonEvent eventPosition ])
		ifTrue: [ self performAction ].
	self redrawNeeded! !


!MenuMorph methodsFor: 'events' stamp: 'jmv 9/20/2021 11:36:05'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse down event."
	(stayUp or: [ self coversPixel: aMouseButtonEvent eventPosition ]) 
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

!MenuMorph methodsFor: 'events' stamp: 'jmv 9/20/2021 11:36:32'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse up event.
	Note: This might be sent from a modal shell."
	(self coversPixel: aMouseButtonEvent eventPosition) ifFalse:[
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


!AutoCompleterMorph methodsFor: 'events' stamp: 'jmv 9/20/2021 11:32:58'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

	(self coversPixel: aMouseButtonEvent eventPosition)
		ifTrue: [
			((self upButtonPosition extent: ScrollBar scrollbarThickness) containsPoint: localEventPosition)
				ifTrue: [ ^self stillActive; goUp ].
			((self downButtonPosition extent: ScrollBar scrollbarThickness) containsPoint: localEventPosition)
				ifTrue: [ ^self stillActive; goDown ].
			self selected: (localEventPosition y // self itemHeight) +  self firstVisible.
			completer insertSelected ]
		ifFalse: [ self delete. completer menuClosed ]! !


!MorphicEvent methodsFor: 'dispatching' stamp: 'jmv 9/20/2021 12:14:09'!
dispatchWith: aMorph
	"Dispatch me. The event will be passed to the front-most visible submorph that contains the position wrt. to the event."
	| handledByInner |

	"Try to get out quickly"
	(aMorph fullOwnsOrCoversPixel: self eventPosition)
		ifFalse: [ ^#rejected ].

	"Now give submorphs a chance to handle the event"
	handledByInner _ false.
	aMorph submorphsDo: [ :eachChild |
		handledByInner ifFalse: [
			(eachChild dispatchEvent: self) == #rejected ifFalse: [
				"Some child did contain the point so aMorph is part of the top-most chain."
				handledByInner _ true ]]].

	"Check for being inside the receiver"
	(handledByInner or: [ (aMorph rejectsEvent: self) not and: [aMorph fullOwnsOrCoversPixel: self eventPosition] ])
		ifTrue: [ ^ self sendEventTo: aMorph ].

	^ #rejected! !


!DropEvent methodsFor: 'dispatching' stamp: 'jmv 9/20/2021 12:14:16'!
dispatchWith: aMorph
	"Drop is done on the innermost target that accepts it."
	| dropped |

	"Try to get out quickly"
	(aMorph fullOwnsOrCoversPixel: position)
		ifFalse: [ ^#rejected ].

	"Go looking if any of our submorphs wants it"
	aMorph submorphsDo: [ :eachChild |
		(eachChild dispatchEvent: self) == #rejected ifFalse: [
			^self ]].

	(aMorph allowsMorphDrop and: [ (aMorph rejectsEvent: self) not and: [aMorph fullOwnsOrCoversPixel: position] ])
		ifTrue: [
			"Do a symmetric check if both morphs like each other"
			dropped _ self contents.
			((aMorph wantsDroppedMorph: dropped event: self)	"I want her"
				and: [dropped wantsToBeDroppedInto: aMorph])		"she wants me"
					ifTrue: [
						^ self sendEventTo: aMorph ]].
	^#rejected! !


!DropFilesEvent methodsFor: 'dispatching' stamp: 'jmv 9/20/2021 12:14:23'!
dispatchWith: aMorph
	"Drop is done on the innermost target that accepts it."

	"Try to get out quickly"
	(aMorph fullOwnsOrCoversPixel: position) ifFalse: [ ^#rejected ].

	"Go looking if any of our submorphs wants it"
	aMorph submorphsDo: [ :eachChild |
		(eachChild dispatchEvent: self) == #rejected ifFalse: [ ^self ]].

	(aMorph allowsFilesDrop and: [ (aMorph rejectsEvent: self) not and: [aMorph fullOwnsOrCoversPixel: position] ])
		ifTrue: [^ self sendEventTo: aMorph ].
	
	^#rejected! !


!KeyboardEvent methodsFor: 'actions' stamp: 'jmv 9/20/2021 11:33:02'!
closeCurrentWindowOf: aMorph

	aMorph owningWindow ifNotNil: [ :w |
		(w coversPixel: position)
			ifTrue: [ w delete ] ].! !


!MouseButtonEvent methodsFor: 'dispatching' stamp: 'jmv 9/20/2021 12:14:44'!
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
	(aMorph fullOwnsOrCoversPixel: position)
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

	(handledByInner or: [ (aMorph rejectsEvent: self) not and: [aMorph fullOwnsOrCoversPixel: position] ]) ifTrue: [
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


!MouseScrollEvent methodsFor: 'dispatching' stamp: 'jmv 9/20/2021 12:14:35'!
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
	aMorph fullOwnsOrCoversPixel: position :: ifFalse: [ ^ #rejected ].
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
		 (aMorph rejectsEvent: self) not and: [aMorph fullOwnsOrCoversPixel: position]]) ifTrue: [
		"aMorph is in the top-most unlocked, visible morph in the chain."
		aMorphHandlesIt ifTrue: [ ^ self sendEventTo: aMorph ]].
	handledByInner ifTrue: [ ^ self ].
	"Mouse was not on aMorph nor any of its children"
	^ #rejected.! !

!methodRemoval: BitBltCanvas #morph:isAtPoint: stamp: 'jmv 9/21/2021 08:46:25'!
BitBltCanvas removeSelector: #morph:isAtPoint:!
!methodRemoval: WindowEdgeAdjustingMorph #morphContainsPoint: stamp: 'jmv 9/21/2021 08:46:52'!
WindowEdgeAdjustingMorph removeSelector: #morphContainsPoint:!
!methodRemoval: WidgetMorph #morphContainsPoint: stamp: 'jmv 9/21/2021 08:46:51'!
WidgetMorph removeSelector: #morphContainsPoint:!
!methodRemoval: HaloMorph #containsGlobalPoint: stamp: 'jmv 9/21/2021 08:46:00'!
HaloMorph removeSelector: #containsGlobalPoint:!
!methodRemoval: KernelMorph #morphContainsPoint: stamp: 'jmv 9/21/2021 08:46:51'!
KernelMorph removeSelector: #morphContainsPoint:!
!methodRemoval: Morph #containsGlobalPoint: stamp: 'jmv 9/21/2021 08:45:59'!
Morph removeSelector: #containsGlobalPoint:!
!methodRemoval: Morph #isCloserThan:to: stamp: 'jmv 9/21/2021 09:51:11'!
Morph removeSelector: #isCloserThan:to:!
!methodRemoval: Morph #fullContainsGlobalPoint: stamp: 'jmv 9/21/2021 08:45:50'!
Morph removeSelector: #fullContainsGlobalPoint:!
!methodRemoval: Morph #isCloserThan:toPoint: stamp: 'jmv 9/21/2021 09:46:24'!
Morph removeSelector: #isCloserThan:toPoint:!

!Morph reorganize!
('accessing' adoptWidgetsColor: beSticky color location location: lock morphId resistsRemoval taskbar toggleStickiness unlock unlockContents)
('accessing - flags' fullRedrawNotNeeded highlighted: isHighlighted isLayoutNeeded isRedrawNeeded isSubmorphRedrawNeeded layoutNeeded: needsRedraw: submorphNeedsRedraw: visible)
('accessing - properties' hasProperty: isLocked isSticky lock: name name: removeProperty: setProperty:toValue: sticky: valueOfProperty: valueOfProperty:ifAbsent: valueOfProperty:ifAbsentPut: valueOfProperty:ifPresentDo:)
('as yet unclassified' canDiscardEdits disregardUnacceptedEdits whenUIinSafeState:)
('caching' clearId fullReleaseCachedState releaseCachedState)
('change reporting' addedMorph: invalidateDisplayRect:for: invalidateLocalRect:)
('classification' isWorldMorph)
('copying' copy copyForClipboard duplicate okayToDuplicate)
('debug and other' addDebuggingItemsTo:hand: buildDebugMenu: inspectOwnerChain ownerChain resumeAfterDrawError resumeAfterStepError)
('drawing' addPossiblyUncoveredAreasIn:to: drawOn: drawingFails drawingFailsNot hide icon imageForm: imageForm:depth: isKnownFailing postDrawOn: refreshWorld show visible:)
('dropping/grabbing' aboutToBeGrabbedBy: aboutToGrab: acceptDroppingMorph:event: dropFiles: justDroppedInto:event: justGrabbedFrom: rejectDropMorphEvent: wantsDroppedMorph:event: wantsToBeDroppedInto:)
('e-toy support' embeddedInMorphicWindowLabeled: unlockOneSubpart wantsRecolorHandle)
('events' click:localPosition: doubleClick:localPosition: dragEvent:localPosition: keyDown: keyStroke: keyUp: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Down:localPosition: mouseButton2Up:localPosition: mouseButton3Down:localPosition: mouseButton3Up:localPosition: mouseEnter: mouseHover:localPosition: mouseLeave: mouseMove:localPosition: mouseScroll:localPosition: mouseStillDown windowEvent:)
('event handling testing' allowsFilesDrop allowsMorphDrop allowsSubmorphDrag handlesKeyboard handlesMouseDown: handlesMouseHover handlesMouseOver: handlesMouseScroll: handlesMouseStillDown:)
('event handling' mouseButton2Activity mouseStillDownStepRate mouseStillDownThreshold)
('events-alarms' addAlarm:after: addAlarm:with:after: addAlarm:withArguments:after: alarmScheduler removeAlarm:)
('events-processing' dispatchEvent: focusKeyboardFor: handleFocusEvent: processDropFiles: processDropMorph: processKeyDown: processKeyUp: processKeystroke: processMouseDown:localPosition: processMouseEnter:localPosition: processMouseLeave:localPosition: processMouseMove:localPosition: processMouseOver:localPosition: processMouseScroll:localPosition: processMouseStillDown processMouseUp:localPosition: processUnknownEvent: processWindowEvent: rejectsEvent: rejectsEventFully:)
('fileIn/out' prepareToBeSaved storeDataOn:)
('focus handling' hasKeyboardFocus hasMouseFocus keyboardFocusChange:)
('geometry' adjustDisplayBoundsBy: basicDisplayBounds displayBounds displayBoundsForHalo displayBoundsSetFrom: displayBoundsUpdateFrom: displayFullBounds extentChanged: externalize: externalizeBoundsToWorld: externalizeDistance: externalizeDistanceToWorld: externalizeToWorld: fontPreferenceChanged fullBoundsInOwner internalize: internalizeDistance: internalizeDistanceFromWorld: internalizeFromWorld: minimumExtent morphLocalBoundsForError morphPosition morphPosition: morphPositionInWorld morphPositionInWorld: rotation rotation:scale: rotationDegrees rotationDegrees: scale)
('geometry services' collides: contourIntersects:top:bottom: coversAnyPixelCloserThan:to: coversPixel: displayBoundsIntersects: fullOwnsOrCoversPixel: isCloserThan:toPixel: ownsOrCoversPixel: ownsPixel:)
('geometry testing' clipsSubmorphs clipsSubmorphsReally hasOwnLocation hasVariableExtent isOrAnyOwnerIsRotated knowsOwnLocalBounds requiresVectorCanvas submorphsMightProtrude wantsContour)
('halos and balloon help' addHalo addHalo: addHandlesTo:box: addOptionalHandlesTo:box: balloonHelpDelayTime balloonText comeToFrontAndAddHalo deleteBalloon editBalloonHelpContent: editBalloonHelpText halo haloShowsCoordinateSystem mouseDownOnHelpHandle: noHelpString okayToBrownDragEasily okayToResizeEasily okayToRotateEasily okayToScaleEasily removeHalo setBalloonText: showBalloon: showBalloon:hand: transferHalo:from: wantsBalloon wantsHalo wantsHaloHandleWithSelector:inHalo:)
('initialization' initialize intoWorld: openInHand openInWorld openInWorld:)
('iteration of all morphs' nextMorph nextMorphPart2 nextMorphThat: previousMorph previousMorphThat:)
('layout' layoutSubmorphs layoutSubmorphsIfNeeded minimumLayoutHeight minimumLayoutWidth minimumShrinkExtent minimumShrinkHeight minimumShrinkWidth someSubmorphPositionOrExtentChanged)
('macpal' flash flashWith:)
('menus' addAddHandMenuItemsForHalo:hand: addColorMenuItems:hand: addCopyItemsTo: addCustomHaloMenuItems:hand: addCustomMenuItems:hand: addExportMenuItems:hand: addHaloActionsTo: addStandardHaloMenuItemsTo:hand: addTitleForHaloMenu: addToggleItemsToHaloMenu: changeColor expand exportAsBMP exportAsJPEG lockUnlockMorph lockedString maybeAddCollapseItemTo: stickinessString)
('meta-actions' addEmbeddingMenuItemsTo:hand: buildHandleMenu: copyToClipboard: dismissMorph duplicateMorph: maybeDuplicateMorph potentialEmbeddingTargets)
('naming' label)
('object serialization' objectForDataStream:)
('printing' printOn:)
('stepping' shouldGetStepsFrom: startStepping startStepping: startStepping:in:stepTime: startStepping:stepTime: startSteppingStepTime: step stepAt: stepTime stopStepping stopStepping: wantsSteps)
('structure' allOwnersDo: allOwnersReverseDo: firstOwnerSuchThat: hasOwner: isInWorld owner owningWindow root topmostWorld veryLastLeaf withAllOwnersDo: withAllOwnersReverseDo: world)
('submorphs-accessing' allMorphsBut:do: allMorphsDo: allSubmorphsDo: findDeepSubmorphThat:ifAbsent: findSubmorphBinary: firstSubmorph hasSubmorphs lastSubmorph noteNewOwner: submorphBehind: submorphCount submorphInFrontOf: submorphs submorphsBehind:do: submorphsDo: submorphsInFrontOf:do: submorphsReverseDo: submorphsSatisfying:)
('submorphs-add/remove' addAllMorphs: addAllMorphs:after: addMorph: addMorph:behind: addMorph:inFrontOf: addMorph:position: addMorphBack: addMorphBack:position: addMorphFront: addMorphFront:position: addMorphFrontFromWorldPosition: atFront canAdd: comeToFront delete dismissViaHalo goBehind privateDelete removeAllMorphs removeAllMorphsIn: removeMorph: removedMorph: replaceSubmorph:by:)
('testing' hasModel is: isCollapsed isIncludedInTaskbar isOwnedByHand isOwnedByWorld isOwnedByWorldOrHand isProportionalHeight isProportionalWidth isReallyVisible)
('updating' invalidateBounds redrawNeeded)
('user interface' activateWindow activateWindowAndSendTopToBack: collapse showAndComeToFront toggleCollapseOrShow)
('private' privateAddAllMorphs:atIndex: privateAddMorph:atIndex: privateAddMorph:atIndex:position: privateAnyOwnerHandlesMouseScroll: privateFlagAt: privateFlagAt:put: privateMoveBackMorph: privateMoveFrontMorph: privateOwner: privatePosition: privateRemove: privateSubmorphs)
('previewing' beginPreview endPreview endPreviewAndToggleCollapseOrShow morphBehindBeforePreview morphBehindBeforePreview: previewing previewing: visibleBeforePreview visibleBeforePreview:)
!

