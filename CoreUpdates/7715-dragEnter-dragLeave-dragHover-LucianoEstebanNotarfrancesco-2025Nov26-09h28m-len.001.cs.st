'From Cuis7.5 [latest update: #7710] on 26 November 2025 at 9:40:05 am'!

!Morph methodsFor: 'events' stamp: 'len 11/17/2025 14:26:59'!
dragEnter: aMouseEnterEvent
	"Handle a mouse enter event, meaning the mouse just entered the bounds of this morph while dragging something. See also #mouseEnter: and #handlesDragOver:."
	self 
		valueOfProperty: #dragEnter: 
		ifPresentDo: [ :handler | handler value: aMouseEnterEvent ]! !

!Morph methodsFor: 'events' stamp: 'len 11/17/2025 14:27:15'!
dragHover: aMouseOverEvent localPosition: localEventPosition
	"Handle a mouse move event while the hand is dragging something. See also #mouseHover:localPosition: and #handlesDragHover."

	self
		valueOfProperty: #dragHover:localPosition:
		ifPresentDo: [ :handler |
			handler
				value: aMouseOverEvent
				value: localEventPosition ].! !

!Morph methodsFor: 'events' stamp: 'len 11/17/2025 14:27:33'!
dragLeave: aMouseLeaveEvent
	"Handle a mouse leave event, meaning the mouse just left the bounds of this morph while dragging something. See also #mouseLeave: and #handleDragOver:."
	self 
		valueOfProperty: #dragLeave: 
		ifPresentDo: [ :handler | handler value: aMouseLeaveEvent ].! !

!Morph methodsFor: 'event handling testing' stamp: 'len 11/13/2025 19:52:35'!
handlesDragHover
	"Do I want to receive unhandled mouseMove events when the hand is dragging something?  The default response is false."
	"Use a property test to allow individual instances to specify this."
	^ self hasProperty: #handlesDragHover.! !

!Morph methodsFor: 'event handling testing' stamp: 'len 11/13/2025 19:52:46'!
handlesDragOver: aMouseEvent
	"Do I want to receive dragEnter: and dragLeave: when the hand is dragging something?  The default response is false." 
	"Use a property test to allow individual instances to specify this."
	^ self hasProperty: #'handlesDragOver:'! !


!MouseOverHandler methodsFor: 'event handling' stamp: 'len 11/22/2025 19:30:53'!
dispatchMouseOverEvent: aMouseEvent
	"Generate and dispatch a MouseOver event.
	If appropriate, also generate and dispatch MouseEnter and MouseLeave events."

	| hand mustSetKeyboardFocus |
	"If leftMorphs, overMorphs, enteredMorphs are not nil, it means this would be a reentrant call.
	inside of the processing of the MouseOver, MouseEnter or MouseLeave dispatched from here.
	We don't want that resulting in spurious repeated MouseOver, MouseEnter or MouseLeave events.
	Therefore it makes no sense to continue. Just exit.
	(Continuing with a reentrant execution will likely result in broken state, such as our ivars becoming
	nil for no apparente reason)."
	leftMorphs ifNotNil: [ ^self ].
	
	hand := aMouseEvent hand.

	"mouseOverMorphs: Array with all morphs that have had a MouseOver event in the previous call.
	leftMorphs is initialized with all morphs that had the mouse over them
	overMorphs and enteredMorphs are initialized empty."
	leftMorphs := mouseOverMorphs asIdentitySet.
	"Assume some coherence for the number of objects in over list"
	overMorphs := WriteStream on: (Array new: leftMorphs size).
	enteredMorphs := WriteStream on: #().
	
	"Now go looking for eventual mouse overs.
	If this MouseOver gets delivered to some morph, it will end calling #noticeMouseOver:event:,
	updating our instance variables.
	leftMorphs will only keep morphs that no longer have the mouse over them.
	enteredMorphs will include morphs that have the mouse over them, and didn't before.
	overMorphs will include all morphs that have the mouse over them."
	aMouseEvent asMouseOver startDispatchFromHand: hand.
	"enteredMorphs and overMorphs are ordered:
	- First is the innest and topmost found morph.
	- Then comes its owner (meaning it is below it), and so on.
	- Last is the World, which is below every other morph."
	
	"Get out early if no change. No morph was entered or left, mouseOverMorphs remains same."
	(leftMorphs isEmpty and: [enteredMorphs position = 0])
		ifTrue: [
			leftMorphs := enteredMorphs := overMorphs := nil.
			^self ].
	enteredMorphs := enteredMorphs contents.
	overMorphs := overMorphs contents.

	mustSetKeyboardFocus := hand hasSubmorphs not and: [Preferences at: #focusFollowsMouse].

	"Send #mouseLeave as appropriate. Release any keyboard focus.
	Order is inner to outer."
	"Keep the order of the left morphs by recreating it from the mouseOverMorphs"
	leftMorphs size > 1 
		ifTrue: [leftMorphs := mouseOverMorphs select: [:m | leftMorphs includes: m]].
	leftMorphs do: [ :m |
		mustSetKeyboardFocus ifTrue: [
			hand releaseKeyboardFocusIf: m ].
		m isInWorld ifTrue: [
			aMouseEvent asMouseLeave sendEventTo: m ]].
	
	"Set keyboard focus to the innermost (i.e. topmost) entered morph that will take it."
	overMorphs do: [ :m |
		m isInWorld ifTrue: [
			(mustSetKeyboardFocus and: [ m handlesKeyboardFocus ]) ifTrue: [
				hand newKeyboardFocus: m.
				"Do it just once!!"
				mustSetKeyboardFocus := false ]]].
	
	"Send #mouseEnter as appropriate. Order is outer to Inner."
	enteredMorphs reverseDo: [ :m |
		m isInWorld ifTrue: [
			aMouseEvent asMouseEnter sendEventTo: m ]].
	
	"And remember the over list for the next call"
	mouseOverMorphs := overMorphs.
	leftMorphs := enteredMorphs := overMorphs := nil.! !


!Morph methodsFor: 'events-processing' stamp: 'len 11/13/2025 19:53:56'!
processMouseEnter: aMouseEnterEvent localPosition: localEventPosition
	"A MouseEnterEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	aMouseEnterEvent hand hasSubmorphs ifTrue:
		[(self handlesDragOver: aMouseEnterEvent) ifTrue:
			[aMouseEnterEvent wasHandled: true.
			self dragEnter: aMouseEnterEvent].
		^ self].
	self wantsBalloon ifTrue: [
		aMouseEnterEvent hand triggerBalloonFor: self after: self balloonHelpDelayTime].
	(self handlesMouseOver: aMouseEnterEvent) ifTrue: [
		aMouseEnterEvent wasHandled: true.
		self mouseEnter: aMouseEnterEvent ].! !

!Morph methodsFor: 'events-processing' stamp: 'len 11/13/2025 22:33:05'!
processMouseLeave: aMouseLeaveEvent localPosition: localEventPosition
	"A MouseLeaveEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	aMouseLeaveEvent hand removePendingBalloonFor: self.
	aMouseLeaveEvent hand hasSubmorphs ifTrue:
		[(self handlesDragOver: aMouseLeaveEvent) ifTrue:
			[aMouseLeaveEvent wasHandled: true.
			self dragLeave: aMouseLeaveEvent].
		^ self].
	(self handlesMouseOver: aMouseLeaveEvent) ifTrue: [
		aMouseLeaveEvent wasHandled: true.
		self mouseLeave: aMouseLeaveEvent ].! !

!Morph methodsFor: 'events-processing' stamp: 'len 11/13/2025 19:58:38'!
processMouseOver: aMouseOverEvent localPosition: localEventPosition
	"A MouseOverEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	aMouseOverEvent hand noticeMouseOver: self event: aMouseOverEvent.

	aMouseOverEvent hand hasSubmorphs ifTrue:
		[self handlesDragHover ifTrue:
			[self dragHover: aMouseOverEvent localPosition: localEventPosition].
		^ self].
	
	"Open question: should any unhandled mouse move events be filtered out?
	(i.e. should mouseHover:localPosition: be called when a mouse button is pressed
	but the morph doesn't have mouse button handlers?
	Essentially, what are the limits of what is considered 'hovering'?"
	self handlesMouseHover ifTrue: [
		self
			mouseHover: aMouseOverEvent
			localPosition: localEventPosition ].! !


!HandMorph methodsFor: 'events-processing' stamp: 'len 11/11/2025 17:17:42'!
startMouseDispatch: aMouseEvent
	"For MouseEvents with default dispatch behavior"

	aMouseEvent dispatchIn: owner! !

!HandMorph methodsFor: 'events-processing' stamp: 'len 11/11/2025 17:15:17'!
startMouseMoveDispatch: aMouseEvent

	lastMouseEvent := aMouseEvent.	
	lastMouseEventTime := Time localMillisecondClock.

	"Check for pending drag or double click operations."
	mouseClickState ifNotNil: [
		mouseClickState handleEvent: aMouseEvent from: self ].

	self morphPosition: aMouseEvent eventPosition.
	self mouseFocus
		ifNotNil: [ mouseFocus handleMouseFocusEvent: aMouseEvent ]
		ifNil: [ aMouseEvent dispatchIn: owner ].

	self dispatchMouseOverEvent! !

