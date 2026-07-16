'From Cuis7.9 [latest update: #8064] on 16 July 2026 at 10:50:51 am'!

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 16/Jul/2026, 1:50:28 pm (UTC)'!
newKeyboardFocusToOrOwnerOf: aMorph
	"Make the given morph the new keyboard focus.
	If it does not take it, find some morph in the owner chain that will.
	Answer whether the focus was actually set (or was already set!!)."

	| candidate |
	candidate := aMorph.
	[ candidate notNil ] whileTrue: [
		candidate handlesKeyboardFocus ifTrue: [
			self newKeyboardFocus: candidate.
			^true ].
		candidate := candidate owner ].
	^false! !


!MouseOverHandler methodsFor: 'event handling' stamp: 'jmv 16/Jul/2026, 1:38:21 pm (UTC)'!
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

	"Set keyboard focus to the innermost (i.e. topmost) entered morph that will take it,
	or its first owner that wants it."
	overMorphs do: [ :m |
		(mustSetKeyboardFocus and: [ m isInWorld ]) ifTrue: [
			mustSetKeyboardFocus := (hand newKeyboardFocusToOrOwnerOf: m) not ]].

	"Send #mouseEnter as appropriate. Order is outer to Inner."
	enteredMorphs reverseDo: [ :m |
		m isInWorld ifTrue: [
			aMouseEvent asMouseEnter sendEventTo: m ]].

	"And remember the over list for the next call"
	mouseOverMorphs := overMorphs.
	leftMorphs := enteredMorphs := overMorphs := nil.! !


!Morph methodsFor: 'events-processing' stamp: 'jmv 16/Jul/2026, 1:14:34 pm (UTC)'!
processMouseDown: aMouseButtonEvent localPosition: localEventPosition
	"A #mouseDown MouseButtonEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true.
	Notes:
		- MouseButtonEvent are sent to the current mouse focus, and if none,
			to the topmost morph under the hand that handles it."
	
	aMouseButtonEvent hand removePendingBalloon; deleteBalloon.
	aMouseButtonEvent wasHandled: true.
	self activateWindow.

	aMouseButtonEvent mouseButton3Pressed ifTrue: [
		self mouseButton3Down: aMouseButtonEvent localPosition: localEventPosition.
		^self ].
	
	aMouseButtonEvent hand newMouseFocus: self.		"Mouse down sets mouse focus"

	aMouseButtonEvent mouseButton2Pressed ifTrue: [
		self mouseButton2Down: aMouseButtonEvent localPosition: localEventPosition.
		^self ].

	(Preferences at: #focusFollowsMouse)
		ifTrue: [" MouseEnterToFocus. Clicks have no relation to keyboard focus. Just deliver the event."
			self mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ]
		ifFalse: [ "ClickToFocus. Still two different cases. Some morphs 'ignore' focus setting MouseDown events."
			self clickToFocusConsumesEvent
				ifTrue: [
					"#clickToFocusConsumesEvent means: Don't deliver a MouseDown if the
					soon-to-be MouseUp will set keyboard focus on us (see #processMouseUp:localPosition:).
					So, deliver it if we already have keyboard focus."
					self hasKeyboardFocus ifTrue: [
						self mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ]]
				ifFalse: [
					"(#clickToFocusConsumesEvent not) means: A keyboard focus setting MouseDown is also a
					regular MouseDown (after setting keyboard focus)"
					aMouseButtonEvent hand newKeyboardFocusToOrOwnerOf: self.
					self mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ]].

	(self handlesMouseStillDown: aMouseButtonEvent) ifTrue: [
		self startStepping: #processMouseStillDown
			in: self mouseStillDownThreshold
			stepTime: self mouseStillDownStepRate ].! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 16/Jul/2026, 1:14:44 pm (UTC)'!
processMouseUp: aMouseButtonEvent localPosition: localEventPosition
	"A #mouseUp MouseButtonEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true.
	Notes:
		- MouseButtonEvent are sent to the current mouse focus, and if none,
			to the topmost morph under the hand that handles it."

	aMouseButtonEvent hand releaseMouseFocus: self.
	aMouseButtonEvent wasHandled: true.
	aMouseButtonEvent mouseButton3Changed ifTrue: [ 
		self mouseButton3Up: aMouseButtonEvent localPosition: localEventPosition.
		^self ].

	aMouseButtonEvent mouseButton2Changed ifTrue: [
		self mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition.
		^self ].

	"If we click to focus, don't process this event as a regular click.
	See also #processMouseDown:localPosition:"
	"(self clickToFocusConsumesEvent and: [
		(Preferences at: #focusFollowsMouse) not and: [
			self hasKeyboardFocus not ]])
		ifTrue: [ aMouseButtonEvent hand newKeyboardFocus: self ]
		ifFalse: [ self mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition ]."
	(self clickToFocusConsumesEvent and: [ (Preferences at: #focusFollowsMouse) not ])
		ifTrue: [
			self hasKeyboardFocus
				ifFalse: [
					"Haven't set keyboard focus but must. Do it.
					MouseDown and any MouseMove were not delivered. Do not deliver #mouseButton1Up:localPosition:"
					aMouseButtonEvent hand newKeyboardFocusToOrOwnerOf: self ]
				ifTrue: [
					"If we already have keyboardFocus, the MouseDown was delivered. Deliver also the MouseMove."
					self mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition ]]
		ifFalse: [
			"See #processMouseDown:localPosition: Mouse down was a regular event. Deliver also the MouseUp."
			self mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition ].

	self stopStepping: #processMouseStillDown.! !

