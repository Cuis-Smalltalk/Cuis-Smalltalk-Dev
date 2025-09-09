'From Cuis7.5 [latest update: #7531] on 9 September 2025 at 11:34:24 am'!

!MouseOverHandler methodsFor: 'event handling' stamp: 'jmv 9/9/2025 10:08:03'!
dispatchMouseOverEvent: aMouseEvent
	"Generate and dispatch a MouseOver event.
	If appropriate, also generate and dispatch MouseEnter and MouseLeave events."

	| hand mouseOverEvent mouseLeaveEvent mouseEnterEvent |
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
	mouseOverEvent := aMouseEvent asMouseOver.
	mouseOverEvent startDispatchFromHand: hand.
	"enteredMorphs and overMorphs are ordered:
	- First is the innest and topmost found morph.
	- Then comes its owner (meaning it is below it), and so on.
	- Last is the World, which is below every other morph."
	
	"Get out early if no change. No morph was entered or left, mouseOverMorphs remains same."
	(leftMorphs isEmpty and: [enteredMorphs position = 0])
		ifTrue: [
			leftMorphs := enteredMorphs := overMorphs := nil.
			^self ].

	"Send #mouseLeave as appropriate. Order is inner to outer."
	mouseLeaveEvent := aMouseEvent asMouseLeave.
	"Keep the order of the left morphs by recreating it from the mouseOverMorphs"
	leftMorphs size > 1 
		ifTrue: [leftMorphs := mouseOverMorphs select: [:m | leftMorphs includes: m]].
	leftMorphs do: [ :m |
		m isInWorld ifTrue: [
			mouseLeaveEvent sendEventTo: m ]].
	
	"Send #mouseEnter as appropriate. Order is outer to Inner."
	mouseEnterEvent := aMouseEvent asMouseEnter.
	enteredMorphs := enteredMorphs contents.
	enteredMorphs reverseDo: [ :m |
		m isInWorld ifTrue: [
			mouseEnterEvent sendEventTo: m ]].
	
	"And remember the over list for the next call"
	mouseOverMorphs := overMorphs contents.
	leftMorphs := enteredMorphs := overMorphs := nil.! !


!Morph methodsFor: 'events-processing' stamp: 'jmv 9/9/2025 09:37:11'!
processMouseOver: aMouseOverEvent localPosition: localEventPosition
	"A MorphicEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	aMouseOverEvent hand noticeMouseOver: self event: aMouseOverEvent.
	
	"Open question: should any unhandled mouse move events be filtered out?
	(i.e. should mouseHover:localPosition: be called when a mouse button is pressed
	but the morph doesn't have mouse button handlers?
	Essentially, what are the limits of what is considered 'hovering'?"
	self handlesMouseHover ifTrue: [
		self
			mouseHover: aMouseOverEvent
			localPosition: localEventPosition ].! !

