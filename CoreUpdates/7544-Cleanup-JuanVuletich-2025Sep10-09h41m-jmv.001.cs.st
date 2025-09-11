'From Cuis7.5 [latest update: #7542] on 10 September 2025 at 9:42:12 am'!

!MouseOverHandler methodsFor: 'event handling' stamp: 'jmv 9/10/2025 09:33:04'!
dispatchMouseOverEvent: aMouseEvent
	"Generate and dispatch a MouseOver event.
	If appropriate, also generate and dispatch MouseEnter and MouseLeave events."

	| hand |
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

	"Send #mouseLeave as appropriate. Order is inner to outer."
	"Keep the order of the left morphs by recreating it from the mouseOverMorphs"
	leftMorphs size > 1 
		ifTrue: [leftMorphs := mouseOverMorphs select: [:m | leftMorphs includes: m]].
	leftMorphs do: [ :m |
		m isInWorld ifTrue: [
			aMouseEvent asMouseLeave sendEventTo: m ]].
	
	"Send #mouseEnter as appropriate. Order is outer to Inner."
	enteredMorphs reverseDo: [ :m |
		m isInWorld ifTrue: [
			aMouseEvent asMouseEnter sendEventTo: m ]].
	
	"And remember the over list for the next call"
	mouseOverMorphs := overMorphs.
	leftMorphs := enteredMorphs := overMorphs := nil.! !


!IndentingListItemMorph methodsFor: 'events' stamp: 'jmv 9/9/2025 17:14:22'!
mouseLeave: aMouseLeaveEvent
	isHighlighted := false.
	self redrawNeeded.
	super mouseLeave: aMouseLeaveEvent.! !


!PluggableButtonMorph methodsFor: 'events' stamp: 'jmv 9/9/2025 17:14:30'!
mouseLeave: aMouseLeaveEvent
	"The mouse has left the area of the receiver"
	mouseIsOver := false.
	self redrawNeeded.
	super mouseLeave: aMouseLeaveEvent.! !


!HoverableButtonMorph methodsFor: 'events' stamp: 'jmv 9/9/2025 17:14:34'!
mouseLeave: aMouseLeaveEvent
	
	mouseLeaveSelector ifNotNil: [
		model perform: mouseLeaveSelector ].
	super mouseLeave: aMouseLeaveEvent.! !

