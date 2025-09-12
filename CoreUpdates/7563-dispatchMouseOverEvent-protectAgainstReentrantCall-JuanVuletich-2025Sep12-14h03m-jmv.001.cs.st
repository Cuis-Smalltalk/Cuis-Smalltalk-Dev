'From Cuis7.5 [latest update: #7562] on 12 September 2025 at 2:04:10 pm'!

!MouseOverHandler methodsFor: 'event handling' stamp: 'jmv 9/12/2025 14:03:39'!
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
	
	mustSetKeyboardFocus := Preferences at: #focusFollowsMouse.

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

