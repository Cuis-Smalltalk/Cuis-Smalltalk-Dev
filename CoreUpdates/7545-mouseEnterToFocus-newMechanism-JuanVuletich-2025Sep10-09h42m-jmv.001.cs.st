'From Cuis7.5 [latest update: #7542] on 10 September 2025 at 9:47:23 am'!

!Morph methodsFor: 'event handling testing' stamp: 'jmv 9/9/2025 16:29:29'!
handlesKeyboardFocus
	"Return true if the receiver wishes to handle keyboard focus"

	^true! !


!EntryFieldMorph methodsFor: 'event handling testing' stamp: 'jmv 9/9/2025 16:54:42'!
handlesKeyboardFocus
	"Not if we are read-only"
	
	^self disablesEditing not! !


!MenuItemMorph methodsFor: 'event handling testing' stamp: 'jmv 9/9/2025 16:59:30'!
handlesKeyboardFocus
	"It is handled by owner?"
	
	^false! !


!InnerHierarchicalListMorph methodsFor: 'event handling testing' stamp: 'jmv 9/9/2025 16:33:31'!
handlesKeyboardFocus
	"Return true if the receiver wishes to handle keyboard focus.
	Answer false because it is handled by our owner, a HierarchicalListMorph"

	^false! !


!InnerListMorph methodsFor: 'event handling testing' stamp: 'jmv 9/9/2025 16:33:58'!
handlesKeyboardFocus
	"Return true if the receiver wishes to handle keyboard focus.
	Answer false because it is handled by our owner, a subinstance of PluggableListMorph"

	^false! !


!InnerTextMorph methodsFor: 'event handling testing' stamp: 'jmv 9/9/2025 16:54:48'!
handlesKeyboardFocus
	"Not if we are read-only"

	^self disablesEditing not! !

!TextModelMorph methodsFor: 'event handling' stamp: 'jmv 9/10/2025 09:54:46'!
keyboardFocusChange: aBoolean
	"If user clicks or mouseEnters into the blank area not not covered by our inner text morph,
	(meaning no scrollbars, and text being shorter than us), then still give keyboard focus to it."
	
	aBoolean ifTrue: [
		self focusText ].! !


!HandMorph methodsFor: 'focus handling' stamp: 'jmv 9/9/2025 17:17:38'!
releaseKeyboardFocusIf: aMorph
	"If the given morph had the keyboard focus before, release it"
	keyboardFocus == aMorph ifTrue: [self releaseKeyboardFocus]! !


!MouseOverHandler methodsFor: 'event handling' stamp: 'jmv 9/10/2025 09:46:10'!
dispatchMouseOverEvent: aMouseEvent
	"Generate and dispatch a MouseOver event.
	If appropriate, also generate and dispatch MouseEnter and MouseLeave events."

	| hand mustSetKeyboardFocus |
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
	enteredMorphs do: [ :m |
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


!Morph methodsFor: 'events' stamp: 'jmv 9/9/2025 17:13:48'!
mouseLeave: aMouseLeaveEvent
	"Handle a mouseLeave event, meaning the mouse just left my bounds with no button pressed.
	Note: a Morph must answer true to #handlesMouseOver: in order to receive this message."

	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #mouseLeave: 
		ifPresentDo: [ :handler | handler value: aMouseLeaveEvent ].! !

