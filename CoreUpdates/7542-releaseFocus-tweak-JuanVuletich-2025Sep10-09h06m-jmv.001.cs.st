'From Cuis7.5 [latest update: #7540] on 10 September 2025 at 9:09:56 am'!

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 9/9/2025 17:17:12'!
releaseKeyboardFocusIfOrOwnedBy: aMorph
	"If the given morph had the keyboard focus before, release it"
	keyboardFocus ifNotNil: [
		keyboardFocus withAllOwnersDo: [ :outerOwner |
			outerOwner == aMorph ifTrue: [self releaseKeyboardFocus]]]! !


!Morph methodsFor: 'events' stamp: 'jmv 9/10/2025 09:06:47'!
mouseLeave: aMouseLeaveEvent
	"Handle a mouseLeave event, meaning the mouse just left my bounds with no button pressed.
	Note: a Morph must answer true to #handlesMouseOver: in order to receive this message."

	(Preferences at: #focusFollowsMouse)
		ifTrue: [aMouseLeaveEvent hand releaseKeyboardFocusIfOrOwnedBy: self].
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #mouseLeave: 
		ifPresentDo: [ :handler | handler value: aMouseLeaveEvent ].! !

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 9/9/2025 17:17:53'!
delete
	"Remove the receiver as a submorph of its owner and make its 
	new owner be nil."

	| aWorld |
	aWorld := self world ifNil: [ self runningWorld ].
	aWorld ifNotNil: [
		aWorld activeHand ifNotNil: [ :h | h
			releaseKeyboardFocusIfOrOwnedBy: self;
			releaseMouseFocus: self ]].
	self clearDependencyAndEvents.
	self allSubmorphsDo: [ :each |
		each clearDependencyAndEvents ].
	self privateDelete.! !

!methodRemoval: HandMorph #releaseKeyboardFocus: stamp: 'jmv 9/10/2025 09:07:09'!
HandMorph removeSelector: #releaseKeyboardFocus:!
