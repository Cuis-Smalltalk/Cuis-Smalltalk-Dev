"
I simulate the MVC menu classes PopUpMenu, SelectionMenu, and CustomMenu when running in a Morphic world. I am also used to implement Utilities>informUser:during:.
"
Class {
	#name : 'MVCMenuMorph',
	#superclass : 'MenuMorph',
	#instVars : [
		'done',
		'mvcSelection'
	],
	#category : 'Morphic-Menus',
	#package : 'Morphic',
	#tag : 'Menus'
}

{ #category : 'instance creation' }
MVCMenuMorph class >> from: aPopupMenu title: titleStringOrNil [
	"Answer a MenuMorph constructed from the given PopUpMenu. Used to simulate MVC-style menus in a Morphic-only world."

	| menu items lines selections labelString labelPlainString j emphasis |
	menu := self new.
	titleStringOrNil ifNotNil: [
		titleStringOrNil isEmpty ifFalse: [menu addTitle: titleStringOrNil]].
	labelString := aPopupMenu labelString.
	labelPlainString := labelString asPlainString.
	items := labelPlainString lines.
	(labelString is: #Text) ifTrue: [
		"Pass along text emphasis if present"
		j := 1.
		items := items collect: [ :item |
			j := labelPlainString findString: item startingAt: j.
			emphasis := TextEmphasis new emphasisCode: (labelString emphasisAt: j).
			item asText addAttribute: emphasis]].
	lines := aPopupMenu lineArray.
	lines ifNil: [lines := #()].
	menu cancelValue: 0.
	menu defaultTarget: menu.
	selections := (1 to: items size) asArray.
	1 to: items size do: [ :i |
		menu add: (items at: i) target: menu action: #selectMVCItem: argument: (selections at: i) icon: (aPopupMenu iconAt: i).
		(lines includes: i) ifTrue: [menu addLine]].
	^ menu

]

{ #category : 'private' }
MVCMenuMorph >> cancelValue: selectionOrNil [
	"Set the value to be returned if the user cancels without making a selection."

	mvcSelection := selectionOrNil.

]

{ #category : 'invoking' }
MVCMenuMorph >> invokeAt: aPoint allowKeyboard: aBoolean [
	"Add this menu to the given world centered at the given point. Wait for the user to make a selection and answer it. The selection value returned is an integer in keeping with PopUpMenu, if the menu is converted from an MVC-style menu."
	"Details: This is invoked synchronously from the caller. In order to keep processing inputs and updating the screen while waiting for the user to respond, this method has its own version of the World's event loop." 
	| w delay |
	w := self runningWorld.
	w ifNil: [
		| answer |
		answer := self.	"Just a marker object, can not use nil, because it is a possible answer (if user cancels)"
		UISupervisor whenUIinSafeState: [ answer := self invokeAt: aPoint allowKeyboard: aBoolean ].
		 [ answer == self ] whileTrue: [ Processor yield ].
		^answer ].
	w doOneMinimalCycleNow.
	self popUpAt: aPoint allowKeyboard: aBoolean.
	done := false.
	delay := Delay forMilliseconds: 20.
	[ done not and: [self isInWorld] ] whileTrue: [
		self comeToFront.
		w doOneMinimalCycleNow. delay wait ].
	^ mvcSelection 
]

{ #category : 'invoking' }
MVCMenuMorph >> invokeNonModalAt: aPoint allowKeyboard: aBoolean [
	"Add this menu to the given world centered at the given point."

	self
		popUpAt: aPoint
		allowKeyboard: aBoolean.
]

{ #category : 'private' }
MVCMenuMorph >> selectMVCItem: item [
	"Called by the MenuItemMorph that the user selects.
	Record the selection and set the done flag to end this interaction."

	mvcSelection := item.
	done := true.

]
