'From Cuis 5.0 of 7 November 2016 [latest update: #3311] on 5 May 2018 at 7:48:24 pm'!

!MenuMorph methodsFor: 'modal progress dialog' stamp: 'jmv 5/5/2018 19:37:23'!
displayAt: aPoint during: aBlock
	"Add this menu to the Morphic world during the execution of the given block."

	self runningWorld ifNotNil: [ :w |
		w addMorph: self centeredNear: aPoint.
		self world ifNotNil: [ w displayWorld ].  "show myself"
		].
	aBlock value.
	self delete! !

!MenuMorph methodsFor: 'modal progress dialog' stamp: 'jmv 5/5/2018 19:37:19'!
informUserAt: aPoint during: aBlock
	"Add this menu to the Morphic world during the execution of the given block."

	| w titleString |

	titleString _ titleMorph submorphs first.
	self visible: false.
	w _ self world ifNil: [ self runningWorld ].
	aBlock value: [ :string |
		self visible ifFalse: [
			w addMorph: self centeredNear: aPoint.
			self visible: true].
		titleString contents: string.
		titleMorph morphWidth: titleString morphWidth + 8.
		self morphPosition: w activeHand morphPosition.
		self adjustSubmorphsLayout.
		self redrawNeeded.
		w ifNotNil: [
			w displayWorld	].	 "show myself"
	]. 
	self delete.
	w ifNotNil: [
		w displayWorld ]! !


!Utilities class methodsFor: 'user interface' stamp: 'jmv 5/5/2018 19:31:18'!
informUser: aString during: aBlock
	"Display a message above (or below if insufficient room) the cursor during execution of the given block."

	"
	Utilities informUser: 'Just a sec!!' during: [(Delay forSeconds: 1) wait]
	"

	(MenuMorph entitled: aString) 
		displayAt: Sensor mousePoint + 60
		during: aBlock! !

!Utilities class methodsFor: 'user interface' stamp: 'jmv 5/5/2018 19:31:32'!
informUserDuring: barBlock 
	"Display a message above (or below if insufficient room) the cursor during execution of the given block."

	"
	Utilities informUserDuring:[:barBlock|
		#(one two three) do:[:info|
			barBlock value: info.
			(Delay forSeconds: 1) wait]]
	"

	(MenuMorph entitled: '						') 
		informUserAt: Sensor mousePoint
		during: barBlock! !


!MenuMorph methodsFor: 'modal control' stamp: 'jmv 5/5/2018 19:34:12'!
invokeModal
	"Invoke this menu and don't return until the user has chosen a value.
	See example below on how to use modal menu morphs."
	^ self invokeModal: Preferences menuKeyboardControl

	"
	| menu sub entry |
	menu _ MenuMorph new.
	1 to: 3 do: [:i |
		entry _ 'Line', i printString.
		sub _ MenuMorph new.
		menu add: entry subMenu: sub.
		#('Item A' 'Item B' 'Item C')  do:[:subEntry|
			sub add: subEntry target: menu 
				action: #modalSelection: argument: {entry. subEntry}]].
	menu invokeModal.
	"! !


!MVCMenuMorph methodsFor: 'invoking' stamp: 'jmv 3/2/2017 20:08:29'!
invokeAt: aPoint allowKeyboard: aBoolean
	"Add this menu to the given world centered at the given point. Wait for the user to make a selection and answer it. The selection value returned is an integer in keeping with PopUpMenu, if the menu is converted from an MVC-style menu."
	"Details: This is invoked synchronously from the caller. In order to keep processing inputs and updating the screen while waiting for the user to respond, this method has its own version of the World's event loop." 
	|actHand w oldFocus |
	self flag: #bob.		"is <aPoint> global or local?"
	self flag: #arNote.	"<aPoint> is local to aWorld"
	w _ self runningWorld.
	actHand _ w activeHand.
	oldFocus _ actHand keyboardFocus.
	w doOneMinimalCycleNow.
	self
		popUpAt: aPoint
		forHand: actHand
		allowKeyboard: aBoolean.
	done _ false.
	[ self isInWorld & done not ] whileTrue: [ w doOneMinimalCycleNow ].
	self delete.
	oldFocus ifNotNil: [ actHand newKeyboardFocus: oldFocus ].
	^ mvcSelection ! !


!PopUpMenu class methodsFor: 'dialogs' stamp: 'jmv 5/5/2018 19:38:51'!
confirm: queryString
	"Put up a yes/no menu with caption queryString. Answer true if the 
	response is yes, false if no. This is a modal question--the user must 
	respond yes or no."

	"
	PopUpMenu confirm: 'Are you hungry?'
	"

	^ self confirm: queryString trueChoice: 'Yes' falseChoice: 'No'! !

!methodRemoval: MVCMenuMorph #displayAt:during:!
MVCMenuMorph removeSelector: #displayAt:during:!
!methodRemoval: MVCMenuMorph #informUserAt:during:!
MVCMenuMorph removeSelector: #informUserAt:during:!

!MenuMorph reorganize!
('accessing' addBlankIconsIfNecessary items lastItem stayUp stayUp:)
('construction' add:action: add:selector:argument: add:subMenu: add:target:action: add:target:action:argument: add:target:action:argumentList: addLine addList: addServices:for:extraLines: addStayUpIcons addTitle: addUpdating:action: addUpdating:target:action: addUpdating:target:action:argumentList: buildFromDictionaries: defaultTarget: labels:lines:selections: title:)
('control' activeSubmenu: deleteIfPopUp: popUpAdjacentTo:from: popUpAt:forHand:allowKeyboard: popUpAt:forHand:in: popUpAt:forHand:in:allowKeyboard: popUpForHand:in: popUpInWorld popUpInWorld: selectItem: wantsToBeDroppedInto:)
('dropping/grabbing' justDroppedInto:event:)
('events' keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Up:localPosition:)
('event handling testing' handlesKeyboard handlesMouseDown:)
('events-processing' handleFocusEvent:)
('initialization' defaultBorderWidth defaultColor delete initialize intoWorld:)
('keyboard control' displayFiltered: keyboardFocusChange: moveSelectionDown:event:)
('menu' removeStayUpBox)
('modal control' invokeModal invokeModal: isModalInvokationDone isModalInvokationDone: modalSelection modalSelection:)
('private' adjustSubmorphsLayout fitInWorld selectedItem)
('drawing' drawOn:)
('misc' activate:)
('modal progress dialog' displayAt:during: informUserAt:during:)
!

