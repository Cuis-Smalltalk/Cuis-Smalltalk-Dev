'From Cuis7.5 [latest update: #7534] on 9 September 2025 at 11:43:42 am'!

!MenuMorph methodsFor: 'keyboard control' stamp: 'jmv 9/9/2025 10:34:57'!
keyboardFocusChange: aBoolean
	"Notify change due to green border for keyboard focus"

	aBoolean ifFalse: [
		(prevKbdFocus isNil or: [ prevKbdFocus hasKeyboardFocus ]) not ifTrue: [
			self deleteIfPopUp: nil.
			self redrawNeeded ]].
! !


!MenuItemMorph methodsFor: 'selecting' stamp: 'jmv 9/9/2025 10:34:29'!
select
	self isSelected: true.
	owner activeSubmenu: subMenu.
	subMenu ifNotNil: [
		self displayBounds ifNotNil: [ :r |
			subMenu delete.
			subMenu
				popUpAdjacentTo: (Array with: r topRight + `4@0` with: r topLeft)
				from: self ].
		subMenu selectItem: nil ]! !


!MenuMorph reorganize!
('accessing' addBlankIconsIfNecessary defaultLayerNumber items itemsDo: label lastItem stayUp stayUp:)
('construction' add:action: add:action:argument: add:action:balloonText: add:action:icon: add:action:icon:enabled: add:subMenu: add:target:action: add:target:action:argument: add:target:action:argument:icon: add:target:action:argumentList: add:target:action:icon: add:targetHighlight:action:argumentList: addItemFromDictionary: addItemFromDictionary:targeting: addItemFromDictionaryOrNil: addItemsFromDictionaries: addLabel: addLine addStayUpIcons addTitle: addUpdating:action: addUpdating:target:action: addUpdating:target:action:argumentList: defaultTarget: labels:lines:selections:)
('control' activeSubmenu: delete deleteIfPopUp: popUpAdjacentTo:from: popUpAt:allowKeyboard: popUpAt:forHand:in: popUpAt:forHand:in:allowKeyboard: popUpForHand:in: popUpInWorld popUpInWorld: selectItem: wantsToBeDroppedInto:)
('dropping/grabbing')
('events' keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Up:localPosition:)
('event handling testing' handlesKeyboard handlesMouseDown:)
('events-processing' handleMouseFocusEvent:)
('geometry' fontPreferenceChanged)
('initialization' defaultBorderWidth defaultColor initialize intoWorld:)
('keyboard control' displayFiltered: keyboardFocusChange: moveSelectionDown:event:)
('menu' removeStayUpBox)
('modal control' invokeModal invokeModal: isModalInvokationDone isModalInvokationDone: modalSelection modalSelection:)
('testing' isIncludedInTaskbar)
('private' adjustSubmorphsLayout fitInWorld selectedItem)
('drawing' drawOn:)
('misc' activate:)
('modal progress dialog' displayAt:during: informUserAt:during:)
('halos and balloon help' addHalo:)
!

