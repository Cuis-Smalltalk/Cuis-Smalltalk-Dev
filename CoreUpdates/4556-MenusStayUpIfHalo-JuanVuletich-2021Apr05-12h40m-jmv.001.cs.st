'From Cuis 5.0 [latest update: #4552] on 5 April 2021 at 12:43:44 pm'!

!MenuMorph methodsFor: 'as yet unclassified' stamp: 'jmv 4/5/2021 12:42:02'!
addHalo: evt
	self stayUp.
	super addHalo: evt! !


!MenuMorph reorganize!
('accessing' addBlankIconsIfNecessary items itemsDo: label lastItem stayUp stayUp:)
('construction' add:action: add:action:balloonText: add:action:icon: add:action:icon:enabled: add:subMenu: add:target:action: add:target:action:argument: add:target:action:argument:icon: add:target:action:argumentList: add:target:action:icon: add:targetHighlight:action:argumentList: addItemsFromDictionaries: addLine addServices:for:extraLines: addStayUpIcons addTitle: addUpdating:action: addUpdating:target:action: addUpdating:target:action:argumentList: defaultTarget: labels:lines:selections:)
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
('halos and balloon help')
('as yet unclassified' addHalo:)
!

