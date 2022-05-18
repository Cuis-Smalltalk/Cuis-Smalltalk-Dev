'From Cuis 6.0 [latest update: #5172] on 18 May 2022 at 8:58:36 am'!

!MenuMorph methodsFor: 'geometry' stamp: 'jmv 5/18/2022 08:58:03'!
fontPreferenceChanged
	super fontPreferenceChanged.
	self adjustSubmorphsLayout.! !


!LabelMorph methodsFor: 'geometry' stamp: 'jmv 5/18/2022 08:56:10'!
fontPreferenceChanged

	super fontPreferenceChanged.
	self font: (PreferenceNG at: #standardListFont).
	self fitContents.! !


!MenuMorph reorganize!
('accessing' addBlankIconsIfNecessary items itemsDo: label lastItem layerNumber stayUp stayUp:)
('construction' add:action: add:action:balloonText: add:action:icon: add:action:icon:enabled: add:subMenu: add:target:action: add:target:action:argument: add:target:action:argument:icon: add:target:action:argumentList: add:target:action:icon: add:targetHighlight:action:argumentList: addItemsFromDictionaries: addLine addServices:for:extraLines: addStayUpIcons addTitle: addUpdating:action: addUpdating:target:action: addUpdating:target:action:argumentList: defaultTarget: labels:lines:selections:)
('control' activeSubmenu: deleteIfPopUp: popUpAdjacentTo:from: popUpAt:forHand:allowKeyboard: popUpAt:forHand:in: popUpAt:forHand:in:allowKeyboard: popUpForHand:in: popUpInWorld popUpInWorld: selectItem: wantsToBeDroppedInto:)
('dropping/grabbing' justDroppedInto:event:)
('events' keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Up:localPosition:)
('event handling testing' handlesKeyboard handlesMouseDown:)
('events-processing' handleFocusEvent:)
('geometry' fontPreferenceChanged)
('initialization' defaultBorderWidth defaultColor delete initialize intoWorld:)
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

