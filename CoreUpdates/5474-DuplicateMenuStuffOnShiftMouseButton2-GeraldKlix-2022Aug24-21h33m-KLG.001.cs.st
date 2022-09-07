'From Haver 6.0 [latest update: #5466] on 25 August 2022 at 9:16:55 pm'!

!MenuItemMorph methodsFor: 'events' stamp: 'KLG 8/25/2022 21:11:47'!
mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition
	"Create a copy of me or my submenu."

	aMouseButtonEvent shiftPressed ifTrue: [
		subMenu
			ifNil: [ self duplicate ]
			ifNotNil: [
				(subMenu duplicate)
					addTitle: contents;
					addStayUpIcons;
					stayUp;
					adjustSubmorphsLayout;
					yourself ] ::
						openInHand ].
	^ super mouseButton2Down: aMouseButtonEvent localPosition: localEventPosition
	! !
