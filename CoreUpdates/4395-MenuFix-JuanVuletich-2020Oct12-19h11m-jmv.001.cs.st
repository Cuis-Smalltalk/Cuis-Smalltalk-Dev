'From Cuis 5.0 [latest update: #4392] on 12 October 2020 at 7:18:56 pm'!

!MenuItemMorph methodsFor: 'selecting' stamp: 'jmv 10/12/2020 19:17:55'!
select
	self isSelected: true.
	owner activeSubmenu: subMenu.
	subMenu ifNotNil: [
		self displayBounds ifNotNil: [ :r |
			subMenu delete.
			subMenu
				popUpAdjacentTo: (Array with: r topRight + `10@0` with: r topLeft)
				from: self ].
		subMenu selectItem: nil ]! !

