'From Cuis7.5 [latest update: #7559] on 11 September 2025 at 3:54:50 pm'!

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

