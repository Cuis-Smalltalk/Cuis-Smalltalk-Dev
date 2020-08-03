'From Cuis 5.0 [latest update: #4280] on 22 July 2020 at 11:10:21 pm'!

!HandleMorph methodsFor: 'stepping and presenter' stamp: 'jmv 7/22/2020 23:08:01'!
stepAt: millisecondSinceLast

	pointBlock value: self displayBounds center! !


!MenuItemMorph methodsFor: 'selecting' stamp: 'jmv 7/22/2020 23:08:37'!
select
	self isSelected: true.
	owner activeSubmenu: subMenu.
	subMenu ifNotNil: [
		subMenu delete.
		subMenu
			popUpAdjacentTo: (Array with: self displayBounds topRight + `10@0`
											with: self displayBounds topLeft)
			from: self.
		subMenu selectItem: nil ]! !

