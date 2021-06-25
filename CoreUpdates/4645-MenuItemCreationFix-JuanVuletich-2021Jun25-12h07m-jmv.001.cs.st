'From Cuis 5.0 [latest update: #4644] on 25 June 2021 at 12:07:37 pm'!

!MenuItemMorph methodsFor: 'initialization' stamp: 'jmv 6/25/2021 12:02:54'!
initialize
	"initialize the state of the receiver"
	super initialize.
	isEnabled _ true.
	subMenu _ nil.
	isSelected _ false.
	target _ nil.
	selector _ nil.
	arguments _ nil.
	font _ Preferences standardMenuFont.
	self contents: ''.! !

