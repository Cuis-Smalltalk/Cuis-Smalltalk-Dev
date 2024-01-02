'From Cuis6.3 [latest update: #6160] on 2 January 2024 at 1:56:32 pm'!

!FontFamily class methodsFor: 'ui' stamp: 'KLG 1/2/2024 13:54:22'!
promptUserForSize: currentSize fontFamily: fontFamily
	| menu |
	menu := MenuMorph entitled: 'Font Sizes'.
	menu defaultTarget: menu.
	fontFamily pointSizes do: [ :pt | | itemText sizeToUse |
		itemText := pt printString.
		sizeToUse := pt.
		menu
			add: (pt = currentSize) asMenuItemTextPrefix, itemText
			action: #modalSelection:
			argument: pt ::
				font: (fontFamily atPointSize: sizeToUse) ].
	^ menu invokeModal! !

