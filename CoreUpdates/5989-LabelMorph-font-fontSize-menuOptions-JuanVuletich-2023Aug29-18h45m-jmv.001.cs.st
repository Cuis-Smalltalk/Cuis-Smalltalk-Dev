'From Cuis 6.0 [latest update: #5987] on 29 August 2023 at 6:47:01 pm'!

!FontFamily class methodsFor: 'ui' stamp: 'jmv 8/29/2023 17:55:45'!
promptUserForSize: currentSize fontFamily: fontFamily
	| menu |
	menu := MenuMorph entitled: 'Font Sizes'.
	menu defaultTarget: menu.
	fontFamily pointSizes do: [ :pt |
		menu
			add: (pt = currentSize ifTrue: ['<on>'] ifFalse: ['<off>']), pt printString
			action: #modalSelection:
			argument: pt ].
	^menu invokeModal! !


!LabelMorph methodsFor: 'menu' stamp: 'jmv 8/29/2023 17:57:15'!
changeFontSize
	| fontFamily |
	fontFamily := self fontToUse family.
	(FontFamily
		promptUserForSize: self fontToUse pointSize
		fontFamily: fontFamily) ifNotNil: [ :ptSize |
			self font: (fontFamily atPointSize: ptSize) ].! !


!LabelMorph methodsFor: 'menu' stamp: 'jmv 8/29/2023 16:59:22'!
addCustomMenuItems: aCustomMenu hand: aHandMorph

	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	aCustomMenu add: 'change font' action: #changeFont.
	aCustomMenu add: 'change font size' action: #changeFontSize.
	aCustomMenu add: 'change emphasis' action: #changeEmphasis.
! !

!LabelMorph methodsFor: 'menu' stamp: 'jmv 8/29/2023 17:58:37'!
changeFont
	| currentFont |
	currentFont := self fontToUse.
	(FontFamily promptUser: currentFont familyName) ifNotNil: [ :fontFamily |
		self font: (fontFamily atPointSize: currentFont pointSize) ].! !

