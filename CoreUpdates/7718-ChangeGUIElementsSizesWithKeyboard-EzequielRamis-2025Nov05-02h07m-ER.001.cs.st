'From Cuis7.5 [latest update: #7641] on 5 November 2025 at 3:43:27 am'!

!Theme class methodsFor: 'user interface' stamp: 'ER 11/5/2025 02:59:44'!
chooseDefaultFontSize: pointSize
	"To help user experiment until happy, reopen menu."

	self setDefaultFontSize: pointSize; changeFontSizes.! !

!Theme class methodsFor: 'user interface' stamp: 'ER 11/5/2025 02:56:51'!
setDefaultFontSize: pointSize

	Preferences at: #defaultFontSize put: pointSize.
	Preferences saveToDisk: #defaultFontSize! !

!Theme class methodsFor: 'user interface' stamp: 'ER 11/5/2025 02:57:04'!
largerDefaultFomt
	self setDefaultFontSize: (Preferences at: #defaultFontSize) + 1! !

!Theme class methodsFor: 'user interface' stamp: 'ER 11/5/2025 02:57:09'!
smallerDefaultFont
	self setDefaultFontSize: (Preferences at: #defaultFontSize) - 1! !


!WorldMorph methodsFor: 'submorphs-menus' stamp: 'ER 11/5/2025 03:11:18'!
theme
	"For the benefit of worldMenuSpec"

	^Theme! !


!Theme class methodsFor: 'user interface' stamp: 'ER 11/5/2025 02:59:44'!
changeFontSizes
	| options menu |
	options := #(
		'40 - very large GUI elements'
		'32 - large GUI elements'
		'28 - large GUI elements'
		'24 - very high dpi laptops'
		'21 - very high dpi laptops'
		'18 - high dpi laptops'
		'16 - high dpi laptops'
		'14 - large desktop displays'
		'13 - large desktop displays'
		'12 - standard dpi displays'
		'11 - standard dpi displays'
		'10 - small GUI elements'
		'  9 - small GUI elements'
		'  6 - very small GUI elements'
		).
	menu := MenuMorph new.
	menu addTitle: 'Point size of default Font'; addStayUpIcons; removeStayUpBox.
	options do: [ :option | | pt addedItem |
		pt := option findPositiveInteger.
		addedItem := menu add: option target: self action: #chooseDefaultFontSize: argument: pt.
		(pt = FontFamily defaultPointSize) ifTrue: [ menu selectItem: addedItem ] ].
	menu popUpInWorld: self runningWorld.! !


!WorldMorph methodsFor: 'submorphs-menus' stamp: 'ER 11/5/2025 03:43:08'!
worldMenuSpec
	"Spec for the menu and shortcut keys for the World.
	See coment at Morph >> #getMenu
	"
	^
	#(
		"Keyboard shortcut only. No menu entry."
		#(991 	nil 	$b 	smalltalk 	browse)
		#(992 	nil 	$f 	smalltalk 	findAndBrowseClass)
		#(993 	nil 	$F 	smalltalk 	findInSourceCode)
		#(994 	nil 	$+ 	theme   	largerDefaultFomt)
		#(995 	nil 	$= 	theme   	largerDefaultFomt) "See comment at #isCmdAltGreaterThan. This hack is needed for the shortcut to work on Mac."
		#(996 	nil 	$- 	theme   	smallerDefaultFont)
	)! !

