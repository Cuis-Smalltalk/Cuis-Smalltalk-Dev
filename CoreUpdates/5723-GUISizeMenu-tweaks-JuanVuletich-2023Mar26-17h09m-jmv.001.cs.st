'From Cuis 6.0 [latest update: #5722] on 26 March 2023 at 5:26:58 pm'!

!Theme class methodsFor: 'user interface' stamp: 'jmv 3/26/2023 17:24:13'!
hugeFonts
	Preferences at: #guiElementsSize put: #hugeFonts.
	"Reopen menu"
	self changeFontSizes.! !

!Theme class methodsFor: 'user interface' stamp: 'jmv 3/26/2023 17:24:10'!
largeFonts
	Preferences at: #guiElementsSize put: #largeFonts.
	"Reopen menu"
	self changeFontSizes.! !

!Theme class methodsFor: 'user interface' stamp: 'jmv 3/26/2023 17:24:05'!
smallFonts
	Preferences at: #guiElementsSize put: #smallFonts.
	"Reopen menu"
	self changeFontSizes.! !

!Theme class methodsFor: 'user interface' stamp: 'jmv 3/26/2023 17:24:00'!
standardFonts
	Preferences at: #guiElementsSize put: #standardFonts.
	"Reopen menu"
	self changeFontSizes.! !

!Theme class methodsFor: 'user interface' stamp: 'jmv 3/26/2023 17:23:57'!
tinyFonts
	Preferences at: #guiElementsSize put: #tinyFonts.
	"Reopen menu"
	self changeFontSizes.! !

!Theme class methodsFor: 'user interface' stamp: 'jmv 3/26/2023 17:23:49'!
veryLargeFonts
	Preferences at: #guiElementsSize put: #veryLargeFonts.
	"Reopen menu"
	self changeFontSizes.! !

!Theme class methodsFor: 'user interface' stamp: 'jmv 3/26/2023 17:23:44'!
verySmallFonts
	Preferences at: #guiElementsSize put: #verySmallFonts.
	"Reopen menu"
	self changeFontSizes.! !


!Theme class methodsFor: 'user interface' stamp: 'jmv 3/26/2023 17:19:20'!
changeFontSizes
	| options menu |
	options := #(
		#('Huge' #hugeFonts)
		#('Very Large' #veryLargeFonts)
		#('Large' #largeFonts)
		#('Default Size' #standardFonts)
		#('Small' #smallFonts)
		#('Very Small' #verySmallFonts)
		#('Tiny' #tinyFonts)).
	menu := MenuMorph new.
	menu addTitle: 'Make GUI elements'.
	options do: [ :pair |
		(menu add: pair first target: self action: pair second)
			isSelected: (Preferences at: #guiElementsSize) == pair second  ].
	menu popUpInWorld: self runningWorld.! !

