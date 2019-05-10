'From Cuis 5.0 of 7 November 2016 [latest update: #3735] on 6 May 2019 at 5:52:48 pm'!

!FontFamily class methodsFor: 'accessing' stamp: 'jmv 5/6/2019 17:50:27'!
defaultPointSize
	^ DefaultPointSize ! !


!FontChanger class methodsFor: 'prompting user' stamp: 'jmv 5/6/2019 17:48:01'!
promptUserWithFamilies: fontFamilies withMessageOnSelection: aMessageSymbol
	"
	Present a menu of available font families, and if one is chosen, 
	send aMessageSymbol to self with the corresponding font family as argument.
	"
	| fontMenu priorFontFamily |
	priorFontFamily _ FontFamily defaultFamilyName.
	fontMenu _ MenuMorph new defaultTarget: self.
	fontFamilies do: [:fontName | 
		| isCurrent label |
		isCurrent _ priorFontFamily sameAs: fontName.
		label _ self toSelectableMenuLabel: fontName isCurrent: isCurrent. 
		fontMenu
			add: label
			target: self
			action: aMessageSymbol
			argument: fontName.
	].
	fontMenu invokeModal.! !

!FontChanger class methodsFor: 'changing font' stamp: 'jmv 5/6/2019 17:50:36'!
changeTo: aFontName
	"Change the font on the whole system without changing point sizes."
	FontFamily defaultFamilyName: aFontName.
	Preferences
			setDefaultFont: FontFamily defaultFamilyName
			spec: {
				{#setSystemFontTo:. FontFamily defaultPointSize}.
				{#setListFontTo:. Preferences standardListFont pointSize.}.
				{#setMenuFontTo:. Preferences standardMenuFont pointSize.}.
				{#setWindowTitleFontTo:. Preferences windowTitleFont pointSize.}.
				{#setCodeFontTo:. Preferences standardCodeFont pointSize.}.
				{#setButtonFontTo:. Preferences standardButtonFont pointSize.}.
			}.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ].

! !

!FontChanger class methodsFor: 'changing font' stamp: 'jmv 5/6/2019 17:52:10'!
installFontIfNecessary: aFontName
	| isFontInstalled |
	isFontInstalled _ FontFamily familyNames includes: aFontName.
	isFontInstalled ifFalse: [StrikeFont install: aFontName.].! !

