'From Cuis 5.0 [latest update: #3867] on 8 September 2019 at 7:37:20 pm'!

!FontChanger class methodsFor: 'changing font' stamp: 'jmv 9/8/2019 19:30:55'!
changeTo: aFontName
	"Change the font on the whole system without changing point sizes."
	FontFamily defaultFamilyName: aFontName.
	Preferences
			setDefaultFont: FontFamily defaultFamilyName
			spec: {
				{#setListFontTo:. Preferences standardListFont pointSize.}.
				{#setMenuFontTo:. Preferences standardMenuFont pointSize.}.
				{#setWindowTitleFontTo:. Preferences windowTitleFont pointSize.}.
				{#setCodeFontTo:. Preferences standardCodeFont pointSize.}.
				{#setButtonFontTo:. Preferences standardButtonFont pointSize.}.
			}.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ].

! !

