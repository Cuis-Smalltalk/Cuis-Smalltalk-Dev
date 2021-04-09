'From Cuis 5.0 [latest update: #4566] on 9 April 2021 at 2:41:58 pm'!

!Preferences class methodsFor: 'fonts' stamp: 'jmv 4/9/2021 14:41:51'!
setDefaultFont: aFontName
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
	UISupervisor ui ifNotNil: [ :w | w fontPreferenceChanged ]

! !

