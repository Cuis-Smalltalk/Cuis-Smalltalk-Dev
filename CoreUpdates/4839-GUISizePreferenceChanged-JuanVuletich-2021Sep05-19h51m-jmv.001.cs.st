'From Cuis 5.0 [latest update: #4837] on 5 September 2021 at 7:52:31 pm'!

!Preferences class methodsFor: 'fonts' stamp: 'jmv 9/5/2021 19:52:05'!
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
	MorphicCanvas allSubclassesDo: [ :c| c guiSizePreferenceChanged ].
	UISupervisor ui ifNotNil: [ :w | w fontPreferenceChanged ].! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 9/5/2021 19:51:58'!
setDefaultFont: fontFamilyName spec: defaultFontsSpec
		
	| font |
	defaultFontsSpec do: [ :triplet |
		font _ FontFamily familyName: fontFamilyName pointSize: triplet second.
		font ifNil: [ font _ FontFamily defaultFamilyAndPointSize ]. 
		triplet size > 2 ifTrue: [
			font _ font emphasized: triplet third ].
		self
			perform: triplet first
			with: font ].
	MorphicCanvas allSubclassesDo: [ :c| c guiSizePreferenceChanged ].
	UISupervisor ui ifNotNil: [ :w | w fontPreferenceChanged ].! !

