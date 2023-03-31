'From Cuis 6.0 [latest update: #5735] on 31 March 2023 at 3:17:21 pm'!

!Theme class methodsFor: 'user interface' stamp: 'jmv 3/31/2023 12:17:06'!
setDefaultFontSize: pointSize
	"To help user experiment until happy, reopen menu."

	PreferenceSet defaultFontSize: pointSize.
	self changeFontSizes.! !


!PreferenceSet class methodsFor: 'sys data' stamp: 'jmv 3/31/2023 12:09:31'!
prefFont
	^ #( 
		#(aaFontsColormapDepth 4)
		#(cacheTrueTypeGlyphs true )
		#(properDisplayAlphaForFonts false )	
		#(subPixelRenderColorFonts true )
		#(subPixelRenderFonts true )	
	)! !

!PreferenceSet class methodsFor: 'sys preference' stamp: 'jmv 3/31/2023 12:09:22'!
init
" Not name #initialize to avoid auto start at class installation "
	#(gui font programming system) with: #(prefGui prefFont prefProgramming prefSystem) do: [ :category :selector |
		(self perform: selector) do: [ :aPrefArray |
			self installDefault: aPrefArray in: category ] ].
	self installHaloPreferencesWith: self iconicHaloSpecifications.
	self installMiscPreferences.
	self defaultFontSize: 20.
	self setDefaultFont: FontFamily defaultFamilyName.! !

!PreferenceSet class methodsFor: 'sys event handlers' stamp: 'jmv 3/31/2023 12:03:53'!
defaultFontSize: pointSize
"
PreferenceSet defaultFontSize: 11
PreferenceSet defaultFontSize: 14
PreferenceSet defaultFontSize: 21
"
	| font titleFont |
	font := FontFamily familyName: FontFamily defaultFamilyName pointSize: pointSize.
	Preferences name: #standardListFont category: #font value: font.
	Preferences name: #standardMenuFont category: #font value: font.
	Preferences name: #standardCodeFont category: #font value: font.
	Preferences name: #standardButtonFont category: #font value: font.
	FontFamily defaultFamilyName: font familyName defaultPointSize: pointSize.	
	titleFont := FontFamily familyName: FontFamily defaultFamilyName pointSize: pointSize * 12//10.
	Preferences name: #windowTitleFont category: #font value: titleFont.
	MorphicCanvas allSubclassesDo: [ :c| c guiSizePreferenceChanged ].
	UISupervisor ui ifNotNil: [ :w | w fontPreferenceChanged ].
	Preferences at: #biggerCursors put: (pointSize > 30).
	Cursor defaultCursor activateCursor.
	MenuItemMorph initializeSubMenuMarker.
	^ font! !


!FontFamily class methodsFor: 'accessing' stamp: 'jmv 3/31/2023 12:10:45'!
defaultPointSize
	"
	FontFamily defaultPointSize
	"
	^ DefaultPointSize ! !


!Theme class methodsFor: 'user interface' stamp: 'jmv 3/31/2023 14:54:33'!
changeFontSizes
	| options menu |
	options := #(
		"'40 - very large'"
		'32 - very high resolution laptops'
		'28 - very high resolution laptops'
		'24 - very high resolution laptops'
		'18 - high resolution laptops'
		'16 - high resolution laptops'
		'14 - large desktop displays'
		'13 - large desktop displays'
		'12 - standard desktop and laptops'
		'11 - standard desktop and laptops'
		'10 - screen with limited space'
		'  9 - screen with limited space'
		"'  6 - displays with very limited space'"
		).
	menu := MenuMorph new.
	menu addTitle: 'Point size of default Font'; addStayUpIcons; removeStayUpBox.
	options do: [ :option | | pt |
		pt := option findPositiveInteger.
		(menu add: option target: self action: #setDefaultFontSize: argument: pt)
			isSelected: pt = FontFamily defaultPointSize ].
	menu popUpInWorld: self runningWorld.! !

!methodRemoval: Theme class #veryLargeFonts stamp: 'jmv 3/31/2023 15:12:29'!
Theme class removeSelector: #veryLargeFonts!
!methodRemoval: Theme class #standardFonts stamp: 'jmv 3/31/2023 15:12:29'!
Theme class removeSelector: #standardFonts!
!methodRemoval: Theme class #verySmallFonts stamp: 'jmv 3/31/2023 15:12:29'!
Theme class removeSelector: #verySmallFonts!
!methodRemoval: Theme class #hugeFonts stamp: 'jmv 3/31/2023 15:12:29'!
Theme class removeSelector: #hugeFonts!
!methodRemoval: Theme class #largeFonts stamp: 'jmv 3/31/2023 15:12:29'!
Theme class removeSelector: #largeFonts!
!methodRemoval: Theme class #smallFonts stamp: 'jmv 3/31/2023 15:12:29'!
Theme class removeSelector: #smallFonts!
!methodRemoval: Theme class #tinyFonts stamp: 'jmv 3/31/2023 15:12:29'!
Theme class removeSelector: #tinyFonts!
!methodRemoval: PreferenceSet class #fontSizes stamp: 'jmv 3/31/2023 15:12:09'!
PreferenceSet class removeSelector: #fontSizes!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Preferences allPreferences removeKey: #guiElementsSize!

