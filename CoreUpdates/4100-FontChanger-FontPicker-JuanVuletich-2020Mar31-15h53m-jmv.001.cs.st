'From Cuis 5.0 [latest update: #4099] on 31 March 2020 at 3:55:45 pm'!
!classDefinition: #FontPicker category: #'Morphic-Widgets'!
Object subclass: #FontPicker
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!FontFamily class methodsFor: 'accessing fonts' stamp: 'jmv 3/31/2020 10:18:34'!
familyNamed: aString
	"
	FontFamily familyNamed: 'DejaVu'
	"
	^AvailableFamilies at: aString ifAbsent: [].! !


!FontPicker class methodsFor: 'prompting user' stamp: 'jmv 3/31/2020 10:42:28'!
familyNames
	| monospacedStrikeFont options |
	monospacedStrikeFont _ 'DejaVu Sans Mono'.
	options _ FontFamily familyNames.
	(options includes: monospacedStrikeFont) ifFalse: [
		options _ options copyWith: monospacedStrikeFont ].
	^ options! !

!FontPicker class methodsFor: 'prompting user' stamp: 'jmv 3/31/2020 10:42:28'!
promptUser
	"Present a menu of font families, answer selection.
	FontChanger promptUser
	"
	^self promptUserWithFamilies: AbstractFont familyNames.! !

!FontPicker class methodsFor: 'prompting user' stamp: 'jmv 3/31/2020 10:42:28'!
promptUserAndSetDefault
	"Present a menu of available font families, and if one is chosen, change to it."
	self promptUserAndSetDefaultWithFamilies: AbstractFont familyNames.! !

!FontPicker class methodsFor: 'prompting user' stamp: 'jmv 3/31/2020 10:42:28'!
promptUserAndSetDefaultWithFamilies: fontFamilies
	"Present a menu of font families, and if one is chosen, change to it."
	self promptUserWithFamilies: fontFamilies withMessageOnSelection: #setDefaultFont:! !

!FontPicker class methodsFor: 'prompting user' stamp: 'jmv 3/31/2020 10:42:28'!
promptUserInstallIfNecessaryWithFamilies: fontFamilies
	"
	Present a menu of available font families, and if one is chosen, 
	install it if necessary, and change to it.
	"
	self promptUserWithFamilies: fontFamilies withMessageOnSelection: #promptUserAndSetDefaultWithFamilies:! !

!FontPicker class methodsFor: 'prompting user' stamp: 'jmv 3/31/2020 10:42:28'!
promptUserWithFamilies: fontFamilies
	"Present a menu of font families, answer selection."
	| fontMenu priorFontFamily |
	priorFontFamily _ FontFamily defaultFamilyName.
	fontMenu _ MenuMorph new defaultTarget: self.
	fontFamilies do: [:fontName | 
		| isCurrent label |
		isCurrent _ priorFontFamily sameAs: fontName.
		label _ self toSelectableMenuLabel: fontName isCurrent: isCurrent. 
		fontMenu
			add: label
			target: fontMenu
			action: #modalSelection:
			argument: fontName.
	].
	^FontFamily familyNamed: fontMenu invokeModal.! !

!FontPicker class methodsFor: 'prompting user' stamp: 'jmv 3/31/2020 10:42:28'!
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
	^fontMenu invokeModal.! !

!FontPicker class methodsFor: 'changing font' stamp: 'jmv 3/31/2020 10:42:28'!
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

!FontPicker class methodsFor: 'changing font' stamp: 'jmv 3/31/2020 10:42:28'!
installFontIfNecessary: aFontName
	| isFontInstalled |
	isFontInstalled _ FontFamily familyNames includes: aFontName.
	isFontInstalled ifFalse: [StrikeFont install: aFontName.].! !

!FontPicker class methodsFor: 'changing font' stamp: 'jmv 3/31/2020 10:42:28'!
setDefaultAndInstallIfNecessary: aFontName
	"install the font if necessary, and change to it on the whole system without changing point sizes."
	self installFontIfNecessary: aFontName.
	self setDefaultFont: aFontName.
! !

!FontPicker class methodsFor: 'changing font' stamp: 'jmv 3/31/2020 10:42:28'!
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
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ].

! !

!FontPicker class methodsFor: 'private' stamp: 'jmv 3/31/2020 10:42:28'!
toSelectableMenuLabel: aString isCurrent: isCurrent
	| label |
	isCurrent ifTrue: [label _ '<on>'] ifFalse: [label _ '<off>'].
	^label, aString! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 3/31/2020 10:43:09'!
preferencesMenu
	"Build the preferences menu for the world."

	^ (self menu: 'Preferences...')
		addItemsFromDictionaries: {
			{
				#label 			-> 		'Focus follows mouse'.
				#object 			-> 		Preferences.
				#selector 		-> 		#enableFocusFollowsMouse.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'At all times, make the active window and widget the one on which the mouse is located.'
			} asDictionary.
			{
				#label 			-> 		'Click to focus'.
				#object 			-> 		Preferences.
				#selector 		-> 		#disableFocusFollowsMouse.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'At all times, make the active window and widget the one where the mouse was clicked.'
			} asDictionary.
			{
				#label 			-> 		'Font Sizes...'.
				#object 			-> 		Theme.
				#selector 		-> 		#changeFontSizes.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'use larger or smaller text and widgets'
			} asDictionary.
			{
				#label			->			'Set font...'.
				#object 			-> 		FontPicker.
				#selector 		-> 		#promptUserInstallIfNecessaryWithFamilies:.
				#arguments 		-> 		{FontPicker familyNames}.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'change the current font family.'
			} asDictionary.
			{
				#label			->			'Load extra fonts'.
				#object 			-> 		FontFamily.
				#selector 		-> 		#readAdditionalTrueTypeFonts.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'Load additional TrueType fonts included with Cuis.'
			} asDictionary.
			{
				#label 			-> 		'Icons...'.
				#object 			-> 		Theme.
				#selector 		-> 		#changeIcons.
				#icon 			-> 		#worldIcon.
				#balloonText 	-> 		'show more or less icons.'
			} asDictionary.
			{
				#label 			-> 		'Themes...'.
				#object 			-> 		Theme.
				#selector 		-> 		#changeTheme.
				#icon 			-> 		#appearanceIcon.
				#balloonText 	-> 		'switch to another theme.'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Show taskbar'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#showTaskbar.
				#icon 			-> 		#expandIcon.
				#balloonText 	-> 		'show the taskbar'
			} asDictionary.
			{
				#label 			-> 		'Hide taskbar'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#hideTaskbar.
				#icon 			-> 		#collapseIcon.
				#balloonText 	-> 		'hide the taskbar'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Full screen on'.
				#selector 		-> 		#fullScreenOn.
				#icon 			-> 		#viewFullscreenIcon.
				#balloonText 	-> 		'puts you in full-screen mode, if not already there.'
			} asDictionary.
			{
				#label 			-> 		'Full screen off'.
				#selector 		-> 		#fullScreenOff.
				#icon 			-> 		#exitFullscreenIcon.
				#balloonText 	-> 		'if in full-screen mode, takes you out of it.'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Set Code Author...'.
				#object 			-> 		Utilities.
				#selector 		-> 		#setAuthor.
				#icon 			-> 		#usersIcon.
				#balloonText 	-> 		'supply initials to be used to identify the author of code and other content.'
			} asDictionary.
			{
				#label 			-> 		'All preferences...'.
				#object 			-> 		Preferences.
				#selector 		-> 		#openPreferencesInspector.
				#icon 			-> 		#preferencesIcon.
				#balloonText 	-> 		'view and change various options.'
			} asDictionary.
		}! !

!classRemoval: #FontChanger stamp: 'jmv 3/31/2020 15:54:45'!
Smalltalk removeClassNamed: #FontChanger!

