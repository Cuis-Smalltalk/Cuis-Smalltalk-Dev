'From Cuis 5.0 of 7 November 2016 [latest update: #3707] on 3 May 2019 at 6:47:45 pm'!
!classDefinition: #FontChanger category: #'Tools-GUI'!
Object subclass: #FontChanger
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-GUI'!

!FontChanger class methodsFor: 'prompting user' stamp: 'EB 5/3/2019 17:16:14'!
promptUser
	"Present a menu of available font families, and if one is chosen, change to it."
	self promptUserWithFamilies: AbstractFont familyNames.! !

!FontChanger class methodsFor: 'prompting user' stamp: 'EB 5/3/2019 18:11:14'!
promptUserAndInstallIfNecessaryWithFamilies: fontFamilies
	"
	Present a menu of available font families, and if one is chosen, 
	install it if necessary, and change to it.
	"
	self promptUserWithFamilies: fontFamilies withMessageOnSelection: #changeToAndInstallIfNecessary:! !

!FontChanger class methodsFor: 'prompting user' stamp: 'EB 5/3/2019 18:11:30'!
promptUserWithFamilies: fontFamilies
	"Present a menu of font families, and if one is chosen, change to it."
	self promptUserWithFamilies: fontFamilies withMessageOnSelection: #changeTo:! !

!FontChanger class methodsFor: 'prompting user' stamp: 'EB 5/3/2019 18:11:03'!
promptUserWithFamilies: fontFamilies withMessageOnSelection: aMessageSymbol
	"
	Present a menu of available font families, and if one is chosen, 
	send aMessageSymbol to self with the corresponding font family as argument.
	"
	| fontMenu priorFontFamily |
	priorFontFamily _ Preferences defaultFontFamily.
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

!FontChanger class methodsFor: 'changing font' stamp: 'EB 5/3/2019 18:13:51'!
changeToAndInstallIfNecessary: aFontName
	"install the font if necessary, and change to it on the whole system without changing point sizes."
	self installFontIfNecessary: aFontName.
	self changeTo: aFontName.
! !

!FontChanger class methodsFor: 'changing font' stamp: 'EB 5/3/2019 18:13:38'!
installFontIfNecessary: aFontName
	| isFontInstalled |
	isFontInstalled _ AbstractFont availableFonts includesKey: aFontName.
	isFontInstalled ifFalse: [StrikeFont install: aFontName.].! !

!FontChanger class methodsFor: 'private' stamp: 'EB 5/3/2019 17:58:16'!
toSelectableMenuLabel: aString isCurrent: isCurrent
	| label |
	isCurrent ifTrue: [label _ '<on>'] ifFalse: [label _ '<off>'].
	^label, aString! !


!TheWorldMenu methodsFor: 'construction' stamp: 'EB 5/3/2019 18:44:00'!
preferencesMenu
	"Build the preferences menu for the world."

	^ (self menu: 'Preferences...')
		addItemsFromDictionaries: `{
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
				#object 			-> 		FontChanger.
				#selector 		-> 		#promptUserAndInstallIfNecessaryWithFamilies:.
				#arguments 		-> 		#(('DejaVu' 'DejaVu Sans Mono')).
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'change the current font family.'
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
		}`! !


!FontChanger class methodsFor: 'changing font' stamp: 'EB 5/3/2019 16:29:08'!
changeTo: aFontName
	"Change the font on the whole system without changing point sizes."
	Preferences setDefaultFontFamilyTo: aFontName.
	Preferences
			setDefaultFont: Preferences defaultFontFamily
			spec: {
				{#setSystemFontTo:. AbstractFont default pointSize}.
				{#setListFontTo:. Preferences standardListFont pointSize.}.
				{#setMenuFontTo:. Preferences standardMenuFont pointSize.}.
				{#setWindowTitleFontTo:. Preferences windowTitleFont pointSize.}.
				{#setCodeFontTo:. Preferences standardCodeFont pointSize.}.
				{#setButtonFontTo:. Preferences standardButtonFont pointSize.}.
			}.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ].

! !


!FontChanger class reorganize!
('prompting user' promptUser promptUserAndInstallIfNecessaryWithFamilies: promptUserWithFamilies: promptUserWithFamilies:withMessageOnSelection:)
('changing font' changeTo: changeToAndInstallIfNecessary: installFontIfNecessary:)
('private' toSelectableMenuLabel:isCurrent:)
!

