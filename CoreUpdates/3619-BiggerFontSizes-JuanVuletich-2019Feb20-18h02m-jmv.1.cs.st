'From Cuis 5.0 of 7 November 2016 [latest update: #3618] on 20 February 2019 at 6:32:54 pm'!

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/20/2019 18:21:26'!
defaultFont11
	"Sets not only fonts but other GUI elements
	to fit medium resolution or medium size screens
	Preferences defaultFont11
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 11)
			(setListFontTo: 11)
			(setMenuFontTo: 12)
			(setWindowTitleFontTo: 14)
			(setCodeFontTo: 11)
			(setButtonFontTo: 11)).
	Preferences enable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/20/2019 18:21:31'!
defaultFont14
	"Sets not only fonts but other GUI elements
	to fit medium resolution or medium size screens
	Preferences defaultFont14
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 14)
			(setListFontTo: 14)
			(setMenuFontTo: 14)
			(setWindowTitleFontTo: 17)
			(setCodeFontTo: 14)
			(setButtonFontTo: 14)).
	Preferences enable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/20/2019 18:21:37'!
defaultFont17
	"Sets not only fonts but other GUI elements
	to fit high resolution or large screens
	Preferences defaultFont17
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 17)
			(setListFontTo: 17)
			(setMenuFontTo: 17)
			(setWindowTitleFontTo: 22)
			(setCodeFontTo: 17)
			(setButtonFontTo: 17)).
	Preferences enable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/20/2019 18:21:43'!
defaultFont22
	"Sets not only fonts but other GUI elements
	to fit high resolution or large screens
	Preferences defaultFont22
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 22)
			(setListFontTo: 22)
			(setMenuFontTo: 22)
			(setWindowTitleFontTo: 28)
			(setCodeFontTo: 22)
			(setButtonFontTo: 22)).
	Preferences enable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/20/2019 18:15:18'!
defaultFont28
	"Sets not only fonts but other GUI elements
	to fit very high resolution or very large screens
	Preferences defaultFont28
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 28)
			(setListFontTo: 28)
			(setMenuFontTo: 28)
			(setWindowTitleFontTo: 36)
			(setCodeFontTo: 28)
			(setButtonFontTo: 28)).
	Preferences enable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/20/2019 18:16:03'!
defaultFont36
	"Sets not only fonts but other GUI elements
	to fit very high resolution or very large screens
	Preferences defaultFont36
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 36)
			(setListFontTo: 36)
			(setMenuFontTo: 36)
			(setWindowTitleFontTo: 46)
			(setCodeFontTo: 36)
			(setButtonFontTo: 36)).
	Preferences enable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/20/2019 18:17:21'!
defaultFont46
	"Sets not only fonts but other GUI elements
	to fit very high resolution or very large screens
	Preferences defaultFont46
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 46)
			(setListFontTo: 46)
			(setMenuFontTo: 46)
			(setWindowTitleFontTo: 60)
			(setCodeFontTo: 46)
			(setButtonFontTo: 46)).
	Preferences enable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/20/2019 18:17:47'!
defaultFont60
	"Sets not only fonts but other GUI elements
	to fit very high resolution or very large screens
	Preferences defaultFont60
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 60)
			(setListFontTo: 60)
			(setMenuFontTo: 60)
			(setWindowTitleFontTo: 80)
			(setCodeFontTo: 60)
			(setButtonFontTo: 60)).
	Preferences enable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/20/2019 18:11:33'!
defaultFont8
	"Sets not only fonts but other GUI elements
	to fit low resolution or small screens
	Preferences defaultFont8
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 8)
			(setListFontTo: 8)
			(setMenuFontTo: 8)
			(setWindowTitleFontTo: 11)
			(setCodeFontTo: 8)
			(setButtonFontTo: 8)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/20/2019 18:18:00'!
defaultFont80
	"Sets not only fonts but other GUI elements
	to fit very high resolution or very large screens
	Preferences defaultFont80
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 80)
			(setListFontTo: 80)
			(setMenuFontTo: 80)
			(setWindowTitleFontTo: 80)
			(setCodeFontTo: 80)
			(setButtonFontTo: 80)).
	Preferences enable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/20/2019 18:20:55'!
defaultFont9
	"Sets not only fonts but other GUI elements
	to fit low resolution or small screens
	Preferences defaultFont9
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 9)
			(setListFontTo: 9)
			(setMenuFontTo: 10)
			(setWindowTitleFontTo: 12)
			(setCodeFontTo: 9)
			(setButtonFontTo: 9)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !


!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/20/2019 18:12:32'!
standardFonts
	"Sets not only fonts but other GUI elements
	to fit regular resolution and size screens
	Preferences standardFonts
	"

	self defaultFont9! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/20/2019 18:21:06'!
verySmallFonts
	"Sets not only fonts but other GUI elements
	to fit very low resolution or very small screens
	Preferences verySmallFonts
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 8)
			(setListFontTo: 7)
			(setMenuFontTo: 7)
			(setWindowTitleFontTo: 9)
			(setCodeFontTo: 7)
			(setButtonFontTo: 7)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !


!StrikeFont class methodsFor: 'instance creation' stamp: 'jmv 2/20/2019 18:03:06'!
install: aString
"
StrikeFont install: 'DejaVu'.

StrikeFont install: 'DejaVu Sans'.
StrikeFont buildLargerPunctuation: 'DejaVu Sans'.
Preferences setDefaultFontFamilyTo: 'DejaVu Sans'.
Preferences standardFonts.
Character initialize.
"
"
StrikeFont install: 'DejaVu Sans Mono'.
Preferences setDefaultFontFamilyTo: 'DejaVu Sans Mono'.
Preferences bigFonts.
Character initialize.
"
"
StrikeFont install: 'Inconsolata'
StrikeFont install: '#PilGi'
StrikeFont install: 'Optima'
StrikeFont install: 'Herculanum'
StrikeFont install: 'Papyrus'
StrikeFont install: 'Handwriting - Dakota'
StrikeFont install: 'Times New Roman'
StrikeFont install: 'Apple Chancery'
StrikeFont install: 'Cochin'
StrikeFont install: 'Cracked'
StrikeFont install: 'Zapfino'
StrikeFont install: 'Brush Script MT'
StrikeFont install: 'Chalkboard'
"
	| fontDict |
	fontDict _ AvailableFonts at: aString ifAbsentPut: Dictionary new.
	#(5 6 7 8 9 10 11 12 14 17 22 28 36 48 60 80) do: [ :s |
		(self create: aString size: s bold: true italic: true boldItalic: false) ifNotNil: [ :font |
			fontDict
				at: s
				put: font ]].
	Preferences restoreDefaultFonts! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/6/2018 16:21:32'!
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


!Theme class methodsFor: 'user interface' stamp: 'jmv 2/20/2019 18:32:44'!
changeFontSizes
	| availableSizes menu |
	availableSizes _ AbstractFont pointSizesFor: Preferences defaultFontFamily.
	menu _ (MenuMorph new defaultTarget: Preferences)
		addTitle: 'Choose a size';
		addStayUpIcons.
	(availableSizes includes: 80) ifTrue: [
		menu add: 'Very High Resolution / Big Fonts (80pix)' action: #defaultFont80 ].
	(availableSizes includes: 80) ifTrue: [
		menu add: 'Very High Resolution / Big Fonts (60pix)' action: #defaultFont60 ].
	(availableSizes includes: 60) ifTrue: [
		menu add: 'Very High Resolution / Reg Fonts (46pix)' action: #defaultFont46 ].
	(availableSizes includes: 46) ifTrue: [
		menu add: 'Very High Resolution / Reg Fonts (36pix)' action: #defaultFont36 ].
	(availableSizes includes: 36) ifTrue: [
		menu add: 'High Resolution / Big Fonts (28pix)' action: #defaultFont28 ].
	(availableSizes includes: 28) ifTrue: [
		menu add: 'High Resolution / Big Fonts (22pix)' action: #defaultFont22 ].
	menu
		add: 'High Resolution / Reg Fonts (17pix)' action: #defaultFont17;
		add: 'High Resolution / Reg Fonts  (14pix)' action: #defaultFont14;
		add: 'Standard Resolution (11pix)' action: #defaultFont11;
		add: 'Standard Resolution (9pix)' action: #defaultFont9;
		add: 'Small Fonts (8pix)' action: #defaultFont8;
		add: 'Very small Fonts'action: #verySmallFonts;
		add: 'Tiny Fonts'action: #tinyFonts;
		popUpInWorld: self runningWorld! !

!methodRemoval: Preferences class #bigFonts!
Preferences class removeSelector: #bigFonts!
!methodRemoval: Preferences class #hugeFonts!
Preferences class removeSelector: #hugeFonts!
!methodRemoval: Preferences class #smallFonts!
Preferences class removeSelector: #smallFonts!
!methodRemoval: Preferences class #veryBigFonts!
Preferences class removeSelector: #veryBigFonts!
