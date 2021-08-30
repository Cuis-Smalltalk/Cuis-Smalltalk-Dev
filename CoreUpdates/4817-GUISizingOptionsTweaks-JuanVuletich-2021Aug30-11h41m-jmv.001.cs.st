'From Cuis 5.0 [latest update: #4815] on 30 August 2021 at 11:52:44 am'!

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 8/30/2021 11:48:44'!
bigFonts
	"Sets not only fonts but other GUI elements
	to fit regular resolution and size screens
	Preferences bigFonts
	"

	self defaultFont22! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 8/30/2021 11:51:19'!
hugeFonts
	"Sets not only fonts but other GUI elements
	to fit regular resolution and size screens
	Preferences hugeFonts
	"

	self defaultFont60! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 8/30/2021 11:47:04'!
smallFonts
	"Sets not only fonts but other GUI elements
	to fit very low resolution or very small screens
	Preferences smallFonts
	"
	
	self defaultFont12! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 8/30/2021 11:50:41'!
veryBigFonts
	"Sets not only fonts but other GUI elements
	to fit regular resolution and size screens
	Preferences veryBigFonts
	"

	self defaultFont36! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 8/30/2021 11:47:10'!
verySmallFonts
	"Sets not only fonts but other GUI elements
	to fit very low resolution or very small screens
	Preferences verySmallFonts
	"
	
	self defaultFont10! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 8/30/2021 11:44:13'!
preferencesMenu
	"Build the preferences menu for the world."

	^ (self menu: 'Preferences...')
		addItemsFromDictionaries: {
			{
				#label 			-> 		'Focus follows mouse'.
				#object 			-> 		Preferences.
				#selector 			-> 		#enableFocusFollowsMouse.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'At all times, make the active window and widget the one on which the mouse is located.'
			} asDictionary.
			{
				#label 			-> 		'Click to focus'.
				#object 			-> 		Preferences.
				#selector 			-> 		#disableFocusFollowsMouse.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'At all times, make the active window and widget the one where the mouse was clicked.'
			} asDictionary.
			{
				#label 			-> 		'Size of GUI elements...'.
				#object 			-> 		Theme.
				#selector 			-> 		#changeFontSizes.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'use larger or smaller text and widgets'
			} asDictionary.
			{
				#label			->		'Set System Font...'.
				#object 			-> 		FontFamily.
				#selector 			-> 		#promptUserAndSetDefault.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'change the current system font family.'
			} asDictionary.
			{
				#label			->		'Load all TrueType Fonts'.
				#object 			-> 		FontFamily.
				#selector 			-> 		#readAdditionalTrueTypeFonts.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'Load additional TrueType fonts included with Cuis.'
			} asDictionary.
			{
				#label 			-> 		'Icons...'.
				#object 			-> 		Theme.
				#selector 			-> 		#changeIcons.
				#icon 			-> 		#worldIcon.
				#balloonText 	-> 		'show more or less icons.'
			} asDictionary.
			{
				#label 			-> 		'Themes...'.
				#object 			-> 		Theme.
				#selector 			-> 		#changeTheme.
				#icon 			-> 		#appearanceIcon.
				#balloonText 	-> 		'switch to another theme.'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Show taskbar'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#showTaskbar.
				#icon 			-> 		#expandIcon.
				#balloonText 	-> 		'show the taskbar'
			} asDictionary.
			{
				#label 			-> 		'Hide taskbar'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#hideTaskbar.
				#icon 			-> 		#collapseIcon.
				#balloonText 	-> 		'hide the taskbar'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Full screen on'.
				#selector 			-> 		#fullScreenOn.
				#icon 			-> 		#viewFullscreenIcon.
				#balloonText 	-> 		'puts you in full-screen mode, if not already there.'
			} asDictionary.
			{
				#label 			-> 		'Full screen off'.
				#selector 			-> 		#fullScreenOff.
				#icon 			-> 		#exitFullscreenIcon.
				#balloonText 	-> 		'if in full-screen mode, takes you out of it.'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Set Code Author...'.
				#object 			-> 		Utilities.
				#selector 			-> 		#setAuthor.
				#icon 			-> 		#usersIcon.
				#balloonText 	-> 		'supply initials to be used to identify the author of code and other content.'
			} asDictionary.
			{
				#label 			-> 		'All preferences...'.
				#object 			-> 		Preferences.
				#selector 			-> 		#openPreferencesInspector.
				#icon 			-> 		#preferencesIcon.
				#balloonText 	-> 		'view and change various options.'
			} asDictionary.
		}! !


!Theme class methodsFor: 'user interface' stamp: 'jmv 8/30/2021 11:43:38'!
changeFontSizes

	(MenuMorph new defaultTarget: Preferences)
		addTitle: 'Make GUI elements';
		addStayUpIcons;
		add: 'Huge' action: #hugeFonts;
		add: 'Very Big' action: #veryBigFonts;
		add: 'Big' action: #bigFonts;
		add: 'Standard Size' action: #standardFonts;
		add: 'Small' action: #smallFonts;
		add: 'Very Small'action: #verySmallFonts;
		add: 'Tiny'action: #tinyFonts;
		popUpInWorld: self runningWorld! !

