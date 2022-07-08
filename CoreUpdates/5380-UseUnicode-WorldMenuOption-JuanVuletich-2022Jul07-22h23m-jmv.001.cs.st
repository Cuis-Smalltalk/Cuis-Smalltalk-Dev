'From Cuis 6.0 [latest update: #5379] on 7 July 2022 at 10:23:36 pm'!

!TextModel methodsFor: 'misc' stamp: 'jmv 7/7/2022 22:16:10'!
refetch
	"Nothing here. Answer true if actualContents was actually fetched."
	self basicActualContents: self actualContents.
	^false! !


!TextModel class methodsFor: 'handling of Unicode' stamp: 'jmv 7/7/2022 21:55:40'!
useUtf8Editors
	"
	Text editors will work on instances of Utf8String and UnicodeCodePoint.
	This supports the whole Unicode Code Point range
	Requires the use of TrueType fonts. StrikeFonts can not be used.
	Requires the use of Utf8Files. ExtendedAscii files can not be used.
	See #useUtf8Files and #useExtendedAsciiFiles.
	See also #useExtendedAsciiEditors
	TextModel useUtf8Editors
	"
	Feature require: 'VectorGraphics'.
	FileIOAccessor useUtf8Files.
	UnicodeContentsHandling _ #useUtf8Editors.
	UISupervisor ui ifNotNil: [ :w | w whenUIinSafeState: [ w fontPreferenceChanged ]].! !


!InnerTextMorph methodsFor: 'notifications' stamp: 'jmv 7/7/2022 21:51:10'!
fontPreferenceChanged

	super fontPreferenceChanged.
	hasUnacceptedEdits
		ifTrue: [ model basicActualContents: model actualContents ]
		ifFalse: [ model refetch ].
	self updateFromTextComposition.! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 7/7/2022 21:37:17'!
preferencesMenu
	"Build the preferences menu for the world."

	^ (self menu: 'Preferences...')
		addItemsFromDictionaries: {
			{
				#label 			-> 		'Focus follows mouse'.
				#selector 			-> 		#enableFocusFollowsMouse.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'At all times, make the active window and widget the one on which the mouse is located.'
			} asDictionary.
			{
				#label 			-> 		'Click to focus'.
				#selector 			-> 		#disableFocusFollowsMouse.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'At all times, make the active window and widget the one where the mouse was clicked.'
			} asDictionary.
			{
				#label 			-> 		'Use Unicode text'.
				#object 			-> 		TextModel.
				#selector 			-> 		#useUtf8Editors.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'Load the VectorGraphics package, use TrueType fonts, and Unicode text.'
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

