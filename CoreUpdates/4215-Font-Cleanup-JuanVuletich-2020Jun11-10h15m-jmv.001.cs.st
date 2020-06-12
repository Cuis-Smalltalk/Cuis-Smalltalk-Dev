'From Cuis 5.0 [latest update: #4214] on 11 June 2020 at 10:21:51 am'!

!Preferences class methodsFor: 'fonts' stamp: 'jmv 6/10/2020 22:06:23'!
setDefaultFont: fontFamilyName spec: defaultFontsSpec
		
	| font |
	defaultFontsSpec do: [ :triplet |
		font _ FontFamily familyName: fontFamilyName pointSize: triplet second.
		font ifNil: [ font _ FontFamily defaultFamilyAndPointSize ]. 
		triplet size > 2 ifTrue: [
			font _ font emphasized: triplet third ].
		self
			perform: triplet first
			with: font]! !


!FontFamily class methodsFor: 'initialization' stamp: 'jmv 6/10/2020 22:07:13'!
defaultFamilyName: aStringOrNil defaultPointSize: aNumberOrNil
	"Nil just means leave it as it is now."

	aStringOrNil ifNotNil: [DefaultFamilyName _ aStringOrNil].
	aNumberOrNil ifNotNil: [DefaultPointSize _ aNumberOrNil].! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 6/11/2020 10:19:09'!
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
				#selector 		-> 		#promptUserAndSetDefaultWithFamilies:.
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

!methodRemoval: FontPicker class #promptUserAndSetDefaultInstallIfNecessaryWithFamilies: stamp: 'jmv 6/11/2020 10:19:35'!
FontPicker class removeSelector: #promptUserAndSetDefaultInstallIfNecessaryWithFamilies:!
!methodRemoval: FontPicker class #setDefaultAndInstallIfNecessary: stamp: 'jmv 6/11/2020 10:15:19'!
FontPicker class removeSelector: #setDefaultAndInstallIfNecessary:!
!methodRemoval: FontPicker class #installFontIfNecessary: stamp: 'jmv 6/11/2020 10:15:27'!
FontPicker class removeSelector: #installFontIfNecessary:!
!methodRemoval: StrikeFont class #install: stamp: 'jmv 6/11/2020 10:15:43'!
StrikeFont class removeSelector: #install:!
!methodRemoval: StrikeFont class #create:size:bold:italic:boldItalic: stamp: 'jmv 6/11/2020 10:16:06'!
StrikeFont class removeSelector: #create:size:bold:italic:boldItalic:!
!methodRemoval: StrikeFont class #buildLargerPunctuation: stamp: 'jmv 6/11/2020 10:16:28'!
StrikeFont class removeSelector: #buildLargerPunctuation:!
!methodRemoval: StrikeFont #fixDerivatives stamp: 'jmv 6/11/2020 10:17:33'!
StrikeFont removeSelector: #fixDerivatives!
!methodRemoval: StrikeFont #widen:by: stamp: 'jmv 6/11/2020 10:16:32'!
StrikeFont removeSelector: #widen:by:!
!methodRemoval: StrikeFont #derivativeFont:at: stamp: 'jmv 6/11/2020 10:16:36'!
StrikeFont removeSelector: #derivativeFont:at:!
!methodRemoval: StrikeFont #pointSize: stamp: 'jmv 6/11/2020 10:18:03'!
StrikeFont removeSelector: #pointSize:!
!methodRemoval: StrikeFont #checkCharacter: stamp: 'jmv 6/11/2020 10:16:40'!
StrikeFont removeSelector: #checkCharacter:!
!methodRemoval: StrikeFont #characterToGlyphMap: stamp: 'jmv 6/11/2020 10:16:45'!
StrikeFont removeSelector: #characterToGlyphMap:!
!methodRemoval: StrikeFont #takeGlyphFor:from:in: stamp: 'jmv 6/11/2020 10:16:51'!
StrikeFont removeSelector: #takeGlyphFor:from:in:!
!methodRemoval: StrikeFont #alter:formBlock: stamp: 'jmv 6/11/2020 10:16:54'!
StrikeFont removeSelector: #alter:formBlock:!
!methodRemoval: StrikeFont #buildFromForm:data:name: stamp: 'jmv 6/11/2020 10:16:07'!
StrikeFont removeSelector: #buildFromForm:data:name:!
!methodRemoval: StrikeFont #maxWidth stamp: 'jmv 6/11/2020 10:16:55'!
StrikeFont removeSelector: #maxWidth!
!methodRemoval: StrikeFont #derivativeFonts stamp: 'jmv 6/11/2020 10:17:00'!
StrikeFont removeSelector: #derivativeFonts!
!methodRemoval: AbstractFont #derivativeFonts stamp: 'jmv 6/11/2020 10:17:00'!
AbstractFont removeSelector: #derivativeFonts!
