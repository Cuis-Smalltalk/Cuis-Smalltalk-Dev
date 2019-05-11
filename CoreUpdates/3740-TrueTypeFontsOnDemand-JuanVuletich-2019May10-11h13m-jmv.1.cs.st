'From Cuis 5.0 of 7 November 2016 [latest update: #3739] on 10 May 2019 at 11:14:14 am'!

!FontFamily class methodsFor: 'file read write' stamp: 'jmv 5/10/2019 10:04:51'!
readAdditionalTrueTypeFonts
	Feature require: 'VectorGraphics'.
	Smalltalk at: #TrueTypeFontFamily ifPresent: [ :cls | cls readAdditionalFonts ]! !


!FontChanger class methodsFor: 'prompting user' stamp: 'jmv 5/10/2019 10:40:37'!
familyNames
	| monospacedStrikeFont options |
	monospacedStrikeFont _ 'DejaVu Sans Mono'.
	options _ FontFamily familyNames.
	(options includes: monospacedStrikeFont) ifFalse: [
		options _ options copyWith: monospacedStrikeFont ].
	^ options! !


!Preferences class methodsFor: 'fonts' stamp: 'jmv 5/10/2019 11:12:15'!
setDefaultFont: fontFamilyName spec: defaultFontsSpec
		
	| font |
	defaultFontsSpec do: [ :triplet |
		font _ FontFamily familyName: fontFamilyName pointSize: triplet second.
		font ifNil: [
			self revisar. "Please remove this hack meant to install larger DejaVu StrikeFonts on demand."
			StrikeFont install: FontFamily defaultFamilyName.
			font _ FontFamily familyName: fontFamilyName pointSize: triplet second ].
		font ifNil: [ font _ FontFamily defaultFamilyAndPointSize ]. 
		triplet size > 2 ifTrue: [
			font _ font emphasized: triplet third ].
		self
			perform: triplet first
			with: font]! !


!StrikeFont methodsFor: 'emphasis' stamp: 'jmv 5/6/2019 18:04:56'!
makeItalicGlyphs
	"Make an italic set of glyphs with same widths by skewing left and right.
	In the process, characters would overlap, so we widen them all first.
	"
	| extraWidth newGlyphs newXTable x newX w extraOnLeft |  
	extraOnLeft _ (self lineSpacing-1-self ascent+4)//4 max: 0.
	extraWidth _ ((self ascent-5+4)//4 max: 0) + extraOnLeft.
	newGlyphs _ Form extent: (glyphs width + (maxAscii + 1 - minAscii*extraWidth)) @ glyphs height depth: glyphs depth.
	newGlyphs fillWhite.
	newXTable _ xTable copy.

	"Copy glyphs into newGlyphs with room on left and right for overlap."
	minAscii to: maxAscii+1 do:
		[:ascii | x _ xTable at: ascii+1.  w _ (xTable at: ascii+2) - x.
		newX _ newXTable at: ascii+1.
		newGlyphs copy: ((newX + extraOnLeft) @ 0 extent: w @ glyphs height)
			from: x @ 0 in: glyphs rule: Form over.
		newXTable at: ascii+2 put: newX + w + extraWidth].		
	glyphs _ newGlyphs. 
	xTable _ newXTable.
	"Slide the bitmaps left and right for synthetic italic effect."
	4 to: self ascent-1 by: 4 do:
		[:y | 		"Slide ascenders right..."
		glyphs copy: (1@0 extent: glyphs width @ (self ascent - y))
			from: 0@0 in: glyphs rule: Form over].
	self ascent to: self lineSpacing-1 by: 4 do:
		[:y | 		"Slide descenders left..."
		glyphs copy: (0@y extent: glyphs width @ glyphs height)
			from: 1@y in: glyphs rule: Form over].
	self isSynthetic: true! !


!FontFamily class methodsFor: 'initialization' stamp: 'jmv 5/10/2019 11:12:22'!
defaultFamilyName: aString
	| family |
	self revisar. "Please remove this hack meant to install larger DejaVu StrikeFonts on demand."
	aString =  'DejaVu' ifTrue: [
		family _ AvailableFamilies at: aString.
		(family pointSizes includes: DefaultPointSize) ifFalse: [ StrikeFont install: aString]].
	DefaultFamilyName _ aString! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 5/10/2019 10:21:36'!
drawLabelOn: aCanvas

	| e x0 y0 f w availableW l |
	f _ Preferences windowTitleFont.
	e _ self boxExtent.
	x0 _  e x * 4 + 14.
	y0 _ 2+3.
	y0 _ e y - f ascent // 2.
	availableW _ extent x - x0.
	l _ labelString.
	w _ f widthOfString: l.
	[ w > availableW ] whileTrue: [
		l _ l squeezedTo: (1.0 * l size * availableW / w) truncated.
		l isEmpty ifTrue: [ ^self ].
		w _ f widthOfString: l ].
	aCanvas
		drawString: l
		at: x0@y0
		font: f
		color: Theme current windowLabel
		embossed: Theme current embossedTitles! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/10/2019 10:39:45'!
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
				#object 			-> 		FontChanger.
				#selector 		-> 		#promptUserAndInstallIfNecessaryWithFamilies:.
				#arguments 		-> 		{FontChanger familyNames}.
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

