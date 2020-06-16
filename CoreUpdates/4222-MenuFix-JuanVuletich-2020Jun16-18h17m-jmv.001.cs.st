'From Cuis 5.0 [latest update: #4221] on 16 June 2020 at 6:17:57 pm'!

!MenuItemMorph methodsFor: 'layout' stamp: 'jmv 6/14/2020 18:18:47'!
magnifiedIcon

	| iconForm w h factor magnifiedExtent magnifiedIcon |
	icon ifNil: [ ^nil ].
	iconForm _ isEnabled ifTrue: [ icon ] ifFalse: [ icon asGrayScaleAndTransparent ].
	magnifiedIcon _ iconForm.
	w _ iconForm width.
	h _ iconForm height.
	w*h = 0 ifFalse: [
		factor _ extent y * 0.8 / h.
		factor = 1.0 ifFalse: [
			magnifiedExtent _ (iconForm extent * factor) rounded.
			magnifiedIcon _ iconForm magnifyTo: magnifiedExtent ]].
	^magnifiedIcon! !


!MenuItemMorph methodsFor: 'drawing' stamp: 'jmv 6/14/2020 18:19:11'!
drawOn: aCanvas 
	| stringColor leftEdge magnifiedIcon |

	stringColor _ color.
	isSelected & isEnabled
		ifTrue: [
			aCanvas fillRectangle: self morphLocalBounds color: Theme current menuHighlight].
	leftEdge _ 0.

	self hasMarker ifTrue: [
		leftEdge _ leftEdge + submorphs first morphWidth + 8 ].

	icon ifNotNil: [
		magnifiedIcon _ self magnifiedIcon.
		aCanvas image: magnifiedIcon at: leftEdge+1 @ (magnifiedIcon height *1//10).
		leftEdge _ magnifiedIcon width *12//10 + leftEdge].

	aCanvas
		drawString: contents
		at: leftEdge @ 1
		font: self fontToUse
		color: stringColor.
	subMenu ifNotNil: [
		aCanvas
			image: self class subMenuMarker
			at: extent x - 8 @ (extent y - self class subMenuMarker height // 2) ]! !

!MenuItemMorph methodsFor: 'layout' stamp: 'jmv 6/14/2020 18:19:22'!
minItemWidth
	| fontToUse iconWidth subMenuWidth markerWidth |
	fontToUse _ self fontToUse.
	subMenuWidth _ self hasSubMenu
				ifFalse: [0]
				ifTrue: [10].
	iconWidth _ icon
				ifNotNil: [self magnifiedIcon width * 12//10]
				ifNil: [0].
	markerWidth _ self hasMarker
		ifTrue: [ submorphs first morphWidth + 8 ]
		ifFalse: [ 0 ].
	^ (fontToUse widthOfString: contents)
		+ subMenuWidth + iconWidth + markerWidth.! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 6/14/2020 18:04:51'!
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
				#label			->			'Set System Font...'.
				#object 			-> 		FontFamily.
				#selector 		-> 		#promptUserAndSetDefault.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'change the current system font family.'
			} asDictionary.
			{
				#label			->			'Load all TrueType Fonts'.
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

!methodRemoval: MenuItemMorph #iconSeparation stamp: 'jmv 6/16/2020 18:17:30'!
MenuItemMorph removeSelector: #iconSeparation!
