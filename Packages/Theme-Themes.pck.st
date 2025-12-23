'From Cuis7.7 [latest update: #7779] on 23 December 2025 at 4:31:49 pm'!
'Description Theme customizer'!
!provides: 'Theme-Themes' 1 20!
SystemOrganization addCategory: #'Theme-Themes'!


!classDefinition: #ClassicTheme category: #'Theme-Themes'!
Theme subclass: #ClassicTheme
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Theme-Themes'!
!classDefinition: 'ClassicTheme class' category: #'Theme-Themes'!
ClassicTheme class
	instanceVariableNames: ''!

!classDefinition: #DarkBlueTheme category: #'Theme-Themes'!
Theme subclass: #DarkBlueTheme
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Theme-Themes'!
!classDefinition: 'DarkBlueTheme class' category: #'Theme-Themes'!
DarkBlueTheme class
	instanceVariableNames: ''!

!classDefinition: #DarkColorTheme category: #'Theme-Themes'!
Theme subclass: #DarkColorTheme
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Theme-Themes'!
!classDefinition: 'DarkColorTheme class' category: #'Theme-Themes'!
DarkColorTheme class
	instanceVariableNames: ''!

!classDefinition: #HighContrastBlackTheme category: #'Theme-Themes'!
Theme subclass: #HighContrastBlackTheme
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Theme-Themes'!
!classDefinition: 'HighContrastBlackTheme class' category: #'Theme-Themes'!
HighContrastBlackTheme class
	instanceVariableNames: ''!

!classDefinition: #HighContrastWhiteTheme category: #'Theme-Themes'!
Theme subclass: #HighContrastWhiteTheme
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Theme-Themes'!
!classDefinition: 'HighContrastWhiteTheme class' category: #'Theme-Themes'!
HighContrastWhiteTheme class
	instanceVariableNames: ''!

!classDefinition: #LightBluetheme category: #'Theme-Themes'!
Theme subclass: #LightBluetheme
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Theme-Themes'!
!classDefinition: 'LightBluetheme class' category: #'Theme-Themes'!
LightBluetheme class
	instanceVariableNames: ''!

!classDefinition: #LightGrayTheme category: #'Theme-Themes'!
Theme subclass: #LightGrayTheme
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Theme-Themes'!
!classDefinition: 'LightGrayTheme class' category: #'Theme-Themes'!
LightGrayTheme class
	instanceVariableNames: ''!

!classDefinition: #LightTheme category: #'Theme-Themes'!
Theme subclass: #LightTheme
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Theme-Themes'!
!classDefinition: 'LightTheme class' category: #'Theme-Themes'!
LightTheme class
	instanceVariableNames: ''!

!classDefinition: #PersonalizedTheme category: #'Theme-Themes'!
Theme subclass: #PersonalizedTheme
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Theme-Themes'!
!classDefinition: 'PersonalizedTheme class' category: #'Theme-Themes'!
PersonalizedTheme class
	instanceVariableNames: 'writing secondary primary labels background savedThemes'!

!classDefinition: #ThemeCustomizerModel category: #'Theme-Themes'!
ActiveModel subclass: #ThemeCustomizerModel
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Theme-Themes'!
!classDefinition: 'ThemeCustomizerModel class' category: #'Theme-Themes'!
ThemeCustomizerModel class
	instanceVariableNames: ''!

!classDefinition: #ThemeCustomizerWindow category: #'Theme-Themes'!
SystemWindow subclass: #ThemeCustomizerWindow
	instanceVariableNames: 'backgroundPalette primaryPalette labelsPalette textPalette secondaryPalette'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Theme-Themes'!
!classDefinition: 'ThemeCustomizerWindow class' category: #'Theme-Themes'!
ThemeCustomizerWindow class
	instanceVariableNames: ''!


!ClassicTheme commentStamp: 'cbr 4/22/2011 12:07' prior: 0!
This is the theme from Cuis 2.9.


Some of the icons in this theme were originally created with:

	Clipboard default storeObject:
	 	((PNGReadWriter bytesFor: (Form fromFileNamed: 'appearanceIcon.png')) asString base64Encoded)!

!DarkBlueTheme commentStamp: '<historical>' prior: 0!
A nice deep blue theme.!

!DarkColorTheme commentStamp: 'jmv 11/18/2010 13:15' prior: 0!
Most colors are the based on ColorTheme:
	baseColor adjustSaturation: -0.1 brightness: -0.7!

!HighContrastBlackTheme commentStamp: '<historical>' prior: 0!
A very dark, high contrast theme.!

!HighContrastWhiteTheme commentStamp: '<historical>' prior: 0!
Bright white theme: good for subclassing.!

!LightBluetheme commentStamp: '<historical>' prior: 0!
A nice deep blue theme.!

!LightGrayTheme commentStamp: '<historical>' prior: 0!
A light gray theme. Easy on those who are used to using gray operating systems and such.!

!LightTheme commentStamp: '<historical>' prior: 0!
A very gray theme. Easy on those who are used to using gray operating systems and such.!

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 16/May/2011 01:12:00'!
acceptIcon

	^ self fetch: #( '16x16' 'smalltalk' 'accept' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 16/May/2011 01:13:00'!
appearanceIcon

	^ self fetch: #( '16x16' 'apps' 'preferences-desktop-theme' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 16/May/2011 01:13:00'!
cancelIcon

	^ self fetch: #( '16x16' 'smalltalk' 'cancel' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 16/May/2011 01:14:00'!
closeIcon

	^ self fetch: #( '10x10' 'buttons' 'close' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 16/May/2011 01:15:00'!
collapseIcon

	^ self fetch: #( '10x10' 'buttons' 'collapse' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 16/May/2011 01:16:00'!
copyIcon

	^ self fetch: #( '16x16' 'actions' 'edit-copy' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 16/May/2011 01:17:00'!
cutIcon

	^ self fetch: #( '16x16' 'actions' 'edit-cut' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 29/Apr/2011 17:49:00'!
deleteIcon

	^ self cutIcon! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 16/May/2011 01:20:00'!
doItIcon

	^ self fetch: #( '16x16' 'smalltalk' 'do-it' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 16/May/2011 01:28:00'!
expandIcon

	^ self fetch: #( '10x10' 'buttons' 'expand' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 16/May/2011 01:28:00'!
findIcon

	^ self fetch: #( '16x16' 'actions' 'edit-find' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 16/May/2011 01:36:00'!
helpIcon

	^ self fetch: #( '16x16' 'apps' 'help-browser' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 16/May/2011 01:39:00'!
morphsIcon

	^ self fetch: #( '16x16' 'smalltalk' 'morphs' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 16/May/2011 01:40:00'!
newIcon

	^ self fetch: #( '16x16' 'actions' 'document-new' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 16/May/2011 01:40:00'!
openIcon

	^ self fetch: #( '16x16' 'actions' 'document-open' )! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 16/May/2011 01:41:00'!
pasteIcon

	^ self fetch: #( '16x16' 'actions' 'edit-paste' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 16/May/2011 01:42:00'!
printIcon

	^ self fetch: #( '16x16' 'actions' 'document-print-preview' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 16/May/2011 01:44:00'!
quitIcon

	^ self fetch: #( '16x16' 'actions' 'system-log-out' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 16/May/2011 01:46:00'!
redoIcon

	^ self fetch: #( '16x16' 'actions' 'edit-redo' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 22/Apr/2011 16:50:00'!
saveAndQuitIcon

	^ self quitIcon
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 16/May/2011 01:47:00'!
saveAsIcon

	^ self fetch: #( '16x16' 'actions' 'document-save-as' )! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 22/Apr/2011 17:10:00'!
saveAsNewVersionIcon

	^ self saveAsIcon
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 16/May/2011 01:48:00'!
saveIcon

	^ self fetch: #( '16x16' 'actions' 'document-save' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 16/May/2011 01:49:00'!
undoIcon

	^ self fetch: #( '16x16' 'actions' 'edit-undo' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 16/May/2011 01:49:00'!
windowIcon

	^ self fetch: #( '16x16' 'apps' 'preferences-system-windows' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 16/May/2011 01:50:00'!
windowMenuIcon

	^ self fetch: #( '10x10' 'buttons' 'window-menu' )
! !

!ClassicTheme methodsFor: 'colors' stamp: 'jmv 27/Nov/2018 10:06:52'!
buttonLabel
	^ `Color black`! !

!ClassicTheme methodsFor: 'other options' stamp: 'cbr 6/Dec/2010 19:44:00'!
embossedTitles
	^false! !

!ClassicTheme methodsFor: 'other options' stamp: 'cbr 6/Dec/2010 19:44:00'!
roundButtons
	^false! !

!ClassicTheme methodsFor: 'other options' stamp: 'cbr 6/Dec/2010 19:53:00'!
roundWindowCorners
	^false! !

!ClassicTheme methodsFor: 'other options' stamp: 'jmv 27/Nov/2018 10:07:12'!
windowLabel
	^ `Color black`! !

!DarkBlueTheme methodsFor: 'colors' stamp: 'jmv 27/Nov/2018 10:05:37'!
buttonLabel
	^ `Color hue: 212 chroma: 0.3 luminance: 0.8`! !

!DarkBlueTheme methodsFor: 'colors' stamp: 'jmv 27/Nov/2018 10:05:43'!
paneBackgroundFrom: aColor
	^ `(Color hue: 212 chroma: 0.8 luminance: 0.4) muchDarker`! !

!DarkBlueTheme methodsFor: 'colors' stamp: 'jmv 2/Apr/2024 09:21:34'!
shout
	"Color symbols as an association list."

	^ {
		#selector  						-> nil.
		#arguments 						-> #white.
		#comment 						-> #(green lighter).
		#tempBar 						-> #(gray quiteWhiter).
		#tempVars 						-> #(gray quiteWhiter).
		#instVar 						-> #(magenta quiteWhiter).
		#pseudoVariables 						-> #(red veryMuchLighter).
		#literals 						-> #(green veryMuchLighter).
		#messages 						-> #(cyan).
		#incompleteMessages 						-> #(gray veryMuchDarker).
		#blockLevelZero 						-> #black.
		#blockLevelOne 						-> #brown.
		#blockLevelTwo 						-> #magenta.
		#blockLevelThree 						-> #red.
		#blockLevelFour 						-> #(green darker).
		#blockLevelFive 						-> #(red darker).
		#blockLevelSix 						-> #(magenta darker).
		#blockLevelSeven 						-> #blue.
		#defaults 						-> #white.
		#undefined 						-> #(orange lighter).
		#methodTags 						-> #(green lighter).
	}! !

!DarkBlueTheme methodsFor: 'colors' stamp: 'jmv 27/Nov/2018 10:05:48'!
text
	^ `Color hue: 212 chroma: 0.3 luminance: 0.8`! !

!DarkBlueTheme methodsFor: 'colors' stamp: 'jmv 27/Nov/2018 10:05:52'!
textHighlight
	^ `Color hue: 212 chroma: 0.4 luminance: 0.3`! !

!DarkBlueTheme methodsFor: 'colors' stamp: 'jmv 23/Nov/2010 11:08:00'!
unfocusedTextHighlightFrom: aColor
	^aColor adjustSaturation: 0.0 brightness: 0.08! !

!DarkBlueTheme methodsFor: 'colors' stamp: 'jmv 27/Nov/2018 10:05:59'!
windowLabel
	^ `Color hue: 212 chroma: 0.4 luminance: 0.6`! !

!DarkBlueTheme methodsFor: 'tool colors' stamp: 'jmv 27/Nov/2018 10:06:06'!
defaultWindowColor
	^ `Color hue: 212 chroma: 0.6 luminance: 0.24`! !

!DarkBlueTheme methodsFor: 'other options' stamp: 'jmv 22/Nov/2010 16:32:00'!
useUniformColors
	^true! !

!DarkBlueTheme methodsFor: 'menu colors' stamp: 'jmv 27/Nov/2018 10:06:16'!
menu
	^ `Color hue: 212 chroma: 0.6 luminance: 0.3`! !

!DarkBlueTheme methodsFor: 'menu colors' stamp: 'jmv 27/Nov/2018 10:06:19'!
menuText
	^ `Color hue: 212 chroma: 0.3 luminance: 0.8`! !

!DarkBlueTheme methodsFor: 'menu colors' stamp: 'jmv 27/Nov/2018 10:06:42'!
menuTitleBar
	^ `(Color hue: 212 chroma: 0.6 luminance: 0.3) twiceDarker`! !

!DarkColorTheme methodsFor: 'colors' stamp: 'jmv 2/Apr/2024 09:03:22'!
background
	^ `Color darkGray`! !

!DarkColorTheme methodsFor: 'colors' stamp: 'jmv 27/Nov/2018 10:08:13'!
buttonLabel
	^ `Color gray: 0.8`! !

!DarkColorTheme methodsFor: 'colors' stamp: 'jmv 23/Nov/2010 11:21:00'!
paneBackgroundFrom: aColor
	^ (aColor adjustSaturation: -0.2 brightness: 0.3) alpha: 0.9! !

!DarkColorTheme methodsFor: 'colors' stamp: 'jmv 27/Nov/2018 10:08:20'!
windowLabel
	^ `Color gray: 0.6`! !

!DarkColorTheme methodsFor: 'tool colors' stamp: 'jmv 27/Nov/2018 10:08:26'!
browser
	^ `Color r: 0.255 g: 0.3 b: 0.21`! !

!DarkColorTheme methodsFor: 'tool colors' stamp: 'jmv 27/Nov/2018 10:08:30'!
debugger
	^ `Color r: 0.3 g: 0.27 b: 0.27`! !

!DarkColorTheme methodsFor: 'tool colors' stamp: 'jmv 27/Nov/2018 10:08:33'!
fileContentsBrowser
	^ `Color r: 0.1 g: 0.1 b: 0.072`! !

!DarkColorTheme methodsFor: 'tool colors' stamp: 'jmv 27/Nov/2018 10:08:36'!
fileList
	^ `Color r: 0.4 g: 0.36 b: 0.4`! !

!DarkColorTheme methodsFor: 'tool colors' stamp: 'jmv 27/Nov/2018 10:08:40'!
messageNames
	^ `Color r: 0.213 g: 0.3 b: 0.165`! !

!DarkColorTheme methodsFor: 'tool colors' stamp: 'jmv 27/Nov/2018 10:08:42'!
messageSet
	^ `Color r: 0.27 g: 0.3 b: 0.3`! !

!DarkColorTheme methodsFor: 'tool colors' stamp: 'jmv 27/Nov/2018 10:08:46'!
object
	^ `Color r: 0.3 g: 0.298 b: 0.298`! !

!DarkColorTheme methodsFor: 'tool colors' stamp: 'jmv 27/Nov/2018 10:08:49'!
packageList
	^ `Color r: 0.2 g: 0.16 b: 0.04`! !

!DarkColorTheme methodsFor: 'tool colors' stamp: 'jmv 27/Nov/2018 10:08:53'!
testRunner
	^ `Color r: 0.211 g: 0.232 b: 0.276`! !

!DarkColorTheme methodsFor: 'tool colors' stamp: 'jmv 27/Nov/2018 10:08:56'!
transcript
	^ `Color r: 0.3 g: 0.25 b: 0.15`! !

!DarkColorTheme methodsFor: 'tool colors' stamp: 'jmv 27/Nov/2018 10:09:00'!
versionsBrowser
	^ `Color r: 0.277 g: 0.256 b: 0.3`! !

!DarkColorTheme methodsFor: 'tool colors' stamp: 'jmv 27/Nov/2018 10:09:03'!
workspace
	^ `Color r: 0.3 g: 0.3 b: 0.27`! !

!DarkColorTheme methodsFor: 'menu colors' stamp: 'jmv 2/Apr/2024 09:02:26'!
menu
	^ `Color gray`! !

!DarkColorTheme methodsFor: 'menu colors' stamp: 'jmv 27/Nov/2018 10:09:17'!
menuText
	^ `Color gray: 0.9`! !

!HighContrastBlackTheme methodsFor: 'colors' stamp: 'jmv 27/Nov/2018 10:09:32'!
background
	^ `Color black twiceLighter lighter`! !

!HighContrastBlackTheme methodsFor: 'colors' stamp: 'jmv 27/Nov/2018 10:09:35'!
buttonLabel
	^ `Color white`! !

!HighContrastBlackTheme methodsFor: 'colors' stamp: 'jmv 27/Nov/2018 10:09:38'!
focusIndicator
	^ `Color cyan muchDarker`! !

!HighContrastBlackTheme methodsFor: 'colors' stamp: 'jmv 23/Nov/2010 11:05:00'!
listMultiHighlightFocused: aBoolean
	^ (self listHighlightFocused: aBoolean) 
		adjustSaturation: 0.0 brightness: 0.15! !

!HighContrastBlackTheme methodsFor: 'colors' stamp: 'jmv 2/Dec/2010 08:28:00'!
paneBackgroundFrom: aColor
	^ aColor alphaMixed: 0.7 with: Color black! !

!HighContrastBlackTheme methodsFor: 'colors' stamp: 'jmv 2/Apr/2024 09:36:53'!
shout
	"Color symbols as an association list."

	^ {
		#selector  						-> nil.
		#arguments 						-> #(white darker).
		#comment 						-> #(magenta darker).
		#tempBar 						-> #veryLightGray.
		#tempVars 						-> #veryLightGray.
		#instVar 						-> #(green darker).
		#pseudoVariables 						-> #(cyan darker).
		#literals 						-> #(green darker).
		#messages 						-> #(yellow darker).
		#incompleteMessages  	-> #gray.
		#blockLevelZero 						-> #black.
		#blockLevelOne 						-> #brown.
		#blockLevelTwo 						-> #magenta.
		#blockLevelThree 						-> #red.
		#blockLevelFour 						-> #(magenta).
		#blockLevelFive 						-> #(orange negated).
		#blockLevelSix 						-> #(green).
		#blockLevelSeven 						-> #blue.
		#defaults 						-> #white.
		#undefined 						-> #cyan.
		#methodTags 						-> #(magenta darker).
	}! !

!HighContrastBlackTheme methodsFor: 'colors' stamp: 'jmv 27/Nov/2018 10:09:48'!
text
	^ `Color white`! !

!HighContrastBlackTheme methodsFor: 'colors' stamp: 'jmv 16/Oct/2013 22:08:00'!
textCursor
	^ super textHighlight! !

!HighContrastBlackTheme methodsFor: 'colors' stamp: 'jmv 27/Nov/2018 10:09:54'!
textHighlight
	^ `Color red muchDarker darker`! !

!HighContrastBlackTheme methodsFor: 'colors' stamp: 'cbr 18/Dec/2010 17:31:00'!
unfocusedTextHighlightFrom: aColor
	^aColor adjustSaturation: 0.0 brightness: -0.15! !

!HighContrastBlackTheme methodsFor: 'colors' stamp: 'jmv 27/Nov/2018 10:10:00'!
windowLabel
	^ `Color white`! !

!HighContrastBlackTheme methodsFor: 'tool colors' stamp: 'jmv 2/Apr/2024 09:34:03'!
defaultWindowColor
	^ `Color veryDarkGray`! !

!HighContrastBlackTheme methodsFor: 'menu colors' stamp: 'jmv 2/Apr/2024 09:04:46'!
menu
	^ `Color darkGray`! !

!HighContrastBlackTheme methodsFor: 'menu colors' stamp: 'jmv 27/Nov/2018 10:10:13'!
menuText
	^ `Color white`! !

!HighContrastBlackTheme methodsFor: 'menu colors' stamp: 'jmv 6/Dec/2010 09:11:00'!
menuTitleBar
	^ self menu darker! !

!HighContrastBlackTheme methodsFor: 'other options' stamp: 'jmv 22/Nov/2010 16:30:00'!
useUniformColors
	^true! !

!HighContrastWhiteTheme methodsFor: 'colors' stamp: 'jmv 2/Apr/2024 09:25:26'!
background
	^ `Color veryVeryLightGray`! !

!HighContrastWhiteTheme methodsFor: 'colors' stamp: 'jmv 27/Nov/2018 10:10:26'!
buttonLabel
	^ `Color black`! !

!HighContrastWhiteTheme methodsFor: 'colors' stamp: 'jmv 16/Oct/2013 22:08:00'!
textCursor
	^ super textHighlight muchDarker! !

!HighContrastWhiteTheme methodsFor: 'colors' stamp: 'jmv 2/Apr/2024 09:24:08'!
textHighlight
	^ `Color veryVeryLightGray`! !

!HighContrastWhiteTheme methodsFor: 'colors' stamp: 'jmv 27/Nov/2018 10:10:36'!
windowLabel
	^ `Color black`! !

!HighContrastWhiteTheme methodsFor: 'tool colors' stamp: 'jmv 27/Nov/2018 10:10:40'!
defaultWindowColor
	^ `Color white duller`! !

!HighContrastWhiteTheme methodsFor: 'menu colors' stamp: 'jmv 2/Apr/2024 09:24:48'!
menu
	^ `Color veryVeryLightGray`! !

!HighContrastWhiteTheme methodsFor: 'other options' stamp: 'jmv 22/Nov/2010 16:35:00'!
useUniformColors
	^true! !

!LightBluetheme methodsFor: 'colors' stamp: 'gsa 18/Jun/2014 09:46:00'!
buttonLabel
	^ Color hue: 0 chroma: 0 luminance: 0.13! !

!LightBluetheme methodsFor: 'colors' stamp: 'gsa 18/Jun/2014 12:17:00'!
paneBackgroundFrom: aColor
	^ Color hue: 0 chroma: 0 luminance: 1.
	"^ (Color hue: 212 chroma: 0.8 luminance: 0.4) muchDarker"! !

!LightBluetheme methodsFor: 'colors' stamp: 'jmv 27/Nov/2018 09:58:17'!
shout
	"Color symbols as an association list."

	^ {
		#selector  					-> nil.
		#arguments 					-> #(cyan muchDarker).
		#comment 					-> #(green muchDarker).
		#tempBar 					-> #gray.
		#tempVars 					-> #(gray muchDarker).
		#instVar 					-> #(magenta muchDarker).
		#pseudoVariables 			-> #(red muchDarker).
		#literals 					-> #(green muchDarker).
		#messages 					-> #(blue darker).
		#incompleteMessages  	-> #(gray veryMuchDarker).
		#blockLevelZero 			-> #black.
		#blockLevelOne 			-> #brown.
		#blockLevelTwo 			-> #magenta.
		#blockLevelThree 			-> #red.
		#blockLevelFour 			-> #(green darker).
		#blockLevelFive 			-> #(orange darker).
		#blockLevelSix 			-> #(magenta darker).
		#blockLevelSeven 			-> #blue.
		#defaults 					-> #black.
		#undefined 					-> #red.
		#methodTags 				-> #(green muchDarker).
	}! !

!LightBluetheme methodsFor: 'colors' stamp: 'gsa 18/Jun/2014 09:47:00'!
text
	^ Color hue: 0 chroma: 0 luminance: 0.13! !

!LightBluetheme methodsFor: 'colors' stamp: 'gsa 18/Jun/2014 13:11:00'!
textHighlight
	"^ Color hue: 0 chroma: 0.56 luminance: 0.45"
	^ Color r: 0 g: 0.584 b: 0.8.! !

!LightBluetheme methodsFor: 'colors' stamp: 'jmv 2/Apr/2024 09:26:11'!
textHighlightFocused: focused
	"A nice light blue."
	| textHighlight |
	Display depth = 1 ifTrue: [^ Color veryVeryLightGray].
	Display depth = 2 ifTrue: [^ Color gray: 0.87].
	textHighlight := self textHighlight.
	^focused
		ifTrue: [ textHighlight ]
		ifFalse: [ self unfocusedTextHighlightFrom: textHighlight ]! !

!LightBluetheme methodsFor: 'colors' stamp: 'gsa 18/Jun/2014 09:27:00'!
unfocusedTextHighlightFrom: aColor
	^aColor adjustSaturation: 0.0 brightness: 0.08! !

!LightBluetheme methodsFor: 'colors' stamp: 'gsa 18/Jun/2014 13:42:00'!
windowLabel
	"^ Color hue: 0 chroma: 0.56 luminance: 0.45	"
	^ Color r: 0.2 g: 0.2 b: 0.2! !

!LightBluetheme methodsFor: 'tool colors' stamp: 'gsa 1/May/2015 11:30:00'!
debugger
	^ Color lightYellow
	! !

!LightBluetheme methodsFor: 'tool colors' stamp: 'gsa 18/Jun/2014 13:44:00'!
defaultWindowColor
	"^ Color hue: 0 chroma: 0 luminance: 1."
	^ Color r: 0.686 g: 0.796 b: 0.910! !

!LightBluetheme methodsFor: 'other options' stamp: 'gsa 18/Jun/2014 14:21:00'!
useUniformColors
	^true! !

!LightBluetheme methodsFor: 'menu colors' stamp: 'gsa 18/Jun/2014 13:47:00'!
menu
	"^ Color hue: 212 chroma: 0.6 luminance: 0.3"
		^ Color r: 0.686 g: 0.796 b: 0.910! !

!LightBluetheme methodsFor: 'menu colors' stamp: 'gsa 18/Jun/2014 13:47:00'!
menuText
	"^ Color hue: 212 chroma: 0.3 luminance: 0.8"
		^ Color hue: 0 chroma: 0 luminance: 0.13! !

!LightBluetheme methodsFor: 'menu colors' stamp: 'gsa 18/Jun/2014 09:27:00'!
menuTitleBar
	^ self menu twiceDarker! !

!LightGrayTheme methodsFor: 'tool colors' stamp: 'jmv 5/Apr/2024 16:35:19'!
defaultWindowColor
	^ `Color gray: 0.85`! !

!LightGrayTheme methodsFor: 'other options' stamp: 'jmv 5/Apr/2024 16:35:19'!
roundedButtonRadius
	"Only effective if #roundButtons answers true.
	Provide a reasonable default for subclasses."
	^ 5! !

!LightGrayTheme methodsFor: 'other options' stamp: 'jmv 5/Apr/2024 16:35:19'!
roundedWindowRadius
	"Only effective if #roundWindowCorners answers true.
	Provide a reasonable default for subclasses."
	^ 5! !

!LightGrayTheme methodsFor: 'other options' stamp: 'jmv 5/Apr/2024 16:35:19'!
useUniformColors
	^true! !

!LightTheme methodsFor: 'other options' stamp: 'jmv 23/Nov/2010 09:22:00'!
useUniformColors
	^true! !

!LightTheme methodsFor: 'colors' stamp: 'jmv 27/Nov/2018 10:09:27'!
windowLabel
	^ `Color gray: 0.3`! !

!PersonalizedTheme methodsFor: 'other options' stamp: 'lucy 20/Jul/2023 15:16:22'!
embossedTitles
	^true! !

!PersonalizedTheme methodsFor: 'other options' stamp: 'lucy 20/Jul/2023 15:12:41'!
roundWindowCorners
	^true! !

!PersonalizedTheme methodsFor: 'other options' stamp: 'lucy 20/Jul/2023 15:12:55'!
useUniformColors
	^true! !

!PersonalizedTheme methodsFor: 'colors' stamp: 'lucy 20/Jul/2023 15:10:17'!
background
	^self class backgroundColor.! !

!PersonalizedTheme methodsFor: 'colors' stamp: 'lucy 20/Jul/2023 15:11:21'!
browser
	^ self class primaryColor.! !

!PersonalizedTheme methodsFor: 'colors' stamp: 'lucy 20/Jul/2023 15:10:06'!
buttonLabel
	^ self class labelsColor.! !

!PersonalizedTheme methodsFor: 'colors' stamp: 'lucy 20/Jul/2023 15:14:33'!
debugger
	^(self class primaryColor) alphaMixed: 0.6 with: Color white.! !

!PersonalizedTheme methodsFor: 'colors' stamp: 'lucy 20/Jul/2023 15:11:01'!
defaultWindowColor
	^self class secondaryColor.! !

!PersonalizedTheme methodsFor: 'colors' stamp: 'lucy 20/Jul/2023 15:10:26'!
menu
	^(self class backgroundColor darker) alphaMixed: 0.5 with: Color white! !

!PersonalizedTheme methodsFor: 'colors' stamp: 'lucy 20/Jul/2023 15:12:35'!
paneBackgroundFrom: aColor
	^ (aColor adjustSaturation: -0.5 brightness: 0.3) alphaMixed: 0.55 with: Color white.! !

!PersonalizedTheme methodsFor: 'colors' stamp: 'lucy 20/Jul/2023 15:12:14'!
text
	^ self class writingColor.! !

!PersonalizedTheme methodsFor: 'colors' stamp: 'lucy 20/Jul/2023 15:11:53'!
textHighlight
	^ (self class primaryColor adjustSaturation: 0.05 brightness: 0.2) alphaMixed: 0.6 with: Color white.! !

!PersonalizedTheme methodsFor: 'colors' stamp: 'lucy 20/Jul/2023 15:12:01'!
textPane
	^ (self class primaryColor adjustSaturation: -0.3 brightness: 0.4) alphaMixed: 0.6 with: Color white.! !

!PersonalizedTheme methodsFor: 'colors' stamp: 'lucy 20/Jul/2023 15:12:08'!
versionsBrowser
	^(self class primaryColor) alphaMixed: 0.6 with: Color white.! !

!PersonalizedTheme methodsFor: 'colors' stamp: 'lucy 20/Jul/2023 15:12:23'!
windowLabel
	^ self class writingColor.! !

!PersonalizedTheme methodsFor: 'colors' stamp: 'lucy 20/Jul/2023 15:11:09'!
workspace
	^ self class secondaryColor.! !

!PersonalizedTheme methodsFor: 'initialization' stamp: 'kdpm 18/May/2024 08:06:44'!
initialize

	PersonalizedTheme backgroundColor ifNil: [
		ThemeCustomizerWindow open.].! !

!PersonalizedTheme class methodsFor: 'changes' stamp: 'HAW 24/Jul/2023 11:15:08'!
changeBackgroundColorTo: color
	background := color.! !

!PersonalizedTheme class methodsFor: 'changes' stamp: 'HAW 24/Jul/2023 11:15:15'!
changeLabelsColorTo: color
	labels := color.! !

!PersonalizedTheme class methodsFor: 'changes' stamp: 'HAW 24/Jul/2023 11:15:20'!
changePrimaryColorTo: color
	primary := color.! !

!PersonalizedTheme class methodsFor: 'changes' stamp: 'HAW 24/Jul/2023 11:15:23'!
changeSecondaryColorTo: color
	secondary := color.! !

!PersonalizedTheme class methodsFor: 'changes' stamp: 'HAW 24/Jul/2023 11:15:27'!
changeTextColorTo: color
	writing := color.! !

!PersonalizedTheme class methodsFor: 'initialize' stamp: 'jmv 16/Jun/2024 16:41:45'!
initialize
	"^self initializeThemeWithDefaultColors "! !

!PersonalizedTheme class methodsFor: 'initialize' stamp: 'BB 4/Dec/2025 19:40:15'!
initializeThemeWithDefaultColors
	savedThemes ifNil: [ savedThemes := Dictionary new ].
	background := Theme current background.
	primary := Theme current browser.
	labels := Theme current buttonLabel.
	writing := Theme current text.
	secondary := Theme current workspace.! !

!PersonalizedTheme class methodsFor: 'accessing' stamp: 'HAW 24/Jul/2023 11:15:08'!
backgroundColor
	^background.! !

!PersonalizedTheme class methodsFor: 'accessing' stamp: 'HAW 24/Jul/2023 11:15:15'!
labelsColor
	^ labels.! !

!PersonalizedTheme class methodsFor: 'accessing' stamp: 'HAW 24/Jul/2023 11:15:20'!
primaryColor
	^ primary.! !

!PersonalizedTheme class methodsFor: 'accessing' stamp: 'BB 4/Dec/2025 17:57:56'!
savedThemes

	savedThemes ifNil: [ savedThemes := Dictionary new ].
	^ savedThemes! !

!PersonalizedTheme class methodsFor: 'accessing' stamp: 'HAW 24/Jul/2023 11:15:23'!
secondaryColor
	^secondary.! !

!PersonalizedTheme class methodsFor: 'accessing' stamp: 'HAW 24/Jul/2023 11:15:27'!
writingColor
	^writing.! !

!PersonalizedTheme class methodsFor: 'manage themes' stamp: 'BB 4/Dec/2025 18:01:40'!
deleteThemeNamed: aName

	self savedThemes removeKey: aName ifAbsent: [^self].
	
	Transcript show: 'Theme deleted successfully: ', aName; cr.! !

!PersonalizedTheme class methodsFor: 'manage themes' stamp: 'BB 4/Dec/2025 17:59:07'!
loadThemeNamed: aThemeName

	| themeData |
	
	themeData := self savedThemes at: aThemeName ifAbsent: [^self].
	
	self changeBackgroundColorTo: (themeData at: #background).
	self changeLabelsColorTo: (themeData at: #labels).
	self changePrimaryColorTo: (themeData at: #primary).
	self changeSecondaryColorTo: (themeData at: #secondary).
	self changeTextColorTo: (themeData at: #writing).
	
	self beCurrent.
	
	self runningWorld fullRepaintNeeded.! !

!PersonalizedTheme class methodsFor: 'manage themes' stamp: 'BB 4/Dec/2025 17:59:18'!
saveCurrentAs: aName

	| currentSettings |
	
	currentSettings := Dictionary new.
	currentSettings at: #background put: background.
	currentSettings at: #labels put: labels.
	currentSettings at: #primary put: primary.
	currentSettings at: #secondary put: secondary.
	currentSettings at: #writing put: writing.
	
	self savedThemes at: aName put: currentSettings.
	
	Transcript show: 'Theme saved: ', aName; cr.! !

!ThemeCustomizerModel methodsFor: 'change colors' stamp: 'lucy 20/Jul/2023 15:24:03'!
changeBackground: arguments
	
	| backgroundColor row |
	backgroundColor := arguments first.
	row := arguments second.
	PersonalizedTheme changeBackgroundColorTo: backgroundColor.
	PersonalizedTheme beCurrent.
	row color: backgroundColor.! !

!ThemeCustomizerModel methodsFor: 'change colors' stamp: 'lucy 20/Jul/2023 15:24:09'!
changeLabels: arguments

	| labelsColor row |
	labelsColor := arguments first.
	row := arguments second.
	PersonalizedTheme changeLabelsColorTo: labelsColor.
	PersonalizedTheme beCurrent.
	row color: labelsColor.! !

!ThemeCustomizerModel methodsFor: 'change colors' stamp: 'lucy 20/Jul/2023 15:24:16'!
changePrimary: arguments

	| primaryColor row |
	primaryColor := arguments first.
	row := arguments second.
	PersonalizedTheme changePrimaryColorTo: primaryColor.
	PersonalizedTheme beCurrent.
	row color: primaryColor.! !

!ThemeCustomizerModel methodsFor: 'change colors' stamp: 'lucy 20/Jul/2023 15:24:22'!
changeSecondary: arguments

	| secondaryColor row |
	secondaryColor := arguments first.
	row := arguments second.
	PersonalizedTheme changeSecondaryColorTo: secondaryColor.
	PersonalizedTheme beCurrent.
	row color: secondaryColor.! !

!ThemeCustomizerModel methodsFor: 'change colors' stamp: 'lucy 20/Jul/2023 15:24:27'!
changeText: arguments

	| textColor row |
	textColor := arguments first.
	row := arguments second.
	PersonalizedTheme changeTextColorTo: textColor.
	PersonalizedTheme beCurrent.
	row color: textColor.! !

!ThemeCustomizerWindow methodsFor: 'colors' stamp: 'lucy 20/Jul/2023 15:25:30'!
buttonsColor

	^ Color white twiceDarker darker darker. ! !

!ThemeCustomizerWindow methodsFor: 'colors' stamp: 'lucy 22/Jul/2023 02:21:47'!
colorFromHexString: aHexString

	^Color fromHexString: aHexString.! !

!ThemeCustomizerWindow methodsFor: 'colors' stamp: 'lucy 22/Jul/2023 02:22:08'!
initMessageBackgroundColor

	^self colorFromHexString: '#0d285f'.! !

!ThemeCustomizerWindow methodsFor: 'colors' stamp: 'lucy 22/Jul/2023 02:22:17'!
initMessageTextColor

	^self colorFromHexString: '#e069e0'.! !

!ThemeCustomizerWindow methodsFor: 'colors' stamp: 'lucy 22/Jul/2023 02:22:36'!
labelsColor
	
	^self colorFromHexString: '#FAF3DD'! !

!ThemeCustomizerWindow methodsFor: 'colors' stamp: 'lucy 22/Jul/2023 02:22:57'!
windowBackColor

	^(self colorFromHexString: '#9ec7e8') muchDarker.! !

!ThemeCustomizerWindow methodsFor: 'palettes' stamp: 'lucy 22/Jul/2023 02:16:50'!
buildBackgroundPalette
	
	| sunset ocean forest ash light |
	sunset := OrderedCollection with: (Color fromHexString: '#FE938C') with: 'Sunset'.
	ocean := OrderedCollection with: (Color fromHexString: '#124559') with: 'Ocean'.
	forest := OrderedCollection with: (Color fromHexString: '#788475') with: 'Forest'.
	ash := OrderedCollection with: (Color fromHexString: '#363946') with: 'Ash'.
	light := OrderedCollection with: (Color fromHexString: '#F2DFD7') with: 'Light'.
	
	backgroundPalette := OrderedCollection with: sunset with: ocean with: forest with: ash with: light.! !

!ThemeCustomizerWindow methodsFor: 'palettes' stamp: 'lucy 22/Jul/2023 02:17:17'!
buildLabelsPalette
	
	| sunset ocean forest ash light |
	sunset := OrderedCollection with: (Color fromHexString: '#fCD89A') with: 'Sunset'.
	ocean := OrderedCollection with: (Color fromHexString: '#EFF6E0') with: 'Ocean'.
	forest := OrderedCollection with: (Color fromHexString: '#8DAA91') with: 'Forest'.
	ash := OrderedCollection with: (Color fromHexString: '#000000') with: 'Ash'.
	light := OrderedCollection with: (Color fromHexString: '#946846') with: 'Light'.
	
	labelsPalette := OrderedCollection with: sunset with: ocean with: forest with: ash with: light.! !

!ThemeCustomizerWindow methodsFor: 'palettes' stamp: 'lucy 22/Jul/2023 02:17:44'!
buildPrimaryPalette
	
	| sunset ocean forest ash light |
	sunset := OrderedCollection with: (Color fromHexString: '#DA627D') with: 'Sunset'.
	ocean := OrderedCollection with: (Color fromHexString: '#598392') with: 'Ocean'.
	forest := OrderedCollection with: (Color fromHexString: '#5E5D5C') with: 'Forest'.
	ash := OrderedCollection with: (Color fromHexString: '#696773') with: 'Ash'.
	light := OrderedCollection with: (Color fromHexString: '#CEB5A7') with: 'Light'.
	
	primaryPalette := OrderedCollection with: sunset with: ocean with: forest with: ash with: light.! !

!ThemeCustomizerWindow methodsFor: 'palettes' stamp: 'lucy 22/Jul/2023 02:18:07'!
buildSecondaryPalette
	
	| sunset ocean forest ash light |
	sunset := OrderedCollection with: (Color fromHexString: '#FCA17D') with: 'Sunset'.
	ocean := OrderedCollection with: (Color fromHexString: '#AEC3B0') with: 'Ocean'.
	forest := OrderedCollection with: (Color fromHexString: '#453643') with: 'Forest'.
	ash := OrderedCollection with: (Color fromHexString: '#8D99AE') with: 'Ash '.
	light := OrderedCollection with: (Color fromHexString: '#BCAA99') with: 'Light'.
	
	secondaryPalette := OrderedCollection with: sunset with: ocean with: forest with: ash with: light.! !

!ThemeCustomizerWindow methodsFor: 'palettes' stamp: 'lucy 22/Jul/2023 02:18:36'!
buildTextPalette
	
	| sunset ocean forest ash light |
	sunset := OrderedCollection with: (Color fromHexString: '#9E3F3F') with: 'Sunset'.
	ocean := OrderedCollection with: (Color fromHexString: '#01161E') with: 'Ocean'.
	forest := OrderedCollection with: (Color fromHexString: '#28112B') with: 'Forest'.
	ash := OrderedCollection with: (Color fromHexString: '#2B2D42') with: 'Ash'.
	light := OrderedCollection with: (Color fromHexString: '#3C3C3B') with: 'Light'.
	
	textPalette := OrderedCollection with: sunset with: ocean with: forest with: ash with: light.! !

!ThemeCustomizerWindow methodsFor: 'initialization' stamp: 'KenD 8/Aug/2024 10:06:26'!
buildMorphicWindow

	| themesMorph mainWindowMorph scrollMorph |
	
	themesMorph := self buildThemesMorph.
	scrollMorph := self buildScrollMorphFor: themesMorph.
	mainWindowMorph := LayoutMorph newRow addMorph: scrollMorph proportionalHeight: 1.
	
	self layoutMorph addMorph: mainWindowMorph.
	self setLabel: 'Theme Customizer'
	
	! !

!ThemeCustomizerWindow methodsFor: 'initialization' stamp: 'lucy 22/Jul/2023 04:07:35'!
buildPalettes

	self buildBackgroundPalette.
	self buildPrimaryPalette.
	self buildSecondaryPalette.
	self buildTextPalette.
	self buildLabelsPalette! !

!ThemeCustomizerWindow methodsFor: 'initialization' stamp: 'HAW 24/Jul/2023 11:06:23'!
buildScrollMorphFor: themesMorph

	| scrollMorph |
	
	scrollMorph := PluggableScrollPane initializedInstance.
	scrollMorph scroller: themesMorph;
	color: self windowBackColor.
	
	^scrollMorph! !

!ThemeCustomizerWindow methodsFor: 'initialization' stamp: 'BB 4/Dec/2025 17:53:39'!
buildThemesMorph

	^ LayoutMorph newColumn ::
		morphExtent: self defaultExtent;
		addMorph: self buildTitleRow;
		addMorph: self buildBackgroundRow;
		addMorph: self buildPrimaryRow;
		addMorph: self buildSecondaryRow;
		addMorph: self buildTextRow;
		addMorph: self buildLabelsRow;
		addMorph: self buildControlButtonsRow.! !

!ThemeCustomizerWindow methodsFor: 'initialization' stamp: 'lucy 22/Jul/2023 04:24:15'!
defaultExtent
	^ 1400 @ 720.! !

!ThemeCustomizerWindow methodsFor: 'initialization' stamp: 'HAW 24/Jul/2023 11:03:52'!
initialize

	super initialize.
	PersonalizedTheme initializeThemeWithDefaultColors.
	self buildPalettes.
	! !

!ThemeCustomizerWindow methodsFor: 'build - private' stamp: 'lucy 20/Jul/2023 15:26:37'!
buildButtonForCustomColorThatExecutes: action forRow: row

	| column buttonMorph argument|
	argument := Array with: action with: row.
	buttonMorph := PluggableButtonMorph model: self stateGetter: nil stateArgument: nil action: #buildCustomColorInputAndDoRowAction: actionArgument: argument label: 'Custom'.
	
	buttonMorph mouseOverColor: Color white; 
	color: self buttonsColor.
	
	column := LayoutMorph newColumn.
	column separation: 5; axisEdgeWeight: 0.5; color: Color gray; borderWidth: 0; addMorph: buttonMorph.
		
	^ column.! !

!ThemeCustomizerWindow methodsFor: 'build - private' stamp: 'lucy 20/Jul/2023 15:26:43'!
buildButtonThatExecutes: action hasLabel: label color: buttonColor forRow: row

	| column buttonMorph argument|
	argument := Array with: buttonColor with: row.
	buttonMorph := PluggableButtonMorph model: self model stateGetter: nil stateArgument: nil action: action actionArgument: argument label: label.
	
	buttonMorph mouseOverColor: buttonColor muchLighter; 
	color: self buttonsColor.
	
	column := LayoutMorph newColumn.
	column separation: 5; axisEdgeWeight: 0.5; color: buttonColor; borderWidth: 0; addMorph: buttonMorph.
		
	^ column.! !

!ThemeCustomizerWindow methodsFor: 'build - private' stamp: 'BB 4/Dec/2025 22:35:48'!
buildButtonWithLabel: aLabelString action: actionSelector

	| buttonMorph |
	
	buttonMorph := PluggableButtonMorph
		model: self
		stateGetter: nil
		stateArgument: nil
		action: actionSelector
		actionArgument: nil
		label: aLabelString.
	
	buttonMorph
		color: self buttonsColor;
		mouseOverColor: self buttonsColor muchLighter.
		
	^ buttonMorph! !

!ThemeCustomizerWindow methodsFor: 'build - private' stamp: 'jmv 12/Sep/2024 12:07:30'!
buildCustomColorInputAndDoRowAction: arguments

	| customColorString action row initialText |
	
	action := arguments first.
	row := arguments second.
	initialText := 'hex code (example: #FFAABB)'.
	customColorString := StringRequestMorph request: 'Enter custom color:' initialAnswer: initialText orCancel: [^''].
	customColorString = initialText ifFalse: [self ensureColorIsHexCode: customColorString andDo: action for: row].
	^customColorString.! !

!ThemeCustomizerWindow methodsFor: 'build - private' stamp: 'lucy 20/Jul/2023 15:26:54'!
buildLabelForRowWithText: label

	| column |
	column := LayoutMorph newColumn.
	column
		separation: 5;
		axisEdgeWeight: 0.5;
		color: self windowBackColor;
		addMorph: (self buildLabelWithNoBordersBold: label
				withColor: self labelsColor).
	^ column.! !

!ThemeCustomizerWindow methodsFor: 'build - private' stamp: 'lucy 20/Jul/2023 15:26:59'!
buildLabelWithNoBordersBold: label withColor: labelColor

	| labelMorph |
	labelMorph := LabelMorph contents: label.
	labelMorph
		emphasis: 1;
		color: labelColor;
		borderWidth: 0.
		
	^ labelMorph.! !

!ThemeCustomizerWindow methodsFor: 'build - private' stamp: 'lucy 22/Jul/2023 03:52:14'!
buildRow

	| row |

	row := LayoutMorph newRow.
	row color: self windowBackColor;
	borderWidth: 5;
	borderColor: self windowBackColor.
	
	
	row separation: 10;
	axisEdgeWeight: 1.
	
	^row.! !

!ThemeCustomizerWindow methodsFor: 'build - private' stamp: 'lucy 22/Jul/2023 03:32:33'!
buildRow: rowLabel thatExecutesAction: action forPalette: palette

	| buttonColor buttonLabel rowName row | 
	
	row := self buildRow.
	
	rowName := self buildLabelForRowWithText: rowLabel.
	row addMorph: rowName.
	
	palette do: [:colorDuo | buttonColor := colorDuo first.
					      buttonLabel := colorDuo second.
					      row addMorph: (self buildButtonThatExecutes: action
									       hasLabel: buttonLabel color: buttonColor forRow: rowName).
					
		].
	
	row addMorph: (self buildButtonForCustomColorThatExecutes: action forRow: rowName).
	
	^row.! !

!ThemeCustomizerWindow methodsFor: 'build - private' stamp: 'HAW 24/Jul/2023 11:10:48'!
ensureColorIsHexCode: customColorHex andDo: action for: row
	
	| customColor |
	
	customColor := Color fromHexString: customColorHex.
	customColor isNil 
		ifTrue: [self inform: self errorNotAHexCode]
		ifFalse: [ self model perform: action with: (Array with: customColor with: row) ]


	! !

!ThemeCustomizerWindow methodsFor: 'error' stamp: 'HAW 24/Jul/2023 11:14:06'!
errorNotAHexCode

	^'The entered string is not a valid hex code'.! !

!ThemeCustomizerWindow methodsFor: 'build rows' stamp: 'lucy 20/Jul/2023 15:21:03'!
buildBackgroundRow
	
	^self buildRow: 'Background' thatExecutesAction: #changeBackground: forPalette: backgroundPalette.! !

!ThemeCustomizerWindow methodsFor: 'build rows' stamp: 'BB 4/Dec/2025 22:35:21'!
buildControlButtonsRow

	| buttonRow saveButton cancelButton manageThemesButton buttonsContainer |
		
	saveButton := self 
		buildButtonWithLabel: 'Save Theme'
		action: #saveThemeAction.

	cancelButton := self 
		buildButtonWithLabel: 'Cancel'
		action: #delete.

	manageThemesButton := self
		buildButtonWithLabel: 'Manage Themes'
		action: #savedThemesMenu.
	
	buttonsContainer := LayoutMorph newRow.
	buttonsContainer
		color: self windowBackColor;
		addMorph: saveButton;
		addMorph: cancelButton;
		addMorph: manageThemesButton.
	
	buttonRow := self buildRow.
	
	buttonRow
		addMorph: buttonsContainer.
		
	^ buttonRow! !

!ThemeCustomizerWindow methodsFor: 'build rows' stamp: 'lucy 20/Jul/2023 15:21:11'!
buildLabelsRow

	^self buildRow: 'Labels' thatExecutesAction: #changeLabels: forPalette: labelsPalette.! !

!ThemeCustomizerWindow methodsFor: 'build rows' stamp: 'lucy 20/Jul/2023 15:21:16'!
buildPrimaryRow

	^self buildRow: 'Primary' thatExecutesAction: #changePrimary: forPalette: primaryPalette.! !

!ThemeCustomizerWindow methodsFor: 'build rows' stamp: 'lucy 20/Jul/2023 15:21:22'!
buildSecondaryRow

	^self buildRow: 'Secondary' thatExecutesAction: #changeSecondary: forPalette: secondaryPalette.! !

!ThemeCustomizerWindow methodsFor: 'build rows' stamp: 'lucy 20/Jul/2023 15:21:27'!
buildTextRow
	
	^self buildRow: 'Text' thatExecutesAction: #changeText: forPalette: textPalette.! !

!ThemeCustomizerWindow methodsFor: 'build rows' stamp: 'LP 11/Nov/2025 23:30:53'!
buildTitleRow

	| titleRow titleLabel |
	
	titleLabel := self buildLabelWithNoBordersBold: 'Make your own theme!!' withColor: self initMessageTextColor.
	titleRow := LayoutMorph newRow.
	titleRow
		separation: 10;
		axisEdgeWeight: 1;
		color: self initMessageBackgroundColor;
		addMorph: titleLabel;
		layoutSpec: (LayoutSpec proportionalHeight: 0).

	^ titleRow.! !

!ThemeCustomizerWindow methodsFor: 'manage themes' stamp: 'BB 4/Dec/2025 18:02:52'!
saveThemeAction

	| themeName |
	
	themeName := StringRequestMorph request: 'Name your theme:' initialAnswer: 'My Theme'.
	
	(themeName isNil or: [ themeName isEmpty ]) ifTrue: [ ^self ].
	
	PersonalizedTheme saveCurrentAs: themeName.
	
	self inform: 'Theme saved successfully!!'.! !

!ThemeCustomizerWindow methodsFor: 'manage themes' stamp: 'BB 4/Dec/2025 17:57:37'!
savedThemesMenu
		
	| menu |
	
	menu := MenuMorph new.
	menu 
		addTitle: 'Saved themes'.
		
	PersonalizedTheme savedThemes isEmpty ifTrue: [
		menu add: 'There are no themes saved yet' target: self action: #yourself.
		^ menu popUpInWorld: self runningWorld.
	].
	
	PersonalizedTheme savedThemes keysAndValuesDo: [ :themeName :themeData |
		| subMenu |
		subMenu := MenuMorph new.
		
		subMenu add: 'Apply'
			target: PersonalizedTheme 
			action: #loadThemeNamed: 
			argument: themeName.
			
		subMenu add: 'Delete theme'
			target: PersonalizedTheme
			action: #deleteThemeNamed:
			argument: themeName.
			
		menu add: themeName subMenu: subMenu.
	].
	
	menu popUpInWorld: self runningWorld.


! !

!ThemeCustomizerWindow class methodsFor: 'opening' stamp: 'HAW 24/Jul/2023 11:01:05'!
open

	self new
		model: ThemeCustomizerModel new;
		buildMorphicWindow;
		openInWorld ! !

!ThemeCustomizerWindow class methodsFor: 'menu-world' stamp: 'HAW 24/Jul/2023 11:26:05'!
worldMenuOptions
	^ `{{
			#submenuOf -> TheWorldMenu preferencesLabel.
			#itemGroup 			->  10.
			#itemOrder 			-> 	85.	
			#label 			->	'Theme customizer...'.
			#object 			-> 	ThemeCustomizerWindow.
			#selector 		-> 		#open.
			#icon 			-> 	#appearanceIcon.
			#balloonText 	-> 		'Allows you to customize the personalized theme'.
		} asDictionary}`! !
PersonalizedTheme initialize!
