'From Cuis 5.0 [latest update: #4012] on 9 January 2020 at 8:04:03 pm'!
'Description '!
!provides: 'Theme-Themes' 1 7!
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

!classDefinition: #LightTheme category: #'Theme-Themes'!
Theme subclass: #LightTheme
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Theme-Themes'!
!classDefinition: 'LightTheme class' category: #'Theme-Themes'!
LightTheme class
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

!LightTheme commentStamp: '<historical>' prior: 0!
A very gray theme. Easy on those who are used to using gray operating systems and such.!

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 5/16/2011 01:12'!
acceptIcon

	^ self fetch: #( '16x16' 'smalltalk' 'accept' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 5/16/2011 01:13'!
appearanceIcon

	^ self fetch: #( '16x16' 'apps' 'preferences-desktop-theme' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 5/16/2011 01:13'!
cancelIcon

	^ self fetch: #( '16x16' 'smalltalk' 'cancel' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 5/16/2011 01:14'!
closeIcon

	^ self fetch: #( '10x10' 'buttons' 'close' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 5/16/2011 01:15'!
collapseIcon

	^ self fetch: #( '10x10' 'buttons' 'collapse' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 5/16/2011 01:16'!
copyIcon

	^ self fetch: #( '16x16' 'actions' 'edit-copy' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 5/16/2011 01:17'!
cutIcon

	^ self fetch: #( '16x16' 'actions' 'edit-cut' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 4/29/2011 17:49'!
deleteIcon

	^ self cutIcon! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 5/16/2011 01:20'!
doItIcon

	^ self fetch: #( '16x16' 'smalltalk' 'do-it' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 5/16/2011 01:28'!
expandIcon

	^ self fetch: #( '10x10' 'buttons' 'expand' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 5/16/2011 01:28'!
findIcon

	^ self fetch: #( '16x16' 'actions' 'edit-find' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 5/16/2011 01:36'!
helpIcon

	^ self fetch: #( '16x16' 'apps' 'help-browser' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 5/16/2011 01:39'!
morphsIcon

	^ self fetch: #( '16x16' 'smalltalk' 'morphs' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 5/16/2011 01:40'!
newIcon

	^ self fetch: #( '16x16' 'actions' 'document-new' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 5/16/2011 01:40'!
openIcon

	^ self fetch: #( '16x16' 'actions' 'document-open' )! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 5/16/2011 01:41'!
pasteIcon

	^ self fetch: #( '16x16' 'actions' 'edit-paste' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 5/16/2011 01:42'!
printIcon

	^ self fetch: #( '16x16' 'actions' 'document-print-preview' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 5/16/2011 01:44'!
quitIcon

	^ self fetch: #( '16x16' 'actions' 'system-log-out' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 5/16/2011 01:46'!
redoIcon

	^ self fetch: #( '16x16' 'actions' 'edit-redo' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 4/22/2011 16:50'!
saveAndQuitIcon

	^ self quitIcon
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 5/16/2011 01:47'!
saveAsIcon

	^ self fetch: #( '16x16' 'actions' 'document-save-as' )! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 4/22/2011 17:10'!
saveAsNewVersionIcon

	^ self saveAsIcon
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 5/16/2011 01:48'!
saveIcon

	^ self fetch: #( '16x16' 'actions' 'document-save' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 5/16/2011 01:49'!
undoIcon

	^ self fetch: #( '16x16' 'actions' 'edit-undo' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 5/16/2011 01:49'!
windowIcon

	^ self fetch: #( '16x16' 'apps' 'preferences-system-windows' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 5/16/2011 01:50'!
windowMenuIcon

	^ self fetch: #( '10x10' 'buttons' 'window-menu' )
! !

!ClassicTheme methodsFor: 'colors' stamp: 'jmv 11/27/2018 10:06:52'!
buttonLabel
	^ `Color black`! !

!ClassicTheme methodsFor: 'other options' stamp: 'cbr 12/6/2010 19:44'!
embossedTitles
	^false! !

!ClassicTheme methodsFor: 'other options' stamp: 'cbr 12/6/2010 19:44'!
roundButtons
	^false! !

!ClassicTheme methodsFor: 'other options' stamp: 'cbr 12/6/2010 19:53'!
roundWindowCorners
	^false! !

!ClassicTheme methodsFor: 'other options' stamp: 'cbr 12/6/2010 19:44'!
useButtonGradient
	^false! !

!ClassicTheme methodsFor: 'other options' stamp: 'cbr 12/6/2010 19:44'!
useWindowTitleGradient
	^false! !

!ClassicTheme methodsFor: 'other options' stamp: 'jmv 11/27/2018 10:07:12'!
windowLabel
	^ `Color black`! !

!DarkBlueTheme methodsFor: 'colors' stamp: 'jmv 11/27/2018 10:05:37'!
buttonLabel
	^ `Color hue: 212 chroma: 0.3 luminance: 0.8`! !

!DarkBlueTheme methodsFor: 'colors' stamp: 'jmv 11/27/2018 10:05:43'!
paneBackgroundFrom: aColor
	^ `(Color hue: 212 chroma: 0.8 luminance: 0.4) muchDarker`! !

!DarkBlueTheme methodsFor: 'colors' stamp: 'jmv 11/27/2018 09:58:03'!
shout
	"Color symbols as an association list."

	^ {
		#selector  						-> nil.
		#arguments 						-> #white.
		#comment 						-> #(green lighter).
		#tempBar 						-> #gray.
		#tempVars 						-> #(gray quiteWhiter).
		#instVar 						-> #(magenta muchDarker).
		#pseudoVariables 				-> #(red veryMuchLighter).
		#literals 						-> #(green veryMuchLighter).
		#messages 						-> #(cyan).
		#incompleteMessages 			-> #(gray veryMuchDarker).
		#blockLevelZero 				-> #black.
		#blockLevelOne 				-> #brown.
		#blockLevelTwo 				-> #magenta.
		#blockLevelThree 				-> #red.
		#blockLevelFour 				-> #(green darker).
		#blockLevelFive 				-> #(red darker).
		#blockLevelSix 				-> #(magenta darker).
		#blockLevelSeven 				-> #blue.
		#defaults 						-> #white.
		#undefined 						-> #(orange lighter).
		#methodTags 					-> #(green lighter).
	}! !

!DarkBlueTheme methodsFor: 'colors' stamp: 'jmv 11/27/2018 10:05:48'!
text
	^ `Color hue: 212 chroma: 0.3 luminance: 0.8`! !

!DarkBlueTheme methodsFor: 'colors' stamp: 'jmv 11/27/2018 10:05:52'!
textHighlight
	^ `Color hue: 212 chroma: 0.4 luminance: 0.3`! !

!DarkBlueTheme methodsFor: 'colors' stamp: 'jmv 11/23/2010 11:08'!
unfocusedTextHighlightFrom: aColor
	^aColor adjustSaturation: 0.0 brightness: 0.08! !

!DarkBlueTheme methodsFor: 'colors' stamp: 'jmv 11/27/2018 10:05:59'!
windowLabel
	^ `Color hue: 212 chroma: 0.4 luminance: 0.6`! !

!DarkBlueTheme methodsFor: 'tool colors' stamp: 'jmv 11/27/2018 10:06:06'!
defaultWindowColor
	^ `Color hue: 212 chroma: 0.6 luminance: 0.24`! !

!DarkBlueTheme methodsFor: 'other options' stamp: 'jmv 11/22/2010 16:05'!
embossedTitles
	^true! !

!DarkBlueTheme methodsFor: 'other options' stamp: 'jmv 11/22/2010 16:05'!
roundWindowCorners
	^true! !

!DarkBlueTheme methodsFor: 'other options' stamp: 'jmv 11/30/2010 10:59'!
useButtonGradient
	^true! !

!DarkBlueTheme methodsFor: 'other options' stamp: 'jmv 11/22/2010 16:32'!
useUniformColors
	^true! !

!DarkBlueTheme methodsFor: 'other options' stamp: 'jmv 11/22/2010 16:05'!
useWindowTitleGradient
	^true! !

!DarkBlueTheme methodsFor: 'menu colors' stamp: 'jmv 11/27/2018 10:06:16'!
menu
	^ `Color hue: 212 chroma: 0.6 luminance: 0.3`! !

!DarkBlueTheme methodsFor: 'menu colors' stamp: 'jmv 11/27/2018 10:06:19'!
menuText
	^ `Color hue: 212 chroma: 0.3 luminance: 0.8`! !

!DarkBlueTheme methodsFor: 'menu colors' stamp: 'jmv 11/27/2018 10:06:42'!
menuTitleBar
	^ `(Color hue: 212 chroma: 0.6 luminance: 0.3) twiceDarker`! !

!DarkColorTheme methodsFor: 'colors' stamp: 'jmv 11/27/2018 10:08:09'!
background
	^ `Color darkGray darker`! !

!DarkColorTheme methodsFor: 'colors' stamp: 'jmv 11/27/2018 10:08:13'!
buttonLabel
	^ `Color gray: 0.8`! !

!DarkColorTheme methodsFor: 'colors' stamp: 'jmv 11/23/2010 11:21'!
paneBackgroundFrom: aColor
	^ (aColor adjustSaturation: -0.2 brightness: 0.3) alpha: 0.9! !

!DarkColorTheme methodsFor: 'colors' stamp: 'jmv 11/27/2018 10:08:20'!
windowLabel
	^ `Color gray: 0.6`! !

!DarkColorTheme methodsFor: 'tool colors' stamp: 'jmv 11/27/2018 10:08:26'!
browser
	^ `Color r: 0.255 g: 0.3 b: 0.21`! !

!DarkColorTheme methodsFor: 'tool colors' stamp: 'jmv 11/27/2018 10:08:30'!
debugger
	^ `Color r: 0.3 g: 0.27 b: 0.27`! !

!DarkColorTheme methodsFor: 'tool colors' stamp: 'jmv 11/27/2018 10:08:33'!
fileContentsBrowser
	^ `Color r: 0.1 g: 0.1 b: 0.072`! !

!DarkColorTheme methodsFor: 'tool colors' stamp: 'jmv 11/27/2018 10:08:36'!
fileList
	^ `Color r: 0.4 g: 0.36 b: 0.4`! !

!DarkColorTheme methodsFor: 'tool colors' stamp: 'jmv 11/27/2018 10:08:40'!
messageNames
	^ `Color r: 0.213 g: 0.3 b: 0.165`! !

!DarkColorTheme methodsFor: 'tool colors' stamp: 'jmv 11/27/2018 10:08:42'!
messageSet
	^ `Color r: 0.27 g: 0.3 b: 0.3`! !

!DarkColorTheme methodsFor: 'tool colors' stamp: 'jmv 11/27/2018 10:08:46'!
object
	^ `Color r: 0.3 g: 0.298 b: 0.298`! !

!DarkColorTheme methodsFor: 'tool colors' stamp: 'jmv 11/27/2018 10:08:49'!
packageList
	^ `Color r: 0.2 g: 0.16 b: 0.04`! !

!DarkColorTheme methodsFor: 'tool colors' stamp: 'jmv 11/27/2018 10:08:53'!
testRunner
	^ `Color r: 0.211 g: 0.232 b: 0.276`! !

!DarkColorTheme methodsFor: 'tool colors' stamp: 'jmv 11/27/2018 10:08:56'!
transcript
	^ `Color r: 0.3 g: 0.25 b: 0.15`! !

!DarkColorTheme methodsFor: 'tool colors' stamp: 'jmv 11/27/2018 10:09:00'!
versionsBrowser
	^ `Color r: 0.277 g: 0.256 b: 0.3`! !

!DarkColorTheme methodsFor: 'tool colors' stamp: 'jmv 11/27/2018 10:09:03'!
workspace
	^ `Color r: 0.3 g: 0.3 b: 0.27`! !

!DarkColorTheme methodsFor: 'other options' stamp: 'jmv 11/18/2010 12:03'!
embossedTitles
	^true! !

!DarkColorTheme methodsFor: 'other options' stamp: 'jmv 11/18/2010 12:03'!
roundWindowCorners
	^true! !

!DarkColorTheme methodsFor: 'other options' stamp: 'jmv 11/30/2010 10:59'!
useButtonGradient
	^true! !

!DarkColorTheme methodsFor: 'other options' stamp: 'jmv 11/18/2010 12:03'!
useWindowTitleGradient
	^true! !

!DarkColorTheme methodsFor: 'menu colors' stamp: 'jmv 11/27/2018 10:09:13'!
menu
	^ `Color darkGray`! !

!DarkColorTheme methodsFor: 'menu colors' stamp: 'jmv 11/27/2018 10:09:17'!
menuText
	^ `Color gray: 0.9`! !

!HighContrastBlackTheme methodsFor: 'colors' stamp: 'jmv 11/27/2018 10:09:32'!
background
	^ `Color black twiceLighter lighter`! !

!HighContrastBlackTheme methodsFor: 'colors' stamp: 'jmv 11/27/2018 10:09:35'!
buttonLabel
	^ `Color white`! !

!HighContrastBlackTheme methodsFor: 'colors' stamp: 'jmv 11/27/2018 10:09:38'!
focusIndicator
	^ `Color cyan muchDarker`! !

!HighContrastBlackTheme methodsFor: 'colors' stamp: 'jmv 11/23/2010 11:05'!
listMultiHighlightFocused: aBoolean
	^ (self listHighlightFocused: aBoolean) 
		adjustSaturation: 0.0 brightness: 0.15! !

!HighContrastBlackTheme methodsFor: 'colors' stamp: 'jmv 12/2/2010 08:28'!
paneBackgroundFrom: aColor
	^ aColor alphaMixed: 0.7 with: Color black! !

!HighContrastBlackTheme methodsFor: 'colors' stamp: 'jmv 11/27/2018 09:58:12'!
shout
	"Color symbols as an association list."

	^ {
		#selector  					-> nil.
		#arguments 					-> #(white darker).
		#comment 					-> #(magenta darker).
		#tempBar 					-> #(gray veryMuchLighter lighter).
		#tempVars 					-> #(gray muchLighter).
		#instVar 					-> #(green darker).
		#pseudoVariables 			-> #(cyan darker).
		#literals 					-> #(green darker).
		#messages 					-> #(yellow darker).
		#incompleteMessages  	-> #gray.
		#blockLevelZero 			-> #black.
		#blockLevelOne 			-> #brown.
		#blockLevelTwo 			-> #magenta.
		#blockLevelThree 			-> #red.
		#blockLevelFour 			-> #(magenta).
		#blockLevelFive 			-> #(orange negated).
		#blockLevelSix 			-> #(green).
		#blockLevelSeven 			-> #blue.
		#defaults 					-> #white.
		#undefined 					-> #cyan.
		#methodTags 				-> #(magenta darker).
	}! !

!HighContrastBlackTheme methodsFor: 'colors' stamp: 'jmv 11/27/2018 10:09:48'!
text
	^ `Color white`! !

!HighContrastBlackTheme methodsFor: 'colors' stamp: 'jmv 10/16/2013 22:08'!
textCursor
	^ super textHighlight! !

!HighContrastBlackTheme methodsFor: 'colors' stamp: 'jmv 11/27/2018 10:09:54'!
textHighlight
	^ `Color red muchDarker darker`! !

!HighContrastBlackTheme methodsFor: 'colors' stamp: 'cbr 12/18/2010 17:31'!
unfocusedTextHighlightFrom: aColor
	^aColor adjustSaturation: 0.0 brightness: -0.15! !

!HighContrastBlackTheme methodsFor: 'colors' stamp: 'jmv 11/27/2018 10:10:00'!
windowLabel
	^ `Color white`! !

!HighContrastBlackTheme methodsFor: 'tool colors' stamp: 'jmv 11/27/2018 10:10:05'!
defaultWindowColor
	^ `Color gray duller darker darker`! !

!HighContrastBlackTheme methodsFor: 'menu colors' stamp: 'jmv 11/27/2018 10:10:10'!
menu
	^ `Color darkGray darker darker`! !

!HighContrastBlackTheme methodsFor: 'menu colors' stamp: 'jmv 11/27/2018 10:10:13'!
menuText
	^ `Color white`! !

!HighContrastBlackTheme methodsFor: 'menu colors' stamp: 'jmv 12/6/2010 09:11'!
menuTitleBar
	^ self menu darker! !

!HighContrastBlackTheme methodsFor: 'other options' stamp: 'jmv 11/22/2010 16:30'!
useUniformColors
	^true! !

!HighContrastWhiteTheme methodsFor: 'colors' stamp: 'jmv 11/27/2018 10:10:23'!
background
	^ `Color lightGray muchLighter`! !

!HighContrastWhiteTheme methodsFor: 'colors' stamp: 'jmv 11/27/2018 10:10:26'!
buttonLabel
	^ `Color black`! !

!HighContrastWhiteTheme methodsFor: 'colors' stamp: 'jmv 10/16/2013 22:08'!
textCursor
	^ super textHighlight muchDarker! !

!HighContrastWhiteTheme methodsFor: 'colors' stamp: 'jmv 11/27/2018 10:10:32'!
textHighlight
	^ `Color lightGray twiceLighter`! !

!HighContrastWhiteTheme methodsFor: 'colors' stamp: 'jmv 11/27/2018 10:10:36'!
windowLabel
	^ `Color black`! !

!HighContrastWhiteTheme methodsFor: 'tool colors' stamp: 'jmv 11/27/2018 10:10:40'!
defaultWindowColor
	^ `Color white duller`! !

!HighContrastWhiteTheme methodsFor: 'menu colors' stamp: 'jmv 11/27/2018 10:10:44'!
menu
	^ `Color lightGray muchLighter`! !

!HighContrastWhiteTheme methodsFor: 'other options' stamp: 'jmv 11/22/2010 16:35'!
useUniformColors
	^true! !

!LightBluetheme methodsFor: 'colors' stamp: 'gsa 6/18/2014 09:46'!
buttonLabel
	^ Color hue: 0 chroma: 0 luminance: 0.13! !

!LightBluetheme methodsFor: 'colors' stamp: 'gsa 6/18/2014 12:17'!
paneBackgroundFrom: aColor
	^ Color hue: 0 chroma: 0 luminance: 1.
	"^ (Color hue: 212 chroma: 0.8 luminance: 0.4) muchDarker"! !

!LightBluetheme methodsFor: 'colors' stamp: 'jmv 11/27/2018 09:58:17'!
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

!LightBluetheme methodsFor: 'colors' stamp: 'gsa 6/18/2014 09:47'!
text
	^ Color hue: 0 chroma: 0 luminance: 0.13! !

!LightBluetheme methodsFor: 'colors' stamp: 'gsa 6/18/2014 13:11'!
textHighlight
	"^ Color hue: 0 chroma: 0.56 luminance: 0.45"
	^ Color r: 0 g: 0.584 b: 0.8.! !

!LightBluetheme methodsFor: 'colors' stamp: 'gsa 6/18/2014 14:05'!
textHighlightFocused: focused
	"A nice light blue."
	| textHighlight |
	Display depth = 1 ifTrue: [^ Color veryLightGray].
	Display depth = 2 ifTrue: [^ Color gray: 0.87].
	textHighlight _ self textHighlight.
	^focused
		ifTrue: [ textHighlight ]
		ifFalse: [ self unfocusedTextHighlightFrom: textHighlight ]! !

!LightBluetheme methodsFor: 'colors' stamp: 'gsa 6/18/2014 09:27'!
unfocusedTextHighlightFrom: aColor
	^aColor adjustSaturation: 0.0 brightness: 0.08! !

!LightBluetheme methodsFor: 'colors' stamp: 'gsa 6/18/2014 13:42'!
windowLabel
	"^ Color hue: 0 chroma: 0.56 luminance: 0.45	"
	^ Color r: 0.2 g: 0.2 b: 0.2! !

!LightBluetheme methodsFor: 'tool colors' stamp: 'gsa 5/1/2015 11:30'!
debugger
	^ Color lightYellow
	! !

!LightBluetheme methodsFor: 'tool colors' stamp: 'gsa 6/18/2014 13:44'!
defaultWindowColor
	"^ Color hue: 0 chroma: 0 luminance: 1."
	^ Color r: 0.686 g: 0.796 b: 0.910! !

!LightBluetheme methodsFor: 'other options' stamp: 'gsa 6/18/2014 09:27'!
embossedTitles
	^true! !

!LightBluetheme methodsFor: 'other options' stamp: 'gsa 6/18/2014 09:27'!
roundWindowCorners
	^true! !

!LightBluetheme methodsFor: 'other options' stamp: 'gsa 6/18/2014 09:27'!
useButtonGradient
	^true! !

!LightBluetheme methodsFor: 'other options' stamp: 'gsa 6/18/2014 14:21'!
useUniformColors
	^true! !

!LightBluetheme methodsFor: 'other options' stamp: 'gsa 6/18/2014 09:27'!
useWindowTitleGradient
	^true! !

!LightBluetheme methodsFor: 'menu colors' stamp: 'gsa 6/18/2014 13:47'!
menu
	"^ Color hue: 212 chroma: 0.6 luminance: 0.3"
		^ Color r: 0.686 g: 0.796 b: 0.910! !

!LightBluetheme methodsFor: 'menu colors' stamp: 'gsa 6/18/2014 13:47'!
menuText
	"^ Color hue: 212 chroma: 0.3 luminance: 0.8"
		^ Color hue: 0 chroma: 0 luminance: 0.13! !

!LightBluetheme methodsFor: 'menu colors' stamp: 'gsa 6/18/2014 09:27'!
menuTitleBar
	^ self menu twiceDarker! !

!LightTheme methodsFor: 'other options' stamp: 'jmv 11/23/2010 09:32'!
embossedTitles
	^true! !

!LightTheme methodsFor: 'other options' stamp: 'jmv 11/23/2010 09:32'!
roundWindowCorners
	^true! !

!LightTheme methodsFor: 'other options' stamp: 'jmv 11/30/2010 10:59'!
useButtonGradient
	^true! !

!LightTheme methodsFor: 'other options' stamp: 'jmv 11/23/2010 09:22'!
useUniformColors
	^true! !

!LightTheme methodsFor: 'other options' stamp: 'jmv 11/23/2010 09:32'!
useWindowTitleGradient
	^true! !

!LightTheme methodsFor: 'colors' stamp: 'jmv 11/27/2018 10:09:27'!
windowLabel
	^ `Color gray: 0.3`! !
