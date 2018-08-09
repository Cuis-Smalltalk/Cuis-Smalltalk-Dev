'From Cuis 4.2 of 25 July 2013 [latest update: #2335] on 25 May 2015 at 8:35:38.933368 pm'!
'Description Please enter a description for this package '!
!provides: 'Theme-Themes' 1 2!
!requires: 'Compression' 1 nil nil!
!classDefinition: #BlueTheme category: #'Theme-Themes'!
Theme subclass: #BlueTheme
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Theme-Themes'!
!classDefinition: 'BlueTheme class' category: #'Theme-Themes'!
BlueTheme class
	instanceVariableNames: ''!

!classDefinition: #ClassicTheme category: #'Theme-Themes'!
Theme subclass: #ClassicTheme
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Theme-Themes'!
!classDefinition: 'ClassicTheme class' category: #'Theme-Themes'!
ClassicTheme class
	instanceVariableNames: ''!

!classDefinition: #DarkGrayTheme category: #'Theme-Themes'!
Theme subclass: #DarkGrayTheme
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Theme-Themes'!
!classDefinition: 'DarkGrayTheme class' category: #'Theme-Themes'!
DarkGrayTheme class
	instanceVariableNames: ''!

!classDefinition: #DarkTheme category: #'Theme-Themes'!
Theme subclass: #DarkTheme
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Theme-Themes'!
!classDefinition: 'DarkTheme class' category: #'Theme-Themes'!
DarkTheme class
	instanceVariableNames: ''!

!classDefinition: #GrayTheme category: #'Theme-Themes'!
Theme subclass: #GrayTheme
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Theme-Themes'!
!classDefinition: 'GrayTheme class' category: #'Theme-Themes'!
GrayTheme class
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

!classDefinition: #SkypeTheme category: #'Theme-Themes'!
Theme subclass: #SkypeTheme
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Theme-Themes'!
!classDefinition: 'SkypeTheme class' category: #'Theme-Themes'!
SkypeTheme class
	instanceVariableNames: ''!


!BlueTheme commentStamp: '<historical>' prior: 0!
A nice deep blue theme.!

!ClassicTheme commentStamp: 'cbr 4/22/2011 12:07' prior: 0!
This is the theme from Cuis 2.9.


Some of the icons in this theme were originally created with:

	Clipboard default storeObject:
	 	((PNGReadWriter bytesFor: (Form fromFileNamed: 'appearanceIcon.png')) asString base64Encoded)!

!DarkGrayTheme commentStamp: 'cbr 10/15/2012 18:19' prior: 0!
A low contrast, darker gray theme with square corners and some alpha. The Shout configuration should be a good starting place for doing darker, lower contrast themes.!

!DarkTheme commentStamp: 'jmv 11/18/2010 13:15' prior: 0!
Most colors are the based on ColorTheme:
	baseColor adjustSaturation: -0.1 brightness: -0.7!

!GrayTheme commentStamp: '<historical>' prior: 0!
A very gray theme. Easy on those who are used to using gray operating systems and such.!

!HighContrastBlackTheme commentStamp: '<historical>' prior: 0!
A very dark, high contrast theme.!

!HighContrastWhiteTheme commentStamp: '<historical>' prior: 0!
Bright white theme: good for subclassing.!

!SkypeTheme commentStamp: '<historical>' prior: 0!
A nice deep blue theme.!

!BlueTheme methodsFor: 'colors' stamp: 'jmv 11/22/2010 16:06'!
buttonLabel
	^ Color hue: 212 chroma: 0.3 luminance: 0.8! !

!BlueTheme methodsFor: 'tool colors' stamp: 'jmv 11/23/2010 09:16'!
defaultWindowColor
	^ Color hue: 212 chroma: 0.6 luminance: 0.24! !

!BlueTheme methodsFor: 'other options' stamp: 'jmv 11/22/2010 16:05'!
embossedTitles
	^true! !

!BlueTheme methodsFor: 'menu colors' stamp: 'jmv 11/22/2010 17:40'!
menu
	^ Color hue: 212 chroma: 0.6 luminance: 0.3! !

!BlueTheme methodsFor: 'menu colors' stamp: 'jmv 11/22/2010 17:40'!
menuText
	^ Color hue: 212 chroma: 0.3 luminance: 0.8! !

!BlueTheme methodsFor: 'menu colors' stamp: 'jmv 11/22/2010 17:39'!
menuTitleBar
	^ self menu twiceDarker! !

!BlueTheme methodsFor: 'colors' stamp: 'cbr 4/21/2011 16:07'!
paneBackgroundFrom: aColor
	^ (Color hue: 212 chroma: 0.8 luminance: 0.4) muchDarker! !

!BlueTheme methodsFor: 'other options' stamp: 'jmv 11/22/2010 16:05'!
roundWindowCorners
	^true! !

!BlueTheme methodsFor: 'colors' stamp: 'DSG 5/23/2012 22:19'!
shout
	"Color symbols as an association list."
	
	^ {
		#defaults 							-> #white.
		#undefined 						-> #(orange lighter).
		#methodTags 					-> #(green lighter).
		#pseudoVariables 			-> #(red veryMuchLighter).
		#messages 						-> #(cyan).
		#arguments 				-> #white.
		#instVar 							-> #(magenta muchDarker).
		#incompleteMessages 	-> #(gray veryMuchDarker).
		#blockLevelFour 				-> #(green darker).
		#blockLevelFive 				-> #(red darker).
		#blockLevelSix 				-> #(magenta darker).
		#blockLevelSeven 			-> #blue.
		#tempBar 							-> #gray.
		#tempVars 						-> #(gray quiteWhiter).
	}! !

!BlueTheme methodsFor: 'colors' stamp: 'jmv 11/22/2010 16:04'!
text
	^ Color hue: 212 chroma: 0.3 luminance: 0.8! !

!BlueTheme methodsFor: 'colors' stamp: 'jmv 11/22/2010 17:36'!
textHighlight
	^ Color hue: 212 chroma: 0.4 luminance: 0.3! !

!BlueTheme methodsFor: 'colors' stamp: 'jmv 11/23/2010 11:08'!
unfocusedTextHighlightFrom: aColor
	^aColor adjustSaturation: 0.0 brightness: 0.08! !

!BlueTheme methodsFor: 'other options' stamp: 'jmv 11/30/2010 10:59'!
useButtonGradient
	^true! !

!BlueTheme methodsFor: 'other options' stamp: 'jmv 11/22/2010 16:32'!
useUniformColors
	^true! !

!BlueTheme methodsFor: 'other options' stamp: 'jmv 11/22/2010 16:05'!
useWindowTitleGradient
	^true! !

!BlueTheme methodsFor: 'colors' stamp: 'jmv 11/22/2010 16:06'!
windowLabel
	^ Color hue: 212 chroma: 0.4 luminance: 0.6! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 5/16/2011 01:12'!
acceptIcon

	^ self fetch: #( '16x16' 'smalltalk' 'accept' )
! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 5/16/2011 01:13'!
appearanceIcon

	^ self fetch: #( '16x16' 'apps' 'preferences-desktop-theme' )
! !

!ClassicTheme methodsFor: 'colors' stamp: 'cbr 12/6/2010 19:44'!
buttonLabel
	^ Color black! !

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

!ClassicTheme methodsFor: 'other options' stamp: 'cbr 12/6/2010 19:44'!
embossedTitles
	^false! !

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

!ClassicTheme methodsFor: 'other options' stamp: 'cbr 12/6/2010 19:44'!
roundButtons
	^false! !

!ClassicTheme methodsFor: 'other options' stamp: 'cbr 12/6/2010 19:53'!
roundWindowCorners
	^false! !

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

!ClassicTheme methodsFor: 'other options' stamp: 'cbr 12/6/2010 19:44'!
useButtonGradient
	^false! !

!ClassicTheme methodsFor: 'other options' stamp: 'cbr 12/6/2010 19:44'!
useWindowTitleGradient
	^false! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 5/16/2011 01:49'!
windowIcon

	^ self fetch: #( '16x16' 'apps' 'preferences-system-windows' )
! !

!ClassicTheme methodsFor: 'other options' stamp: 'cbr 12/6/2010 19:56'!
windowLabel
	^Color black! !

!ClassicTheme methodsFor: 'icons' stamp: 'cbr 5/16/2011 01:50'!
windowMenuIcon

	^ self fetch: #( '10x10' 'buttons' 'window-menu' )
! !

!DarkGrayTheme methodsFor: 'colors' stamp: 'cbr 10/10/2012 20:32'!
background
	^Color black! !

!DarkGrayTheme methodsFor: 'other options' stamp: 'cbr 10/10/2012 19:04'!
embossedTitles
	^false! !

!DarkGrayTheme methodsFor: 'colors' stamp: 'cbr 10/10/2012 20:18'!
missingCommentTextColor

	^ TextColor cyan! !

!DarkGrayTheme methodsFor: 'colors' stamp: 'cbr 10/15/2012 18:10'!
paneBackgroundFrom: aColor
	^ (aColor alphaMixed: 0.3 with: Color black) alpha: 0.9! !

!DarkGrayTheme methodsFor: 'other options' stamp: 'cbr 10/15/2012 18:14'!
roundButtons
	^true! !

!DarkGrayTheme methodsFor: 'as yet unclassified' stamp: 'cbr 10/10/2012 18:35'!
roundWindowCorners
	^false! !

!DarkGrayTheme methodsFor: 'colors' stamp: 'jmv 11/14/2012 23:32'!
shout
	"Color symbols as an association list."
	
	^ {
		#defaults 				-> #white.
		#undefined 				-> #cyan.
		#methodTags 			-> #magenta.
		#pseudoVariables 		-> #(cyan darker).
		#messages 				-> #(yellow darker).
		#arguments 				-> #(cyan muchDarker).
		#instVar 					-> #green.
		#incompleteMessages -> #(gray muchLighter).
		#blockLevelFour 		-> #magenta.
		#blockLevelFive 		-> #(blue darker).
		#blockLevelSix 			-> #green.
		#blockLevelSeven 		-> #yellow.
		#tempBar 				-> #gray.
		#tempVars 				-> #(gray muchLighter).
	}! !

!DarkGrayTheme methodsFor: 'colors' stamp: 'cbr 10/10/2012 22:09'!
text
	^ Color white! !

!DarkGrayTheme methodsFor: 'colors' stamp: 'cbr 10/15/2012 18:10'!
textPane
	^ Color gray! !

!DarkGrayTheme methodsFor: 'as yet unclassified' stamp: 'cbr 10/10/2012 18:32'!
useUniformColors
	^ true! !

!DarkGrayTheme methodsFor: 'colors' stamp: 'cbr 10/10/2012 19:08'!
windowLabel
	^ Color black! !

!DarkTheme methodsFor: 'colors' stamp: 'jmv 11/19/2010 11:08'!
background
	^ Color darkGray darker! !

!DarkTheme methodsFor: 'tool colors' stamp: 'jmv 11/18/2010 13:14'!
browser
	^ Color r: 0.255 g: 0.3 b: 0.21! !

!DarkTheme methodsFor: 'colors' stamp: 'jmv 11/18/2010 13:26'!
buttonLabel
	^Color gray: 0.8! !

!DarkTheme methodsFor: 'tool colors' stamp: 'jmv 11/18/2010 13:09'!
debugger
	^ Color r: 0.3 g: 0.27 b: 0.27! !

!DarkTheme methodsFor: 'other options' stamp: 'jmv 11/18/2010 12:03'!
embossedTitles
	^true! !

!DarkTheme methodsFor: 'tool colors' stamp: 'jmv 11/18/2010 13:10'!
fileContentsBrowser
	^ Color r: 0.1 g: 0.1 b: 0.072! !

!DarkTheme methodsFor: 'tool colors' stamp: 'jmv 11/18/2010 13:11'!
fileList
	^ Color r: 0.4 g: 0.36 b: 0.4! !

!DarkTheme methodsFor: 'menu colors' stamp: 'jmv 11/19/2010 11:09'!
menu
	^ Color darkGray! !

!DarkTheme methodsFor: 'menu colors' stamp: 'jmv 11/19/2010 11:10'!
menuText
	^Color gray: 0.9! !

!DarkTheme methodsFor: 'tool colors' stamp: 'jmv 11/18/2010 13:11'!
messageNames
	^ Color r: 0.213 g: 0.3 b: 0.165! !

!DarkTheme methodsFor: 'tool colors' stamp: 'jmv 11/18/2010 13:12'!
messageSet
	^ Color r: 0.27 g: 0.3 b: 0.3! !

!DarkTheme methodsFor: 'tool colors' stamp: 'jmv 11/18/2010 13:12'!
object
	^Color r: 0.3 g: 0.298 b: 0.298! !

!DarkTheme methodsFor: 'tool colors' stamp: 'jmv 3/28/2012 09:40'!
packageList
	^ Color r: 0.2 g: 0.16 b: 0.04! !

!DarkTheme methodsFor: 'colors' stamp: 'jmv 11/23/2010 11:21'!
paneBackgroundFrom: aColor
	^ (aColor adjustSaturation: -0.2 brightness: 0.3) alpha: 0.9! !

!DarkTheme methodsFor: 'other options' stamp: 'jmv 11/18/2010 12:03'!
roundWindowCorners
	^true! !

!DarkTheme methodsFor: 'tool colors' stamp: 'jmv 11/18/2010 13:13'!
testRunner
	^ Color r: 0.211 g: 0.232 b: 0.276! !

!DarkTheme methodsFor: 'tool colors' stamp: 'jmv 11/18/2010 13:13'!
transcript
	^ Color r: 0.3 g: 0.25 b: 0.15! !

!DarkTheme methodsFor: 'other options' stamp: 'jmv 11/30/2010 10:59'!
useButtonGradient
	^true! !

!DarkTheme methodsFor: 'other options' stamp: 'jmv 11/18/2010 12:03'!
useWindowTitleGradient
	^true! !

!DarkTheme methodsFor: 'tool colors' stamp: 'jmv 11/18/2010 13:13'!
versionsBrowser
	^ Color r: 0.277 g: 0.256 b: 0.3! !

!DarkTheme methodsFor: 'colors' stamp: 'jmv 11/18/2010 13:05'!
windowLabel
	^Color gray: 0.6! !

!DarkTheme methodsFor: 'tool colors' stamp: 'jmv 11/18/2010 13:14'!
workspace
	^ Color r: 0.3 g: 0.3 b: 0.27! !

!GrayTheme methodsFor: 'other options' stamp: 'jmv 11/23/2010 09:32'!
embossedTitles
	^true! !

!GrayTheme methodsFor: 'other options' stamp: 'jmv 11/23/2010 09:32'!
roundWindowCorners
	^true! !

!GrayTheme methodsFor: 'other options' stamp: 'jmv 11/30/2010 10:59'!
useButtonGradient
	^true! !

!GrayTheme methodsFor: 'other options' stamp: 'jmv 11/23/2010 09:22'!
useUniformColors
	^true! !

!GrayTheme methodsFor: 'other options' stamp: 'jmv 11/23/2010 09:32'!
useWindowTitleGradient
	^true! !

!GrayTheme methodsFor: 'colors' stamp: 'jmv 11/23/2010 09:34'!
windowLabel
	^Color gray: 0.3! !

!HighContrastBlackTheme methodsFor: 'colors' stamp: 'cbr 12/18/2010 17:49'!
background
	^ Color black twiceLighter lighter! !

!HighContrastBlackTheme methodsFor: 'colors' stamp: 'jmv 11/18/2010 13:02'!
buttonLabel
	^ Color white! !

!HighContrastBlackTheme methodsFor: 'tool colors' stamp: 'cbr 12/18/2010 17:04'!
defaultWindowColor
	^ Color gray duller darker darker! !

!HighContrastBlackTheme methodsFor: 'colors' stamp: 'cbr 12/18/2010 18:03'!
focusIndicator
	^ Color cyan muchDarker! !

!HighContrastBlackTheme methodsFor: 'colors' stamp: 'jmv 11/23/2010 11:05'!
listMultiHighlightFocused: aBoolean
	^ (self listHighlightFocused: aBoolean) 
		adjustSaturation: 0.0 brightness: 0.15! !

!HighContrastBlackTheme methodsFor: 'menu colors' stamp: 'cbr 12/18/2010 18:04'!
menu
	^ Color darkGray darker darker! !

!HighContrastBlackTheme methodsFor: 'menu colors' stamp: 'cbr 11/7/2010 18:30'!
menuText
	^ Color white! !

!HighContrastBlackTheme methodsFor: 'menu colors' stamp: 'jmv 12/6/2010 09:11'!
menuTitleBar
	^ self menu darker! !

!HighContrastBlackTheme methodsFor: 'colors' stamp: 'jmv 12/2/2010 08:28'!
paneBackgroundFrom: aColor
	^ aColor alphaMixed: 0.7 with: Color black! !

!HighContrastBlackTheme methodsFor: 'colors' stamp: 'DSG 5/23/2012 22:16'!
shout
	"Color symbols as an association list."
	
	^ {
		#defaults 				-> #white.
		#undefined 				-> #cyan.
		#methodTags 			-> #(magenta darker).
		#pseudoVariables 		-> #(cyan darker).
		#messages 				-> #(yellow darker).
		#arguments 				-> #(white darker).
		#instVar 					-> #(green darker).
		#incompleteMessages -> #gray.
		#blockLevelFour 		-> #(magenta).
		#blockLevelFive 		-> #(orange negated).
		#blockLevelSix 			-> #(green).
		#blockLevelSeven 		-> #blue.
		#tempBar 				-> #(gray veryMuchLighter lighter).
		#tempVars 				-> #(gray muchLighter).
	}! !

!HighContrastBlackTheme methodsFor: 'colors' stamp: 'cbr 11/7/2010 23:08'!
text
	^ Color white! !

!HighContrastBlackTheme methodsFor: 'colors' stamp: 'jmv 10/16/2013 22:08'!
textCursor
	^ super textHighlight! !

!HighContrastBlackTheme methodsFor: 'colors' stamp: 'cbr 12/18/2010 17:12'!
textHighlight
	^ Color red muchDarker darker! !

!HighContrastBlackTheme methodsFor: 'colors' stamp: 'cbr 12/18/2010 17:31'!
unfocusedTextHighlightFrom: aColor
	^aColor adjustSaturation: 0.0 brightness: -0.15! !

!HighContrastBlackTheme methodsFor: 'other options' stamp: 'jmv 11/22/2010 16:30'!
useUniformColors
	^true! !

!HighContrastBlackTheme methodsFor: 'colors' stamp: 'jmv 11/18/2010 13:02'!
windowLabel
	^ Color white! !

!HighContrastWhiteTheme methodsFor: 'colors' stamp: 'cbr 11/7/2010 13:57'!
background
	^ Color lightGray muchLighter! !

!HighContrastWhiteTheme methodsFor: 'colors' stamp: 'jmv 11/18/2010 13:02'!
buttonLabel
	^ Color black! !

!HighContrastWhiteTheme methodsFor: 'tool colors' stamp: 'jmv 11/23/2010 09:16'!
defaultWindowColor
	^ Color white duller! !

!HighContrastWhiteTheme methodsFor: 'menu colors' stamp: 'cbr 11/7/2010 12:00'!
menu
	^ Color lightGray muchLighter! !

!HighContrastWhiteTheme methodsFor: 'colors' stamp: 'jmv 10/16/2013 22:08'!
textCursor
	^ super textHighlight muchDarker! !

!HighContrastWhiteTheme methodsFor: 'colors' stamp: 'cbr 11/7/2010 12:00'!
textHighlight
	^ Color lightGray twiceLighter! !

!HighContrastWhiteTheme methodsFor: 'other options' stamp: 'jmv 11/22/2010 16:35'!
useUniformColors
	^true! !

!HighContrastWhiteTheme methodsFor: 'colors' stamp: 'jmv 11/18/2010 13:02'!
windowLabel
	^ Color black! !

!SkypeTheme methodsFor: 'colors' stamp: 'gsa 6/18/2014 09:46'!
buttonLabel
	^ Color hue: 0 chroma: 0 luminance: 0.13! !

!SkypeTheme methodsFor: 'tool colors' stamp: 'gsa 5/1/2015 11:30'!
debugger
	^ Color lightYellow
	! !

!SkypeTheme methodsFor: 'tool colors' stamp: 'gsa 6/18/2014 13:44'!
defaultWindowColor
	"^ Color hue: 0 chroma: 0 luminance: 1."
	^ Color r: 0.686 g: 0.796 b: 0.910! !

!SkypeTheme methodsFor: 'other options' stamp: 'gsa 6/18/2014 09:27'!
embossedTitles
	^true! !

!SkypeTheme methodsFor: 'widget colors' stamp: 'gsa 6/18/2014 14:09'!
listSelectedRowText
	^ Color white! !

!SkypeTheme methodsFor: 'menu colors' stamp: 'gsa 6/18/2014 13:47'!
menu
	"^ Color hue: 212 chroma: 0.6 luminance: 0.3"
		^ Color r: 0.686 g: 0.796 b: 0.910! !

!SkypeTheme methodsFor: 'menu colors' stamp: 'gsa 6/18/2014 13:47'!
menuText
	"^ Color hue: 212 chroma: 0.3 luminance: 0.8"
		^ Color hue: 0 chroma: 0 luminance: 0.13! !

!SkypeTheme methodsFor: 'menu colors' stamp: 'gsa 6/18/2014 09:27'!
menuTitleBar
	^ self menu twiceDarker! !

!SkypeTheme methodsFor: 'colors' stamp: 'gsa 6/18/2014 12:17'!
paneBackgroundFrom: aColor
	^ Color hue: 0 chroma: 0 luminance: 1.
	"^ (Color hue: 212 chroma: 0.8 luminance: 0.4) muchDarker"! !

!SkypeTheme methodsFor: 'other options' stamp: 'gsa 6/18/2014 09:27'!
roundWindowCorners
	^true! !

!SkypeTheme methodsFor: 'colors' stamp: 'gsa 6/18/2014 12:28'!
shout
	"Color symbols as an association list."
	
	
	^ {
		#defaults 				-> #black.
		#undefined 				-> #red.
		#methodTags 			-> #(green muchDarker).
		#pseudoVariables 		-> #(red muchDarker).
		#messages 				-> #(blue darker).
		#arguments 				-> #(cyan muchDarker).
		#instVar 					-> #(magenta muchDarker).
		#incompleteMessages -> #(gray veryMuchDarker).
		#blockLevelFour 		-> #(green darker).
		#blockLevelFive 		-> #(orange darker).
		#blockLevelSix 			-> #(magenta darker).
		#blockLevelSeven 		-> #blue.
		#tempBar 				-> #gray.
		#tempVars 				-> #(gray muchDarker).
	}! !

!SkypeTheme methodsFor: 'colors' stamp: 'gsa 6/18/2014 09:47'!
text
	^ Color hue: 0 chroma: 0 luminance: 0.13! !

!SkypeTheme methodsFor: 'colors' stamp: 'gsa 6/18/2014 13:11'!
textHighlight
	"^ Color hue: 0 chroma: 0.56 luminance: 0.45"
	^ Color r: 0 g: 0.584 b: 0.8.! !

!SkypeTheme methodsFor: 'colors' stamp: 'gsa 6/18/2014 14:05'!
textHighlightFocused: focused
	"A nice light blue."
	| textHighlight |
	Display depth = 1 ifTrue: [^ Color veryLightGray].
	Display depth = 2 ifTrue: [^ Color gray: 0.87].
	textHighlight _ self textHighlight.
	^focused
		ifTrue: [ textHighlight ]
		ifFalse: [ self unfocusedTextHighlightFrom: textHighlight ]! !

!SkypeTheme methodsFor: 'colors' stamp: 'gsa 6/18/2014 09:27'!
unfocusedTextHighlightFrom: aColor
	^aColor adjustSaturation: 0.0 brightness: 0.08! !

!SkypeTheme methodsFor: 'other options' stamp: 'gsa 6/18/2014 09:27'!
useButtonGradient
	^true! !

!SkypeTheme methodsFor: 'other options' stamp: 'gsa 6/18/2014 14:21'!
useUniformColors
	^true! !

!SkypeTheme methodsFor: 'other options' stamp: 'gsa 6/18/2014 09:27'!
useWindowTitleGradient
	^true! !

!SkypeTheme methodsFor: 'colors' stamp: 'gsa 6/18/2014 13:42'!
windowLabel
	"^ Color hue: 0 chroma: 0.56 luminance: 0.45	"
	^ Color r: 0.2 g: 0.2 b: 0.2! !
