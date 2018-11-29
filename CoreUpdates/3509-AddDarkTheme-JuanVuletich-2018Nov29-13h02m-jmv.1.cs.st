'From Cuis 5.0 of 7 November 2016 [latest update: #3508] on 29 November 2018 at 1:05:06 pm'!
!classDefinition: #BrightColorTheme category: #'Theme-Core'!
Theme subclass: #BrightColorTheme
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Theme-Core'!

!BrightColorTheme commentStamp: '<historical>' prior: 0!
Default bright colored theme for Cuis.!

!classDefinition: #DarkTheme category: #'Theme-Core'!
Theme subclass: #DarkTheme
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Theme-Core'!

!DarkTheme commentStamp: '<historical>' prior: 0!
A low contrast, darker gray theme.!


!DarkTheme methodsFor: 'colors' stamp: 'jmv 11/27/2018 10:07:19'!
background
	^ `Color black`! !

!DarkTheme methodsFor: 'colors' stamp: 'jmv 11/26/2018 18:24:15'!
buttonLabel
	^ `Color gray: 0.48`! !

!DarkTheme methodsFor: 'colors' stamp: 'jmv 11/27/2018 10:07:25'!
defaultWindowColor
	^ `Color hue: 212 chroma: 0.015 luminance: 0.25`! !

!DarkTheme methodsFor: 'colors' stamp: 'cbr 10/10/2012 20:18'!
missingCommentTextColor

	^ TextColor cyan! !

!DarkTheme methodsFor: 'colors' stamp: 'jmv 11/26/2018 16:59:09'!
paneBackgroundFrom: aColor
	^ aColor alphaMixed: 0.7 with: Color black! !

!DarkTheme methodsFor: 'colors' stamp: 'jmv 11/27/2018 09:59:07'!
shout
	"Color symbols as an association list."

	^ {
		#selector  					-> '#B59F60'.
		#arguments 					-> '#289078'.
		#comment 					-> #(green duller).
		#tempBar 					-> #gray.
		#tempVars 					-> '#767836'.
		#instVar 					-> '#B3695A'.
		#pseudoVariables 			-> '#2070E0'.
		#literals 					-> #green.
		#messages 					-> '#6FB3BD'.
		#incompleteMessages  	-> '#F08060'.
		#blockLevelZero 			-> '#6FB3BD'.
		#blockLevelOne 			-> '#FFB0B0'.
		#blockLevelTwo 			-> '#B0FFB0'.
		#blockLevelThree 			-> '#B0B0FF'.
		#blockLevelFour 			-> '#00B0B0'.
		#blockLevelFive 			-> '#E03030'.
		#blockLevelSix 			-> '#30E030'.
		#blockLevelSeven 			-> '#3030E0'.
		#defaults 					-> '#A1AFBF'.
		#undefined 					-> '#E04020'.
		#methodTags 				-> #green.
	}! !

!DarkTheme methodsFor: 'colors' stamp: 'jmv 11/27/2018 10:07:33'!
text
	^ `Color veryLightGray`! !

!DarkTheme methodsFor: 'colors' stamp: 'jmv 11/26/2018 18:08:53'!
textHighlight
	"A nice light blue."
	"
	^ Color r: 0.71 g: 0.835 b: 1.0
	^ Color hue: 214 chroma: 0.29 luminance: 0.816
	"
	^ `Color hue: 204 chroma: 0.29 luminance: 0.22`! !

!DarkTheme methodsFor: 'colors' stamp: 'jmv 11/27/2018 10:07:40'!
textPane
	^ `Color gray`! !

!DarkTheme methodsFor: 'colors' stamp: 'jmv 11/27/2018 10:07:46'!
unfocusedTextHighlightFrom: aColor
	^ aColor adjustSaturation: -0.15 brightness: -0.07! !

!DarkTheme methodsFor: 'colors' stamp: 'cbr 10/10/2012 18:32'!
useUniformColors
	^ true! !

!DarkTheme methodsFor: 'other options' stamp: 'jmv 11/27/2018 10:07:56'!
windowLabel
	^ `Color gray: 0.55`! !

!DarkTheme methodsFor: 'menu colors' stamp: 'jmv 11/26/2018 18:29:15'!
menu
	^ `Color darkGray`! !

!DarkTheme methodsFor: 'menu colors' stamp: 'jmv 11/26/2018 18:31:04'!
menuText
	^ self text! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
BrightColorTheme beCurrent!

