"
A low contrast, darker gray theme.
"
Class {
	#name : 'DarkTheme',
	#superclass : 'Theme',
	#category : 'Graphics-Themes',
	#package : 'Graphics',
	#tag : 'Themes'
}

{ #category : 'colors' }
DarkTheme >> background [
	^ `Color veryVeryDarkGray`
]

{ #category : 'colors' }
DarkTheme >> buttonLabel [
	^ `Color gray: 0.48`
]

{ #category : 'colors' }
DarkTheme >> defaultWindowColor [
	^ `Color hue: 212 chroma: 0.015 luminance: 0.15`
]

{ #category : 'menu colors' }
DarkTheme >> menu [
	^ `Color darkGray`
]

{ #category : 'menu colors' }
DarkTheme >> menuText [
	^ self text
]

{ #category : 'colors' }
DarkTheme >> paneBackgroundFrom: aColor [
	^ aColor alphaMixed: 0.4 with: Color black
]

{ #category : 'colors' }
DarkTheme >> shout [
	"Color symbols as an association list."

	^ {
		#selector  						-> '#B59F60'.
		#arguments 					-> '#289078'.
		#comment 						-> #(green duller).
		#tempBar 						-> '#707070'.
		#tempVars 					-> '#767836'.
		#instVar 						-> '#B3695A'.
		#pseudoVariables 				-> '#2070E0'.
		#literals 						-> #green.
		#messages 					-> '#6FB3BD'.
		#incompleteMessages  		-> '#F08060'.
		#blockLevelZero 				-> '#6FB3BD'.
		#blockLevelOne 				-> '#FFB0B0'.
		#blockLevelTwo 				-> '#B0FFB0'.
		#blockLevelThree 				-> '#B0B0FF'.
		#blockLevelFour 				-> '#00B0B0'.
		#blockLevelFive 				-> '#E03030'.
		#blockLevelSix 				-> '#30E030'.
		#blockLevelSeven 				-> '#3030E0'.
		#defaults 						-> '#A1AFBF'.
		#undefined 					-> '#E04020'.
		#methodTags 					-> #green.
	}
]

{ #category : 'colors' }
DarkTheme >> text [
	^ `Color veryVeryLightGray`
]

{ #category : 'colors' }
DarkTheme >> textHighlight [
	"A nice light blue."
	"
	^ Color r: 0.71 g: 0.835 b: 1.0
	^ Color hue: 214 chroma: 0.29 luminance: 0.816
	"
	^ `Color hue: 204 chroma: 0.29 luminance: 0.22`
]

{ #category : 'colors' }
DarkTheme >> textPane [
	^ `Color gray`
]

{ #category : 'colors' }
DarkTheme >> unfocusedTextHighlightFrom: aColor [
	^ aColor adjustSaturation: -0.15 brightness: -0.07
]

{ #category : 'colors' }
DarkTheme >> useUniformColors [
	^ true
]

{ #category : 'other options' }
DarkTheme >> windowLabel [
	^ `Color gray: 0.55`
]
