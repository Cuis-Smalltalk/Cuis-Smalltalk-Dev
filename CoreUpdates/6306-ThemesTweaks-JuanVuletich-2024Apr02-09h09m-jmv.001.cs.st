'From Cuis6.3 [latest update: #6305] on 2 April 2024 at 9:42:29 am'!

!Theme methodsFor: 'colors' stamp: 'jmv 4/2/2024 09:41:00'!
textEmptyDisplayMessage
	^ `Color veryLightGray`! !

!Theme methodsFor: 'tool colors' stamp: 'jmv 4/2/2024 09:38:55'!
defaultWindowColor
	^ `Color veryLightGray`! !

!Theme methodsFor: 'shout' stamp: 'jmv 4/2/2024 09:14:53'!
shout
	"Color symbols as an association list.
	SHTextStylerST80 initialize
	"
	^ {
		#selector  						-> nil.
		#arguments  						-> #(cyan muchDarker).
		#comment 						-> #(green muchDarker).
		#tempBar 						-> #gray.
		#tempVars 						-> #(gray muchDarker).
		#instVar 						-> #(magenta muchDarker).
		#pseudoVariables 						-> #(red muchDarker).
		#literals 						-> #(green muchDarker).
		#messages 						-> #(blue darker).
		#incompleteMessages 						-> #(gray veryMuchDarker).
		#blockLevelZero 						-> #black.
		#blockLevelOne 						-> #brown.
		#blockLevelTwo 						-> #magenta.
		#blockLevelThree 						-> #red.
		#blockLevelFour 						-> #(orange darker).
		#blockLevelFive 						-> #(orange muchDarker).
		#blockLevelSix 						-> #(green muchDarker).
		#blockLevelSeven 						-> #blue.
		#defaults 						-> #black.
		#undefined 						-> #red.
		#methodTags 						-> #(green muchDarker).
	}! !


!DarkTheme methodsFor: 'colors' stamp: 'jmv 4/2/2024 09:37:35'!
shout
	"Color symbols as an association list."

	^ {
		#selector  						-> '#B59F60'.
		#arguments 						-> '#289078'.
		#comment 						-> #(green duller).
		#tempBar 						-> #gray.
		#tempVars 						-> '#767836'.
		#instVar 						-> '#B3695A'.
		#pseudoVariables 						-> '#2070E0'.
		#literals 						-> #green.
		#messages 						-> '#6FB3BD'.
		#incompleteMessages  					-> '#F08060'.
		#blockLevelZero 						-> '#6FB3BD'.
		#blockLevelOne 						-> '#FFB0B0'.
		#blockLevelTwo 						-> '#B0FFB0'.
		#blockLevelThree 						-> '#B0B0FF'.
		#blockLevelFour 						-> '#00B0B0'.
		#blockLevelFive 						-> '#E03030'.
		#blockLevelSix 						-> '#30E030'.
		#blockLevelSeven 						-> '#3030E0'.
		#defaults 						-> '#A1AFBF'.
		#undefined 						-> '#E04020'.
		#methodTags 						-> #green.
	}! !

!DarkTheme methodsFor: 'colors' stamp: 'jmv 4/2/2024 09:41:46'!
text
	^ `Color veryVeryLightGray`! !

