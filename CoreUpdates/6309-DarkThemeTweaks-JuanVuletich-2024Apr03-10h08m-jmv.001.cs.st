'From Cuis6.3 [latest update: #6308] on 3 April 2024 at 10:16:03 am'!

!DarkTheme methodsFor: 'colors' stamp: 'jmv 4/3/2024 10:15:30'!
background
	^ `Color veryVeryDarkGray`! !

!DarkTheme methodsFor: 'colors' stamp: 'jmv 4/3/2024 10:14:46'!
shout
	"Color symbols as an association list."

	^ {
		#selector  						-> '#B59F60'.
		#arguments 						-> '#289078'.
		#comment 						-> #(green duller).
		#tempBar 						-> '#707070'.
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

