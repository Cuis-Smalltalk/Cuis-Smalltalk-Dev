'From Cuis 4.0 of 21 April 2012 [latest update: #1289] on 23 May 2012 at 10:26:51 pm'!

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

