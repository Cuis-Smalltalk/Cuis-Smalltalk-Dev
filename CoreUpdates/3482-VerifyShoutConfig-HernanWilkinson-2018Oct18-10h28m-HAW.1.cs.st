'From Cuis 5.0 of 7 November 2016 [latest update: #3481] on 18 October 2018 at 10:40:55 am'!

!Theme class methodsFor: 'consistency verification' stamp: 'HAW 10/10/2018 16:57:50'!
verifyShoutConfig

	"
	self verifyShoutConfig
	"
	^self allSubclasses
		inject: OrderedCollection new into: [ :errors :themeClass |
			[ themeClass new generateShoutConfig ] on: Error do: [:anError | | atIfAbsentContext key |
				"I have to do this because error is not resumable - Hernan"
				atIfAbsentContext := thisContext.
				10 timesRepeat: [ atIfAbsentContext := atIfAbsentContext sender ].
				key := atIfAbsentContext tempAt: 1.
				errors add: themeClass -> key.
				atIfAbsentContext receiver at: key put: Color black.
				atIfAbsentContext restart ].
			errors ]! !


!Theme methodsFor: 'shout' stamp: 'jmv 10/18/2018 10:39:38'!
shout
	"Color symbols as an association list.
	SHTextStylerST80 initialize
	"

	^ {
		#defaults 					-> #black.
		#undefined 					-> #red.
		#comment 					-> #(green muchDarker).
		#methodTags 				-> #(green muchDarker).
		#pseudoVariables 			-> #(red muchDarker).
		#literals 					-> #(green muchDarker).
		#messages 					-> #(blue darker).
		#arguments 					-> #(cyan muchDarker).
		#instVar 					-> #(magenta muchDarker).
		#incompleteMessages 		-> #(gray veryMuchDarker).
		#blockLevelZero 			-> #black.
		#blockLevelOne 			-> #brown.
		#blockLevelTwo 			-> #magenta.
		#blockLevelThree 			-> #red.
		#blockLevelFour 			-> #(orange darker).
		#blockLevelFive 			-> #(orange muchDarker).
		#blockLevelSix 			-> #(green muchDarker).
		#blockLevelSeven 			-> #blue.
		#tempBar 					-> #gray.
		#tempVars 					-> #(gray muchDarker).
	}! !

