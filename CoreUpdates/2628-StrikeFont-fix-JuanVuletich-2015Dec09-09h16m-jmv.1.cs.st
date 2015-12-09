'From Cuis 4.2 of 25 July 2013 [latest update: #2627] on 9 December 2015 at 9:16:53.663128 am'!

!StrikeFont methodsFor: 'accessing' stamp: 'jmv 12/8/2015 21:07'!
descent
	"Answer the receiver's maximum extent of characters below the baseline."

	| answer |
	answer _ descent.
	emphasis = 32 ifTrue: [ answer _ answer * 2 ].
	^ answer! !

