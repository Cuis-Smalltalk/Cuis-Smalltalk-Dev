'From Cuis 6.0 [latest update: #5092] on 8 March 2022 at 9:11:26 pm'!

!Morph methodsFor: 'halos and balloon help' stamp: 'HAW 3/8/2022 20:40:29'!
showBalloon: msgStringOrText hand: aHand
	"Pop up a balloon containing the given string,
	first removing any existing BalloonMorphs in the world."

	| doWordWrap msg |
	doWordWrap _ true.
	msg _ msgStringOrText.
	(msgStringOrText notEmpty and: [ msgStringOrText last isLineSeparator ]) ifTrue: [
		doWordWrap _ false.
		msg _ msg allButLast ].
	self
		showBalloon: msg
		hand: aHand
		doWordWrap: doWordWrap.! !

