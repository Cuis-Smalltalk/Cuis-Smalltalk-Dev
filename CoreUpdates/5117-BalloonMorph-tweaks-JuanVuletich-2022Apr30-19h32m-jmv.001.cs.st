'From Cuis 6.0 [latest update: #5116] on 30 April 2022 at 7:35:28 pm'!

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 4/30/2022 19:34:06'!
showBalloon: msgString
	"Pop up a balloon containing the given string,
	first removing any existing BalloonMorphs in the world."

	self world ifNotNil: [ :w |
		self showBalloon: msgString hand: w activeHand ].! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 4/30/2022 19:32:54'!
showBalloon: msgStringOrText hand: aHand doWordWrap: aBoolean
	"Pop up a balloon containing the given string,
	first removing any existing BalloonMorphs in the world."

	| balloon |
	balloon _ HoverHelpMorph contents: msgStringOrText doWordWrap: aBoolean.
	
	"Do it in a while. In some cases, processing the event that
	might have triggered us might also remove any Help Balloon"
	UISupervisor whenUIinSafeState: [
		balloon popUpForHand: aHand ].! !

