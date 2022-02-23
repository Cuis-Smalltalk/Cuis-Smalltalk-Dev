'From Cuis 6.0 [latest update: #5075] on 23 February 2022 at 9:14:18 am'!

!SmalltalkEditor methodsFor: 'quick action (shift+click)' stamp: 'jmv 2/23/2022 09:03:41'!
quickPrintOfNodeUnder: aMousePosition

	| mousePositionInText |
	
	mousePositionInText := morph positionInTextOf: aMousePosition.

	self 
		withMethodNodeAndClassDo: [ :methodNode :class | 
			methodNode 
				withParseNodeIncluding: mousePositionInText 
				do: [ :aNodeUnderCursor | morph showBalloon: (self hoverHelpToShowEvaluating: aNodeUnderCursor)]
				ifAbsent: [  morph flash  ]]
		ifErrorsParsing: [ :anError | morph showBalloon: anError messageText ]! !


!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 2/23/2022 09:01:52'!
showBalloon: msgString
	"Pop up a balloon containing the given string,
	first removing any existing BalloonMorphs in the world."

	self showBalloon: msgString hand: nil! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 2/23/2022 09:12:43'!
showBalloon: msgString hand: aHand
	"Pop up a balloon containing the given string,
	first removing any existing BalloonMorphs in the world."

	| w balloon h |
	w _ self world ifNil: [^ self].
	h _ aHand ifNil: [ w activeHand ].
	balloon _ HoverHelpMorph contents: msgString.
	
	"Do it in a while. In some cases, processing the event that
	might have triggered us might also remove any Help Balloon"
	UISupervisor whenUIinSafeState: [
		balloon popUpForHand: h ].! !

!methodRemoval: InnerTextMorph #showHoverHelpWith: stamp: 'jmv 2/23/2022 09:03:46'!
InnerTextMorph removeSelector: #showHoverHelpWith:!
