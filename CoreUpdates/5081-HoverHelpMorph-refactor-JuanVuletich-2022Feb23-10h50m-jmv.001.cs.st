'From Cuis 6.0 [latest update: #5078] on 23 February 2022 at 10:51:01 am'!

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 2/23/2022 10:00:14'!
showBalloon: msgStringOrText hand: aHand doWordWrap: aBoolean
	"Pop up a balloon containing the given string,
	first removing any existing BalloonMorphs in the world."

	| w balloon h |
	w _ self world ifNil: [^ self].
	h _ aHand ifNil: [ w activeHand ].
	balloon _ HoverHelpMorph contents: msgStringOrText doWordWrap: aBoolean.
	
	"Do it in a while. In some cases, processing the event that
	might have triggered us might also remove any Help Balloon"
	UISupervisor whenUIinSafeState: [
		balloon popUpForHand: h ].! !


!HoverHelpMorph methodsFor: 'accessing' stamp: 'jmv 2/23/2022 09:59:48'!
contents: aStringOrText wrappedTo: wordWrapWidthOrNil
	"If wordWrapWidthOrNil is nil, don't do wordwrap, and make the morph as wide as needed"

	| width |
	contents _ aStringOrText.
	width _ wordWrapWidthOrNil ifNil: [9999999].
	textComposition _ TextComposition new.
	textComposition
		setModel: (TextModel withText: contents asText);
		extentForComposing: 	width@9999999.
	textComposition composeAll.
	self morphExtent: textComposition usedExtent + 8.! !


!HoverHelpMorph class methodsFor: 'instance creation' stamp: 'jmv 2/23/2022 09:58:59'!
contents: aStringOrText
	"Make up and return a HoverHelp for morph.
	(HoverHelpMorph contents: 'This is a HoverHelpMorph with a rather largish contents, to show the effect of word wrap.') openInHand
	"

	^ self contents: aStringOrText doWordWrap: true! !

!HoverHelpMorph class methodsFor: 'instance creation' stamp: 'jmv 2/23/2022 09:59:37'!
contents: aStringOrText wrappedTo: wordWrapWidthOrNil
	"Make up and return a HoverHelp for morph.
	(HoverHelpMorph contents: 'This is a HoverHelpMorph with a rather long contents to see how it gets wrapped. Is this long enough? Maybe a few more words are in order.'
		wrappedTo: 150) openInHand
	"

	^self new contents: aStringOrText wrappedTo: wordWrapWidthOrNil! !

