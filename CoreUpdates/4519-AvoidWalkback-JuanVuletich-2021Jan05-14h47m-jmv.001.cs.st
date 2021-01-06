'From Cuis 5.0 [latest update: #4518] on 5 January 2021 at 2:48:26 pm'!

!MorphicEvent methodsFor: 'dispatching' stamp: 'jmv 1/5/2021 14:47:51'!
sentTo: aMorph localPosition: positionInAMorph
	"Dispatch the receiver into aMorph"

	^ self wasHandled: true! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
[
	(Delay forSeconds: 1) wait.
	SystemChangeNotifier uniqueInstance doSilently: [
		MorphicEvent removeSelector: #sentTo:localPosition:.
		SmalltalkCompleter initialize]
] fork!

