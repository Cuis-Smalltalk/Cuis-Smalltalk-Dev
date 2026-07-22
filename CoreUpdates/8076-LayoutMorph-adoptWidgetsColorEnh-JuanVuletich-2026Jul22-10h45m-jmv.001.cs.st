'From Cuis7.9 [latest update: #8075] on 22 July 2026 at 11:00:05 am'!

!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 22/Jul/2026, 1:56:25 pm (UTC)'!
doAdoptPaneBackgroundColor
	adoptsWidgetsColor := #paneBackground.! !


!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 22/Jul/2026, 1:52:53 pm (UTC)'!
adoptWidgetsColor: paneColor
	super adoptWidgetsColor: paneColor.
	adoptsWidgetsColor ifNotNil: [
		adoptsWidgetsColor == #buttonColor
			ifTrue: [ self color: (Theme current buttonColorFrom: paneColor) paler ].
		adoptsWidgetsColor == #paneBackground
			ifTrue: [ self color: (Theme current paneBackgroundFrom: paneColor) paler ]].! !

!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 22/Jul/2026, 1:56:34 pm (UTC)'!
doAdoptWidgetsColor
	adoptsWidgetsColor := #buttonColor.! !

!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 22/Jul/2026, 1:50:22 pm (UTC)'!
initialize
	super initialize.
	adoptsWidgetsColor := nil.! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
LayoutMorph allSubInstancesDo: [ :lm |
	lm instVarNamed: 'adoptsWidgetsColor' put:
		((lm instVarNamed: 'adoptsWidgetsColor') ifTrue: [#buttonColor]) ].!

