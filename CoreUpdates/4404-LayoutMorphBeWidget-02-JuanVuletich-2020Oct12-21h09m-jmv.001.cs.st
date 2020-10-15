'From Cuis 5.0 [latest update: #4396] on 12 October 2020 at 9:10:58 pm'!
!classDefinition: #LayoutMorph category: #'Morphic-Layouts'!
WidgetMorph subclass: #LayoutMorph
	instanceVariableNames: 'direction separation axisEdgeWeight doAdoptWidgetsColor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Layouts'!

!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 10/12/2020 21:00:28'!
defaultBorderWidth
	"answer the default border width for the receiver"
	^ 0! !
	
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
LayoutMorph allSubInstancesDo: [ :each | each instVarNamed: 'borderWidth' put: 0; instVarNamed: 'borderColor' put: Color white ].!

