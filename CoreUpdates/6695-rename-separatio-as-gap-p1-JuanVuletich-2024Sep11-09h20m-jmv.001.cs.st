'From Cuis7.1 [latest update: #6693] on 11 September 2024 at 9:20:36 am'!
!classDefinition: #LayoutMorph category: #'Morphic-Layouts'!
BorderedBoxMorph subclass: #LayoutMorph
	instanceVariableNames: 'direction separation axisEdgeWeight doAdoptWidgetsColor gap '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Layouts'!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
LayoutMorph allSubInstances do: [ :m | m instVarNamed: 'gap' put: (m instVarNamed: 'separation') ].
!
