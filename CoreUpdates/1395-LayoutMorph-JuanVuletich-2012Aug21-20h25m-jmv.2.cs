'From Cuis 4.0 of 21 April 2012 [latest update: #1394] on 21 August 2012 at 8:29:11 pm'!
!classDefinition: #LayoutMorph category: #'Morphic-Layouts'!
BorderedRectMorph subclass: #LayoutMorph
	instanceVariableNames: 'direction separation padding'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Layouts'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
RectangleLikeMorph allSubInstancesDo: [ :m |
	(m instVarNamed: 'xtent') ifNil: [ m instVarNamed: 'xtent' put: (m instVarNamed: 'extent') ].
	(m instVarNamed: 'ccolor') ifNil: [ m instVarNamed: 'ccolor' put: (m instVarNamed: 'color') ]].
BorderedRectMorph allSubInstancesDo: [ :m |
	(m instVarNamed: 'borderWidth') ifNil: [ m instVarNamed: 'borderWidth' put: (m instVarNamed: '0') ].
	(m instVarNamed: 'borderColor') ifNil: [ m instVarNamed: 'borderColor' put: (m instVarNamed: 'color') ]].!

