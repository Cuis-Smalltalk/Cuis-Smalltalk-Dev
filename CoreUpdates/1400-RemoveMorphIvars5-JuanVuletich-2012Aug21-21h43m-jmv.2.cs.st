'From Cuis 4.0 of 21 April 2012 [latest update: #1399] on 21 August 2012 at 9:46:20 pm'!
!classDefinition: #RectangleLikeMorph category: #'Morphic-Kernel'!
Morph subclass: #RectangleLikeMorph
	instanceVariableNames: 'xtent ccolor color extent '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!
!classDefinition: #RectangleLikeMorph category: #'Morphic-Kernel'!
Morph subclass: #RectangleLikeMorph
	instanceVariableNames: 'xtent ccolor extent color'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
RectangleLikeMorph allSubInstancesDo: [ :m |
	m instVarNamed: 'color' put: (m instVarNamed: 'ccolor').
	m instVarNamed: 'extent' put: (m instVarNamed: 'xtent') ]!