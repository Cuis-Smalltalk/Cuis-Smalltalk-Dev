'From Cuis 6.0 [latest update: #5607] on 21 December 2022 at 12:56:09 pm'!
!classDefinition: #VectorEngineSmalltalk category: #'Morphic-Support'!
VectorEngineDrawer subclass: #VectorEngineSmalltalk
	instanceVariableNames: 'spanLeft spanTop spanRight spanBottom prevYTruncated prevYRounded leftAtThisY rightAtThisY aawScaledInverse swDilatedHalf swDilatedHalfSquared swErodedHalfSquared'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
!classDefinition: #VectorEngineSubPixel category: #'Morphic-Support'!
VectorEngineSmalltalk subclass: #VectorEngineSubPixel
	instanceVariableNames: 'subPixelDelta'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
!classDefinition: #VectorEngineWholePixel category: #'Morphic-Support'!
VectorEngineSmalltalk subclass: #VectorEngineWholePixel
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
SystemOrganization removeCategory: 'VectorEngineInSmalltalk'!

