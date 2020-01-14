'From Cuis 5.0 [latest update: #3959] on 11 January 2020 at 5:40:48 pm'!
!classDefinition: #MorphicCanvas category: #'Morphic-Support'!
Object subclass: #MorphicCanvas
	instanceVariableNames: 'form clipLeft clipTop clipRight clipBottom transformations currentTransformation cti currentMorph drawingMorphStack engine '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!

!BitBltCanvas methodsFor: 'private' stamp: 'jmv 1/11/2020 17:40:22'!
resetGrafPort
	"Private!! Create a new grafPort for a new copy."

	port _ GrafPort toForm: form.
	"Init BitBlt so that the first call to the 'primitiveDisplayString' primitive will not fail"
	port sourceX: 0; width: 0.
	engine _ port! !

!classDefinition: #MorphicCanvas category: #'Morphic-Support'!
Object subclass: #MorphicCanvas
	instanceVariableNames: 'form clipLeft clipTop clipRight clipBottom transformations currentTransformation cti currentMorph drawingMorphStack engine'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

BitBltCanvas allInstancesDo: [ :canvas | canvas resetGrafPort ]!

