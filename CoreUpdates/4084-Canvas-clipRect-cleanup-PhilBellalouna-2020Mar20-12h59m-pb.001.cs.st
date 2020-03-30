'From Cuis 5.0 [latest update: #4070] on 20 March 2020 at 1:01:06 pm'!

!classDefinition: #MorphicCanvas category: #'Morphic-Support'!
Object subclass: #MorphicCanvas
	instanceVariableNames: 'form clipRect clipLeft clipTop clipRight clipBottom transformations currentTransformation cti currentMorph drawingMorphStack engine '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
BitBltCanvas allInstances do:[:eaCanvas| eaCanvas clipRect]!

!MorphicCanvas methodsFor: 'accessing' stamp: 'pb 3/20/2020 12:59:56'!
clipRect
	"Return the currently active clipping rectangle"
	"In targetForm coordinates"
	^ clipRect! !

!classDefinition: #MorphicCanvas category: #'Morphic-Support'!
Object subclass: #MorphicCanvas
	instanceVariableNames: 'form clipRect transformations currentTransformation cti currentMorph drawingMorphStack engine'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
