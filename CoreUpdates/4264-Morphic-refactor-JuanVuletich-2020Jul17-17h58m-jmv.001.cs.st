'From Cuis 5.0 [latest update: #4260] on 17 July 2020 at 5:58:42 pm'!
!classDefinition: #MorphicCanvas category: #'Morphic-Support'!
Object subclass: #MorphicCanvas
	instanceVariableNames: 'form clipRect transformations currentTransformation cti currentMorph drawingMorphStack engine world '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!

!MorphicCanvas methodsFor: 'initialization' stamp: 'jmv 7/11/2020 00:01:48'!
world: aPasteUpMorph
	world _ aPasteUpMorph.
	self into: world! !


!WorldState methodsFor: 'canvas' stamp: 'jmv 7/11/2020 00:01:55'!
setCanvas: aMorphicCanvas
	canvas _ aMorphicCanvas.
	canvas world: world.
	damageRecorder
		ifNil: [ damageRecorder _ DamageRecorder new].
	damageRecorder doFullRepaint! !

!classDefinition: #MorphicCanvas category: #'Morphic-Support'!
Object subclass: #MorphicCanvas
	instanceVariableNames: 'world form clipRect transformations currentTransformation cti currentMorph drawingMorphStack engine'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
