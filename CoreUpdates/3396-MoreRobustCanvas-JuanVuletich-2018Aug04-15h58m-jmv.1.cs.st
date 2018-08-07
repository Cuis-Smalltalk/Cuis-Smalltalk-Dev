'From Cuis 5.0 of 7 November 2016 [latest update: #3395] on 4 August 2018 at 3:59:49 pm'!
!classDefinition: #MorphicCanvas category: #'Morphic-Support'!
Object subclass: #MorphicCanvas
	instanceVariableNames: 'form clipLeft clipTop clipRight clipBottom transformations currentTransformation cti currentMorph drawingMorphStack '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 8/4/2018 15:46:00'!
into: aMorph
	| location previousLast |
	location _ aMorph location.
	drawingMorphStack ifNil: [ drawingMorphStack _ transformations collect: [ :t | nil ]].
	drawingMorphStack size = transformations size ifFalse:  [ drawingMorphStack _ transformations collect: [ :t | nil ]].
	currentMorph _ aMorph.
	cti _ cti + 1.
	transformations size < cti
		ifTrue: [
			drawingMorphStack add: aMorph.
			currentTransformation _ currentTransformation composedWith: location.
			transformations add: currentTransformation ]
		ifFalse: [
			drawingMorphStack at: cti put: aMorph.
			previousLast _ currentTransformation.
			currentTransformation _ transformations at: cti.
			"reuse the instance if possible"
			(previousLast class == location class and: [ previousLast class == currentTransformation class ])
				ifTrue: [
					previousLast composedWith: location into: currentTransformation ]
				ifFalse: [
					currentTransformation _ previousLast composedWith: location.
					transformations at: cti put: currentTransformation ]]! !

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 8/4/2018 15:58:56'!
outOfMorph

	drawingMorphStack at: cti put: nil.			"Don't hold any morphs that could be collected"
	cti _ cti - 1.
	currentTransformation _ transformations at: cti.
"	currentMorph _ drawingMorphStack at: cti"
	currentMorph _ currentMorph owner! !

!MorphicCanvas methodsFor: 'initialization' stamp: 'jmv 8/4/2018 15:37:46'!
initializeWith: aForm origin: aPoint
	self initialize.
	self setForm: aForm.

	"We currently set up these only on initialization.
	This is safe (wrt walkbacks during world redraw) because a new instance is created
	each time the world is redrawn. See #drawInvalidAreasWorld:submorphs:
	Maybe this cleanup should be in an aux method that can be called each time on an existing instance..."
	currentTransformation _ MorphicTranslation withTranslation: aPoint.
	cti _ 1.
	transformations
		ifNil: [ transformations _ OrderedCollection with: currentTransformation ]
		ifNotNil: [ transformations at: cti put: currentTransformation ].
	drawingMorphStack
		ifNil: [ drawingMorphStack _ OrderedCollection with: nil ]
		ifNotNil: [ drawingMorphStack at: cti put: nil ].! !

!methodRemoval: MorphicCanvas #intoLocation:!
MorphicCanvas removeSelector: #intoLocation:!
!methodRemoval: MorphicCanvas #outOfLocation!
MorphicCanvas removeSelector: #outOfLocation!
!classDefinition: #MorphicCanvas category: #'Morphic-Support'!
Object subclass: #MorphicCanvas
	instanceVariableNames: 'form clipLeft clipTop clipRight clipBottom transformations currentTransformation cti currentMorph drawingMorphStack'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
MorphicCanvas allSubInstancesDo: [ :each | each instVarNamed: 'drawingMorphStack' put: ((each instVarNamed: 'transformations') collect: [ :t | nil])].!

