'From Cuis 5.0 [latest update: #4595] on 12 May 2021 at 1:06:30 pm'!

!MorphicCanvas methodsFor: 'initialization' stamp: 'jmv 5/12/2021 11:10:35'!
initializeWith: aForm origin: aPoint
	self initialize.
	self setForm: aForm.

	"Set up these only on initialization.
	This is safe (wrt walkbacks during world redraw) because a new instance is created
	on draw error. See #displayWorldSafely"
	currentTransformation _ MorphicTranslation withTranslation: aPoint.
	cti _ 1.
	transformations
		ifNil: [ transformations _ OrderedCollection with: currentTransformation ]
		ifNotNil: [ transformations at: cti put: currentTransformation ].
	drawingMorphStack
		ifNil: [ drawingMorphStack _ OrderedCollection with: nil ]
		ifNotNil: [ drawingMorphStack at: cti put: nil ].! !

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 5/12/2021 11:11:22'!
into: aMorph
	| locationOrNil previousLast |
	locationOrNil _ aMorph location.
	currentMorph _ aMorph.
	cti _ cti + 1.
	transformations size < cti
		ifTrue: [
			drawingMorphStack add: aMorph.
			currentTransformation _ locationOrNil
				ifNotNil: [ currentTransformation composedWith: locationOrNil]
				ifNil: [ currentTransformation copy ].
			transformations add: currentTransformation ]
		ifFalse: [
			drawingMorphStack at: cti put: aMorph.
			previousLast _ currentTransformation.
			locationOrNil
				ifNil: [
					currentTransformation _ previousLast copy.
					transformations at: cti put: currentTransformation ]
				ifNotNil: [ :location |
					currentTransformation _ transformations at: cti.
					"reuse the instance if possible"
					(previousLast class == location class and: [ previousLast class == currentTransformation class ])
						ifTrue: [
							previousLast composedWith: location into: currentTransformation ]
						ifFalse: [
							currentTransformation _ previousLast composedWith: location.
							transformations at: cti put: currentTransformation ]
				]
		]! !

