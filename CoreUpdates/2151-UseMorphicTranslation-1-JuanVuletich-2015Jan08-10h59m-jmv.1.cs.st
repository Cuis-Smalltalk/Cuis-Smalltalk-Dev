'From Cuis 4.2 of 25 July 2013 [latest update: #2149] on 8 January 2015 at 11:05:27.75089 am'!

!Morph methodsFor: 'initialization' stamp: 'jmv 1/7/2015 09:59'!
initialize
	"initialize the state of the receiver"

	owner _ nil.
	submorphs _ #().
	location _ MorphicTranslation new.
	layoutNeeded _ false! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 1/8/2015 11:03'!
intoLocation: aMatrixTransform2x3

	| last |
	cti _ cti + 1.
	transformations size < cti
		ifTrue: [	
			currentTransformation _ currentTransformation composedWith: aMatrixTransform2x3.
			transformations add: currentTransformation ]
		ifFalse: [
			
			"reuse the instance"
			" last _ currentTransformation.
			currentTransformation _ transformations at: cti.
			last composedWith: aMatrixTransform2x3 into: currentTransformation" 
			
			currentTransformation _ currentTransformation composedWith: aMatrixTransform2x3.
			transformations at: cti put: currentTransformation
			
			
			]! !

!MorphicCanvas methodsFor: 'initialization' stamp: 'jmv 1/7/2015 10:00'!
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
		ifNotNil: [ transformations at: cti put: currentTransformation ]! !

