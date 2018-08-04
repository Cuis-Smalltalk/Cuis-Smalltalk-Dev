'From Cuis 5.0 of 7 November 2016 [latest update: #3395] on 4 August 2018 at 4:01:01 pm'!

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 8/4/2018 16:00:30'!
into: aMorph
	| location previousLast |
	location _ aMorph location.
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

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 8/4/2018 16:00:39'!
outOfMorph

	drawingMorphStack at: cti put: nil.			"Don't hold any morphs that could be collected"
	cti _ cti - 1.
	currentTransformation _ transformations at: cti.
	currentMorph _ drawingMorphStack at: cti! !

