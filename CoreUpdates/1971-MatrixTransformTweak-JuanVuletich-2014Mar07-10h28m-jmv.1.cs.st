'From Cuis 4.2 of 25 July 2013 [latest update: #1969] on 7 March 2014 at 10:33:40.776018 am'!

!FormCanvas methodsFor: 'morphic' stamp: 'jmv 3/4/2014 18:35'!
intoLocation: aMatrixTransform2x3

	| last |
	cti _ cti + 1.
	transformations size < cti
		ifTrue: [	
			currentTransformation _ currentTransformation composedWith: aMatrixTransform2x3.
			transformations add: currentTransformation ]
		ifFalse: [
			"reuse the instance"
			last _ currentTransformation.
			currentTransformation _ transformations at: cti.
			last composedWith: aMatrixTransform2x3 into: currentTransformation ]! !


!FormCanvas methodsFor: 'morphic' stamp: 'jmv 3/4/2014 18:35'!
into: aMorph

	self intoLocation: aMorph location! !


!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 3/7/2014 10:32'!
autoHint: p
	"Round p for text autoHinting in Morphic 3.
	Rounding to integer is appropriate for fonts drawn with a strike.
	TrueType uses fills with zero width borders. I that case, something like
	(p y - 0.5) rounded + 0.5
	is appropriate.
	Leave x alone."

	^p x @ p y rounded! !

