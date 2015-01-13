'From Cuis 4.2 of 25 July 2013 [latest update: #2149] on 8 January 2015 at 11:28:20.04889 am'!

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 1/8/2015 11:11'!
intoLocation: aMatrixTransform2x3

	| last |
	cti _ cti + 1.
	transformations size < cti
		ifTrue: [	
			currentTransformation _ currentTransformation composedWith: aMatrixTransform2x3.
			transformations add: currentTransformation ]
		ifFalse: [
			last _ currentTransformation.
			currentTransformation _ transformations at: cti.
			"reuse the instance if possible"
			(last class == aMatrixTransform2x3 class and: [ last class == currentTransformation class ])
				ifTrue: [
					last composedWith: aMatrixTransform2x3 into: currentTransformation ]
				ifFalse: [
					currentTransformation _ last composedWith: aMatrixTransform2x3.
					transformations at: cti put: currentTransformation ]]! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
| m t | 
Taskbar reset.
self runningWorld instVarNamed: 'location'  put: MorphicTranslation new.
self runningWorld clearCanvas.
self runningWorld activeHand instVarNamed: 'location'  put: MorphicTranslation new.
m _ MatrixTransform2x3 allInstances select: [ :e | e isPureTranslation ]. t _ m collect: [ :e | MorphicTranslation withTranslation: e translation ]. m elementsExchangeIdentityWith: t.
Smalltalk garbageCollect. MatrixTransform2x3 allInstances.
BitBltCanvas allInstances.!

