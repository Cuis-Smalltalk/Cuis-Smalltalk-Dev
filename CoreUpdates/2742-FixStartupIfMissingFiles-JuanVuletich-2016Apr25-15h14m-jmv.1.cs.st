'From Cuis 4.2 of 25 July 2013 [latest update: #2741] on 25 April 2016 at 3:14:48.16292 pm'!

!StandardFileStream class methodsFor: 'registry' stamp: 'jmv 4/25/2016 15:14'!
retryWithGC: execBlock until: testBlock forFileNamed: fullName
	"Re-implemented to only force GC if a file with the given name exists"
	| blockValue foundIt |
	blockValue := execBlock value.
	(testBlock value: blockValue) ifTrue:[^blockValue].
	"See if we have a file with the given name"
	foundIt _ self registry keys "hold on strongly for now" 
		anySatisfy:[:file| file name sameAs: fullName].
	foundIt ifFalse:[^blockValue].
	Smalltalk garbageCollectMost.
	blockValue := execBlock value.
	(testBlock value: blockValue) ifTrue:[^blockValue].
	Smalltalk garbageCollect.
	^execBlock value.! !

