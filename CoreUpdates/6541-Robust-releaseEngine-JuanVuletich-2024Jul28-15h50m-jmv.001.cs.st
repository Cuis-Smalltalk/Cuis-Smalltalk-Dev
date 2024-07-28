'From Cuis7.1 [latest update: #6539] on 28 July 2024 at 3:50:38 pm'!

!VectorCanvas methodsFor: 'private' stamp: 'jmv 7/28/2024 13:48:42'!
releaseEngine
	super releaseEngine.
	boundsFinderCanvas notNil ifTrue: [
		boundsFinderCanvas releaseEngine ].
	auxBitBltEngine := nil.! !


!BitBltCanvas methodsFor: 'private' stamp: 'jmv 7/28/2024 13:48:49'!
releaseEngine
	super releaseEngine.
	boundsFinderCanvas notNil ifTrue: [
		boundsFinderCanvas releaseEngine ].! !

