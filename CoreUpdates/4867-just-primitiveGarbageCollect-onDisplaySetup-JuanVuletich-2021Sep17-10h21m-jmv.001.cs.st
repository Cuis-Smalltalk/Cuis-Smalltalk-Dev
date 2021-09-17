'From Cuis 5.0 [latest update: #4866] on 17 September 2021 at 10:22:21 am'!

!DisplayScreen class methodsFor: 'snapshots' stamp: 'jmv 9/17/2021 10:21:57'!
setupDisplay: doGarbageCollection
	"
	DisplayScreen setupDisplay: true.
	Display forceToScreen.
	"

	self terminateScreenUpdater.
	doGarbageCollection ifTrue: [
		Display setExtent: 0@0 depth: 0 bits: nil.
		Smalltalk primitiveGarbageCollect. ].
	Display setExtent: self actualScreenSize depth: Display nativeDepth.
	Display beDisplay.
	self installScreenUpdater.! !

