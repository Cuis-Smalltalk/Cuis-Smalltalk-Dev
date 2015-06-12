'From Cuis 4.2 of 25 July 2013 [latest update: #2359] on 11 June 2015 at 10:04:18.678313 am'!

!StandardFileStream class methodsFor: 'cached state access' stamp: 'jmv 6/11/2015 10:03'!
releaseClassCachedState

	"Make the best effort to empty the registry.
	Only keep files that are still open have references from some object."
	Registry isEmpty ifFalse: [
		Smalltalk garbageCollectMost.
		Registry isEmpty ifFalse: [
			Smalltalk garbageCollect.
			Registry isEmpty ifFalse: [
				Registry finalizeValues ]]].

	"No open files. Can nil the Registry."
	Registry isEmpty ifTrue: [
		Registry _ nil ]! !

