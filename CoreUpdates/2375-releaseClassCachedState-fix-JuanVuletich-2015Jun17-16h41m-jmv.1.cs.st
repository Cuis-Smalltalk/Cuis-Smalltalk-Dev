'From Cuis 4.2 of 25 July 2013 [latest update: #2372] on 17 June 2015 at 4:43:10.014066 pm'!

!StandardFileStream class methodsFor: 'cached state access' stamp: 'jmv 6/17/2015 16:43'!
releaseClassCachedState

	Registry ifNotNil: [
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
			Registry _ nil ]
	]! !

