'From Cuis7.3 [latest update: #6988] on 16 January 2025 at 4:35:07 pm'!

!UniFileStream class methodsFor: 'cached state access' stamp: 'jmv 1/16/2025 16:34:16'!
releaseClassCachedState

	Registry ifNotNil: [
		"Make the best effort to empty the registry.
		Only keep files that are still open have references from some object."
		Registry isEmpty ifFalse: [
			Smalltalk garbageCollectMost.
			Registry isEmpty ifFalse: [
				Smalltalk garbageCollect]].

		"No open files. Can nil the Registry."
		Registry isEmpty ifTrue: [
			Registry := nil ]
	]! !
