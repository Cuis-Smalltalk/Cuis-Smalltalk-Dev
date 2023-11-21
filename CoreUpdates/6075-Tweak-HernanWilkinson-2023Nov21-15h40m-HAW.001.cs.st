'From Cuis 6.0 [latest update: #6074] on 21 November 2023 at 3:41:56 pm'!

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 11/7/2023 10:53:41'!
lookForBinarySelectorAfter: aStopToken startingAt: anIndex with: aCandidate

	| rangeAtCurrentIndex currentIndex |
	
	currentIndex := anIndex.
	[ rangeAtCurrentIndex := allRanges at: currentIndex.
	rangeAtCurrentIndex rangeType ~= aStopToken and: [ currentIndex > 1 ]] whileTrue: [ currentIndex := currentIndex - 1 ].

	^currentIndex > 1
		ifTrue: [ allRanges at: currentIndex - 1 ]
		ifFalse: [ aCandidate ].
! !

