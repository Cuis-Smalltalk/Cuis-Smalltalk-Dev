'From Cuis 6.0 [latest update: #5448] on 16 August 2022 at 11:23:54 am'!

!PositionableStream methodsFor: 'testing' stamp: 'jmv 8/16/2022 11:20:00'!
atStart
	"Answer whether the receiver is at the start position."

	^position = 0.! !


!ChangeList methodsFor: 'scanning' stamp: 'jmv 8/16/2022 11:22:46'!
scanUpTo: stopPosition informing: barBlock
	
	[
		file skipSeparators.
		file position < stopPosition
	] whileTrue: [	
		barBlock value: file position.
		"A line starting with $!! means a specific ChangeRecord type"
		((file atStart or: [ file peekBack isLineSeparator]) and: [ file peekFor: $!! ])
			ifTrue: [ self scanSpecificChangeRecordType]
			ifFalse: [ self scanDoIt ]]! !

!methodRemoval: ChangeList #scanSpecificChangeRecordTypeIfNotAtEnd: stamp: 'jmv 8/16/2022 11:08:28'!
ChangeList removeSelector: #scanSpecificChangeRecordTypeIfNotAtEnd:!
!methodRemoval: ChangeList #notSeparatorChar stamp: 'jmv 8/16/2022 11:07:46'!
ChangeList removeSelector: #notSeparatorChar!
