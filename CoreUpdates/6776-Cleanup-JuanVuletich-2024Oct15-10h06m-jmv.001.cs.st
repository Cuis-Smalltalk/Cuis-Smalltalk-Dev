'From Cuis7.1 [latest update: #6774] on 15 October 2024 at 10:08:20 am'!

!ChangeList methodsFor: 'scanning' stamp: 'jmv 10/14/2024 18:00:09'!
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


!FeatureRequirement methodsFor: 'testing' stamp: 'jmv 10/15/2024 10:02:37'!
checkRequirement
	"Answer if I am satisfied by package found at pathName"

	^ self codePackageFile provides satisfies: self.! !

