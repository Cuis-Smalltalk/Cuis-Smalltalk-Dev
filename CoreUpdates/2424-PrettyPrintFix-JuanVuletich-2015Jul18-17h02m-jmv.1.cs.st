'From Cuis 4.2 of 25 July 2013 [latest update: #2422] on 18 July 2015 at 5:03:35.025126 pm'!

!AssignmentNode methodsFor: 'printing' stamp: 'jmv 7/18/2015 17:03'!
printOn: aStream indent: level 
	variable printOn: aStream indent: level.
	Preferences allowUnderscoreAssignments
		ifTrue: [ aStream nextPutAll: ' _ ' ]
		ifFalse: [ aStream nextPutAll: ' := ' ].
	value printOn: aStream indent: level! !

!AssignmentNode methodsFor: 'printing' stamp: 'jmv 7/18/2015 17:03'!
printWithClosureAnalysisOn: aStream indent: level 
	variable printWithClosureAnalysisOn: aStream indent: level.
	Preferences allowUnderscoreAssignments
		ifTrue: [ aStream nextPutAll: ' _ ' ]
		ifFalse: [ aStream nextPutAll: ' := ' ].
	value printWithClosureAnalysisOn: aStream indent: level! !

