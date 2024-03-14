'From Cuis6.3 [latest update: #6274] on 14 March 2024 at 11:59:22 am'!

!AssignmentNode methodsFor: 'printing' stamp: 'jmv 3/14/2024 11:54:17'!
printOn: aStream indent: level 
	variable printOn: aStream indent: level.
	aStream nextPutAll: ' := '.
	value printOn: aStream indent: level! !

!AssignmentNode methodsFor: 'printing' stamp: 'jmv 3/14/2024 11:54:06'!
printWithClosureAnalysisOn: aStream indent: level 
	variable printWithClosureAnalysisOn: aStream indent: level.
	aStream nextPutAll: ' := '.
	value printWithClosureAnalysisOn: aStream indent: level! !

