'From Cuis7.1 [latest update: #6740] on 23 September 2024 at 12:22:26 pm'!

!Parser methodsFor: 'error correction' stamp: 'jmv 9/23/2024 12:15:14'!
rangesForRemovableUnusedTempsInBlockNode: aNodeWithTemporaries
	
	| definedButUnusedTemps unusedTemps removableTemps |
	
	definedButUnusedTemps := aNodeWithTemporaries temporaries select: [ :temporaryNode |
		temporaryNode isUnusedTemp & temporaryNode isUndefTemp not ].
	definedButUnusedTemps notEmpty ifTrue: [
		definedButUnusedTemps := definedButUnusedTemps collect: [ :tempNode | tempNode name ].
		'There are defined but unused temporal variables.' print.
		('Consider removing them: ', definedButUnusedTemps asArray printString) print ].
	
	unusedTemps := aNodeWithTemporaries temporaries select: [ :temporaryNode |
		temporaryNode isUnusedTemp & temporaryNode isUndefTemp ].

	removableTemps := unusedTemps select: [ :temporaryNode |
		UnusedVariable name: temporaryNode name ].
	
	^self declarationRangesForTemps: removableTemps.! !

!methodRemoval: Parser #selectRemovableUnusedTempsFrom: stamp: 'jmv 9/23/2024 12:22:23'!
Parser removeSelector: #selectRemovableUnusedTempsFrom:!
!methodRemoval: Parser #unusedTempsOf: stamp: 'jmv 9/23/2024 12:22:23'!
Parser removeSelector: #unusedTempsOf:!
