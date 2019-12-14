'From Cuis 5.0 [latest update: #3968] on 12 December 2019 at 5:46:19 pm'!

!Parser methodsFor: 'error correction' stamp: 'HAW 12/12/2019 17:34:00'!
filterSeparatorsToTheLeftOn: currentSource startingAt: anInitialPosition

	| start |
	
	start := anInitialPosition. 
	[ (currentSource at: start-1) isSeparator ] whileTrue: [ start := start - 1 ].
	
	^start 
	 	! !

!Parser methodsFor: 'error correction' stamp: 'HAW 12/12/2019 17:34:04'!
filterSeparatorsToTheRightOn: currentSource startingAt: anInitialPosition

	| end |
	
	end := anInitialPosition.	
	[ (currentSource at: end+1) isSeparator ] whileTrue: [ end := end + 1 ].
	
	^end
! !

!Parser methodsFor: 'error correction' stamp: 'HAW 12/12/2019 16:49:15'!
ifHasToRemove: aTemp addTo: tempsToRemove
	
	(encoder lookupVariable: aTemp ifAbsent: []) isUndefTemp
		ifTrue: [ tempsToRemove add: ((encoder positionsForTemporaryVariable: aTemp ifAbsent: []) first)]
		ifFalse: [ self inform: 'You''ll first have to remove the\statement where it''s stored into' withNewLines]! !

!Parser methodsFor: 'error correction' stamp: 'HAW 12/12/2019 17:44:22'!
newRangeRemovingTempsDeclarationFrom: currentSource 
	startingAt: start
	to: originalEnd
	
	| end |
	
	end := self filterSeparatorsToTheRightOn: currentSource startingAt: originalEnd.
	
	(currentSource at: end+1) = $| ifTrue: [ | possibleNewStart |
		possibleNewStart := (self filterSeparatorsToTheLeftOn: currentSource startingAt: start) - 1.
		(currentSource at: possibleNewStart) = $| ifTrue: [ ^Array with: possibleNewStart with: end + 1 ]]. 
	
	^Array with: start with: end
! !

!Parser methodsFor: 'error correction' stamp: 'HAW 12/12/2019 17:42:34'!
removeUnusedTempRange: aRangeToRemove with: delta

	| currentSource start end newRangeToRemove | 

	currentSource := requestor text. 
	newRangeToRemove := self 
		newRangeRemovingTempsDeclarationFrom: currentSource 
		startingAt: aRangeToRemove first - delta 
		to: aRangeToRemove last - delta.
	start := newRangeToRemove first.
	end := newRangeToRemove last.
		
	requestor correctFrom: start to: end with: ''.
	
	^delta + end - start + 1
! !

!Parser methodsFor: 'error correction' stamp: 'HAW 12/12/2019 17:23:59'!
removeUnusedTempsRanges: tempsToRemove
	
	tempsToRemove inject: 0 into: [ :delta :aRangeToRemove |
		self removeUnusedTempRange: aRangeToRemove with: delta ]! !


!Parser methodsFor: 'error correction' stamp: 'HAW 12/12/2019 17:20:50'!
removeUnusedTemps
	"Scan for unused temp names, and prompt the user about the prospect of removing each one found"

	| tempsToRemove | 

	tempsToRemove := SortedCollection sortBlock: [ :leftRange :rightRange | leftRange first < rightRange first ].

	encoder unusedTempNames do: [:temp | 
		(UnusedVariable name: temp) ifTrue: [ self ifHasToRemove: temp addTo: tempsToRemove ]].
	
	self removeUnusedTempsRanges: tempsToRemove
! !

!methodRemoval: Parser #tryToRemoveUnusedTemp: stamp: 'HAW 12/12/2019 17:35:23'!
Parser removeSelector: #tryToRemoveUnusedTemp:!
!methodRemoval: Parser #removeUnusedTemp: stamp: 'HAW 12/12/2019 17:35:28'!
Parser removeSelector: #removeUnusedTemp:!
