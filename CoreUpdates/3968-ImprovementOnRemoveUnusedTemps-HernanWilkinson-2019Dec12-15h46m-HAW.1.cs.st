'From Cuis 5.0 [latest update: #3965] on 12 December 2019 at 4:27:54 pm'!

!Parser methodsFor: 'error correction' stamp: 'HAW 12/12/2019 16:13:57'!
tryToRemoveUnusedTemp: aTemp
	
	(encoder lookupVariable: aTemp ifAbsent: []) isUndefTemp
		ifTrue: [ 
			self removeUnusedTemp: aTemp.
			^true. ]
		ifFalse: [
			self inform: 'You''ll first have to remove the\statement where it''s stored into' withNewLines.
			^false]! !


!Parser methodsFor: 'error correction' stamp: 'HAW 12/12/2019 16:13:50'!
removeUnusedTemp: aTemp
	
	| positionsForVariable end start | 
	
	positionsForVariable := (encoder positionsForTemporaryVariable: aTemp ifAbsent: []) first.
	start := positionsForVariable first.
	end := positionsForVariable last.
	
	requestor correctFrom: start to: end with: ''.! !

!Parser methodsFor: 'error correction' stamp: 'HAW 12/12/2019 16:19:26'!
removeUnusedTemps
	"Scan for unused temp names, and prompt the user about the prospect of removing each one found"

	| index | 

	index := encoder unusedTempNames findFirst: [:temp | 
		(UnusedVariable name: temp)
			ifTrue: [ self tryToRemoveUnusedTemp: temp ]
			ifFalse: [ false ]].
	
	index ~=0 ifTrue: [ReparseAfterSourceEditing signal]! !

