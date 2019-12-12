'From Cuis 5.0 [latest update: #3959] on 12 December 2019 at 2:15:11 am'!

!Parser methodsFor: 'error correction' stamp: 'EB 12/12/2019 02:12:58'!
removeUnusedTemp: aTempName
	
	| positionsForVariable end start | 
	
	positionsForVariable := (encoder positionsForTemporaryVariable: aTempName ifAbsent: []) first.
	start := positionsForVariable first.
	end := positionsForVariable last.
	requestor correctFrom: start to: end with: ''.! !


!Parser methodsFor: 'error correction' stamp: 'EB 12/12/2019 02:10:49'!
removeUnusedTemps
	"Scan for unused temp names, and prompt the user about the prospect of removing each one found"

	| str madeChanges | 
	madeChanges := false.
	str := requestor text asString.
	((tempsMark between: 1 and: str size)
		and: [(str at: tempsMark) = $|]) ifFalse: [^ self].
	encoder unusedTempNames do:
		[:temp | (UnusedVariable name: temp) ifTrue:
			[(encoder lookupVariable: temp ifAbsent: []) isUndefTemp
				ifTrue: [ 
					self removeUnusedTemp: temp.
					madeChanges := true. ]
				ifFalse:
					[self inform:
'You''ll first have to remove the\statement where it''s stored into' withNewLines]]].
	madeChanges ifTrue: [ReparseAfterSourceEditing signal]! !

