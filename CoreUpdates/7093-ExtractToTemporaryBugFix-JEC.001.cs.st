'From Cuis7.3 [latest update: #7071] on 18 March 2025 at 9:25:43 am'!

!ExtractToTemporary methodsFor: 'private - applying steps' stamp: 'JEC 3/18/2025 09:20:04'!
temporariesDeclarationInterval

	^ (completeSourceRanges 
		at: parseNodeWithNewVariableScope temporariesDeclaration
		ifAbsent: [self shouldNotHappen]) first! !

!ExtractToTemporary methodsFor: 'private - applying steps' stamp: 'JEC 3/18/2025 09:23:31'!
updatedSourceCodeHasSpaceAt: lastSeparatorIndex

	^ (updatedSourceCode at: lastSeparatorIndex ifAbsent: [ self shouldNotHappen ]) = Character space! !


!ExtractToTemporary methodsFor: 'private - applying steps' stamp: 'JEC 3/18/2025 09:23:37'!
positionToInsertNewVariableDeclaration

	| lastSeparatorIndex |
	lastSeparatorIndex := self temporariesDeclarationInterval last.
	
	^ (self updatedSourceCodeHasSpaceAt: lastSeparatorIndex - 1)
		ifTrue: [ lastSeparatorIndex - 1 ]
		ifFalse: [ lastSeparatorIndex ]! !

