'From Cuis7.1 [latest update: #6770] on 25 November 2024 at 2:14:34 pm'!
Smalltalk renameClassNamed: #AddAsSubclassResponsabilityApplier as: #AddAsSubclassResponsibilityApplier!

!Browser methodsFor: 'refactorings' stamp: 'RMV 11/25/2024 14:11:05'!
addAsSubclassResponsibilitySelector

	self selectedMessageName ifNotNil: [ :selectedSelector |
		(AddAsSubclassResponsibilityApplier
			on: self for: self selectedClassOrMetaClass>>selectedSelector)
				value ].! !

!methodRemoval: Browser #addAsSubclassResponsabilitySelector stamp: 'RMV 11/25/2024 14:12:14'!
Browser removeSelector: #addAsSubclassResponsabilitySelector!

