'From Cuis7.1 [latest update: #6479] on 18 June 2024 at 2:27:00 pm'!

!Browser methodsFor: 'self-updating' stamp: 'jmv 6/18/2024 14:14:18'!
classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName

	| currentSelectedClass |
	
	selectedSystemCategory = aCategoryName ifTrue: [
		currentSelectedClass := self selectedClass.
		currentSelectedClass ifNil: [ 
			selectedClassName = oldClassName ifTrue: [ 
				currentSelectedClass := aClass ]].
	 	self changed: #classList.
		self selectClass: currentSelectedClass ].! !


!BrowserWindow methodsFor: 'updating' stamp: 'jmv 6/18/2024 14:13:16'!
classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName

	self canDiscardEdits ifTrue: [
		self model classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName ].! !

!methodRemoval: Browser #originalSelectedClassName stamp: 'jmv 6/18/2024 14:26:33'!
Browser removeSelector: #originalSelectedClassName!
