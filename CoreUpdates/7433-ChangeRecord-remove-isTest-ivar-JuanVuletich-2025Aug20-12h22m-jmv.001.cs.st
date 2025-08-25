'From Cuis7.5 [latest update: #7428] on 20 August 2025 at 12:28:06 pm'!
!classDefinition: #ChangeRecord category: #'Tools-Changes'!
ChangeListElement subclass: #ChangeRecord
	instanceVariableNames: 'file position type class category meta stamp prior isTest overrides '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!

!ChangeList methodsFor: 'scanning' stamp: 'jmv 8/20/2025 12:27:11'!
classDefinitionRecordFrom: tokens
	
	| classDefinition isMeta itemPosition className record fullClassName category stamp |
	
	itemPosition := file position.
	fullClassName := tokens second.
	isMeta := fullClassName includesSubString: ' class'.
	className := isMeta ifTrue: [fullClassName substrings first] ifFalse: [fullClassName].
	category := CompiledMethod classCategoryFrom: tokens.
	stamp := CompiledMethod stampFrom: tokens.
	classDefinition := file nextChunk.
	
	record := ChangeRecord new 
		file: file 
		position: itemPosition 
		type: #classDefinition 
		class: className asSymbol 
		category: category 
		meta: isMeta 
		stamp: stamp.
	
	^record
! !


!ChangeRecord methodsFor: 'initialization' stamp: 'jmv 8/20/2025 12:27:18'!
file: aFile position: aPosition type: aType

	file := aFile.
	position := aPosition.
	type := aType.
"
file closed ifFalse: [
	'' print.
	file print.
	self print.
	thisContext printStack: 10 ]
"! !

!methodRemoval: ChangeRecord #isTestClassChange stamp: 'jmv 8/20/2025 12:27:02'!
ChangeRecord removeSelector: #isTestClassChange!
!methodRemoval: ChangeRecord #markAsTest: stamp: 'jmv 8/20/2025 12:27:27'!
ChangeRecord removeSelector: #markAsTest:!
!classDefinition: #ChangeRecord category: #'Tools-Changes'!
ChangeListElement subclass: #ChangeRecord
	instanceVariableNames: 'file position type class category meta stamp prior overrides'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!
