'From Cuis7.1 [latest update: #6760] on 4 October 2024 at 2:12:18 pm'!

!Class methodsFor: 'subclass creation' stamp: 'jmv 10/4/2024 13:52:21'!
subclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s
	"Some chunk files may not include class category in the class declarations."
	^self
		subclass: t
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: SystemOrganizer defaultClassCategory.! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 10/4/2024 13:53:50'!
methods
	"Answer a ClassCategoryReader for compiling messages that are not classified.
	Some chunk files may not include class category in the class declarations."

	^ ClassCategoryReader new setClass: self category: ClassOrganizer defaultMethodCategory.! !


!Class methodsFor: 'subclass creation' stamp: 'jmv 10/4/2024 13:50:25'!
subclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver)."

	| answer |
	answer := ClassBuilder new
		superclass: self
		subclass: t
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat.

	Smalltalk
		logChange: answer definition
		preamble: answer definitionPreamble.
	^answer 
	
! !

