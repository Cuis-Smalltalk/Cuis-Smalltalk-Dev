'From Cuis 6.0 [latest update: #5492] on 24 September 2022 at 9:59:53 am'!

!Class methodsFor: 'subclass creation' stamp: 'HAW 9/24/2022 09:59:03'!
subclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat 
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver)."
	
	| answer |
	answer _ ClassBuilder new
		superclass: self
		subclass: t
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat.
		
	answer ifNotNil: [ Smalltalk
		logChange: answer definition 
		preamble: answer definitionPreamble ].
	
	^answer 
	
! !

