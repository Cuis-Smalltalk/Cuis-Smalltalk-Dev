'From Cuis7.3 [latest update: #6910] on 12 December 2024 at 8:58:48 am'!

!Class methodsFor: 'subclass creation' stamp: 'eem 8/25/2015 16:37'!
ephemeronSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat 
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver) in which the subclass is to 
	have ephemeron semantics, i.e. where the object will be queued for
	finalization when the key (first) inst var is not reachable other than through
	the other fields of ephemerons with unreachable keys."

	| answer |
	answer := ClassBuilder new
			superclass: self
			ephemeronSubclass: t
			instanceVariableNames: f
			classVariableNames: d
			poolDictionaries: s
			category: cat.
		
	Smalltalk
		logChange: answer definition 
		preamble: answer definitionPreamble.
	^answer! !

