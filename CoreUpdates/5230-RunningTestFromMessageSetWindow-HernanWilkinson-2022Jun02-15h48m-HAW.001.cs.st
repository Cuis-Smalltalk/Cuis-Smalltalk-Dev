'From Cuis 6.0 [latest update: #5213] on 2 June 2022 at 3:49:24 pm'!

!SmalltalkEditor methodsFor: 'tdd' stamp: 'HAW 6/2/2022 15:48:59'!
testSuiteForCategoryOf: aClass

	| selectedSystemCategory |
	
	"If we are in a senders or implementors window, a MessageSet, the selectedSystemCategory is nil.
	In that case most of those cases the user wants to run the tests of the category of the selected method class - Hernan"
	selectedSystemCategory := self codeProvider selectedSystemCategory ifNil: [ aClass category ].
	^TestSuite forSystemCategoryNamed: selectedSystemCategory using: SystemOrganization 
! !

