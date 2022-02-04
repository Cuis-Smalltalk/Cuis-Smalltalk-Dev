'From Cuis 6.0 [latest update: #5061] on 4 February 2022 at 6:35:06 pm'!

!SmalltalkEditor methodsFor: 'tdd' stamp: 'HAW 2/4/2022 18:33:36'!
testSuiteForCategoryOf: aClass

	^TestSuite forSystemCategoryNamed: self codeProvider selectedSystemCategory using: SystemOrganization 
! !

