'From Cuis 6.0 [latest update: #5060] on 4 February 2022 at 6:11:13 pm'!

!TestSuite class methodsFor: 'instance creation' stamp: 'HAW 2/4/2022 16:59:36'!
forSystemCategoryNamed: aCategoryName  using: aSystemOrganizer

	| testCaseClasses |

	testCaseClasses _ aSystemOrganizer testCaseClassesAt: aCategoryName.

	^ testCaseClasses isEmpty 
		ifTrue: [ self forClasses: (aSystemOrganizer allClassesAt: aCategoryName) named: aCategoryName, ' infered tests' ]
		ifFalse: [ self forTestCaseClasses: testCaseClasses named: aCategoryName, ' tests' ]
		
! !

!TestSuite class methodsFor: 'instance creation - private' stamp: 'HAW 2/4/2022 18:06:52'!
forClasses: classes named: aName

	| testMethods suite classTests tests testPrintString |
	
	"I don't want repeated tests. TestCase does not redefine #= so instead of redefining it and use a Set
	I decided to keep the related tests methods in a different set and decide to add it or note base on that
	- Hernan"
	
	testMethods := Set new.
	tests := OrderedCollection new.
	
	classes do: [ :aClass |
		classTests := (self forClass: aClass) tests.
		classTests do: [ :aTest | 
				testPrintString := aTest printString.
				(testMethods includes: testPrintString) ifFalse: [ 
					testMethods add: testPrintString.
					tests add: aTest ]]].
		
	suite := self named: aName.
	suite addTests: tests.
	
	^suite
		
! !

