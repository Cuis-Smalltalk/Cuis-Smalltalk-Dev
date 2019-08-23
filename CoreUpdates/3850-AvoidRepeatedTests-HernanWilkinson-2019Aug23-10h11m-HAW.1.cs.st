'From Cuis 5.0 [latest update: #3846] on 23 August 2019 at 3:54:52 pm'!

!TestSuite class methodsFor: 'instance creation - private' stamp: 'HAW 8/23/2019 15:53:43'!
forClasses: classes named: aName

	| testMethods suite classTests tests testMethod |
	
	"I don't want repeated tests. TestCase does not redefine #= so instead of redefining it and use a Set
	I decided to keep the related tests methods in a different set and decide to add it or note base on that
	- Hernan"
	
	testMethods := IdentitySet new.
	tests := OrderedCollection new.
	
	classes do: [ :aClass |
		classTests := (self forClass: aClass) tests.
		classTests do: [ :aTest |
				testMethod := aTest methodForTest.
				(testMethods includes: testMethod) ifFalse: [ 
					testMethods add: testMethod.
					tests add: aTest ]]].
		
	suite := self named: aName.
	suite addTests: tests.
	
	^suite
		
! !

