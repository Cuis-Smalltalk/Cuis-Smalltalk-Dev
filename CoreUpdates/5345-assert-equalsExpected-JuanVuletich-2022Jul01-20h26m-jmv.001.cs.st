'From Cuis 6.0 [latest update: #5344] on 1 July 2022 at 8:26:48 pm'!

!TestCase methodsFor: 'assertions' stamp: 'jmv 7/1/2022 19:55:24'!
assert: actual equalsExpected: expected
	^ self
		assert: actual = expected
		description: [ self comparingStringBetween: expected and: actual ]
! !

