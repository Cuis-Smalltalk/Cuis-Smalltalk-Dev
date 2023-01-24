'From Cuis 6.0 [latest update: #5650] on 23 January 2023 at 6:15:36 pm'!

!TestCase methodsFor: 'assertions' stamp: 'HAW 1/23/2023 18:14:52'!
assert: expected equals: actual
	self
		assert: expected = actual
		description: [ self comparingStringBetween: expected and: actual ]
! !

!TestCase methodsFor: 'assertions' stamp: 'HAW 1/23/2023 18:14:57'!
assert: actual equalsExpected: expected
	self
		assert: actual = expected
		description: [ self comparingStringBetween: expected and: actual ]
! !

!TestCase methodsFor: 'assertions' stamp: 'HAW 1/23/2023 18:15:02'!
assert: aCollection includes: anObject 
	
	self assert: (aCollection includes: anObject) description: [ aCollection asString, ' does not include ', anObject asString ]! !

