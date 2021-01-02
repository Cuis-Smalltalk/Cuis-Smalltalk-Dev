'From Cuis 5.0 [latest update: #4384] on 30 December 2020 at 7:34:30 pm'!

!TestCase methodsFor: 'assertions' stamp: 'HAW 12/30/2020 19:33:44'!
assert: expected equals: actual
	^ self
		assert: expected = actual
		description: [ self comparingStringBetween: expected and: actual ]
! !

