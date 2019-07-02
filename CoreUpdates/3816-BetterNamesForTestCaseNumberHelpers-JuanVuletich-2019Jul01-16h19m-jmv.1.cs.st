'From Cuis 5.0 [latest update: #3815] on 1 July 2019 at 4:37:34 pm'!

!TestCase methodsFor: 'assertions' stamp: 'jmv 7/1/2019 16:22:32'!
assert: aNumber isCloseTo: anotherNumber 

	self assert: aNumber isCloseTo: anotherNumber withPrecision: self defaultPrecision ! !

!TestCase methodsFor: 'assertions' stamp: 'jmv 7/1/2019 16:22:01'!
assert: aNumber isCloseTo: anotherNumber withPrecision: aPrecision 

	self assert: (self is: aNumber closeTo: anotherNumber withPrecision: aPrecision)! !

!TestCase methodsFor: 'assertions' stamp: 'jmv 7/1/2019 16:23:17'!
assert: aNumber isNotCloseTo: anotherNumber
 
	self assert: aNumber isNotCloseTo: anotherNumber withPrecision: self defaultPrecision ! !

!TestCase methodsFor: 'assertions' stamp: 'jmv 7/1/2019 16:23:12'!
assert: aFloatNumber isNotCloseTo: anotherFloatNumber withPrecision: aPrecision 

	self deny: (self is: aFloatNumber closeTo: anotherFloatNumber withPrecision: aPrecision)
! !

!TestCase methodsFor: 'assertions' stamp: 'jmv 7/1/2019 16:37:25'!
is: aNumber closeTo: anotherNumber withPrecision: aPrecision
	"This way of comparing numbers could be useful for many tests, but there is no single correct way to do it for numerical algorithms. That's why this method is here and not at Float."

	aNumber = 0 ifTrue: [ ^ anotherNumber abs < aPrecision ].
	
	^ (aNumber - anotherNumber) abs < (aPrecision * (aNumber abs max: anotherNumber abs))! !

!methodRemoval: TestCase #assert:isNearTo:!
TestCase removeSelector: #assert:isNearTo:!
!methodRemoval: TestCase #assert:isNearTo:withPrecision:!
TestCase removeSelector: #assert:isNearTo:withPrecision:!
!methodRemoval: TestCase #assert:isNotNearTo:!
TestCase removeSelector: #assert:isNotNearTo:!
!methodRemoval: TestCase #assert:isNotNearTo:withPrecision:!
TestCase removeSelector: #assert:isNotNearTo:withPrecision:!
!methodRemoval: TestCase #is:biggerThan:withPrecision:!
TestCase removeSelector: #is:biggerThan:withPrecision:!
