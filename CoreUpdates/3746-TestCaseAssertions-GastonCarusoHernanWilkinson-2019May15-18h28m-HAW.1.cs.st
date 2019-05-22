'From Cuis 5.0 of 7 November 2016 [latest update: #3745] on 15 May 2019 at 7:29:25 pm'!

!TestCase methodsFor: 'assertions' stamp: 'HAW 5/15/2019 19:24:37'!
assert: anAction changes: aCondition 

	| after before |
	
	before := aCondition value.
	anAction value.
	after := aCondition value.
	
	self deny: after = before! !

!TestCase methodsFor: 'assertions' stamp: 'HAW 5/15/2019 19:25:11'!
assert: anAction changes: aCondition by: aDifference 
	
	| after before |
	
	before := aCondition value.
	anAction value.
	after := aCondition value.
	
	self assert: after equals: before + aDifference! !

!TestCase methodsFor: 'assertions' stamp: 'HAW 5/15/2019 19:27:33'!
assert: anAction changes: aCondition from: anInitialObject to: aFinalObject 

	| after before |
	
	before := aCondition value.
	anAction value.
	after := aCondition value.
	
	self assert: before equals: anInitialObject.
	self assert: after equals: aFinalObject! !

!TestCase methodsFor: 'assertions' stamp: 'HAW 5/15/2019 19:27:43'!
assert: anAction doesNotChange: aCondition 

	| after before |
	
	before := aCondition value.
	anAction value.
	after := aCondition value.
	
	self assert: after equals: before! !

!TestCase methodsFor: 'assertions' stamp: 'HAW 5/15/2019 18:53:33'!
assert: aCollection includes: anObject 
	
	^ self assert: (aCollection includes: anObject) description: [ aCollection asString, ' does not include ', anObject asString ]! !

!TestCase methodsFor: 'assertions' stamp: 'HAW 5/15/2019 18:57:54'!
assert: aNumber isNearTo: anotherNumber 

	self assert: aNumber isNearTo: anotherNumber withPrecision: self defaultPrecision ! !

!TestCase methodsFor: 'assertions' stamp: 'HAW 5/15/2019 19:23:07'!
assert: aNumber isNearTo: anotherNumber withPrecision: aPrecision 

	self assert: (self is: aNumber biggerThan: anotherNumber withPrecision: aPrecision)! !

!TestCase methodsFor: 'assertions' stamp: 'HAW 5/15/2019 18:58:32'!
assert: aNumber isNotNearTo: anotherNumber
 
	self assert: aNumber isNotNearTo: anotherNumber withPrecision: self defaultPrecision ! !

!TestCase methodsFor: 'assertions' stamp: 'HAW 5/15/2019 19:23:13'!
assert: aFloatNumber isNotNearTo: anotherFloatNumber withPrecision: aPrecision 

	self deny: (self is: aFloatNumber biggerThan: anotherFloatNumber withPrecision: aPrecision)
! !

!TestCase methodsFor: 'assertions' stamp: 'HAW 5/15/2019 19:09:01'!
defaultPrecision

	^ 0.0001
	! !

!TestCase methodsFor: 'assertions' stamp: 'HAW 5/15/2019 19:23:27'!
is: aNumber biggerThan: anotherNumber withPrecision: aPrecision

	aNumber = 0 ifTrue: [ ^ anotherNumber abs < aPrecision ].
	
	^ (aNumber - anotherNumber) abs < (aPrecision * (aNumber abs max: anotherNumber abs))! !

!TestCase methodsFor: 'assertions' stamp: 'HAW 5/15/2019 19:04:32'!
should: aClosure notTakeMoreThan: aLimit 

	| millisecondsLimit |
	
	millisecondsLimit := aLimit totalMilliseconds.
	self assert: aClosure timeToRun <= millisecondsLimit description: [ 'Took more than ', millisecondsLimit printString, ' milliseconds' ]! !


!TestCase methodsFor: 'assertions' stamp: 'HAW 5/15/2019 18:58:49'!
deny: aBoolean description: aString

	self assert: aBoolean not description: aString
			! !

!methodRemoval: TestCase #should:takeLessThan:!
TestCase removeSelector: #should:takeLessThan:!
