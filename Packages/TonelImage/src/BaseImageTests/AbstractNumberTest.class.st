Class {
	#name : 'AbstractNumberTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Kernel-Numbers',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Numbers'
}

{ #category : 'testing' }
AbstractNumberTest class >> isAbstract [

	^self subclasses notEmpty
]

{ #category : 'helpers' }
AbstractNumberTest >> effectiveZeroExponentForTrigonometry [

	^-50
]

{ #category : 'helpers' }
AbstractNumberTest >> hyperbolicPrecision [

	^16
]

{ #category : 'helpers' }
AbstractNumberTest >> trigonometricDegreePrecision [

	^4
]

{ #category : 'helpers' }
AbstractNumberTest >> trigonometricPrecision [

	^3
]

{ #category : 'helpers' }
AbstractNumberTest >> verify: aFloat is: anInteger floatsAwayFrom: anotherFloat [

	self assert: (aFloat floatsAwayFrom: anotherFloat) = anInteger.
	self assert: (anotherFloat floatsAwayFrom: aFloat) = anInteger negated.
	self assert: (aFloat negated floatsAwayFrom: anotherFloat negated) = anInteger negated.
	self assert: (anotherFloat negated floatsAwayFrom: aFloat negated) = anInteger
]

{ #category : 'helpers' }
AbstractNumberTest >> verify: aFloat isDegreeTrigonometricallyEqualTo: anotherFloat [

	aFloat = 0.0 ifTrue: [
		anotherFloat = 0.0 ifTrue: [^self ].
		^self assert: anotherFloat asFloat exponent <= self effectiveZeroExponentForTrigonometry].
	anotherFloat = 0.0 ifTrue: [^self assert: aFloat asFloat exponent <= self effectiveZeroExponentForTrigonometry].
	self verify: aFloat asFloat isWithin: self trigonometricDegreePrecision floatsAwayFrom: anotherFloat asFloat
]

{ #category : 'helpers' }
AbstractNumberTest >> verify: aFloat isHyperbolicallyEqualTo: anotherFloat [

	aFloat = 0.0 ifTrue: [
		anotherFloat = 0.0 ifTrue: [^self ].
		^anotherFloat asFloat exponent <= self effectiveZeroExponentForTrigonometry].
	anotherFloat = 0.0 ifTrue: [^aFloat asFloat exponent <= self effectiveZeroExponentForTrigonometry].
	self verify: aFloat asFloat isWithin: self hyperbolicPrecision floatsAwayFrom: anotherFloat asFloat
]

{ #category : 'helpers' }
AbstractNumberTest >> verify: aFloat isTrigonometricallyEqualTo: anotherFloat [

	aFloat = 0.0 ifTrue: [
		anotherFloat = 0.0 ifTrue: [^self ].
		^self assert: anotherFloat asFloat exponent <= self effectiveZeroExponentForTrigonometry].
	anotherFloat = 0.0 ifTrue: [^self assert: aFloat asFloat exponent <= self effectiveZeroExponentForTrigonometry].
	self verify: aFloat asFloat isWithin: self trigonometricPrecision floatsAwayFrom: anotherFloat asFloat
]

{ #category : 'helpers' }
AbstractNumberTest >> verify: aFloat isWithin: anInteger floatsAwayFrom: anotherFloat [

	self assert: (aFloat floatsAwayFrom: anotherFloat) abs <= anInteger.
	self assert: (anotherFloat floatsAwayFrom: aFloat) abs <= anInteger.
	self assert: (aFloat negated floatsAwayFrom: anotherFloat negated) abs <= anInteger.
	self assert: (anotherFloat negated floatsAwayFrom: aFloat negated) abs <= anInteger
]

{ #category : 'helpers' }
AbstractNumberTest >> verify: aFloat isWithinOneFloatAwayFrom: anotherFloat [

	self verify: aFloat isWithin: 1 floatsAwayFrom: anotherFloat
]
