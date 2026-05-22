Class {
	#name : 'BooleanTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Kernel-Objects',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Objects'
}

{ #category : 'combination generation' }
BooleanTest >> evaluate: aBlock collectingValuesInto: values at: aPosition  [
	
	aPosition = 0 
		ifTrue: [ aBlock valueWithArguments: values ]
		ifFalse: [ 
			#(true false) do: [ :value | 
				values at: aPosition put: value. 
				self evaluate: aBlock collectingValuesInto: values at: aPosition - 1 ]]
]

{ #category : 'combination generation' }
BooleanTest >> evaluate: aBlock forCombinationOf: aNumberOfValues [

	self evaluate: aBlock collectingValuesInto: (Array new: aNumberOfValues) at: aNumberOfValues 
	
]

{ #category : 'and tests' }
BooleanTest >> testAndAndAndAndTrueTable [

	self 
		evaluate: [ :first :second :third :fourth :fifth | 
			self 
				assert: (first and: [second] and: [third] and: [fourth] and: [fifth]) 
				equals: ((((first and: [second]) and: [third]) and: [ fourth ]) and: [ fifth ]) ]
		forCombinationOf: 5
		
	
	
	
]

{ #category : 'and tests' }
BooleanTest >> testAndAndAndTrueTable [

	self 
		evaluate: [ :first :second :third :fourth | 
			self 
				assert: (first and: [second] and: [third] and: [fourth]) 
				equals: (((first and: [second]) and: [third]) and: [ fourth ]) ]
		forCombinationOf: 4
		
	
	
	
]

{ #category : 'and tests' }
BooleanTest >> testAndAndTrueTable [

	self 
		evaluate: [ :first :second :third | 
			self 
				assert: (first and: [second] and: [third]) 
				equals: ((first and: [second]) and: [third]) ]
		forCombinationOf: 3
		
	
	
	
]

{ #category : 'testing' }
BooleanTest >> testIsBoolean [
	self assert: (true is: #Boolean).
	self assert: (false is: #Boolean).
]

{ #category : 'or tests' }
BooleanTest >> testOrOrOrOrTrueTable [

	self 
		evaluate: [ :first :second :third :fourth :fifth | 
			self 
				assert: (first or: [second] or: [third] or: [fourth] or: [fifth]) 
				equals: ((((first or: [second]) or: [third]) or: [ fourth ]) or: [ fifth ]) ]
		forCombinationOf: 5
		
	
	
	
]

{ #category : 'or tests' }
BooleanTest >> testOrOrOrTrueTable [

	self 
		evaluate: [ :first :second :third :fourth | 
			self 
				assert: (first or: [second] or: [third] or: [fourth]) 
				equals: (((first or: [second]) or: [third]) or: [ fourth ]) ]
		forCombinationOf: 4
		
	
	
	
]

{ #category : 'or tests' }
BooleanTest >> testOrOrTrueTable [

	self 
		evaluate: [ :first :second :third | 
			self 
				assert: (first or: [second] or: [third]) 
				equals: ((first or: [second]) or: [third]) ]
		forCombinationOf: 3
		
	
	
	
]
