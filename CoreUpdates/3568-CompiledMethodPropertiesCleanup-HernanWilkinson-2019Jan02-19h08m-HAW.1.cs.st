'From Cuis 5.0 of 7 November 2016 [latest update: #3562] on 7 January 2019 at 4:06:33 pm'!

!CompiledMethod methodsFor: 'accessing-pragmas & properties' stamp: 'HAW 1/3/2019 07:04:14'!
removeProperties
	
	self penultimateLiteral: self selector! !

!CompiledMethod methodsFor: 'accessing-pragmas & properties' stamp: 'HAW 1/3/2019 07:06:51'!
withPropertiesDo: withBlock

	^self withPropertiesDo: withBlock ifSelector: [ :selector | nil ]! !

!CompiledMethod methodsFor: 'accessing-pragmas & properties' stamp: 'HAW 1/3/2019 07:03:13'!
withPropertiesDo: withBlock ifSelector: notBlock

	| penultimalLiteral |
	
	penultimalLiteral := self penultimateLiteral.
	
	^penultimalLiteral isMethodProperties 
		ifTrue: [ withBlock value: penultimalLiteral ]
		ifFalse: [ notBlock value: penultimalLiteral ]! !


!CompiledMethod methodsFor: 'accessing' stamp: 'HAW 1/3/2019 07:03:14'!
properties

	"Answer the method properties of the receiver."
	
	^self 
		withPropertiesDo: [ :properties | properties ] 
		ifSelector: [ :selector | AdditionalMethodState forMethod: self selector: selector ]! !

!CompiledMethod methodsFor: 'accessing' stamp: 'HAW 1/7/2019 15:49:22'!
selector
	"Answer a method's selector.  This is either the penultimate literal,
	 or, if the method has any properties or pragmas, the selector of
	 the MethodProperties stored in the penultimate literal."
	
	^self 
		withPropertiesDo: [ :properties | properties selector ] 
		ifSelector: [ :selector | selector ]
! !

!CompiledMethod methodsFor: 'accessing' stamp: 'HAW 1/7/2019 15:56:36'!
selector: aSelector
	"Set a method's selector.  This is either the penultimate literal,
	 or, if the method has any properties or pragmas, the selector of
	 the MethodProperties stored in the penultimate literal."
	
	| numberOfLiterals | 
	
	self 
		withPropertiesDo: [ :properties | properties selector: aSelector ] 
		ifSelector: [ :selector |
			(numberOfLiterals := self numLiterals) < 2 ifTrue: [self error: 'insufficient literals to hold selector'].
			self literalAt: numberOfLiterals - 1 put: aSelector]! !

!CompiledMethod methodsFor: 'literals' stamp: 'HAW 1/3/2019 07:06:51'!
hasLiteralSuchThat: litBlock
	"Answer true if litBlock returns true for any literal in this method, even if embedded in array structure."
	
	| lit |
	
	self withPropertiesDo: [ :properties | (properties hasLiteralSuchThat: litBlock) ifTrue: [ ^true ]]. 

	2 to: self numLiterals + 1 do: [ :index |
		lit := self objectAt: index.
		((litBlock value: lit)
		or: [lit isArray and: [lit hasLiteralSuchThat: litBlock]]) ifTrue:
			[^true]].
	^false! !

!CompiledMethod methodsFor: 'literals' stamp: 'HAW 1/3/2019 07:06:51'!
hasLiteralThorough: literal
	"Answer true if any literal in this method is literal,
	even if embedded in array structure."

	| lit |
	
	self withPropertiesDo: [ :properties | (properties hasLiteralThorough: literal) ifTrue:[^true]].
	
	2 to: self numLiterals - 1 "exclude superclass + selector/properties"
	   do: [ :index |
		(((lit := self objectAt: index) literalEqual: literal)
		 or: [(lit isVariableBinding and: [lit key == literal])
		 or: [lit isArray and: [lit hasLiteral: literal]]]) ifTrue:
			[^ true]].
	^ false ! !

!CompiledMethod methodsFor: 'accessing-pragmas & properties' stamp: 'HAW 1/3/2019 07:03:14'!
pragmaAt: aKey

	"Answer the pragma with selector aKey, or nil if none."

	^self withPropertiesDo: [ :properties | properties at: aKey ifAbsent: nil ] ifSelector: [ :selector | nil ].
	! !

!CompiledMethod methodsFor: 'accessing-pragmas & properties' stamp: 'HAW 1/3/2019 07:03:14'!
pragmas
	
	^self withPropertiesDo: [ :properties | properties pragmas ] ifSelector: [ :selector | #() ]! !

!CompiledMethod methodsFor: 'accessing-pragmas & properties' stamp: 'HAW 1/3/2019 07:18:59'!
propertyKeysAndValuesDo: aBlock

	"Enumerate the receiver with all the keys and values."

	self withPropertiesDo: [ :properties | properties propertyKeysAndValuesDo: aBlock]! !

!CompiledMethod methodsFor: 'accessing-pragmas & properties' stamp: 'HAW 1/3/2019 16:57:04'!
propertyValueAt: propName

	^self withPropertiesDo: [ :properties | properties propertyValueAt: propName ifAbsent: nil] ! !

!CompiledMethod methodsFor: 'accessing-pragmas & properties' stamp: 'HAW 1/3/2019 17:01:07'!
propertyValueAt: propName ifAbsent: aBlock
	
	^self withPropertiesDo: [ :properties | properties propertyValueAt: propName ifAbsent: aBlock ] ifSelector: [ :selector | aBlock value ]
	! !

!CompiledMethod methodsFor: 'accessing-pragmas & properties' stamp: 'HAW 1/7/2019 15:45:43'!
propertyValueAt: propName put: propValue
	"Set or add the property with key propName and value propValue.
	 If the receiver does not yet have a method properties create one and replace
	 the selector with it.  Otherwise, either relace propValue in the method properties
	 or replace method properties with one containing the new property."
	
	self 
		withPropertiesDo: [:properties | 
			(properties includesProperty: propName) ifTrue: [^properties at: propName put: propValue].
			self penultimateLiteral: (properties
								copyWith: (Association
												key: propName asSymbol
												value: propValue)).
			^propValue ] 
		ifSelector: [ :selector |
			self penultimateLiteral: ((AdditionalMethodState
									selector: selector
									with: (Association
											key: propName asSymbol
											value: propValue))
									setMethod: self;
									yourself).
			^propValue].! !
