Class {
	#name : 'CompiledMethodTest',
	#superclass : 'TestCase',
	#instVars : [
		'iv1',
		'iv2'
	],
	#category : 'BaseImageTests-Kernel-Methods',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Methods'
}

{ #category : 'tests - accessors' }
CompiledMethodTest >> indexOfIv1 [
	
	^self class indexOfInstanceVariable: 'iv1'
]

{ #category : 'tests - accessors' }
CompiledMethodTest >> indexOfIv2 [
	
	^self class indexOfInstanceVariable: 'iv2'
]

{ #category : 'tests - accessors' }
CompiledMethodTest >> iv1 [

	^iv1
]

{ #category : 'tests - accessors' }
CompiledMethodTest >> iv1: anObject [

	iv1 := anObject 
]

{ #category : 'tests - accessors' }
CompiledMethodTest >> iv1_notGetter [

	^iv1
]

{ #category : 'tests - accessors' }
CompiledMethodTest >> iv1_notSetter: anObject [

	iv1 := anObject 
]

{ #category : 'tests - accessors' }
CompiledMethodTest >> iv1ReadWrite [

	iv1 := iv1 yourself
]

{ #category : 'tests - accessors' }
CompiledMethodTest >> iv2 [

	^iv2 yourself
]

{ #category : 'tests - accessors' }
CompiledMethodTest >> iv2: anObject [

	iv2 := 1
]

{ #category : 'tests - accessors' }
CompiledMethodTest >> test01isGetterWhenSameSelectorAsInstanceVariableAndOnlyReturnsThatVariable [

	self assert: (self class>>#iv1 isGetterOf: 'iv1' at: self indexOfIv1)
]

{ #category : 'tests - properties' }
CompiledMethodTest >> test01withPropertiesDoIfSelectorEvaluatesFirstBlockWhenMethodHasProperties [

	| thisMethod propertyName methodProperties |
	
	propertyName := #testProperty.
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: propertyName put: true.

	thisMethod 
		withPropertiesDo: [ :properties | methodProperties := properties ]
		ifSelector: [ self fail ].
	
	"I don't assert inside the block because if no block is evaluated the test passes - Hernan"	
	self assert: (methodProperties at: propertyName)
]

{ #category : 'tests - accessors' }
CompiledMethodTest >> test02isNotGetterWhenDifferentSelectorAsInstanceVariable [

	self deny: (self class>>#iv1_notGetter isGetterOf: 'iv1' at: self indexOfIv1)
]

{ #category : 'tests - properties' }
CompiledMethodTest >> test02removePropertiesRestoresPenultimalLiteralToSelector [

	| thisMethod propertyName |
	
	propertyName := #testProperty.
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: propertyName put: true.
	thisMethod removeProperties.

	self assert: thisMethod penultimateLiteral equals: thisContext selector.
	
]

{ #category : 'tests - accessors' }
CompiledMethodTest >> test03isNotGetterWhenSameSelectorAsInstanceVariableButDoesOtherThingBesidesReturningVariable [

	self deny: (self class>>#iv2 isGetterOf: 'iv2' at: self indexOfIv2)
]

{ #category : 'tests - properties' }
CompiledMethodTest >> test03removePropertiesKeepsSelectorIfMethodDoesNotHaveProperties [

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	"I have to remove it twice because when running with live typing methods always have method properties - Hernan"
	thisMethod removeProperties.
	thisMethod removeProperties.

	self assert: thisMethod penultimateLiteral equals: thisContext selector.
	
]

{ #category : 'tests - accessors' }
CompiledMethodTest >> test04isSetterWhenSameKeywordSelectorAsInstanceVariableAndOnlySetsTheVariable [

	self assert: (self class>>#iv1: isSetterOf: 'iv1' at: self indexOfIv1)
]

{ #category : 'tests - properties' }
CompiledMethodTest >> test04withPropertiesDoIfSelectorEvaluatesSecondBlockWhenMethodHasNoProperties [

	| thisMethod methodSelector |
	
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.
	
	thisMethod 
		withPropertiesDo: [ :properties | self fail ]
		ifSelector: [ :selector | methodSelector := selector ].
		
	"I don't assert inside the block because if no block is evaluated the test passes - Hernan"	
	self assert: methodSelector equals: thisContext selector
]

{ #category : 'tests - properties' }
CompiledMethodTest >> test05hasLiteralSuchThatTravelsListeralsWhenMethodHasProperties [

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: #testProperty put: true.
	
	self assert: (thisMethod hasLiteralSuchThat: [ :aLiteral | aLiteral = #class ]).
	
	
]

{ #category : 'tests - accessors' }
CompiledMethodTest >> test05isNotSetterWhenDifferentKeywordSelectorAsInstanceVariable [

	self deny: (self class>>#iv1_notSetter: isSetterOf: 'iv1' at: self indexOfIv1)
]

{ #category : 'tests - properties' }
CompiledMethodTest >> test06hasLiteralSuchThatTravelsListeralsWhenMethodHasNoProperties [

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.
	
	self assert: (thisMethod hasLiteralSuchThat: [ :aLiteral | aLiteral = #class ]).
	
	
]

{ #category : 'tests - accessors' }
CompiledMethodTest >> test06isNotSetterWhenSameKeywordSelectorAsInstanceVariableButDoesNotAssignParameter [

	self deny: (self class>>#iv2: isSetterOf: 'iv2' at: self indexOfIv2)
]

{ #category : 'tests - properties' }
CompiledMethodTest >> test0701withPropertiesDoReturnsNilIfMethodHasNoProperties [

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.

	self assert: (thisMethod withPropertiesDo: [ :properties | self fail ]) isNil
]

{ #category : 'tests - accessors' }
CompiledMethodTest >> test07AccessorDescriptionIsGetterForAGetter [

	self assert: 'getter' equals: (self class>>#iv1 accessorDescriptionOf: 'iv1' at: self indexOfIv1)
]

{ #category : 'tests - properties' }
CompiledMethodTest >> test07withPropertiesDoEvaluatesBlockOnlyWhenMethodHasProperties [

	| thisMethod propertyName methodProperties |
	
	propertyName := #testProperty.
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: propertyName put: true.

	thisMethod withPropertiesDo: [ :properties | methodProperties := properties ].
	
	"I don't assert inside the block because if no block is evaluated the test passes - Hernan"	
	self assert: (methodProperties at: propertyName)
]

{ #category : 'tests - accessors' }
CompiledMethodTest >> test08AccessorDescriptionIsSetterForASetter [

	self assert: 'setter' equals: (self class>>#iv1: accessorDescriptionOf: 'iv1' at: self indexOfIv1)
]

{ #category : 'tests - properties' }
CompiledMethodTest >> test08hasLiteralReturnTrueWhenMethodHasPropertiesAndHasLiteral [

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: #testProperty put: true.
	
	self assert: (thisMethod hasLiteral: #class).
	
	
]

{ #category : 'tests - accessors' }
CompiledMethodTest >> test09AccessorDescriptionIsReadForMethodThatReadsInstanceVariable [

	self assert: 'read' equals: (self class>>#iv1_notGetter accessorDescriptionOf: 'iv1' at: self indexOfIv1)
]

{ #category : 'tests - properties' }
CompiledMethodTest >> test09hasLiteralReturnFalseWhenMethodHasPropertiesAndHasNoLiteral [

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: #testProperty put: true.
	
	self deny: (thisMethod hasLiteral: 'abc' asSymbol).
	
	
]

{ #category : 'tests - accessors' }
CompiledMethodTest >> test10AccessorDescriptionIsWriteForMethodThatWritesInstanceVariable [

	self assert: 'write' equals: (self class>>#iv1_notSetter: accessorDescriptionOf: 'iv1' at: self indexOfIv1)
]

{ #category : 'tests - properties' }
CompiledMethodTest >> test10hasLiteralReturnTrueWhenMethodHasNoPropertiesAndHasLiteral [

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.
	
	self assert: (thisMethod hasLiteral: #class).
	
	
]

{ #category : 'tests - accessors' }
CompiledMethodTest >> test11AccessorDescriptionIsWriteReadForMethodThatWritesAndReadInstanceVariable [

	self assert: 'write/read' equals: (self class>>#iv1ReadWrite accessorDescriptionOf: 'iv1' at: self indexOfIv1)
]

{ #category : 'tests - properties' }
CompiledMethodTest >> test11hasLiteralReturnFalseWhenMethodHasNoPropertiesAndHasNoLiteral [

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.
	
	self deny: (thisMethod hasLiteral: 'abc' asSymbol).
	
	
]

{ #category : 'tests - properties' }
CompiledMethodTest >> test12pragmaAtReturnsPragmaIfExist [

	<test>
	
	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	
	self assert: (thisMethod pragmaAt: #test) key equals: #test
]

{ #category : 'tests - properties' }
CompiledMethodTest >> test13pragmaAtReturnsNilIfPragmaDoesNotExist [

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	
	self assert: (thisMethod pragmaAt: #test) isNil
]

{ #category : 'tests - properties' }
CompiledMethodTest >> test14pragmasReturnsCollectionOfMethodPragmas [

	<test>
	
	| thisMethod pragmas |
	
	thisMethod := self class>>thisContext selector.
	pragmas := thisMethod pragmas.
	
	self assert: pragmas size equals: 1.
	self assert: pragmas first key equals: #test
]

{ #category : 'tests - properties' }
CompiledMethodTest >> test15pragmasReturnsEmptyCollectionWhenMethodHasNoProperties [

	| thisMethod pragmas |
	
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.
	pragmas := thisMethod pragmas.
	
	self assert: pragmas isEmpty 
]

{ #category : 'tests - properties' }
CompiledMethodTest >> test16propertiesReturnsAnNewAdditionalMethodStateWhenNoPropertiesHasBeenSet [

	| thisMethod properties |
	
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.
	
	properties := thisMethod properties.
	
	self assert: properties isEmpty.
	self assert: properties selector equals: thisContext selector.
	self deny: properties == thisMethod properties 
]

{ #category : 'tests - properties' }
CompiledMethodTest >> test17propertiesReturnsTheAlreadySetAdditionalMethodStateWhenHasProperties [

	| propertyName thisMethod properties |
	
	propertyName := #testProperty.
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: propertyName put: true.
	
	properties := thisMethod properties.
	self assert: (properties includesKey: propertyName).
	self assert: properties selector equals: thisContext selector.
	
]

{ #category : 'tests - properties' }
CompiledMethodTest >> test18propertyKeysAndValuesDoIteratesOverProperties [

	| propertyName thisMethod properties |
	
	propertyName := #testProperty.
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: propertyName put: true.
	
	properties := OrderedCollection new.
	thisMethod propertyKeysAndValuesDo: [ :key :value | properties add: key -> value ].
	
	self assert: properties size equals: 1.
	self assert: properties first equals: propertyName -> true
]

{ #category : 'tests - properties' }
CompiledMethodTest >> test19propertyKeysAndValuesDoDoesNotEvaluateBlockWhenMethodHasNoProperties [

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.
	
	thisMethod propertyKeysAndValuesDo: [ :key :value | self fail ]
]

{ #category : 'tests - properties' }
CompiledMethodTest >> test20propertyValueAtReturnsPropertyValueWhenSetted [

	| propertyName propertyValue thisMethod |
	
	propertyName := #testProperty.
	propertyValue := 1.
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: propertyName put: propertyValue.
	
	self assert: (thisMethod propertyValueAt: propertyName) equals: propertyValue 
]

{ #category : 'tests - properties' }
CompiledMethodTest >> test21propertyValueAtReturnsNilWhenPropertyIsNotFound [

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: #testProperty put: 1.
	
	self assert: (thisMethod propertyValueAt: #abc) isNil
]

{ #category : 'tests - properties' }
CompiledMethodTest >> test22propertyValueAtReturnsNilWhenMethodHasNoProperties [

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.
	
	self assert: (thisMethod propertyValueAt: #testProperty) isNil
]

{ #category : 'tests - properties' }
CompiledMethodTest >> test23propertyValueAtIfAbsetReturnsPropertyValueWhenFound [

	| propertyName propertyValue thisMethod |
	
	propertyName := #testProperty.
	propertyValue := 1.
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: propertyName put: propertyValue.
	
	self assert: (thisMethod propertyValueAt: propertyName ifAbsent: [ self fail ]) equals: propertyValue 
]

{ #category : 'tests - properties' }
CompiledMethodTest >> test24propertyValueAtIfAbsetEvaluatesIfAbsentBlockWhenNotFound [

	| propertyName propertyValue thisMethod |
	
	propertyName := #testProperty.
	propertyValue := 1.
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: propertyName put: propertyValue.
	
	self assert: (thisMethod propertyValueAt: #abc ifAbsent: [ propertyValue + 1]) equals: propertyValue + 1 
]

{ #category : 'tests - properties' }
CompiledMethodTest >> test25propertyValueAtIfAbsetEvaluatesIfAbsentBlockWhenMethodHasNoProperties [

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.
	
	self assert: (thisMethod propertyValueAt: #testProperty ifAbsent: [ 1 ]) equals: 1
]

{ #category : 'tests - properties' }
CompiledMethodTest >> test26propertyValueAtPutCreatesPropertiesWithNewOne [

	| thisMethod testProperty propertyValue |
	
	testProperty := #testProperty.
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.
	
	propertyValue := thisMethod propertyValueAt: testProperty put: 1.
	
	self assert: propertyValue equals: 1.
	self assert: (thisMethod propertyValueAt: testProperty) equals: 1
]

{ #category : 'tests - properties' }
CompiledMethodTest >> test27propertyValueAtPutReplacesPropertyIfAlreadyExisted [

	| thisMethod testProperty propertyValue |
	
	testProperty := #testProperty.
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: testProperty put: 1.
	propertyValue := thisMethod propertyValueAt: testProperty put: 2.
	
	self assert: propertyValue equals: 2.
	self assert: (thisMethod propertyValueAt: testProperty) equals: 2
]

{ #category : 'tests - properties' }
CompiledMethodTest >> test28propertyValueAtPutAddsNewPropertyIfPropertiesExisted [

	| thisMethod testProperty1 testProperty2 propertyValue |
	
	testProperty1 := #testProperty1.
	testProperty2 := #testProperty2.
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: testProperty1 put: 1.
	propertyValue := thisMethod propertyValueAt: testProperty2 put: 2.
	
	self assert: propertyValue equals: 2.
	self assert: (thisMethod propertyValueAt: testProperty1) equals: 1.
	self assert: (thisMethod propertyValueAt: testProperty2) equals: 2
]

{ #category : 'tests - properties' }
CompiledMethodTest >> test29selectorReturnsMethodSelectorWhenNoProperties [

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.
	
	self assert: thisMethod selector equals: thisContext selector
]

{ #category : 'tests - properties' }
CompiledMethodTest >> test30selectorReturnsMethodSelectorWhenHasProperties [

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: #testProperty put: 1.
	
	self assert: thisMethod selector equals: thisContext selector
]

{ #category : 'tests - properties' }
CompiledMethodTest >> test31SettingSelectorWhenHasNoProperties [

	| currentSelector thisMethod |
	
	currentSelector := thisContext selector.
	thisMethod := self class>>currentSelector.
	thisMethod removeProperties.
	
	[ thisMethod selector: #test.
	self assert: thisMethod selector equals: #test ] ensure: [ thisMethod selector: currentSelector ]
]

{ #category : 'tests - properties' }
CompiledMethodTest >> test32SettingSelectorWhenHasProperties [

	| currentSelector thisMethod |
	
	currentSelector := thisContext selector.
	thisMethod := self class>>currentSelector.
	thisMethod propertyValueAt: #testProperty put: 1.
	
	[ thisMethod selector: #test.
	self assert: thisMethod selector equals: #test ] ensure: [ thisMethod selector: currentSelector ]
]
