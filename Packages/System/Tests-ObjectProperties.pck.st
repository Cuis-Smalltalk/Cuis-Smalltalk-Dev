'From Cuis 6.0 [latest update: #6072] on 16 November 2023 at 11:48:32 am'!
'Description '!
!provides: 'Tests-ObjectProperties' 1 0!
!requires: 'ObjectProperties' 1 0!
SystemOrganization addCategory: #'Tests-ObjectProperties'!


!classDefinition: #TestObjectProperties category: #'Tests-ObjectProperties'!
TestCase subclass: #TestObjectProperties
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-ObjectProperties'!
!classDefinition: 'TestObjectProperties class' category: #'Tests-ObjectProperties'!
TestObjectProperties class
	instanceVariableNames: ''!


!TestObjectProperties methodsFor: 'tests' stamp: 'jmv 11/16/2023 11:40:22'!
testObjectProperties01None
	| collected subject |
	subject := 3@5.
	self deny: (subject hasProperty: #propertyName1).
	self assert: (subject propertyAt: #propertyName1) isNil.
	self assert: (subject propertyAt: #propertyName1 ifAbsent: 7) equals: 7.
	collected := OrderedCollection new.
	subject propertyAt: #propertyName1 ifPresentDo: [ :value | collected add: value].
	self assert: collected isEmpty.
	collected := OrderedCollection new.
	subject propertyNamesDo: [ :propName | collected add: propName ].
	self assert: collected isEmpty.
	collected := OrderedCollection new.
	subject propertyNamesAndValuesDo: [ :propName | collected add: propName ].
	self assert: collected isEmpty.! !

!TestObjectProperties methodsFor: 'tests' stamp: 'jmv 11/16/2023 11:43:42'!
testObjectProperties02Some
	| collected subject |
	subject := 3@5.
	subject propertyAt: #property1Name put: #property1Value.
	self assert: (subject hasProperty: #property1Name).
	self assert: (subject propertyAt: #property1Name) equals: #property1Value.
	self assert: (subject propertyAt: #property1Name ifAbsent: 7) equals: #property1Value.
	collected := OrderedCollection new.
	subject propertyAt: #property1Name ifPresentDo: [ :value | collected add: value].
	self assert: collected size = 1.
	self assert: collected anyOne equals: #property1Value.
	collected := OrderedCollection new.
	subject propertyNamesDo: [ :propName | collected add: propName ].
	self assert: collected size = 1.
	self assert: collected anyOne equals: #property1Name.
	collected := OrderedCollection new.
	subject propertyNamesAndValuesDo: [ :propName :propValue |
		collected add: {propName. propValue} ].
	self assert: collected size = 1.
	self assert: collected anyOne first equals: #property1Name.
	self assert: collected anyOne second equals: #property1Value.! !

!TestObjectProperties methodsFor: 'tests' stamp: 'jmv 11/16/2023 11:44:18'!
testObjectProperties03AddAndRemove
	| collected subject |
	subject := 3@5.
	subject propertyAt: #property1Name put: #property1Value.
	subject removeProperty: #property1Name.
	self deny: (subject hasProperty: #propertyName1).
	self assert: (subject propertyAt: #propertyName1) isNil.
	self assert: (subject propertyAt: #propertyName1 ifAbsent: 7) equals: 7.
	collected := OrderedCollection new.
	subject propertyAt: #propertyName1 ifPresentDo: [ :value | collected add: value].
	self assert: collected isEmpty.
	collected := OrderedCollection new.
	subject propertyNamesDo: [ :propName | collected add: propName ].
	self assert: collected isEmpty.
	collected := OrderedCollection new.
	subject propertyNamesAndValuesDo: [ :propName | collected add: propName ].
	self assert: collected isEmpty.! !

!TestObjectProperties methodsFor: 'tests' stamp: 'jmv 11/16/2023 11:45:24'!
testObjectProperties04NilValue
	| collected subject |
	subject := 3@5.
	subject propertyAt: #property1Name put: #property1Value.
	subject propertyAt: #property1Name put: nil.
	self assert: (subject hasProperty: #property1Name).
	self assert: (subject propertyAt: #property1Name) equals: nil.
	self assert: (subject propertyAt: #property1Name ifAbsent: 7) equals: nil.
	collected := OrderedCollection new.
	subject propertyAt: #property1Name ifPresentDo: [ :value | collected add: value].
	self assert: collected size = 1.
	self assert: collected anyOne equals: nil.
	collected := OrderedCollection new.
	subject propertyNamesDo: [ :propName | collected add: propName ].
	self assert: collected size = 1.
	self assert: collected anyOne equals: #property1Name.
	collected := OrderedCollection new.
	subject propertyNamesAndValuesDo: [ :propName :propValue |
		collected add: {propName. propValue} ].
	self assert: collected size = 1.
	self assert: collected anyOne first equals: #property1Name.
	self assert: collected anyOne second equals: nil.! !

!TestObjectProperties methodsFor: 'tests' stamp: 'jmv 11/16/2023 11:48:15'!
testObjectWithProperties11None
	| collected subject |
	subject := ObjectWithProperties new.
	self deny: (subject hasProperty: #propertyName1).
	self assert: (subject propertyAt: #propertyName1) isNil.
	self assert: (subject propertyAt: #propertyName1 ifAbsent: 7) equals: 7.
	collected := OrderedCollection new.
	subject propertyAt: #propertyName1 ifPresentDo: [ :value | collected add: value].
	self assert: collected isEmpty.
	collected := OrderedCollection new.
	subject propertyNamesDo: [ :propName | collected add: propName ].
	self assert: collected isEmpty.
	collected := OrderedCollection new.
	subject propertyNamesAndValuesDo: [ :propName | collected add: propName ].
	self assert: collected isEmpty.! !

!TestObjectProperties methodsFor: 'tests' stamp: 'jmv 11/16/2023 11:48:18'!
testObjectWithProperties12Some
	| collected subject |
	subject := ObjectWithProperties new.
	subject propertyAt: #property1Name put: #property1Value.
	self assert: (subject hasProperty: #property1Name).
	self assert: (subject propertyAt: #property1Name) equals: #property1Value.
	self assert: (subject propertyAt: #property1Name ifAbsent: 7) equals: #property1Value.
	collected := OrderedCollection new.
	subject propertyAt: #property1Name ifPresentDo: [ :value | collected add: value].
	self assert: collected size = 1.
	self assert: collected anyOne equals: #property1Value.
	collected := OrderedCollection new.
	subject propertyNamesDo: [ :propName | collected add: propName ].
	self assert: collected size = 1.
	self assert: collected anyOne equals: #property1Name.
	collected := OrderedCollection new.
	subject propertyNamesAndValuesDo: [ :propName :propValue |
		collected add: {propName. propValue} ].
	self assert: collected size = 1.
	self assert: collected anyOne first equals: #property1Name.
	self assert: collected anyOne second equals: #property1Value.! !

!TestObjectProperties methodsFor: 'tests' stamp: 'jmv 11/16/2023 11:48:22'!
testObjectWithProperties13AddAndRemove
	| collected subject |
	subject := ObjectWithProperties new.
	subject propertyAt: #property1Name put: #property1Value.
	subject removeProperty: #property1Name.
	self deny: (subject hasProperty: #propertyName1).
	self assert: (subject propertyAt: #propertyName1) isNil.
	self assert: (subject propertyAt: #propertyName1 ifAbsent: 7) equals: 7.
	collected := OrderedCollection new.
	subject propertyAt: #propertyName1 ifPresentDo: [ :value | collected add: value].
	self assert: collected isEmpty.
	collected := OrderedCollection new.
	subject propertyNamesDo: [ :propName | collected add: propName ].
	self assert: collected isEmpty.
	collected := OrderedCollection new.
	subject propertyNamesAndValuesDo: [ :propName | collected add: propName ].
	self assert: collected isEmpty.! !

!TestObjectProperties methodsFor: 'tests' stamp: 'jmv 11/16/2023 11:48:25'!
testObjectWithProperties14NilValue
	| collected subject |
	subject := ObjectWithProperties new.
	subject propertyAt: #property1Name put: #property1Value.
	subject propertyAt: #property1Name put: nil.
	self assert: (subject hasProperty: #property1Name).
	self assert: (subject propertyAt: #property1Name) equals: nil.
	self assert: (subject propertyAt: #property1Name ifAbsent: 7) equals: nil.
	collected := OrderedCollection new.
	subject propertyAt: #property1Name ifPresentDo: [ :value | collected add: value].
	self assert: collected size = 1.
	self assert: collected anyOne equals: nil.
	collected := OrderedCollection new.
	subject propertyNamesDo: [ :propName | collected add: propName ].
	self assert: collected size = 1.
	self assert: collected anyOne equals: #property1Name.
	collected := OrderedCollection new.
	subject propertyNamesAndValuesDo: [ :propName :propValue |
		collected add: {propName. propValue} ].
	self assert: collected size = 1.
	self assert: collected anyOne first equals: #property1Name.
	self assert: collected anyOne second equals: nil.! !
