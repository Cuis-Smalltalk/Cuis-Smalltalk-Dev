'From Cuis 5.0 [latest update: #4415] on 22 October 2020 at 9:11:52 pm'!
'Description '!
!provides: 'Tests-Immutability' 1 0!
!requires: 'Immutability' 1 2 nil!
SystemOrganization addCategory: 'Tests-Immutability'!


!classDefinition: #ImmutabilityTest category: 'Tests-Immutability'!
TestCase subclass: #ImmutabilityTest
	instanceVariableNames: 'instVar1'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Immutability'!
!classDefinition: 'ImmutabilityTest class' category: 'Tests-Immutability'!
ImmutabilityTest class
	instanceVariableNames: ''!


!ImmutabilityTest methodsFor: 'tests' stamp: 'HAW 7/3/2019 18:36:37'!
test01AnObjectCanBeMarkAsImmutable

	| testObject |
	
	testObject := self class new.
	testObject beImmutable.
	
	self assert: testObject isImmutable! !

!ImmutabilityTest methodsFor: 'tests' stamp: 'HAW 7/3/2019 18:36:49'!
test02beImmutableReturnsThePreviousMutableState

	| testObject wasImmutable |
	
	testObject := self class new.
	wasImmutable := testObject beImmutable.
	
	self deny: wasImmutable! !

!ImmutabilityTest methodsFor: 'tests' stamp: 'HAW 7/3/2019 18:37:01'!
test03ImmutableObjectCanBeTurnMutable

	| testObject |
	
	testObject := self class new.
	testObject 
		beImmutable;
		beMutable.
	
	self deny: testObject isImmutable.! !

!ImmutabilityTest methodsFor: 'tests' stamp: 'HAW 7/3/2019 18:37:13'!
test04beMutableReturnsThePreviousMutableState

	| testObject wasImmutable |
	
	testObject := self class new.
	wasImmutable := testObject 
		beImmutable;
		beMutable.
	
	self assert: wasImmutable! !

!ImmutabilityTest methodsFor: 'tests' stamp: 'HAW 7/3/2019 18:44:32'!
test05InstanceVariablesOfImmutableObjectsCanNotBeModified

	| testObject newValue |
	
	testObject := self class new.
	testObject beImmutable.
	newValue := 'something'.
	
	self 
		should: [ testObject instVar1: newValue ]
		raise: ModificationForbidden 
		withExceptionDo: [ :modificationForbidden |
			self assert: testObject instVar1 isNil.
			self assert: ModificationForbidden messageText equals: modificationForbidden messageText.
			self assert: testObject == modificationForbidden object.
			self assert: (self class indexOfInstanceVariable: 'instVar1') equals: modificationForbidden fieldIndex.
			self assert: newValue equals: modificationForbidden newValue.
			self assert: #instVarAt:put: equals: modificationForbidden retrySelector ]! !

!ImmutabilityTest methodsFor: 'tests' stamp: 'HAW 7/3/2019 18:47:50'!
test06retryModificationMutatesTheObjectIfMadeMutable

	| testObject newValue |
	
	testObject := self class new.
	testObject beImmutable.
	newValue := 'something'.
	
	[ testObject instVar1: newValue ]
		on: ModificationForbidden 
		do: [ :modificationForbidden |
			testObject beMutable.
			modificationForbidden retryModification ].
		
	self assert: newValue equals: testObject instVar1 ! !

!ImmutabilityTest methodsFor: 'tests' stamp: 'HAW 7/4/2019 15:58:54'!
test07atPutInformsModificationForbiddenForImmutableObjects

	| testObject newValue |
	
	testObject := Array new: 1.
	testObject beImmutable.
	newValue := 'something'.
	
	self 
		should: [ testObject at: 1 put: newValue ]
		raise: ModificationForbidden 
		withExceptionDo: [ :modificationForbidden |
			self assert: testObject first isNil.
			self assert: #at:put: equals: modificationForbidden retrySelector ]		
	! !

!ImmutabilityTest methodsFor: 'tests' stamp: 'HAW 7/4/2019 15:59:38'!
test08retryModificationWorkAsExpectedWhenSentatPut

	| testObject newValue |
	
	testObject := Array new: 1.
	testObject beImmutable.
	newValue := 'something'.
	
	[ testObject at: 1 put: newValue ]
		on: ModificationForbidden 
		do: [ :modificationForbidden |
			testObject beMutable.
			modificationForbidden retryModification ].
		
	self assert: newValue equals: testObject first! !

!ImmutabilityTest methodsFor: 'tests' stamp: 'HAW 7/4/2019 15:59:56'!
test09basicAtPutInformsModificationForbiddenForImmutableObjects

	| testObject newValue |
	
	testObject := Array new: 1.
	testObject beImmutable.
	newValue := 'something'.
	
	self 
		should: [ testObject basicAt: 1 put: newValue ]
		raise: ModificationForbidden 
		withExceptionDo: [ :modificationForbidden |
			self assert: testObject first isNil.
			self assert: #basicAt:put: equals: modificationForbidden retrySelector ]		
	! !

!ImmutabilityTest methodsFor: 'tests' stamp: 'HAW 7/4/2019 16:00:11'!
test10rawBasicAtPutInformsModificationForbiddenForImmutableObjects

	| testObject newValue |
	
	testObject := Array new: 1.
	testObject beImmutable.
	newValue := 'something'.
	
	self 
		should: [ testObject rawBasicAt: 1 put: newValue ]
		raise: ModificationForbidden 
		withExceptionDo: [ :modificationForbidden |
			self assert: testObject first isNil.
			self assert: #rawBasicAt:put: equals: modificationForbidden retrySelector ]		
	! !

!ImmutabilityTest methodsFor: 'tests' stamp: 'HAW 7/4/2019 15:56:21'!
test11AnArray2DCanBeImmutable

	| testObject newValue |
	
	testObject := Array2D height: 2 width: 2.
	testObject beImmutable.
	newValue := 'something'.
	
	self 
		should: [ testObject i: 1 j: 1 put: newValue ]
		raise: ModificationForbidden 
		withExceptionDo: [ :modificationForbidden |
			self assert: (testObject i: 1 j: 1) isNil.
			"Currently informs elements as the object to modify instead of the Array2D instance
			This should be changed but more work is needed and I'm not sure it is worth at this time - Hernan"
			self assert: #at:put: equals: modificationForbidden retrySelector.
			self assert: testObject elements == modificationForbidden object ]		
	! !

!ImmutabilityTest methodsFor: 'tests' stamp: 'HAW 7/4/2019 15:56:29'!
test12retryModificationWorksAsExpectedWithAnArray2D

	| testObject newValue |
	
	testObject := Array2D height: 2 width: 2.
	testObject beImmutable.
	newValue := 'something'.
	
	[ testObject i: 1 j: 1 put: newValue ]
		on: ModificationForbidden 
		do: [ :modificationForbidden |
			testObject beMutable.
			modificationForbidden retryModification ].
		
	self assert: newValue equals: (testObject i: 1 j: 1)		
	! !

!ImmutabilityTest methodsFor: 'tests' stamp: 'HAW 7/4/2019 15:57:11'!
test13DictionariesCanBeImmutable

	| testObject newValue |
	
	testObject := Dictionary new.
	testObject beImmutable.
	newValue := 'something'.
	
	self 
		should: [ testObject at: 1 put: newValue ]
		raise: ModificationForbidden 
		withExceptionDo: [ :modificationForbidden |
			self assert: (testObject at:1 ifAbsent: [ nil ]) isNil.
			"Currently informs the inst var array as the object to modify instead of the Dictionary instance
			This should be changed but more work is needed and I'm not sure it is worth at this time - Hernan"
			self assert: #at:put: equals: modificationForbidden retrySelector.
			self assert: testObject array == modificationForbidden object ]		
	! !

!ImmutabilityTest methodsFor: 'tests' stamp: 'HAW 7/4/2019 15:56:46'!
test14retryModificationWorksAsExpectedWithDictionaries

	| testObject newValue |
	
	testObject := Dictionary new.
	testObject beImmutable.
	newValue := 'something'.
	
	[ testObject at: 1 put: newValue ]
		on: ModificationForbidden 
		do: [ :modificationForbidden |
			testObject beMutable.
			modificationForbidden retryModification ].
		
	self assert: newValue equals: (testObject at: 1).
	self assert: 1 equals: testObject size		
	! !

!ImmutabilityTest methodsFor: 'tests' stamp: 'HAW 7/4/2019 15:57:33'!
test15SetsCanBeImmutable

	| testObject newValue |
	
	testObject := Set new.
	testObject beImmutable.
	newValue := 'something'.
	
	self 
		should: [ testObject add: newValue ]
		raise: ModificationForbidden 
		withExceptionDo: [ :modificationForbidden |
			self deny: (testObject includes: newValue).
			"Currently informs the inst var array as the object to modify instead of the Set instance
			This should be changed but more work is needed and I'm not sure it is worth at this time - Hernan"
			self assert: #at:put: equals: modificationForbidden retrySelector.
			self assert: testObject array == modificationForbidden object ]		
	! !

!ImmutabilityTest methodsFor: 'tests' stamp: 'HAW 7/4/2019 15:57:39'!
test16retryModificationWorksAsExpectedWithSets

	| testObject newValue |
	
	testObject := Set new.
	testObject beImmutable.
	newValue := 'something'.
	
	[ testObject add: newValue ]
		on: ModificationForbidden 
		do: [ :modificationForbidden |
			testObject beMutable.
			modificationForbidden retryModification ].
		
	self assert: (testObject includes: newValue).
	self assert: 1 equals: testObject size		
	! !

!ImmutabilityTest methodsFor: 'tests' stamp: 'HAW 7/4/2019 15:57:50'!
test17OrderedCollectionsCanBeImmutable

	| testObject newValue |
	
	testObject := OrderedCollection new.
	testObject beImmutable.
	newValue := 'something'.
	
	self 
		should: [ testObject add: newValue ]
		raise: ModificationForbidden 
		withExceptionDo: [ :modificationForbidden |
			self deny: (testObject includes: newValue).
			self assert: #instVarAt:put: equals: modificationForbidden retrySelector.
			self assert: testObject == modificationForbidden object ]		
	! !

!ImmutabilityTest methodsFor: 'tests' stamp: 'HAW 7/4/2019 15:57:59'!
test18retryModificationWorksAsExpectedWithOrderedCollections

	| testObject newValue |
	
	testObject := OrderedCollection new.
	testObject beImmutable.
	newValue := 'something'.
	
	[ testObject add: newValue ]
		on: ModificationForbidden 
		do: [ :modificationForbidden |
			testObject beMutable.
			modificationForbidden retryModification ].
		
	self assert: newValue equals: testObject first.
	self assert: 1 equals: testObject size		
	! !

!ImmutabilityTest methodsFor: 'tests' stamp: 'HAW 7/4/2019 15:50:18'!
test19StringsCanBeImmutable

	| testObject newValue |
	
	testObject := 'aString'.
	testObject beImmutable.
	newValue := $x.
	
	self 
		should: [ testObject at: 1 put: newValue ]
		raise: ModificationForbidden 
		withExceptionDo: [ :modificationForbidden |
			self assert: $a equals: testObject first.
			self assert: #at:put: equals: modificationForbidden retrySelector.
			self assert: testObject == modificationForbidden object ]		
	! !

!ImmutabilityTest methodsFor: 'tests' stamp: 'HAW 7/4/2019 15:51:39'!
test20retryModificationWorksAsExpectedWithStrings

	| testObject newValue |
	
	testObject := 'aString'.
	testObject beImmutable.
	newValue := $x.
	
	[ testObject at: 1 put: newValue ]
		on: ModificationForbidden 
		do: [ :modificationForbidden |
			testObject beMutable.
			modificationForbidden retryModification ].
		
	self assert: newValue equals: testObject first.
		! !

!ImmutabilityTest methodsFor: 'tests' stamp: 'HAW 7/12/2019 14:37:29'!
test21InstVarAtPutSignalModificationForbiddenWhenObjectIsImmutable

	| testObject newValue instVar1Index |
	
	testObject := self class new.
	testObject beImmutable.
	newValue := 'something'.
	instVar1Index := self class indexOfInstanceVariable: 'instVar1'.
	
	self 
		should: [ testObject instVarAt: instVar1Index put: newValue ]
		raise: ModificationForbidden 
		withExceptionDo: [ :modificationForbidden |
			self assert: (testObject instVarAt: instVar1Index) isNil.
			self assert: #instVarAt:put: equals: modificationForbidden retrySelector.
			self assert: testObject == modificationForbidden object ]			! !

!ImmutabilityTest methodsFor: 'tests' stamp: 'HAW 7/12/2019 14:37:11'!
test22retyModificationWorksAsExpectedWithInstVarAtPut

	| testObject newValue instVar1Index |
	
	testObject := self class new.
	testObject beImmutable.
	newValue := 'something'.
	instVar1Index := self class indexOfInstanceVariable: 'instVar1'.
	
	[ testObject instVarAt: instVar1Index put: newValue ]
		on: ModificationForbidden 
		do: [ :modificationForbidden |
			testObject beMutable.
			modificationForbidden retryModification ].
		
	self assert: newValue equals: (testObject instVarAt: instVar1Index).
! !

!ImmutabilityTest methodsFor: 'test support' stamp: 'HAW 7/3/2019 18:27:52'!
instVar1
	
	^instVar1! !

!ImmutabilityTest methodsFor: 'test support' stamp: 'HAW 7/3/2019 18:20:51'!
instVar1: anObject

	instVar1 := anObject ! !
