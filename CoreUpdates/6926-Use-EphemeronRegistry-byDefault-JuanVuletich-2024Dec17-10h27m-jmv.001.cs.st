'From Cuis7.3 [latest update: #6925] on 17 December 2024 at 10:39:08 am'!
"Change Set:		6926-CuisCore-AuthorName-2024Dec17-10h27m
Date:			17 December 2024
Author:			Juan Vuletich

<your descriptive text goes here>"
WeakRegistry default notEmpty ifTrue: [ self error: 'WeakRegistry default needs to be empty  to install further updates' ].!

!classDefinition: #WeakRegistry category: #'Collections-Weak'!
Collection subclass: #WeakRegistry
	instanceVariableNames: 'valueDictionary accessLock '
	classVariableNames: 'Default '
	poolDictionaries: ''
	category: 'Collections-Weak'!

!Object methodsFor: 'finalization' stamp: 'jmv 12/13/2024 12:04:39'!
finalizationRegistry
	"Answer the finalization registry associated with the receiver."
	^FinalizationRegistry default! !

!methodRemoval: WeakRegistry class #default stamp: 'jmv 12/17/2024 10:38:05'!
WeakRegistry class removeSelector: #default!
!classDefinition: #WeakRegistry category: #'Collections-Weak'!
Collection subclass: #WeakRegistry
	instanceVariableNames: 'valueDictionary accessLock'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Weak'!
