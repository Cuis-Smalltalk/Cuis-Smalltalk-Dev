'From Cuis7.1 [latest update: #6763] on 9 October 2024 at 4:21:20 pm'!
!classDefinition: #AssertionFailure category: #'Exceptions-Extensions'!
Exception subclass: #AssertionFailure
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Exceptions-Extensions'!

!AssertionFailure commentStamp: '<historical>' prior: 0!
AssertionFailure is the exception signaled from Object>>assert: when the assertion block evaluates to false.!


!AssertionFailure methodsFor: 'description' stamp: 'tfei 5/10/1999 14:24'!
isResumable

	^true! !

!AssertionFailure methodsFor: 'priv handling' stamp: 'SqR 3/6/2015 20:57'!
defaultAction

	self noHandler! !


!AssertionFailure reorganize!
('description' isResumable)
('priv handling' defaultAction)
!

