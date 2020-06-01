'From Cuis 5.0 [latest update: #4171] on 25 May 2020 at 12:59:52 am'!
"Change Set:		4192-CuisCore-NahuelGarbezza-2020May27-18h58m
Date:			27 May 2020
Author:			Nahuel Garbezza

* Fix confusing error message in extract temporary refactoring
* Redefine #asSourceCodeInterval in SourceCodeInterval for performance reasons"!

!SourceCodeInterval methodsFor: 'converting' stamp: 'RNG 5/28/2020 00:00:00'!
asSourceCodeInterval

	^ self! !


!NewTemporaryPrecondition class methodsFor: 'error messages' stamp: 'RNG 5/28/2020 00:28:53'!
errorMessageFor: aNewVariable canNotBeNamedDueToInstanceVariableDefinedIn: aClass

	^ aNewVariable, ' cannot be used as a temporary variable name because it is defined as an instance variable in ', aClass name! !


!SourceCodeInterval reorganize!
('converting' asSourceCodeInterval)
('source code' expandToMatchExpressionOn: trimToMatchExpressionOn:)
('private - source code' canBeExpandedStartingWith:endingWith: canBeTrimmed: expandBy: trimLeft:right:)
!

