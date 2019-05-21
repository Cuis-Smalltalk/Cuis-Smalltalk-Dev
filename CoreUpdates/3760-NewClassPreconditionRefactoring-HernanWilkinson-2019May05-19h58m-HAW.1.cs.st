'From Cuis 5.0 of 7 November 2016 [latest update: #3740] on 10 May 2019 at 12:46:11 pm'!

!NewClassPrecondition methodsFor: 'evaluating - private' stamp: 'HAW 5/10/2019 12:24:16'!
assertNewClassNameIsNotDeclaredInUndeclared

	(undeclared includesKey: newClassName) ifTrue: [ self signalNewClassIsUndeclared]! !

!NewClassPrecondition methodsFor: 'evaluating - private' stamp: 'HAW 5/10/2019 12:30:59'!
assertNewClassNameStartsWithRightLetter

	newClassName first isUppercase ifFalse: [ self signalNewNameMustStartWithRightLetter]! !

!NewClassPrecondition methodsFor: 'exceptions' stamp: 'HAW 5/10/2019 12:31:44'!
signalNewNameMustStartWithRightLetter

	self refactoringError: self class newNameMustStartWithRightLetterErrorMessage.! !


!NewClassPrecondition class methodsFor: 'error messages' stamp: 'HAW 5/10/2019 12:31:44'!
newNameMustStartWithRightLetterErrorMessage

	^'New class name must start with an uppercase letter'! !


!NewClassPrecondition methodsFor: 'evaluating' stamp: 'HAW 5/10/2019 12:24:16'!
value

	self assertNewClassNameIsNotEmpty.
	self assertNewClassNameSymbol.
	self assertNewClassNameStartsWithRightLetter.
	self assertNewClassNameHasNoSeparators.
	self assertNewClassNameDoesNotExistInSystem.
	self assertNewClassNameIsNotDeclaredInUndeclared.

! !

!methodRemoval: NewClassPrecondition class #newNameMustStartWithUppercaseLetterErrorMessage!
NewClassPrecondition class removeSelector: #newNameMustStartWithUppercaseLetterErrorMessage!
!methodRemoval: NewClassPrecondition #assertNewClassNameIsNotUndeclaredInUndeclared!
NewClassPrecondition removeSelector: #assertNewClassNameIsNotUndeclaredInUndeclared!
!methodRemoval: NewClassPrecondition #assertNewClassNameStartsWithUppercaseLetter!
NewClassPrecondition removeSelector: #assertNewClassNameStartsWithUppercaseLetter!
!methodRemoval: NewClassPrecondition #signalNewNameMustStartWithUppercaseLetter!
NewClassPrecondition removeSelector: #signalNewNameMustStartWithUppercaseLetter!

!NewClassPrecondition reorganize!
('evaluating' value)
('evaluating - private' assertNewClassNameDoesNotExistInSystem assertNewClassNameHasNoSeparators assertNewClassNameIsNotDeclaredInUndeclared assertNewClassNameIsNotEmpty assertNewClassNameStartsWithRightLetter assertNewClassNameSymbol)
('initialization' initializeFor:in:undeclared:)
('exceptions' signalClassAlreadyExists signalGlobalAlreadyExists signalNewClassIsUndeclared signalNewClassNameCanNotBeEmpty signalNewClassNameCanNotHaveSeparators signalNewNameMustBeSymbol signalNewNameMustStartWithRightLetter)
!

