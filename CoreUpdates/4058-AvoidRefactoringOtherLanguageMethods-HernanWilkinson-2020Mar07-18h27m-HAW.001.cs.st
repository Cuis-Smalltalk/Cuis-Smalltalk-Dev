'From Cuis 5.0 [latest update: #4057] on 7 March 2020 at 6:54:33 pm'!

!ChangeSelector class methodsFor: 'implementors and senders' stamp: 'HAW 3/7/2020 18:53:49'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders of: aPotentialClassToRefactor

	| potentialImplementor |

	"Phil B. requested to avoid refactoring OMeta2 classes, so right now
	it avoids implementors and senders whose compilerClass is not Compiler
	It is not common to subclass Compiler and keep Smalltalk syntax, that it is why I
	check for Compiler and not for a list of allowed/disallowed compilers - Hernan"
	aPotentialClassToRefactor compilerClass = Compiler ifFalse: [^self ].

	potentialImplementor := aPotentialClassToRefactor compiledMethodAt: anOldSelector ifAbsent: [ nil ].
	potentialImplementor ifNotNil: [ implementors add: potentialImplementor ].

	(aPotentialClassToRefactor whichSelectorsReferTo: anOldSelector) do: [ :aSelector |
		senders add: (MethodReference class: aPotentialClassToRefactor selector: aSelector) ]! !

!ChangeSelector class reorganize!
('pre-conditions' assertAllImplementors:haveSame: assertAllSenders:send: assertIsNotEmpty:signalMessageText:)
('exceptions' errorMessageForInvalidImplementors: errorMessageForInvalidSenders:of: signalInvalidImplementors: signalInvalidSenders:of:)
('implementors and senders' addImplementorsOf:to:andSendersTo:forClassAndMetaOf: addImplementorsOf:to:andSendersTo:inCategoriesAndHierarchyOf:organizedBy: addImplementorsOf:to:andSendersTo:inCategory:organizedBy: addImplementorsOf:to:andSendersTo:inHierarchyOf: addImplementorsOf:to:andSendersTo:inHierarchyOf:doingPerClass: addImplementorsOf:to:andSendersTo:inSystem: addImplementorsOf:to:andSendersTo:of:)
!
