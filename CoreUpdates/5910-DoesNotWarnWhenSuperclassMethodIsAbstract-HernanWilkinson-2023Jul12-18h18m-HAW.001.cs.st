'From Cuis 6.0 [latest update: #5908] on 12 July 2023 at 7:28:16 pm'!

!PushUpMethod class methodsFor: 'warnings' stamp: 'HAW 7/12/2023 19:27:40'!
warnIfExistMethodToPushUpOnSuperClass: aMethodToPushUp

	aMethodToPushUp methodClass superclass 
		compiledMethodAt: aMethodToPushUp selector
		ifPresent: [ :superclassMethod | ((superclassMethod equivalentTo: aMethodToPushUp) or: [ superclassMethod isAbstract ])
			ifFalse: [ self refactoringWarning: self methodToPushUpExistOnSuperclassWarningMessage ]]
		ifAbsent: []! !

