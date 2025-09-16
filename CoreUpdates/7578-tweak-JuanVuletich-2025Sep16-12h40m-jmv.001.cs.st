'From Cuis7.5 [latest update: #7577] on 16 September 2025 at 12:41:15 pm'!

!SystemDictionary methodsFor: 'miscellaneous' library: #MinimalKernel05ClassAndMethodCreation stamp: 'HAW 1/17/2024 20:44:30'!
isDevelopmentEnvironmentPresent
	"Or we can't open a Smalltalk debugger"

	^ (self includesKey: #Debugger) and: [self isHeadless not]! !

