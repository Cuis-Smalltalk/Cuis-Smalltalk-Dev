'From Cuis 4.2 of 25 July 2013 [latest update: #2829] on 18 June 2016 at 9:35:32.404368 pm'!

!SmalltalkEditor methodsFor: 'explain' stamp: 'dhn 6/16/2016 17:47'!
explainTemp: string 
	"Is string the name of a temporary variable (method or block argument or temporary)?"

	| provider selectedClass methodNode tempNode |
	provider _ self codeProvider.
	(provider respondsTo: #selectedMessageName) ifFalse: [^ nil].
	provider selectedMessageName ifNil: [^nil].	"not in a method"
	selectedClass := provider selectedClassOrMetaClass.
	methodNode := selectedClass parserClass new parse: provider selectedMessage class: selectedClass.
	tempNode := methodNode encoder tempNodes detect: [ :n | n name = string ] ifNone: [^nil].
	^(tempNode isArg
		ifTrue: [string, ' is an argument to this ']
		ifFalse: [string, ' is a temporary variable in this ']),
	   (tempNode isDeclaredAtMethodLevel
		ifTrue: ['method ']
		ifFalse: ['block '])! !
