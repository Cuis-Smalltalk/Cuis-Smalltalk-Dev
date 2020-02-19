'From Cuis 5.0 [latest update: #4043] on 19 February 2020 at 12:22:13 pm'!

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 2/19/2020 12:18:39'!
referencesToIt
	"Open a references browser on the selected symbol: a variable name or class name"

	| selectedSymbol provider environment selectedString |
	self hasSelection ifFalse: [ self selectWord ].
	selectedSymbol _ self selectedSymbol.

	"convenient access to class variables, including those in SharedPools"
	provider _ self codeProvider.
	environment _ (provider respondsTo: #selectedClassOrMetaClass) ifTrue: [ provider selectedClassOrMetaClass ].
	environment _ environment ifNil: [ Smalltalk ].

	(selectedSymbol ifNotNil: [environment bindingOf: selectedSymbol]) ifNotNil: [ :reference |
		Smalltalk browseAllCallsOn: reference.
		^ self ].

	selectedString _ self selectedString withoutSeparators.
	(environment ~= Smalltalk and: [ environment definesInstanceVariableNamedInHierarchy: selectedString ]) ifTrue: [
		Smalltalk browseAllAccessesTo: selectedString from: environment.
		^ self ].

	self referencesToSelectedLiteral! !

