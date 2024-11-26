'From Cuis7.1 [latest update: #6850] on 26 November 2024 at 11:35:38 am'!

!SmalltalkEditor methodsFor: 'contextual add/remove parameter' stamp: 'jmv 11/26/2024 10:27:25'!
extractAsParameter

	^ RefactoringApplier extractAsParameterApplier createAndValueHandlingExceptions: [
		RefactoringApplier extractAsParameterApplier
			from: self selectionInterval 
			on: model textProvider 
			for: self codeProvider selectedMessageName 
			in: self codeProvider selectedClassOrMetaClass  ]! !

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'jmv 11/26/2024 10:27:37'!
withMethodNodeAndClassDo: aBlock ifErrorsParsing: anErrorBlock

	| selectedClass methodNode |

	selectedClass := self codeProvider selectedClassOrMetaClass.
	[ 
		[ methodNode := selectedClass methodNodeFor: model actualContents string ]
			on: UndeclaredVariableWarning do: [ :ex | ex resume ]
	] on: Error do: [ :anError |  ^ anErrorBlock value: anError ].

	^aBlock value: methodNode value: selectedClass.! !

!methodRemoval: SmalltalkEditor #selectedClassOrMetaClassOrUndefinedObject stamp: 'jmv 11/26/2024 11:35:29'!
SmalltalkEditor removeSelector: #selectedClassOrMetaClassOrUndefinedObject!
