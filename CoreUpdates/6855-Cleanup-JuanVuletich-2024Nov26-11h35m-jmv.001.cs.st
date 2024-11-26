'From Cuis7.1 [latest update: #6850] on 26 November 2024 at 11:36:16 am'!

!SmalltalkEditor methodsFor: 'private' stamp: 'jmv 11/26/2024 10:34:12'!
performCodeExtractionRefactoringWith: aRefactoringApplierClass

	self codeProvider currentCompiledMethod ifNotNil: [ :cm |
		self ifSourceCodeRefactoringCanBeAppliedDo: [
			aRefactoringApplierClass createAndValueHandlingExceptions: [
				aRefactoringApplierClass
					on: self codeProvider
					for: self selectionInterval asSourceCodeInterval
					of: cm ] ] ].! !

!SmalltalkEditor methodsFor: 'inline temporaray variable' stamp: 'jmv 11/26/2024 10:33:38'!
inlineTemporaryVariable

	self codeProvider currentCompiledMethod ifNotNil: [ :cm |
		InlineTemporaryVariableApplier createAndValueHandlingExceptions: [
			InlineTemporaryVariableApplier
				on: self
				for: self selectionInterval asSourceCodeInterval
				of: cm ]].! !

!methodRemoval: SmalltalkEditor #hasValidCurrentCompiledMethod stamp: 'jmv 11/26/2024 11:36:08'!
SmalltalkEditor removeSelector: #hasValidCurrentCompiledMethod!
