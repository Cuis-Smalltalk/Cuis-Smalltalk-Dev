'From Cuis 5.0 [latest update: #3992] on 23 December 2019 at 11:48:22 am'!
!classDefinition: #ExtractMethodExpressionValidation category: #'Tools-Refactoring'!
Object subclass: #ExtractMethodExpressionValidation
	instanceVariableNames: 'method intervalToExtract methodNode sourceCode initialNode finalNode initialNodeAncestors finalNodeAncestors'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractMethodIntervalTrimmer category: #'Tools-Refactoring'!
Object subclass: #ExtractMethodIntervalTrimmer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractMethod category: #'Tools-Refactoring'!
Refactoring subclass: #ExtractMethod
	instanceVariableNames: 'intervalToExtract categoryOfNewSelector newMessage extractedSourceCode existingMethod'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractMethodApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #ExtractMethodApplier
	instanceVariableNames: 'intervalToExtract methodToExtractCodeFrom newSelector newMessageArguments'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractMethodNewSelectorPrecondition category: #'Tools-Refactoring'!
RefactoringPrecondition subclass: #ExtractMethodNewSelectorPrecondition
	instanceVariableNames: 'selectorToValidate classToDefineSelector'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!RefactoringMenues class methodsFor: 'editor menus' stamp: 'HAW 12/23/2019 11:45:31'!
smalltalkEditorMenuOptions

	^`{
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		10.
				#label 			-> 		'Rename... (R)'.
				#selector 		-> 		#contextualRename.
				#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		20.
				#label 			-> 		'Extract Method... (K)'.
				#selector 		-> 		#extractMethod.
				#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		30.
				#label 			-> 		'Add Parameter... (A)'.
				#selector 		-> 		#contextualAddParameter.
				#icon 			-> 		#listAddIcon
		} asDictionary.
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		40.
				#label 			-> 		'Remove Parameter... (S)'.
				#selector 		-> 		#contextualRemoveParameter.
				#icon 			-> 		#listRemoveIcon
		} asDictionary.
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		50.
				#label 			-> 		'Temporary to Instance Variable (O)'.
				#selector 		-> 		#temporaryToInstanceVariable.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
	}`! !

!RefactoringMenues class methodsFor: 'shortcuts' stamp: 'HAW 12/23/2019 11:44:05'!
smalltalkEditorCmdShortcutsSpec

	^#(
		#(		$R	#contextualRename:		'Renames what is under cursor')
		#(		$A	#contextualAddParameter:		'Adds parameter to message that is under cursor')
		#(		$S	#contextualRemoveParameter:		'Removes parameter to message that is under cursor')
		#(		$O	#temporaryToInstanceVariable:		'Extracts temporary variable to instance variable')
		#(		$K #extractMethod: 'Extracts the selected code into a separate method')
		)
! !

!classRemoval: #ExtractMethodEditorMenu stamp: 'HAW 12/23/2019 11:45:43'!
Smalltalk removeClassNamed: #ExtractMethodEditorMenu!

"Postscript:"
SystemOrganization removeSystemCategory: 'Refactorings-ExtractMethod'.
!
