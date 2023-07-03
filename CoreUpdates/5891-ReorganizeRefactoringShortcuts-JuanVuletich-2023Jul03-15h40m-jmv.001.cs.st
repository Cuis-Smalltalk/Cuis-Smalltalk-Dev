'From Cuis 6.0 [latest update: #5889] on 3 July 2023 at 4:25:28 pm'!

!RefactoringMenues class methodsFor: 'editor menus' stamp: 'jmv 7/3/2023 16:24:32'!
smalltalkEditorMenuOptions

	self flag: #assumesKeyboardShortcuts.
	^`{
	{
			#itemGroup 			-> 		35.
			#itemOrder 			-> 		10.
			#label 			-> 		RefactoringMenues refactoringsLabel.
			#selector 			-> 		#yourself.
			#icon 			-> 		#listAddIcon
	} asDictionary.
		{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		10.
				#label 			-> 		'Rename... (R)'.
				#selector 			-> 		#contextualRename.
				#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		20.
				#label 			-> 		'Extract as Parameter... (V)'.
				#selector 			-> 		#contextualExtractAsParameter.
				#icon 			-> 		#listAddIcon
		} asDictionary.
		{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		30.
				#label 			-> 		'Add Parameter... (A)'.
				#selector 			-> 		#contextualAddParameter.
				#icon 			-> 		#listAddIcon
		} asDictionary.
		{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		40.
				#label 			-> 		'Remove Parameter... (S)'.
				#selector 			-> 		#contextualRemoveParameter.
				#icon 			-> 		#listRemoveIcon
		} asDictionary.
		{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'Extract Temporary... (J)'.
				#selector 			-> 		#extractToTemporary.
				#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		20.
				#label 			-> 		'Extract Method... (K)'.
				#selector 			-> 		#extractMethod.
				#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		10.
				#label 			-> 		'Inline Temporary... (O)'.
				#selector 			-> 		#inlineTemporaryVariable.
				#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		20.
				#label 			-> 		'Inline Method... (F)'.
				#selector 			-> 		#contextualInlineMethod.
				#icon 			-> 		#saveAsIcon
		} asDictionary.		
		{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		10.
				#label 			-> 		'Temporary to Instance Variable (G)'.
				#selector 			-> 		#temporaryToInstanceVariable.
				#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		20.
				#label 			-> 		'Push Up Instance Variable'.
				#selector 			-> 		#contextualPushUpInClassDefinition.
				#icon 			-> 		#goTopIcon
		} asDictionary.
		{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		30.
				#label 			-> 		'Push Down Instance Variable'.
				#selector 			-> 		#contextualPushDownInClassDefinition.
				#icon 			-> 		#goBottomIcon
		} asDictionary.
	
	}`! !

!RefactoringMenues class methodsFor: 'shortcuts' stamp: 'jmv 7/3/2023 16:22:42'!
smalltalkEditorCmdShortcutsSpec
	"
	Editor initialize
	"
	"arranged in QWERTY keyboard order"
	self flag: #definesKeyboardShortcuts.
	^#(
		#(	$R	#contextualRename:								'Renames what is under cursor')
		#(	$F	#contextualInlineMethod:								'Inlines the selected message send into desired colaborations')
		#(	$G	#temporaryToInstanceVariable:								'Extracts temporary variable to instance variable')
		#(	$O	#inlineTemporaryVariable:								'Inlines the selected temporary variable into each usage')

		#(	$A	#contextualAddParameter:								'Adds parameter to message that is under cursor')
		#(	$S	#contextualRemoveParameter:								'Removes parameter to message that is under cursor')
		#(	$V 	#contextualExtractAsParameter: 								'Extracts the selected code as parameter')
		#(	$J	#extractToTemporary:								'Extracts the selected code into a temporary variable')
		#(	$K	#extractMethod:								'Extracts the selected code into a separate method')		
	)! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
	Editor initialize!

