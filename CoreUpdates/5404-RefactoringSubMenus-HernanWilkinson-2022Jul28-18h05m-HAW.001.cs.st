'From Cuis 6.0 [latest update: #5401] on 28 July 2022 at 6:36:40 pm'!

!RefactoringMenues class methodsFor: 'editor menus' stamp: 'HAW 7/28/2022 18:35:08'!
smalltalkEditorMenuOptions

	^`{
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		10.
				#label 			-> 	RefactoringMenues refactoringsLabel.
				#selector 		-> 		#yourself.
				#icon 			-> 		#listAddIcon
		} asDictionary.
		{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'Rename... (R)'.
				#selector 		-> 		#contextualRename.
				#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		20.
				#label 			-> 		'Extract as Parameter... (1)'.
				#selector 		-> 		#contextualExtractAsParameter.
				#icon 			-> 		#listAddIcon
		} asDictionary.
		{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		30.
				#label 			-> 		'Add Parameter... (A)'.
				#selector 		-> 		#contextualAddParameter.
				#icon 			-> 		#listAddIcon
		} asDictionary.
		{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		40.
				#label 			-> 		'Remove Parameter... (S)'.
				#selector 		-> 		#contextualRemoveParameter.
				#icon 			-> 		#listRemoveIcon
		} asDictionary.
		{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'Extract Temporary... (J)'.
				#selector 		-> 		#extractToTemporary.
				#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		20.
				#label 			-> 		'Extract Method... (K)'.
				#selector 		-> 		#extractMethod.
				#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		10.
				#label 			-> 		'Inline Temporary... (2)'.
				#selector 		-> 		#inlineTemporaryVariable.
				#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		20.
				#label 			-> 		'Inline Method... (3)'.
				#selector 		-> 		#contextualInlineMethod.
				#icon 			-> 		#saveAsIcon
		} asDictionary.		
		{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		10.
				#label 			-> 		'Temporary to Instance Variable (O)'.
				#selector 		-> 		#temporaryToInstanceVariable.
				#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		20.
				#label 			-> 		'Push Up Instance Variable'.
				#selector 		-> 		#contextualPushUpInClassDefinition.
				#icon 			-> 		#goTopIcon
		} asDictionary.
		{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		30.
				#label 			-> 		'Push Down Instance Variable'.
				#selector 		-> 		#contextualPushDownInClassDefinition.
				#icon 			-> 		#goBottomIcon
		} asDictionary.
	
	}`! !

!RefactoringMenues class methodsFor: 'browser menues' stamp: 'HAW 7/28/2022 18:12:44'!
classListMenuOptions

	^ `{
			{
				#itemGroup 	-> 		5.
				#itemOrder 		-> 		10.
				#label 			-> 	RefactoringMenues refactoringsLabel.
				#selector 		-> 		#yourself.
				#icon 			-> 		#listAddIcon
			} asDictionary.
			{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'rename class... (R)'.
				#object 			-> 		#model.
				#selector 		-> 		#renameClass.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		20.
				#label 			-> 		'insert superclass...'.
				#object 			-> 		#model.
				#selector 		-> 		#insertSuperclass.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		30.
				#label 			-> 		'safely remove class (x)'.
				#object 			-> 		#model.
				#selector 		-> 		#safelyRemoveClass.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'add inst var...'.
				#selector 		-> 		#addInstVar.
				#icon 			-> 		#listAddIcon
			} asDictionary.
			{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		20.
				#label 			-> 		'rename inst var...'.
				#selector 		-> 		#renameInstVar.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		30.
				#label 			-> 		'remove inst var...'.
				#selector 		-> 		#removeInstVar.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 	-> 			20.
				#itemOrder 		-> 		40.
				#label 			-> 		'remove all unreferenced inst var...'.
				#selector 		-> 		#removeAllUnreferencedInstVar.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		50.
				#label 			-> 		'push up inst var...'.
				#selector 		-> 		#pushUpInstanceVariable.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		60.
				#label 			-> 		'push down inst var...'.
				#selector 		-> 		#pushDownInstanceVariable.
				#icon 			-> 		#goBottomIcon
			} asDictionary.				
			{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 	-> 			30.
				#itemOrder 		-> 		10.
				#label 			-> 		'rename global...'.
				#object 			-> 		#model.
				#selector 		-> 		#renameGlobal.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
	}`.
	! !

!RefactoringMenues class methodsFor: 'browser menues' stamp: 'HAW 7/28/2022 18:33:52'!
messageListMenuOptions

	^ `{
			{
				#itemGroup 	-> 		5.
				#itemOrder 		-> 		10.
				#label 			-> 		RefactoringMenues refactoringsLabel.
				#selector 		-> 		#yourself.
				#icon 			-> 		#listAddIcon
			} asDictionary.
			{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'rename... (R)'.
				#selector 		-> 		#renameSelector.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		15.
				#label 			-> 		'change keyword order...'.
				#selector 		-> 		#changeKeywordOrder.
				#icon 			-> 		#sendReceiveIcon
			} asDictionary.
			{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		20.
				#label 			-> 		'add parameter...'.
				#selector 		-> 		#addParameter.
				#icon 			-> 		#listAddIcon
			} asDictionary.
			{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		30.
				#label 			-> 		'remove parameter...'.
				#selector 		-> 		#removeParameter.
				#icon 			-> 		#listRemoveIcon
			} asDictionary.
			{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'inline method... (3)'.
				#selector 		-> 		#inlineMethod.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		20.
				#label 			-> 		'move to instance/class method'.
				#selector 		-> 		#moveToInstanceOrClassMethod.
				#icon 			-> 		#changesIcon
			} asDictionary.
			{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		30.
				#label 			-> 		'push up'.
				#selector 		-> 		#pushUpSelector.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		40.
				#label 			-> 		'push down'.
				#selector 		-> 		#pushDownSelector.
				#icon 			-> 		#goBottomIcon
			} asDictionary.
		}`.
	! !

!methodRemoval: RefactoringMenues class #smalltalkEditorRefactoringMenuOptions stamp: 'HAW 7/28/2022 18:28:04'!
RefactoringMenues class removeSelector: #smalltalkEditorRefactoringMenuOptions!
!methodRemoval: RefactoringMenues class #messageRefactoringMenuOptions stamp: 'HAW 7/28/2022 18:19:04'!
RefactoringMenues class removeSelector: #messageRefactoringMenuOptions!
!methodRemoval: RefactoringMenues class #classRefactoringMenuOptions stamp: 'HAW 7/28/2022 18:19:46'!
RefactoringMenues class removeSelector: #classRefactoringMenuOptions!
!methodRemoval: BrowserWindow #openMessageRefactoringMenu stamp: 'HAW 7/28/2022 18:18:42'!
BrowserWindow removeSelector: #openMessageRefactoringMenu!
!methodRemoval: BrowserWindow #messageRefactoringMenu stamp: 'HAW 7/28/2022 18:18:49'!
BrowserWindow removeSelector: #messageRefactoringMenu!
!methodRemoval: BrowserWindow #openClassRefactoringMenu stamp: 'HAW 7/28/2022 18:19:28'!
BrowserWindow removeSelector: #openClassRefactoringMenu!
!methodRemoval: BrowserWindow #classRefactoringMenu stamp: 'HAW 7/28/2022 18:19:35'!
BrowserWindow removeSelector: #classRefactoringMenu!
!methodRemoval: SmalltalkEditor #refactoringMenu stamp: 'HAW 7/28/2022 18:28:01'!
SmalltalkEditor removeSelector: #refactoringMenu!
!methodRemoval: SmalltalkEditor #openSmalltalkEditorRefactoringMenu stamp: 'HAW 7/28/2022 18:27:57'!
SmalltalkEditor removeSelector: #openSmalltalkEditorRefactoringMenu!

!RefactoringMenues class reorganize!
('editor menus' smalltalkEditorMenuOptions)
('browser menues' classListMenuOptions messageListMenuOptions refactoringsLabel)
('initialization' initialize)
('shortcuts' smalltalkEditorCmdShortcutsSpec)
!

