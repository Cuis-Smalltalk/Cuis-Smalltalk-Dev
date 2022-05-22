'From Cuis 6.0 [latest update: #5173] on 21 May 2022 at 11:26:58 pm'!

!RefactoringMenues class methodsFor: 'browser menues' stamp: 'HAW 5/21/2022 23:26:24'!
messageRefactoringMenuOptions

	^ `{
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'rename... (R)'.
				#selector 		-> 		#renameSelector.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		15.
				#label 			-> 		'change keyword order...'.
				#selector 		-> 		#changeKeywordOrder.
				#icon 			-> 		#sendReceiveIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		20.
				#label 			-> 		'add parameter...'.
				#selector 		-> 		#addParameter.
				#icon 			-> 		#listAddIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		30.
				#label 			-> 		'remove parameter...'.
				#selector 		-> 		#removeParameter.
				#icon 			-> 		#listRemoveIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		40.
				#label 			-> 		'push up'.
				#selector 		-> 		#pushUpSelector.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		50.
				#label 			-> 		'push down'.
				#selector 		-> 		#pushDownSelector.
				#icon 			-> 		#goBottomIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		60.
				#label 			-> 		'move to instance/class method'.
				#selector 		-> 		#moveToInstanceOrClassMethod.
				#icon 			-> 		#changesIcon
			} asDictionary.
	}`.

	! !


!BrowserWindow methodsFor: 'refactorings' stamp: 'HAW 5/21/2022 23:26:24'!
messageRefactoringMenu

	^DynamicMenuBuilder buildTitled: 'Refactorings' targeting: self collectingMenuOptionsWith: #messageRefactoringMenuOptions.! !

!methodRemoval: RefactoringMenues class #messsageRefactoringMenuOptions stamp: 'HAW 5/21/2022 23:26:24'!
RefactoringMenues class removeSelector: #messsageRefactoringMenuOptions!

!BrowserWindow reorganize!
('GUI building' buildLowerPanes buildMorphicClassColumnWith: buildMorphicClassList buildMorphicCommentPane buildMorphicMessageCatList buildMorphicMessageList buildMorphicSwitches buildMorphicSystemCategoryList buildMorphicWindow buildNoSysCatMorphicWindow createClassButton createCodePaneMorph createCommentButton createInstanceButton windowColor)
('menu building' addExtraMenu2ItemsTo: classListMenu classListMenu2 messageCategoryMenu messageListMenu messageListMenu2 systemCatSingletonMenu systemCategoryMenu)
('menu commands' browseAllClasses openSystemCategoryBrowser)
('keyboard shortcuts' messageListKey:from: systemCatListKey:from: systemCatSingletonKey:from:)
('updating' classAdded: classRenamed:from:to:inCategory: disableCodePaneEditing editSelectionChanged enableCodePaneEditing isEditSelectionNone update:)
('refactorings' addInstVar addParameter changeKeywordOrder classRefactoringMenu messageRefactoringMenu moveToInstanceOrClassMethod openClassRefactoringMenu openMessageRefactoringMenu pushDownInstanceVariable pushDownSelector pushUpInstanceVariable pushUpSelector removeAllUnreferencedInstVar removeInstVar removeParameter renameInstVar renameSelector)
('commands' findClass)
!

