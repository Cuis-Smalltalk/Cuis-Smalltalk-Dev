'From Cuis7.1 [latest update: #6421] on 31 May 2024 at 3:17:38 pm'!

!Browser methodsFor: 'refactorings' stamp: 'jmv 5/31/2024 11:57:35'!
addAsSubclassResponsabilitySelector

	self selectedMessageName ifNotNil: [ :selectedSelector |
		(AddAsSubclassResponsabilityApplier
			on: self for: self selectedClassOrMetaClass>>selectedSelector)
				value ].! !

!Browser methodsFor: 'refactorings' stamp: 'jmv 5/31/2024 11:24:58'!
addInstVar

	self selectedClassOrMetaClass ifNotNil: [ :aClass |
		(AddInstanceVariableApplier on: self for: aClass) value ].! !

!Browser methodsFor: 'refactorings' stamp: 'jmv 5/31/2024 11:58:41'!
addParameter

	self selectedMessageName ifNotNil: [ :oldSelector | 
		RefactoringApplier addParameterApplier
			createAndValueHandlingExceptionsOn: self for: oldSelector in: self selectedClassOrMetaClass ]! !

!Browser methodsFor: 'refactorings' stamp: 'jmv 5/31/2024 12:00:19'!
changeKeywordOrder

	self selectedMessageName ifNotNil: [ :oldSelector |
		ChangeKeywordsSelectorOrderApplier createAndValueHandlingExceptions: [
			ChangeKeywordsSelectorOrderApplier
				on: self for: oldSelector in: self selectedClassOrMetaClass ]].! !

!Browser methodsFor: 'refactorings' stamp: 'jmv 5/31/2024 12:00:47'!
inlineMethod

	self selectedMessageName ifNotNil: [ :selectorToInline |
		RefactoringApplier inlineMethodApplier
			createAndValueHandlingExceptionsOn: self
			forMethod: (MethodReference class: self selectedClassOrMetaClass selector: selectorToInline )]! !

!Browser methodsFor: 'refactorings' stamp: 'jmv 5/31/2024 12:01:50'!
moveToInstanceOrClassMethod

	self selectedMessageName ifNotNil: [ :selectedSelector |
		(MoveToInstanceOrClassMethodApplier
			on: self
			for: self selectedClassOrMetaClass>>selectedSelector) value ].! !

!Browser methodsFor: 'refactorings' stamp: 'jmv 5/31/2024 11:34:13'!
pushDownInstanceVariable

	self selectedClassOrMetaClass ifNotNil: [ :aClass |
		(PushDownInstanceVariableApplier on: self at: aClass ) value].! !

!Browser methodsFor: 'refactorings' stamp: 'jmv 5/31/2024 12:02:52'!
pushDownSelectorToOneSubclass

	self selectedMessageName ifNotNil: [ :selectedSelector |
		(PushDownMethodToOneSubclassApplier
			on: self for: self selectedClassOrMetaClass>>selectedSelector) value ].! !

!Browser methodsFor: 'refactorings' stamp: 'jmv 5/31/2024 12:03:36'!
pushDownSelectorToSubclasses

	self selectedMessageName ifNotNil: [ :selectedSelector |
		(PushDownMethodToSubclassesApplier
			on: self for: self selectedClassOrMetaClass>>selectedSelector) value ].! !

!Browser methodsFor: 'refactorings' stamp: 'jmv 5/31/2024 11:32:46'!
pushUpInstanceVariable

	self selectedClassOrMetaClass ifNotNil: [ :aClass |
		(PushUpInstanceVariableApplier on: self at: aClass ) value].! !

!Browser methodsFor: 'refactorings' stamp: 'jmv 5/31/2024 12:04:20'!
pushUpSelector

	self selectedMessageName ifNotNil: [ :selectedSelector |
		(PushUpMethodApplier
			on: self for: self selectedClassOrMetaClass>>selectedSelector)
				value ].! !

!Browser methodsFor: 'refactorings' stamp: 'jmv 5/31/2024 11:31:35'!
removeAllUnreferencedInstVar

	self selectedClassOrMetaClass ifNotNil: [ :aClass |
		(RemoveAllUnreferencedInstanceVariablesApplier on: self for: aClass) value ].! !

!Browser methodsFor: 'refactorings' stamp: 'jmv 5/31/2024 11:29:58'!
removeInstVar

	self selectedClassOrMetaClass ifNotNil: [ :aClass |
		(RemoveInstanceVariableApplier on: self for: aClass) value ].! !

!Browser methodsFor: 'refactorings' stamp: 'jmv 5/31/2024 12:05:37'!
removeParameter

	self selectedMessageName ifNotNil: [ :oldSelector |
		RefactoringApplier removeParameterApplier
			createAndValueHandlingExceptionsOn: self
			for: oldSelector
			in: self selectedClassOrMetaClass ].! !

!Browser methodsFor: 'refactorings' stamp: 'jmv 5/31/2024 11:28:11'!
renameInstVar

	self selectedClassOrMetaClass ifNotNil: [ :aClass |
		(RenameInstanceVariableApplier on: self at: aClass) value ].! !

!Browser methodsFor: 'refactorings' stamp: 'jmv 5/31/2024 12:06:22'!
renameSelector

	self selectedMessageName ifNotNil: [ :oldSelector |
		RefactoringApplier renameSelectorApplier
			createAndValueHandlingExceptionsOn: self
			for: oldSelector
			in: self selectedClassOrMetaClass ].! !


!BrowserWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 5/31/2024 12:06:28'!
messageListKey: aChar from: view
	"Respond to a Command key.  I am a model with a code pane, and I also
	have a listView that has a list of methods.  The view knows how to get
	the list and selection."

	| sel |
	self flag: #definesKeyboardShortcuts.
	sel := model selectedMessageName.
	sel
		ifNotNil: [
			"The following require a method selection"
			aChar = $c ifTrue: [ ^ self browseCategoryOfCurrentMethod ].
			aChar = $R ifTrue: [^ model renameSelector].
			aChar = $A ifTrue: [^ model addParameter ].
			aChar = $S ifTrue: [^ model removeParameter ].
			aChar = $F ifTrue: [^ model inlineMethod ]].
	super messageListKey: aChar from: view! !


!RefactoringMenues class methodsFor: 'browser menues' stamp: 'jmv 5/31/2024 11:33:49'!
classListMenuOptions

	self flag: #assumesKeyboardShortcuts.
	^ `{
			{
				#itemGroup 				-> 		5.
				#itemOrder 				-> 		10.
				#label 				-> 		RefactoringMenues refactoringsLabel.
				#selector 				-> 		#yourself.
				#icon 				-> 		#listAddIcon
			} asDictionary.
			{
				#submenuOf 				-> 		RefactoringMenues refactoringsLabel.
				#itemGroup 				-> 		10.
				#itemOrder 				-> 		20.
				#label 				-> 		'insert superclass...'.
				#object 				-> 		#model.
				#selector 				-> 		#insertSuperclass.
				#icon 				-> 		#saveAsIcon
			} asDictionary.
			{
				#submenuOf 				-> 		RefactoringMenues refactoringsLabel.
				#itemGroup 				-> 		20.
				#itemOrder 				-> 		10.
				#label 				-> 		'add inst var...'.
				#object 				-> 		#model.
				#selector 				-> 		#addInstVar.
				#icon 				-> 		#listAddIcon
			} asDictionary.
			{
				#submenuOf 				-> 		RefactoringMenues refactoringsLabel.
				#itemGroup 				-> 		20.
				#itemOrder 				-> 		20.
				#label 				-> 		'rename inst var...'.
				#object 				-> 		#model.
				#selector 				-> 		#renameInstVar.
				#icon 				-> 		#saveAsIcon
			} asDictionary.
			{
				#submenuOf 				-> 		RefactoringMenues refactoringsLabel.
				#itemGroup 				-> 		20.
				#itemOrder 				-> 		30.
				#label 				-> 		'remove inst var...'.
				#object 				-> 		#model.
				#selector 				-> 		#removeInstVar.
				#icon 				-> 		#deleteIcon
			} asDictionary.
			{
				#submenuOf 				-> 		RefactoringMenues refactoringsLabel.
				#itemGroup 				-> 		20.
				#itemOrder 				-> 		40.
				#label 				-> 		'remove all unreferenced inst var...'.
				#object 				-> 		#model.
				#selector 				-> 		#removeAllUnreferencedInstVar.
				#icon 				-> 		#deleteIcon
			} asDictionary.
			{
				#submenuOf 				-> 		RefactoringMenues refactoringsLabel.
				#itemGroup 				-> 		20.
				#itemOrder 				-> 		50.
				#label 				-> 		'push up inst var...'.
				#object 				-> 		#model.
				#selector 				-> 		#pushUpInstanceVariable.
				#icon 				-> 		#goTopIcon
			} asDictionary.
			{
				#submenuOf 				-> 		RefactoringMenues refactoringsLabel.
				#itemGroup 				-> 		20.
				#itemOrder 				-> 		60.
				#label 				-> 		'push down inst var...'.
				#object 				-> 		#model.
				#selector 				-> 		#pushDownInstanceVariable.
				#icon 				-> 		#goBottomIcon
			} asDictionary.				
			{
				#submenuOf 				-> 		RefactoringMenues refactoringsLabel.
				#itemGroup 				-> 		30.
				#itemOrder 				-> 		10.
				#label 				-> 		'rename global...'.
				#object 				-> 		#model.
				#selector 				-> 		#renameGlobal.
				#icon 				-> 		#saveAsIcon
			} asDictionary.
	}`.! !

!RefactoringMenues class methodsFor: 'browser menues' stamp: 'jmv 5/31/2024 12:06:33'!
messageListMenuOptions

	self flag: #assumesKeyboardShortcuts.
	^ `{
		{
			#itemGroup 			-> 		5.
			#itemOrder 			-> 		10.
			#label 			-> 		RefactoringMenues refactoringsLabel.
			#selector 			-> 		#yourself.
			#icon 			-> 		#listAddIcon
		} asDictionary.
			{
				#submenuOf 	-> 				RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		10.
				#label 			-> 		'rename... (R)'.
				#object 			-> 		#model.
				#selector 			-> 		#renameSelector.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#submenuOf -> 					RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		15.
				#label 			-> 		'change keyword order...'.
				#object 			-> 		#model.
				#selector 			-> 		#changeKeywordOrder.
				#icon 			-> 		#sendReceiveIcon
			} asDictionary.
			{
				#submenuOf -> 					RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		20.
				#label 			-> 		'add parameter...(A)'.
				#object 			-> 		#model.
				#selector 			-> 		#addParameter.
				#icon 			-> 		#listAddIcon
			} asDictionary.
			{
				#submenuOf -> 					RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		30.
				#label 			-> 		'remove parameter...(S)'.
				#object 			-> 		#model.
				#selector 			-> 		#removeParameter.
				#icon 			-> 		#listRemoveIcon
			} asDictionary.
			{
				#submenuOf -> 					RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'inline method... (F)'.
				#object 			-> 		#model.
				#selector 			-> 		#inlineMethod.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#submenuOf -> 					RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		20.
				#label 			-> 		'move to instance/class method'.
				#object 			-> 		#model.
				#selector 			-> 		#moveToInstanceOrClassMethod.
				#icon 			-> 		#changesIcon
			} asDictionary.
			{
				#submenuOf -> 					RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		30.
				#label 			-> 		'push up'.
				#object 			-> 		#model.
				#selector 			-> 		#pushUpSelector.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#submenuOf -> 					RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		40.
				#label 			-> 		'add in superclass as subclassResponsability'.
				#object 			-> 		#model.
				#selector 			-> 		#addAsSubclassResponsabilitySelector.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#submenuOf -> 					RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		50.
				#label 			-> 		'push down to subclasses'.
				#object 			-> 		#model.
				#selector 			-> 		#pushDownSelectorToSubclasses.
				#icon 			-> 		#goBottomIcon
			} asDictionary.
			{
				#submenuOf -> 					RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		60.
				#label 			-> 		'push down to one subclass'.
				#object 			-> 		#model.
				#selector 			-> 		#pushDownSelectorToOneSubclass.
				#icon 			-> 		#goBottomIcon
			} asDictionary.
			
		}`.
	! !

!methodRemoval: BrowserWindow #addParameter stamp: 'jmv 5/31/2024 15:15:46'!
BrowserWindow removeSelector: #addParameter!
!methodRemoval: BrowserWindow #pushUpInstanceVariable stamp: 'jmv 5/31/2024 15:15:50'!
BrowserWindow removeSelector: #pushUpInstanceVariable!
!methodRemoval: BrowserWindow #removeInstVar stamp: 'jmv 5/31/2024 15:15:52'!
BrowserWindow removeSelector: #removeInstVar!
!methodRemoval: BrowserWindow #renameSelector stamp: 'jmv 5/31/2024 15:15:54'!
BrowserWindow removeSelector: #renameSelector!
!methodRemoval: BrowserWindow #changeKeywordOrder stamp: 'jmv 5/31/2024 15:15:47'!
BrowserWindow removeSelector: #changeKeywordOrder!
!methodRemoval: BrowserWindow #removeParameter stamp: 'jmv 5/31/2024 15:15:52'!
BrowserWindow removeSelector: #removeParameter!
!methodRemoval: BrowserWindow #pushDownSelectorToSubclasses stamp: 'jmv 5/31/2024 15:15:50'!
BrowserWindow removeSelector: #pushDownSelectorToSubclasses!
!methodRemoval: BrowserWindow #addAsSubclassResponsabilitySelector stamp: 'jmv 5/31/2024 15:15:45'!
BrowserWindow removeSelector: #addAsSubclassResponsabilitySelector!
!methodRemoval: BrowserWindow #inlineMethod stamp: 'jmv 5/31/2024 15:15:47'!
BrowserWindow removeSelector: #inlineMethod!
!methodRemoval: BrowserWindow #renameInstVar stamp: 'jmv 5/31/2024 15:15:53'!
BrowserWindow removeSelector: #renameInstVar!
!methodRemoval: BrowserWindow #addInstVar stamp: 'jmv 5/31/2024 15:15:45'!
BrowserWindow removeSelector: #addInstVar!
!methodRemoval: BrowserWindow #pushUpSelector stamp: 'jmv 5/31/2024 15:15:51'!
BrowserWindow removeSelector: #pushUpSelector!
!methodRemoval: BrowserWindow #pushDownInstanceVariable stamp: 'jmv 5/31/2024 15:15:49'!
BrowserWindow removeSelector: #pushDownInstanceVariable!
!methodRemoval: BrowserWindow #pushDownSelectorToOneSubclass stamp: 'jmv 5/31/2024 15:15:49'!
BrowserWindow removeSelector: #pushDownSelectorToOneSubclass!
!methodRemoval: BrowserWindow #removeAllUnreferencedInstVar stamp: 'jmv 5/31/2024 15:15:51'!
BrowserWindow removeSelector: #removeAllUnreferencedInstVar!
!methodRemoval: BrowserWindow #moveToInstanceOrClassMethod stamp: 'jmv 5/31/2024 15:15:48'!
BrowserWindow removeSelector: #moveToInstanceOrClassMethod!

!BrowserWindow reorganize!
('GUI building' buildLowerPanes buildMorphicClassColumnWith: buildMorphicClassList buildMorphicCommentPane buildMorphicMessageCatList buildMorphicMessageList buildMorphicSwitches buildMorphicSystemCategoryList buildMorphicWindow buildNoSysCatMorphicWindow createClassButton createCodePaneMorph createCommentButton createInstanceButton windowColor)
('menu building' addExtraMenu2ItemsTo: classListMenu messageCategoryMenu messageListMenu messageListMenu2 openMessageListMenu openMessageListMenu2 systemCatSingletonMenu systemCategoryMenu)
('menu commands' browseAllClasses browseCategoryOfCurrentMethod openSystemCategoryBrowser)
('keyboard shortcuts' classListKey:from: messageCatListKey:from: messageListKey:from: systemCatListKey:from: systemCatSingletonKey:from:)
('updating' classAdded: classRenamed:from:to:inCategory: disableCodePaneEditing editSelectionChanged enableCodePaneEditing update:)
('commands' findClass)
!

