'From Cuis 6.0 [latest update: #5889] on 3 July 2023 at 4:37:10 pm'!

!BrowserWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 7/3/2023 16:36:34'!
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
			aChar = $R ifTrue: [^ self renameSelector].
			aChar = $A ifTrue: [^ self addParameter ].
			aChar = $S ifTrue: [^ self removeParameter ].
			aChar = $F ifTrue: [^ self inlineMethod ]].
	super messageListKey: aChar from: view! !


!RefactoringMenues class methodsFor: 'browser menues' stamp: 'jmv 7/3/2023 16:36:28'!
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
				#selector 			-> 		#renameSelector.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#submenuOf -> 					RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		15.
				#label 			-> 		'change keyword order...'.
				#selector 			-> 		#changeKeywordOrder.
				#icon 			-> 		#sendReceiveIcon
			} asDictionary.
			{
				#submenuOf -> 					RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		20.
				#label 			-> 		'add parameter...(A)'.
				#selector 			-> 		#addParameter.
				#icon 			-> 		#listAddIcon
			} asDictionary.
			{
				#submenuOf -> 					RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		30.
				#label 			-> 		'remove parameter...(S)'.
				#selector 			-> 		#removeParameter.
				#icon 			-> 		#listRemoveIcon
			} asDictionary.
			{
				#submenuOf -> 					RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'inline method... (F)'.
				#selector 			-> 		#inlineMethod.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#submenuOf -> 					RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		20.
				#label 			-> 		'move to instance/class method'.
				#selector 			-> 		#moveToInstanceOrClassMethod.
				#icon 			-> 		#changesIcon
			} asDictionary.
			{
				#submenuOf -> 					RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		30.
				#label 			-> 		'push up'.
				#selector 			-> 		#pushUpSelector.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#submenuOf -> 					RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		40.
				#label 			-> 		'add in superclass as subclassResponsability'.
				#selector 			-> 		#addAsSubclassResponsabilitySelector.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#submenuOf -> 					RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		50.
				#label 			-> 		'push down to subclasses'.
				#selector 			-> 		#pushDownSelectorToSubclasses.
				#icon 			-> 		#goBottomIcon
			} asDictionary.
			{
				#submenuOf -> 					RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		60.
				#label 			-> 		'push down to one subclass'.
				#selector 			-> 		#pushDownSelectorToOneSubclass.
				#icon 			-> 		#goBottomIcon
			} asDictionary.
			
		}`.
	! !

