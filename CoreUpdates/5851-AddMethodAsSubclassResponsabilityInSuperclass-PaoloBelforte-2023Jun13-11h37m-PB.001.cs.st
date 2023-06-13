'From Cuis 6.0 [latest update: #5849] on 13 June 2023 at 11:45:26 am'!
!classDefinition: #AddAsSubclassResponsability category: #'Tools-Refactoring'!
Refactoring subclass: #AddAsSubclassResponsability
	instanceVariableNames: 'method'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #AddAsSubclassResponsabilityApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #AddAsSubclassResponsabilityApplier
	instanceVariableNames: 'method browser'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!BrowserWindow methodsFor: 'refactorings' stamp: 'PB 6/10/2023 10:21:05'!
addAsSubclassResponsabilitySelector

	model selectedMessageName ifNotNil: [ :selectedSelector |
		(AddAsSubclassResponsabilityApplier on: model for: model selectedClassOrMetaClass>>selectedSelector) value ].! !


!AddAsSubclassResponsability methodsFor: 'applying - private' stamp: 'PB 6/10/2023 09:56:42'!
implementSubclassResponsibility

	^ method methodClass compile: self subclassResponsibilitySourceCode! !

!AddAsSubclassResponsability methodsFor: 'applying - private' stamp: 'PB 6/10/2023 09:58:04'!
moveMethod

	method methodClass superclass
		compile: self subclassResponsibilitySourceCode 
		classified: self methodCategory! !

!AddAsSubclassResponsability methodsFor: 'applying - private' stamp: 'PB 6/10/2023 09:56:49'!
subclassResponsibilitySourceCode

	^ String streamContents: [ :stream |
		stream
			nextPutAll: method selectorAndArgumentsAsString;
			newLine; newLine;
			tab;
			nextPutAll: 'self ';
			nextPutAll: #subclassResponsibility asString ]! !

!AddAsSubclassResponsability methodsFor: 'initialization' stamp: 'PB 6/10/2023 10:24:01'!
initializeFor: aMethodToPushup

	method := aMethodToPushup.! !

!AddAsSubclassResponsability methodsFor: 'applying' stamp: 'PB 6/10/2023 09:55:04'!
apply

	self moveMethod! !

!AddAsSubclassResponsability methodsFor: 'applying' stamp: 'PB 6/10/2023 09:58:17'!
methodCategory

	^method methodClass organization categoryOfElement: method selector! !


!AddAsSubclassResponsability class methodsFor: 'instance creation' stamp: 'PB 6/10/2023 16:39:27'!
for: aMethod

	self warnIfExistMethodToAddAsSubclassResponsabilityOnSuperClass: aMethod.

	^self new initializeFor: aMethod! !

!AddAsSubclassResponsability class methodsFor: 'warnings' stamp: 'PB 6/10/2023 22:57:44'!
methodToAddAsSubclassResponsabilityExistOnSuperclassWarningMessage

	^'Method already exist in superclass'.

	! !

!AddAsSubclassResponsability class methodsFor: 'warnings' stamp: 'PB 6/10/2023 22:58:06'!
warnIfExistMethodToAddAsSubclassResponsabilityOnSuperClass: aMethod

	aMethod methodClass superclass 
		compiledMethodAt: aMethod selector
		ifPresent: [ :superclassMethod | (superclassMethod equivalentTo: aMethod)
			ifFalse: [ self refactoringWarning: self methodToAddAsSubclassResponsabilityExistOnSuperclassWarningMessage ]]
		ifAbsent: []! !


!AddAsSubclassResponsabilityApplier methodsFor: 'refactoring - creation' stamp: 'PB 6/10/2023 10:04:53'!
createRefactoring

	^self refactoringClass for: method! !

!AddAsSubclassResponsabilityApplier methodsFor: 'refactoring - creation' stamp: 'PB 6/10/2023 10:21:45'!
refactoringClass

	^AddAsSubclassResponsability ! !

!AddAsSubclassResponsabilityApplier methodsFor: 'refactoring - parameters request' stamp: 'PB 6/10/2023 10:05:04'!
implementsEquivalentMethod: aSubclass

	^ aSubclass ~= method methodClass and: [
		aSubclass 
			compiledMethodAt: method selector
			ifPresent: [ :subclassMethod | subclassMethod equivalentTo: method ]
			ifAbsent: [ false ]]! !

!AddAsSubclassResponsabilityApplier methodsFor: 'as yet unclassified' stamp: 'PB 6/10/2023 10:10:58'!
informChangesToBrowser

	browser
		reformulateList;
		changed: #messageList;
		setClassOrganizer! !

!AddAsSubclassResponsabilityApplier methodsFor: 'as yet unclassified' stamp: 'PB 6/10/2023 10:12:13'!
initializeOn: aBrowser for: aMethod

	browser := aBrowser.
	method := aMethod.! !

!AddAsSubclassResponsabilityApplier methodsFor: 'as yet unclassified' stamp: 'PB 6/10/2023 10:15:06'!
requestRefactoringParameters! !

!AddAsSubclassResponsabilityApplier methodsFor: 'as yet unclassified' stamp: 'PB 6/10/2023 10:10:47'!
showChanges

	self informChangesToBrowser.! !


!AddAsSubclassResponsabilityApplier class methodsFor: 'as yet unclassified' stamp: 'PB 6/10/2023 10:11:42'!
on: aBrowser for: aMethod

	^self new initializeOn: aBrowser for: aMethod ! !


!RefactoringMenues class methodsFor: 'browser menues' stamp: 'PB 6/10/2023 11:31:55'!
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
				#label 			-> 		'add in superclass as subclassResponsability'.
				#selector 		-> 		#addAsSubclassResponsabilitySelector.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		50.
				#label 			-> 		'push down to subclasses'.
				#selector 		-> 		#pushDownSelectorToSubclasses.
				#icon 			-> 		#goBottomIcon
			} asDictionary.
			{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		60.
				#label 			-> 		'push down to one subclass'.
				#selector 		-> 		#pushDownSelectorToOneSubclass.
				#icon 			-> 		#goBottomIcon
			} asDictionary.
			
		}`.
	! !


!AddAsSubclassResponsability class reorganize!
('instance creation' for:)
('warnings' methodToAddAsSubclassResponsabilityExistOnSuperclassWarningMessage warnIfExistMethodToAddAsSubclassResponsabilityOnSuperClass:)
!


!AddAsSubclassResponsabilityApplier reorganize!
('refactoring - creation' createRefactoring refactoringClass)
('refactoring - parameters request' implementsEquivalentMethod:)
('as yet unclassified' informChangesToBrowser initializeOn:for: requestRefactoringParameters showChanges)
!

