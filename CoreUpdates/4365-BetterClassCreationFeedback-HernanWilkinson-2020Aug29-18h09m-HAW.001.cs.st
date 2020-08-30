'From Cuis 5.0 [latest update: #4364] on 30 August 2020 at 6:56:19 pm'!

!CodeWindow methodsFor: 'updating' stamp: 'HAW 8/30/2020 18:28:24'!
classAdded: addedClass

	"Keep default behavior. Subclasses like BrowserWindow, redefine it - Hernan"
	self updateListsAndCode ! !


!BrowserWindow methodsFor: 'updating' stamp: 'HAW 8/30/2020 18:31:12'!
classAdded: addedClass

	self model selectedSystemCategoryName = addedClass category 
		ifTrue: [ self model changed: #classList ]! !


!Parser methodsFor: 'error correction' stamp: 'HAW 8/30/2020 18:33:15'!
correctVariable: proposedVariable interval: aSpot
	"Correct the proposedVariable to a known variable, or declare it as a new
	variable if such action is requested.  We support declaring lowercase
	variables as temps or inst-vars, and uppercase variables as Globals or 
	ClassVars, depending on whether the context is nil (class=UndefinedObject).
	Spot is the interval within the test stream of the variable.
	rr 3/4/2004 10:26 : adds the option to define a new class. "

	"Check if this is an i-var, that has been corrected already (ugly)"

	"Display the pop-up menu"

	| userSelection action delta spot |
	(encoder classEncoding instVarNames includes: proposedVariable) ifTrue: [
		^InstanceVariableNode new 
			name: proposedVariable
			index: (encoder classEncoding allInstVarNames indexOf: proposedVariable)].

	"First check to see if the requestor knows anything about the variable"
	(requestor bindingOf: proposedVariable)
		ifNotNil: [ :binding | ^ encoder global: binding name: proposedVariable].

	"If we can't ask the user for correction, make it undeclared"
	self interactive ifFalse: [^encoder undeclared: proposedVariable].

	userSelection _ requestor selectionInterval.
	delta _ self sourceDelta.
	spot _ aSpot first - delta to: aSpot last - delta.
	requestor selectFrom: spot first to: spot last.

	"Build the menu with alternatives"
	action _ UndeclaredVariable 
				signalFor: self
				name: proposedVariable
				inRange: spot.
	action ifNil: [^self fail].

	"Execute the selected action"
	requestor selectInvisiblyFrom: userSelection first to: userSelection last.
	^action value ifNil: [self fail].
	! !

!Parser methodsFor: 'error correction' stamp: 'HAW 8/30/2020 18:54:32'!
defineClass: className
	"prompts the user to define a new class,  
	asks for it's category, and lets the users edit further  
	the definition"
	| classNameAsSymbol classCategory classDefinition userClassDefinition newClass |
	
	classNameAsSymbol := className asSymbol.
	classCategory := self 
		request: 'Enter class category:' 
		initialAnswer: self encoder classEncoding theNonMetaClass category 
		orCancel: [ ^nil ].
	classCategory ifEmpty: [classCategory := 'Unknown'].
	
	classDefinition := 'Object subclass: #' , classNameAsSymbol , '
		instanceVariableNames: '''' 
		classVariableNames: ''''
		poolDictionaries: ''''
		category: ''' , classCategory , ''''.
	userClassDefinition := self 
		request: 'Edit class definition:' 
		initialAnswer: classDefinition 
		orCancel: [ ^nil ].
	userClassDefinition ifEmpty: [userClassDefinition := classDefinition].
	
	^[ newClass := Compiler evaluate: userClassDefinition.
	(newClass isKindOf: Behavior)
		ifTrue: [
			encoder
				global: (Smalltalk associationAt: classNameAsSymbol)
				name: classNameAsSymbol]
		ifFalse: [ 
			self inform: 
				('The provided class definition did not created a class but\the object: ', newClass printString) withNewLines.
			nil ]] 
		on: Error
		do: [ :anError | 
			self inform: ('There is an error in the provided class definition:\', anError description) withNewLines.
			anError return: nil ].
		
	! !


!CodeWindow methodsFor: 'notification actions' stamp: 'HAW 8/30/2020 18:27:15'!
registerNotificationActions

	SystemChangeNotifier uniqueInstance
		when: #classAdded send: #classAdded: to: self;
		when: #classCommented send: #updateListsAndCode to: self;
		when: #classDefinitionChanged send: #updateListsAndCode to: self;
		when: #classRecategorized send: #updateListsAndCode to: self;
		when: #classRemoved send: #updateListsAndCode to: self;
		when: #classRenamed send: #classRenamed:from:to:inCategory: to: self;
		when: #classReorganized send: #updateListsAndCode to: self;
		when: #methodAddedInProtocol send: #updateListsAndCode to: self;
		when: #methodChanged send: #updateListsAndCode to: self;
		when: #methodRemoved send: #updateListsAndCode to: self;
		when: #selectorRecategorized send: #updateListsAndCode to: self ! !


!BrowserWindow reorganize!
('GUI building' buildLowerPanes buildMorphicClassColumnWith: buildMorphicClassList buildMorphicCodePane buildMorphicCommentPane buildMorphicMessageCatList buildMorphicMessageList buildMorphicSwitches buildMorphicSystemCatList buildMorphicWindow buildNoSysCatMorphicWindow createClassButton createCommentButton createInstanceButton windowColor)
('menu building' addExtraMenu2ItemsTo: classListMenu classListMenu2 messageCategoryMenu messageListMenu messageListMenu2 systemCatSingletonMenu systemCategoryMenu)
('menu commands' browseAllClasses openSystemCategoryBrowser)
('keyboard shortcuts' systemCatListKey:from: systemCatSingletonKey:from:)
('updating' classAdded: classRenamed:from:to:inCategory: disableCodePaneEditing editSelectionChanged enableCodePaneEditing isEditSelectionNone update:)
('refactorings' addInstVar addParameter changeKeywordOrder classRefactoringMenu messageRefactoringMenu moveToInstanceOrClassMethod openClassRefactoringMenu openMessageRefactoringMenu pushDownInstanceVariable pushDownSelector pushUpInstanceVariable pushUpSelector removeAllUnreferencedInstVar removeInstVar removeParameter renameInstVar renameSelector)
('commands')
!

