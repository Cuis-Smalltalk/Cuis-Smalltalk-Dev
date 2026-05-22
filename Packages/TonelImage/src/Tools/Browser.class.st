"
I represent a query path into the class descriptions, the software of the system.
"
Class {
	#name : 'Browser',
	#superclass : 'CodeProvider',
	#instVars : [
		'systemOrganizer',
		'classOrganizer',
		'metaClassOrganizer',
		'selectedSystemCategory',
		'selectedClassName',
		'selectedMessageCategory',
		'selectedMessage',
		'editSelection',
		'metaClassIndicated',
		'listClassesHierarchically'
	],
	#classVars : [
		'RecentClasses'
	],
	#category : 'Tools-Browser',
	#package : 'Tools',
	#tag : 'Browser'
}

{ #category : 'class list' }
Browser class >> disableListClassesHierarchically [
	
	^Preferences at: #listClassesHierarchically put: false
]

{ #category : 'class list' }
Browser class >> enableListClassesHierarchically [
	
	^Preferences at: #listClassesHierarchically put: true
]

{ #category : 'ui' }
Browser class >> open [
	| browser |
	browser := self new.
	^ BrowserWindow open: browser label: browser defaultBrowserTitle
]

{ #category : 'cached state access' }
Browser class >> recentClasses [
	RecentClasses ifNil: [
		RecentClasses := OrderedCollection new ].
	^RecentClasses
]

{ #category : 'cached state access' }
Browser class >> releaseClassCachedState [
	"
	Browser releaseClassCachedState
	"
	RecentClasses := nil
]

{ #category : 'shout styling' }
Browser >> aboutToStyleWith: anSHTextStyler [
	"This is a notification that anSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in anSHTextStyler, so that identifiers
	will be resolved correctly."

	anSHTextStyler classOrMetaClass:
		((#(editClass newClass) includes: self editSelection)  ifFalse:[
			self selectedClassOrMetaClass ]). "nil for class definitions"
]

{ #category : 'accessing' }
Browser >> acceptedStringOrText [
	"Depending on the current selection, different information is retrieved.
	Answer a string description of that information. This information is the
	method of the currently selected class and message."

	| comment theClass latestCompiledMethod |
	latestCompiledMethod := currentCompiledMethod.
	currentCompiledMethod := nil.

	editSelection == #none ifTrue: [^ ''].
	editSelection == #editSystemCategories 
		ifTrue: [^ systemOrganizer printString].
	self isEditingNewClass ifTrue: [
		selectedSystemCategory ifNil: [ ^'' ].
		^ (theClass := self selectedClass)
			ifNil: [
				Class template: selectedSystemCategory]
			ifNotNil: [
				Class templateForSubclassOf: theClass category: selectedSystemCategory]].
	self isEditingExistingClass 
		ifTrue: [^ self classDefinitionText ].
	editSelection == #editComment 
		ifTrue: [
			(theClass := self selectedClass) ifNil: [^ ''].
			comment := theClass smalltalkComment.
			currentCompiledMethod := theClass organization commentRemoteStr.
			^ comment isEmpty
				ifTrue: ['This class has not yet been commented.']
				ifFalse: [comment]].
	editSelection == #editMessageCategories 
		ifTrue: [^ self classOrMetaClassOrganizer printString].
	editSelection == #newMessage
		ifTrue: [
			^ (theClass := self selectedClassOrMetaClass) 
				ifNil: ['']
				ifNotNil: [theClass sourceCodeTemplate]].
	editSelection == #editMessage
		ifTrue: [
			self showingByteCodes ifTrue: [^ self selectedBytecodes].
			currentCompiledMethod := latestCompiledMethod.
			^ self selectedMessage].

	self error: 'Browser internal error: unknown edit selection.'
]

{ #category : 'refactorings' }
Browser >> addAsSubclassResponsibilitySelector [

	AddAsSubclassResponsibilityApplier applyFrom: self.
]

{ #category : 'message category functions' }
Browser >> addCategory [
	"Present a choice of categories or prompt for a new category name and add it before the current selection, or at the end if no current selection"

	| oldIndex newName |

	selectedClassName ifNil: [ ^self ].
	
	oldIndex := self messageCategoryListIndex.
	newName := self newMethodCategoryNameIfNone: [ ^self ].
	
	self classOrMetaClassOrganizer
		addCategory: newName
		before: selectedMessageCategory.
	self changed: #messageCategoryList.
	self messageCategoryListIndex:
		(oldIndex = 0
			ifTrue: [self classOrMetaClassOrganizer categories size + 1]
			ifFalse: [oldIndex]).
	self changed: #messageCategoryList.
	

]

{ #category : 'refactorings - category' }
Browser >> addCategoryAsSubclassResponsibilitySelectors [


	| methodsOfCategory |
			
	methodsOfCategory := self messageList.
	
	methodsOfCategory do: [ :selectedSelector | selectedSelector ifNotNil: 
			[(AddAsSubclassResponsibilityApplier on: self for: self selectedClassOrMetaClass>>selectedSelector) value ]
		].
]

{ #category : 'message category functions' }
Browser >> addExtensionCategory [
	"Present a choice package names and let the user enter a new category suffx.
	Take the suffix form the selected category."

	| packageNames packageNameIndex newCategoryName oldMessageCategoryListIndex |
	
	selectedClassName ifNil: [ ^self ].

	packageNames := (CodePackage installedPackages collect: [ :package |
		package packageName ]) sorted.
	packageNames ifEmpty: [ ^ self ].
	
	packageNameIndex := PopUpMenu 
		withCaption: 'Which package to extend?'
		chooseFrom: packageNames.
	packageNameIndex = 0 ifTrue: [ ^ self ].

	(newCategoryName := self 
		request: 'Enter the category name'
		initialAnswer: 
			(String streamContents: [ :stream |
				stream
					nextPut: $*;
					nextPutAll: (packageNames at: packageNameIndex).
				selectedMessageCategory ifNotNil: [
					stream
						nextPut: $-;
						nextPutAll: selectedMessageCategory ] ])) ifEmpty: [ ^ self ].
	(newCategoryName at: 1) = $*
		ifFalse: [
			self confirm:
				"I know about with #withNewlines ..."
				`'The category choosen does not start with an asterix (*).',
				String newLineString,
				'Do you want to use it anyway?'` :: ifFalse: [ ^ self ] ]
		ifTrue: [
			packageNames includes: newCategoryName allButFirst ::
				ifTrue: [
					self confirm:
						`'The name choosen denotes a package without a documenting suffix',
						String newLineString,
						'Do you want to use it anyway?'`:: ifFalse: [ ^ self ] ]
				ifFalse: [
					packageNames includes: (newCategoryName copyUpToLast: $-) allButFirst :: ifFalse: [
						self confirm:
							`'The category or prefix choosen does not match any package name.', 
							String newLineString,
							'Do you want to use it anyway?'` :: ifFalse: [ ^ self ] ] ] ].
	oldMessageCategoryListIndex := self messageCategoryListIndex.
		self classOrMetaClassOrganizer
			addCategory: newCategoryName
			before: selectedMessageCategory.
		self changed: #messageCategoryList.
		self messageCategoryListIndex:
			(oldMessageCategoryListIndex = 0
				ifTrue: [ self classOrMetaClassOrganizer categories size + 1 ]
				ifFalse: [ oldMessageCategoryListIndex ]).
		self changed: #messageCategoryList
]

{ #category : 'refactorings' }
Browser >> addInstVar [

	self selectedClassOrMetaClass ifNotNil: [ :aClass |
		(AddInstanceVariableApplier on: self for: aClass) value ].
]

{ #category : 'refactorings' }
Browser >> addParameter [

	RefactoringApplier addParameterApplier applyFrom: self
	
]

{ #category : 'system category functions' }
Browser >> addSystemCategory [
	"Prompt for a new category name and add it before the
	current selection, or at the end if no current selection"
	
	| newName |
	newName := self newSystemCategoryNameIfNone: [ ^self ].	
	systemOrganizer
		addCategory: newName
		before: selectedSystemCategory.
	self setSelectedSystemCategory: newName.
	self changed: #systemCategoryRoots
]

{ #category : 'shout styling' }
Browser >> allowsStylingFor: selector isTextDirty: hasUnsavedEdits [
	"Answer true to allow styling to proceed, or false to veto the styling"

	| type |
	self isModeStyleable ifFalse: [^false].
	type := self editSelection.
	^ #(newMessage editMessage editClass newClass) includes: type.
]

{ #category : 'message category functions' }
Browser >> alphabetizeMessageCategories [
	selectedClassName ifNil: [^ false].
	self classOrMetaClassOrganizer sortCategories.
	self changed: #clearUserEdits.
	self editClass.
	self classListIndex: self classListIndex.
	^ true
]

{ #category : 'system category functions' }
Browser >> alphabetizeSystemCategories [
	systemOrganizer sortCategories.
	self setSelectedSystemCategory: nil.
	self changed: #systemCategoryRoots
]

{ #category : 'annotation' }
Browser >> annotation [
	"Provide a line of content for an annotation pane, representing information about the method associated with the selected class and selector in the receiver."

	|  aSelector aClass |
	(aClass := self selectedClassOrMetaClass)
		ifNil: [
			self selectedSystemCategory ifNotNil: [ :sysCat |
				^self annotationForSystemCategory: sysCat ].
			^ ''].
	self editSelection == #editComment
		ifTrue: [^ self annotationForSelector: #Comment ofClass: aClass].
	self isEditingExistingClass 
		ifTrue: [^ self annotationForSelector: #Definition ofClass: aClass].
	(aSelector := self selectedMessageName)
		ifNil: [
			self selectedMessageCategoryName ifNotNil: [ :messageCategory |
				^self annotationForMessageCategory: messageCategory ofClass: aClass ].
			^ ''].
	^ self annotationForSelector: aSelector ofClass: aClass.
]

{ #category : 'class list' }
Browser >> basicClassList [
	"Answer an array of the class names of the selected category. Answer an 
	empty array if no selection exists."
	selectedSystemCategory ifNil: [^ #()].
	^ systemOrganizer fullListAtCategoryNamed: selectedSystemCategory
]

{ #category : 'message category list' }
Browser >> categorizeAllUncategorizedMethods [
	"Categorize methods by looking in parent classes for a method category."

	| organizer organizers |
	organizer := self classOrMetaClassOrganizer.
	organizers := self selectedClassOrMetaClass withAllSuperclasses collect: [:ea | ea organization].
	(organizer listAtCategoryNamed: ClassOrganizer defaultMethodCategory) do: [:sel | | found |
		found := (organizers collect: [ :org | org categoryOfElement: sel])
			detect: [:ea | ea ~= ClassOrganizer defaultMethodCategory and: [ ea notNil]]
			ifNone: nil.
		found ifNotNil: [organizer classify: sel under: found]].

	self changed: #messageCategoryList
]

{ #category : 'system category functions' }
Browser >> categorizeUnderCategory: systemCategoryName class: className [
	systemCategoryName isNil ifTrue: [^ self].
	(Smalltalk at: className asSymbol ifAbsent: [^ self]) category: systemCategoryName
]

{ #category : 'message category functions' }
Browser >> categorizeUnderCategoryAt: aMessageCategoryListIndex selector: aSelectorToCategorize [
	aMessageCategoryListIndex isNil ifTrue: [^ self categorizeUnderNewCategorySelector: aSelectorToCategorize].
	self selectedClassOrMetaClass ifNotNil: [ :class | | categorySelector |
		categorySelector := self messageCategoryList at: aMessageCategoryListIndex ifAbsent: [^self].
		categorySelector ~= Categorizer allCategory
			ifTrue: [
				class organization classify: aSelectorToCategorize under: categorySelector suppressIfDefault: false.
				self changed: #messageList]]
]

{ #category : 'message category functions' }
Browser >> categorizeUnderNewCategorySelector: aSelectorToCategorize [

	self selectedClassOrMetaClass ifNotNil: [ :class | | newCategory |
		newCategory := self newMethodCategoryNameIfNone: [ ^self ].
		class organization 
			addCategory: newCategory;
			classify: aSelectorToCategorize under: newCategory suppressIfDefault: false.
			
		self changed: #messageCategoryList.
		self changed: #messageList]
]

{ #category : 'message category functions' }
Browser >> categoryOfCurrentMethod [
	"Determine the method category associated with the receiver at the current moment, or nil if none"

	| category |
	^ super categoryOfCurrentMethod ifNil: [
		category := selectedMessageCategory.
		category == ClassOrganizer allCategory
			ifTrue: [nil]
			ifFalse: [category]]
]

{ #category : 'refactorings' }
Browser >> changeKeywordOrder [

	ChangeKeywordsSelectorOrderApplier applyFrom: self.
]

{ #category : 'message category functions' }
Browser >> changeMessageCategories: aString  [
	"The characters in aString represent an edited version of the the message 
	categories for the selected class. Update this information in the system 
	and inform any dependents that the categories have been changed. This 
	message is invoked because the user had issued the categories command 
	and edited the message categories. Then the user issued the accept 
	command."

	self classOrMetaClassOrganizer changeFromString: aString.
	self changed: #clearUserEdits.
	self editClass.
	self classListIndex: self classListIndex.
	^ true
]

{ #category : 'system category functions' }
Browser >> changeSystemCategories: aString  [
	"Update the class categories by parsing the argument aString."

	systemOrganizer changeFromString: aString.
	self changed: #systemCategoryRoots.
	^ true
]

{ #category : 'metaclass' }
Browser >> classCommentIndicated [
	"Answer true iff we're viewing the class comment."

	^ editSelection == #editComment 

]

{ #category : 'class functions' }
Browser >> classCommentText [
	"return the text to display for the comment of the currently selected class"
	| theClass |
	theClass := self selectedClassOrMetaClass.
	^ Text
		initialFont: (Preferences at: #standardCodeFont)
		stringOrText:
			((theClass notNil and: [ theClass hasComment ])
				ifTrue: [ theClass smalltalkComment ]
				ifFalse: [ '' ]).
]

{ #category : 'class functions' }
Browser >> classDefinitionText [
	"return the text to display for the definition of the currently selected class"
	
	^self selectedClassOrMetaClass 
		ifNil: [''] 
 		ifNotNil: [ :theClass | theClass definition]
]

{ #category : 'class list' }
Browser >> classList [

	^ listClassesHierarchically
		ifTrue: [self hierarchicalClassList]
		ifFalse: [self basicClassList].
]

{ #category : 'class list' }
Browser >> classListIndex [
	"Answer the index of the current class selection."

	^self classListIndexOf: selectedClassName 
]

{ #category : 'class list' }
Browser >> classListIndex: anInteger [
	"Set anInteger to be the index of the current class selection."

	| className recent canSelectClass classList prevSelectedClassName prevSelectedMessage |
	classList := self classList.
	canSelectClass := classList isInBounds: anInteger.
	prevSelectedClassName := selectedClassName.
	selectedClassName := canSelectClass ifTrue: [
		| newClassName |
		newClassName := classList at: anInteger ifAbsent: nil.
		newClassName := newClassName ifNotNil: [newClassName withoutLeadingBlanks asSymbol].
		newClassName ].
	self setClassOrganizer.
	prevSelectedMessage := selectedMessage.
	selectedMessage := nil.

	self classCommentIndicated ifFalse: [
		self editSelection: (canSelectClass
			ifTrue: [#editClass]
			ifFalse: [ metaClassIndicated | selectedSystemCategory isNil
				ifTrue: [#none]
				ifFalse: [#newClass]])].

	self selectedClass ifNotNil: [
		recent := self class recentClasses.
		className := self selectedClass name.
		(recent includes: className) ifTrue: [recent remove: className].
		recent addFirst: className.
		recent size > 16 ifTrue: [recent removeLast]].

	"Clear selectedMessageCategory if there is no match in the new list of categories"
	(self messageCategoryList indexOf: selectedMessageCategory) = 0 ifTrue: [
		selectedMessageCategory := nil].

	self changed: #classSelectionChanged.
	self changed: #classCommentText.
	self changed: #classListIndex.	"update my selection"
	self changed: #messageCategoryList.
	self changed: #messageList.
	self changed: #relabel.
	self acceptedContentsChanged.
	prevSelectedClassName = selectedClassName ifFalse: [
		"If clicked on already selected class, deselect selector, so class definition is shown"
		self setSelector: prevSelectedMessage ].
]

{ #category : 'class list' }
Browser >> classListIndexOf: classNameToFind [

	"Answer the index of the aClassName selection."

	classNameToFind ifNil: [ ^0 ].
	^self classList findFirst: [ :displayed | | start |
		"Works regardless of currently showing hierarchically or alphabetically."
		start := displayed firstNonSeparator.
		start-1 + classNameToFind size		 = 		displayed size and: [
			displayed is: classNameToFind substringAt: start ]]
]

{ #category : 'metaclass' }
Browser >> classMessagesIndicated [
	"Answer whether the messages to be presented should come from the metaclass."

	^ self metaClassIndicated and: [self classCommentIndicated not]
]

{ #category : 'metaclass' }
Browser >> classOrMetaClassOrganizer [
	"Answer the class organizer for the metaclass or class, depending on 
	which (instance or class) is indicated."

	self metaClassIndicated
		ifTrue: [^metaClassOrganizer]
		ifFalse: [^classOrganizer]
]

{ #category : 'self-updating' }
Browser >> classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName [

	| currentSelectedClass |
	
	selectedSystemCategory = aCategoryName ifTrue: [
		currentSelectedClass := self selectedClass.
		currentSelectedClass ifNil: [ 
			selectedClassName = oldClassName ifTrue: [ 
				currentSelectedClass := aClass ]].
	 	self changed: #classList.
		self selectClass: currentSelectedClass ].
]

{ #category : 'code pane' }
Browser >> compileMessage: aText notifying: aRequestor [
	"Compile the code that was accepted by the user, placing the compiled method into an appropriate message category.  Return true if the compilation succeeded, else false."

	"If no category (not even the '-- all --' category) is selected, select one."
	self selectedMessageCategoryName ifNil: [
		self selectOriginalCategoryForCurrentMethod 	
			ifFalse: [ "Select the '--all--' category"
				self messageCategoryListIndex: 1]]. 

	^(self defineMessageFrom: aText notifying: aRequestor) notNil.
]

{ #category : 'accessing' }
Browser >> contents: input notifying: aRequestor [
	"The retrieved information has changed and its source must now be
	 updated. The information can be a variety of things, depending on
	 the list selections (such as templates for class or message definition,
	 methods) or the user menu commands (such as definition, comment).
	Answer the result of updating the source."

	| codeString theClass |
	codeString := input string.
	editSelection == #editSystemCategories ifTrue: [
		^ self changeSystemCategories: codeString ].
	self isEditingClass ifTrue: [
		[
			self defineClass: codeString notifying: aRequestor 
		] on: RecompilationFailure do: [ :ex |
			self inform: ex messageText.
			^ false].
		^ true].
	editSelection == #editComment
		ifTrue: [
			theClass := self selectedClass.
			theClass
				ifNil: [
					self inform: 'You must select a class
before giving it a comment.'.
					^ false].
			theClass comment: codeString stamp: Utilities changeStamp.
			self changed: #classCommentText.
			^ true].
	editSelection == #editMessageCategories ifTrue: [ ^ self changeMessageCategories: codeString ].
	editSelection == #editMessage | (editSelection == #newMessage)
		ifTrue: [
			^ self okayToAccept
				ifFalse:[ false ]
				ifTrue: [
					(self compileMessage: codeString notifying: aRequestor)
						ifTrue: [ self triggerEvent: #annotationChanged ];
						yourself ]].
	editSelection == #none
		ifTrue: [
			self inform: 'This text cannot be accepted
in this part of the browser.'.
			^ false].
	self error: 'unacceptable accept'
]

{ #category : 'accessing' }
Browser >> contentsSelection [
	"Return the interval of text in the code pane to select when I set the pane's contents"

	^(selectedMessageCategory notNil and: [ selectedMessage isNil ])
		ifTrue: [ #all ]	"entire empty method template"
	
	"or null selection"
]

{ #category : 'class functions' }
Browser >> copyClass [
	| originalClass originalName copysName newDefinition newMetaDefinition newClass |
	selectedClassName ifNil: [^ self].
	originalClass := self selectedClass.
	originalName := originalClass name.
	self request: 'New class name?'
		initialAnswer: originalName
		verifying: [:aString| aString notEmpty and: [aString ~= originalName]]
		do: [:aString|
			copysName := aString asSymbol.
			(Smalltalk includesKey: copysName)
				ifTrue: [self error: copysName , ' already exists'].
			newDefinition := originalClass definition
				copyReplaceAll: originalName printString
				with: copysName printString.
			newClass := Compiler evaluate: newDefinition logged: true.
			newMetaDefinition := originalClass class definition
				copyReplaceAll: originalClass class name
				with: newClass class name.
			Compiler evaluate: newMetaDefinition logged: true.
			newClass copyAllCategoriesFrom: originalClass.
			newClass class copyAllCategoriesFrom: originalClass class.
			originalClass hasComment ifTrue: [newClass comment: originalClass smalltalkComment].
			self classListIndex: 0.
			self changed: #classList]
]

{ #category : 'class list' }
Browser >> createHierarchyTreeOf: classes [
	"Create a tree from a collection of classes"

	^ self createHierarchyTreeOf: classes comparingWith: [:potentialSuperClass :potentialSubClass | potentialSuperClass = potentialSubClass superclass].
]

{ #category : 'class list' }
Browser >> createHierarchyTreeOf: classes comparingWith: aBlockForComparingClasses [
	"Create a tree from a collection of classes. Keep ordering while doing it.
	The Each node in the tree is an Association with a class as key and a collection
	of child nodes as value. The first argument of the comparing block is the potential superclass 
	and the second the potential subclass."

	| classesWithSubclassIndexes |

	"For each class, collect the indexes of its included subclasses."
	classesWithSubclassIndexes := classes collect: [ :eachClass | 
		| subclasses subclassIndexes |
		subclasses := classes select: [ :class | aBlockForComparingClasses value: eachClass value: class].
		subclassIndexes := subclasses collect: [ :child | classes indexOf: child ].
		Association key: eachClass value: subclassIndexes ].

	"Now replace the indexes by the actual Associations, forming a tree.
	Remove them from the root collection."
	classesWithSubclassIndexes copy do: [ :classAndSubclassIndexes |
		classAndSubclassIndexes value: (classAndSubclassIndexes value collect: [ :idx | 
			| subclassAssociation |
			subclassAssociation := classesWithSubclassIndexes at: idx.
			classesWithSubclassIndexes at: idx put: nil.
			subclassAssociation ])].

	^ classesWithSubclassIndexes select: [ :ea | ea notNil ].

]

{ #category : 'class functions' }
Browser >> createInstVarAccessors [
	"Create getters and setters for all inst vars defined at the level of the current class selection, except do NOT clobber or override any selectors already understood by the instances of the selected class"
	self selectedClassOrMetaClass ifNotNil: [ :aClass |
		aClass instVarNames do: [ :aName | | newMessage setter |
			(aClass canUnderstand: aName asSymbol) ifFalse: [
				newMessage := aName , '
	"Answer the value of ' , aName , '"

	^ ' , aName.
				aClass
					compile: newMessage
					classified: Categorizer accessing ].
			(aClass canUnderstand: (setter := aName , ':') asSymbol) ifFalse: [
				newMessage := setter , ' anObject
	"Set the value of ' , aName , '"

	' , aName , ' := anObject'.
				aClass
					compile: newMessage
					classified: Categorizer accessing ]]]
]

{ #category : 'running tests' }
Browser >> debugMethodTest [
	
	currentCompiledMethod ifNotNil: [ 
		currentCompiledMethod isTestMethod ifTrue: [
			currentCompiledMethod methodClass debugAsFailure: currentCompiledMethod selector ]]

]

{ #category : 'initialization' }
Browser >> defaultBrowserTitle [
	^ 'System Browser'
]

{ #category : 'class functions' }
Browser >> defineClass: defString notifying: aRequestor   [
	"The receiver's textual content is a request to define a new class. The
	source code is defString. If any errors occur in compilation, notify
	aRequestor."
	| oldClass class newClassName defTokens keywdIx |
	oldClass := self selectedClassOrMetaClass.
	defTokens := defString findTokens: Character separators.
	keywdIx := defTokens findFirst: [ :x | x beginsWith: 'category' ].
	keywdIx := defTokens findFirst: [ :x | '*subclass*' match: x ].
	newClassName := (defTokens at: keywdIx+1) copyWithoutAll: '#()'.
	((oldClass isNil or: [ oldClass theNonMetaClass name asPlainString ~= newClassName ])
		and: [ Smalltalk includesKey: newClassName asSymbol ]) ifTrue: [
			"Attempting to define new class over existing one when
				not looking at the original one in this browser..."
			(self confirm: (newClassName bold , ' is an existing class in this system.
Redefining it might cause serious problems.
Is this really what you want to do?'))
				ifFalse: [ ^ false ]].
	"ar 8/29/1999: Use oldClass superclass for defining oldClass
	since oldClass superclass knows the definerClass of oldClass."
	oldClass ifNotNil: [ oldClass := oldClass superclass ].
	class := Compiler
				evaluate: defString
				notifying: aRequestor
				logged: true.
	(class isKindOf: Behavior)
		ifTrue: [
				self changed: #systemCategoryRoots.
				self changed: #classList.
				self changed: #clearUserEdits.
				self setClass: class selector: nil.
				"self clearUserEditFlag; editClass."
				^ true ]
		ifFalse: [ ^ false ]
]

{ #category : 'message functions' }
Browser >> defineMessageFrom: aString notifying: aRequestor [
	"Compile the expressions in aString. Notify aRequestor if a syntax error occurs. Install the compiled method in the selected class classified under  the currently selected message category name. Answer the selector obtained if compilation succeeds, nil otherwise."
	| selectedMessageName selector category oldMessageList scarySelector |
	selectedMessageName := self selectedMessageName.
	oldMessageList := self messageList.
	 self metaClassIndicated ifTrue: [
		scarySelector := self selectedClassOrMetaClass parserClass selectorFrom: aString.
		((self selectedClassOrMetaClass includesSelector: scarySelector) not
			and: [Metaclass isScarySelector: scarySelector])
			ifTrue: ["A frist-time definition overlaps the protocol of Metaclasses"
					(self confirm: (scarySelector bold, ' is used in the existing class system.
Overriding it could cause serious problems.
Is this really what you want to do?'))
					ifFalse: [^nil]]].
	category := self selectedMessageCategoryName ifNotNil: [ :n | n asSymbol ].
	category == ClassOrganizer allCategory ifTrue: [
		"Current category for existing methods or if a method was selected,
		nil for new methods when no previous method was selected."
		category := self categoryOfCurrentMethod ].
	selector := self selectedClassOrMetaClass
				compile: aString
				classified: category
				notifying: aRequestor.
	selector
		ifNil: [^ nil].
	selector ~~ selectedMessageName
		ifTrue: [
			self setClassOrganizer.  "In case organization not cached"
			(oldMessageList includes: selector)
				ifFalse: [self changed: #messageList].
			self messageListIndex: (self messageList indexOf: selector)].
	^ selector
]

{ #category : 'accessing' }
Browser >> doItReceiver [
	"This class's classPool has been jimmied to be the classPool of the class 
	being browsed. A doIt in the code pane will let the user see the value of 
	the class variables."

	^ self selectedClass
]

{ #category : 'class functions' }
Browser >> editClass [
	"Retrieve the description of the class definition."

	selectedClassName ifNil: [^ self].
	self messageCategoryListIndex: 0.
	self editSelection: #editClass.
	self acceptedContentsChanged.
	self changed: #classCommentText.

]

{ #category : 'class functions' }
Browser >> editComment [
	"Retrieve the description of the class comment."

	selectedClassName ifNil: [ ^self ].
	self messageCategoryListIndex: 0.
	metaClassIndicated := false.
	self editSelection: #editComment.
	self changed: #classSelectionChanged.
	self changed: #messageCategoryList.
	self changed: #messageList.
	self acceptedContentsChanged

]

{ #category : 'message category functions' }
Browser >> editMessageCategories [
	"Indicate to the receiver and its dependents that the message categories of 
	the selected class have been changed."

	selectedClassName ifNotNil: [
			self messageCategoryListIndex: 0.
			self editSelection: #editMessageCategories.
			self changed: #editMessageCategories.
			self acceptedContentsChanged ]
]

{ #category : 'accessing' }
Browser >> editSelection [
	^editSelection
]

{ #category : 'accessing' }
Browser >> editSelection: aSelection [
	"Set the editSelection as requested."

	editSelection := aSelection.
	self changed: #editSelection.
]

{ #category : 'system category functions' }
Browser >> editSystemCategories [
	"Retrieve the description of the class categories of the system organizer."

	self setSelectedSystemCategory: nil.
	self editSelection: #editSystemCategories.
	self changed: #editSystemCategories.
	self acceptedContentsChanged
]

{ #category : 'class list' }
Browser >> enableListClassesAlphabetically [

	self listClassesHierarchically: false
]

{ #category : 'class list' }
Browser >> enableListClassesHierarchically [

	self listClassesHierarchically: true
]

{ #category : 'class functions' }
Browser >> explainSpecial: string  [
	"Answer a string explaining the code pane selection if it is displaying 
	one of the special edit functions."

	| classes whole lits reply |
	self isEditingClass
		ifTrue: 
			["Selector parts in class definition"
			string last == $: ifFalse: [^nil].
			lits := Array with:
				#subclass:instanceVariableNames:classVariableNames:poolDictionaries:category:.
			(whole := lits detect: [:each | (each keywords
					detect: [:frag | frag = string] ifNone: nil) notNil]
						ifNone: nil) notNil
				ifTrue: [reply := '"' , string , ' is one part of the message selector ' , whole , '.']
				ifFalse: [^nil].
			classes := Smalltalk allClassesImplementing: whole.
			classes := 'these classes ' , classes printString.
			^reply , '  It is defined in ' , classes , '."
Smalltalk browseAllImplementorsOf: #' , whole].

	editSelection == #editSystemCategories ifTrue: [^nil].
	editSelection == #editMessageCategories ifTrue: [^nil].
	^nil
]

{ #category : 'class functions' }
Browser >> fileOutClass [
	"Print a description of the selected class onto a file whose name is the 
	class name followed by .st."

	selectedClassName ifNotNil: [ self selectedClass fileOut ]
]

{ #category : 'class functions' }
Browser >> fileOutClassAlphabetically [
	"Print a description of the selected class onto a file whose name is the 
	class name followed by .st.
	Sort methods alphabetically."

	selectedClassName ifNotNil: [ self selectedClass fileOutAlphabetically ]
]

{ #category : 'message category functions' }
Browser >> fileOutMessageCategories [
	"Print a description of the selected message category of the selected class 
	onto an external file."

	selectedMessageCategory ifNotNil: [
		self selectedClassOrMetaClass fileOutCategory:
			self selectedMessageCategoryName ]
]

{ #category : 'system category functions' }
Browser >> fileOutSystemCategory [
	"Print a description of each class in the selected category onto a file 
	whose name is the category name followed by .st."

	selectedSystemCategory
		ifNotNil: [ systemOrganizer fileOutCategory: selectedSystemCategory ]
]

{ #category : 'class list' }
Browser >> flattenHierarchyTree: classHierarchy on: col indent: indent [

	^ self
		flattenHierarchyTree: classHierarchy
		on: col
		indent: indent
		by: '  '.
]

{ #category : 'class list' }
Browser >> flattenHierarchyTree: classHierarchy on: col indent: indent by: indentChars [

	"Recursively add to col the names in classHierarchy indenting to show the hierarchical relationship. Use indentChars to do the indenting: spaces, tabs, etc."

	| plusIndent |

	plusIndent := indentChars.
	classHierarchy do: [:assoc |
		| class childs |
		class := assoc key.
		col add: indent , class name.
		childs := assoc value.
		self
			flattenHierarchyTree: childs
			on: col
			indent: indent , plusIndent
			by: indentChars].
	^ col
]

{ #category : 'class list' }
Browser >> hierarchicalClassList [
	"classNames are an arbitrary collection of classNames of the system.
	Reorder those class names so that they are sorted and indended by inheritance"

	| classes |
	classes := self basicClassList collect: [:sym | Smalltalk classNamed: sym].
	^ self
		flattenHierarchyTree: (self createHierarchyTreeOf: classes)
		on: OrderedCollection new
		indent: ''.

]

{ #category : 'system category list' }
Browser >> indexIsOne [
	"When used as a singleton list, index is always one"
	^ 1
]

{ #category : 'system category list' }
Browser >> indexIsOne: value [
	"When used as a singleton list, can't change it"

	^ self
]

{ #category : 'metaclass' }
Browser >> indicateClassMessages [
	"Indicate that the message selection should come from the metaclass 
	messages."

	self metaClassIndicated: true
]

{ #category : 'metaclass' }
Browser >> indicateInstanceMessages [
	"Indicate that the message selection should come from the class (instance) 
	messages."

	self metaClassIndicated: false
]

{ #category : 'initialization' }
Browser >> initialize [
	super initialize.
	self systemOrganizer: SystemOrganization.
	listClassesHierarchically := Preferences at: #listClassesHierarchically
]

{ #category : 'refactorings' }
Browser >> inlineMethod [

	RefactoringApplier inlineMethodApplier applyFrom: self
]

{ #category : 'refactorings' }
Browser >> insertSuperclass [

	self selectedClassOrMetaClass ifNotNil: [ :aBehavior |
		(InsertSuperclassApplier on: self for: aBehavior theNonMetaClass) value ].
]

{ #category : 'metaclass' }
Browser >> instanceMessagesIndicated [
	"Answer whether the messages to be presented should come from the 
	class."

	^metaClassIndicated not and: [self classCommentIndicated not]
]

{ #category : 'testing' }
Browser >> is: aSymbol [

	^ aSymbol == #Browser or: [ super is: aSymbol ]
]

{ #category : 'testing' }
Browser >> isEditingClass [

	^self isEditingExistingClass or: [ self isEditingNewClass ]
]

{ #category : 'testing' }
Browser >> isEditingExistingClass [

	^editSelection == #editClass
]

{ #category : 'testing' }
Browser >> isEditingExistingMethod [

	^editSelection = #editMessage
]

{ #category : 'testing' }
Browser >> isEditingMethod [

	^editSelection = #editMessage or: [ editSelection = #newMessage ]

]

{ #category : 'testing' }
Browser >> isEditingNewClass [

	^editSelection == #newClass 
]

{ #category : 'testing' }
Browser >> isEditingNone [

	^self editSelection = #none
]

{ #category : 'initialization' }
Browser >> labelString [
	^self selectedClassOrMetaClass
		ifNil: [ self defaultBrowserTitle ]
		ifNotNil: [ :c |
			self selectedMessageName
				ifNil: [
					self selectedMessageCategoryName
						ifNil: [ self defaultBrowserTitle, ': ', c printString ]
						ifNotNil: [ :cat | self defaultBrowserTitle, ': ', c printString, ' - ', cat ]]
				ifNotNil: [ :m |
					self defaultBrowserTitle, ': ', c printString, ' >> ', m printString ]].


]

{ #category : 'class list' }
Browser >> listClassesHierarchically: aBoolean [

	listClassesHierarchically := aBoolean.
	
	self changed: #classList.
	self changed: #classListIndex.

]

{ #category : 'class functions' }
Browser >> makeNewSubclass [

	self selectedClassOrMetaClass ifNil: [^ self].
	self editSelection: #newClass.
	self acceptedContentsChanged
]

{ #category : 'message category list' }
Browser >> messageCategoryList [
	"Answer the selected category of messages."
	^selectedClassName
		ifNil: [ #() ]
		ifNotNil: [ (Array with: ClassOrganizer allCategory), self rawMessageCategoryList ]
]

{ #category : 'message category list' }
Browser >> messageCategoryListIndex [
	"Answer the index of the selected message category."


	selectedMessageCategory ifNil: [ ^0 ].
	^self messageCategoryList indexOf: selectedMessageCategory
]

{ #category : 'message category list' }
Browser >> messageCategoryListIndex: anInteger [
	"Set the selected message category to be the one indexed by anInteger."

	| index messageCategoryList |
	
	messageCategoryList := self messageCategoryList.
	index := messageCategoryList ifInBounds: anInteger ifNot: 0.

	selectedMessageCategory := index = 0 ifFalse: [messageCategoryList at: index ].
	selectedMessage := nil.
	self changed: #messageCategorySelectionChanged.
	self changed: #messageCategoryListIndex. "update my selection"
	self changed: #messageList.
	self editSelection: (index > 0
		ifTrue: [#newMessage]
		ifFalse: [self classListIndex > 0
			ifTrue: [#editClass]
			ifFalse: [#newClass]]).
	self changed: #relabel.
	self acceptedContentsChanged.
]

{ #category : 'message list' }
Browser >> messageList [
	"Answer an Array of the message selectors of the currently selected message category, provided that the messageCategoryListIndex is in proper range.  Otherwise, answer an empty Array  If messageCategoryListIndex is found to be larger than the number of categories (it happens!), it is reset to zero."
	| classOrMetaClassOrganizer answer |
	classOrMetaClassOrganizer := self classOrMetaClassOrganizer.
	classOrMetaClassOrganizer isNil ifTrue: [ ^#() ].
	answer := (selectedMessageCategory isNil or: [ selectedMessageCategory == ClassOrganizer allCategory ])
		ifTrue: [classOrMetaClassOrganizer allMethodSelectors]
		ifFalse: [classOrMetaClassOrganizer listAtCategoryNamed: selectedMessageCategory].	
	answer isNil ifTrue: [
		selectedMessageCategory := nil.
		answer := #() ].
	^answer
]

{ #category : 'message list' }
Browser >> messageListIndex [
	"Answer the index of the selected message selector into the currently 
	selected message category."

	selectedMessage ifNil: [ ^0 ].
	^self messageList indexOf: selectedMessage
]

{ #category : 'message list' }
Browser >> messageListIndex: anInteger [
	"Set the selected message selector to be the one indexed by anInteger."
	
	| index messageList |
	
	messageList := self messageList.
	index := messageList ifInBounds: anInteger ifNot: 0.

	selectedMessage := index = 0 ifFalse: [ (messageList at: index) string ].
	self editSelection: (index > 0
		ifTrue: [#editMessage]
		ifFalse: [self messageCategoryListIndex > 0
			ifTrue: [#newMessage]
			ifFalse: [self classListIndex > 0
				ifTrue: [#editClass]
				ifFalse: [#newClass]]]).
	self changed: #messageListIndex. "update my selection"
	self changed: #relabel.
	self acceptedContentsChanged
]

{ #category : 'metaclass' }
Browser >> metaClassIndicated [
	"Answer the boolean flag that indicates which of the method dictionaries, 
	class or metaclass."

	^metaClassIndicated
]

{ #category : 'metaclass' }
Browser >> metaClassIndicated: trueOrFalse  [
	"Indicate whether browsing instance or class messages."

	metaClassIndicated := trueOrFalse.
	self setClassOrganizer.
	selectedSystemCategory ifNotNil: [
		self editSelection: (selectedClassName
			ifNil: [metaClassIndicated
				ifTrue: [#none]
				ifFalse: [#newClass]]
			ifNotNil: [#editClass])].
	selectedMessageCategory := nil.
	selectedMessage := nil.
	self changed: #classSelectionChanged.
	self changed: #messageCategoryList.
	self changed: #messageList.
	self changed: #instanceMessagesIndicated.
	self changed: #classCommentIndicated.
	self changed: #classMessagesIndicated.
	self acceptedContentsChanged
]

{ #category : 'initialization' }
Browser >> methodCategoryChanged [
	self changed: #messageCategoryList.
	self changed: #messageList.
	self triggerEvent: #annotationChanged.
	self reformulateList
]

{ #category : 'system category functions' }
Browser >> moveAllToOtherSystemCategory [
	"If a class category is selected, prompt user for category to move to,
	create a Confirmer so the user can verify that all the classes in current category
 	should be moved to the selected category."
	| newSystemCategory |
	selectedSystemCategory ifNil: [ ^ self ].
	newSystemCategory := Smalltalk systemCategoryFromUserWithPrompt: 'Move classes to System Category...'.
	(newSystemCategory notNil and: [
		self classList notEmpty and: [ self confirm: 'Are you sure you want to
move classes from ' , selectedSystemCategory , ' 
to ' , newSystemCategory , '?' ]]) ifTrue: [
		"Safer this way (#classList will be a collection of strings with spaces and who knows what in the future.  So let's just get the classes we need directly)"
		(SystemOrganization classesAt: selectedSystemCategory) do: [ :eaClass |
			eaClass category: newSystemCategory ].
		self changed: #systemCategoryRoots ].
]

{ #category : 'system category functions' }
Browser >> moveSystemCategoryBottom [
	selectedSystemCategory ifNil: [^ self].
	systemOrganizer moveCategoryBottom: selectedSystemCategory.
	self changed: #systemCategoryRoots
]

{ #category : 'system category functions' }
Browser >> moveSystemCategoryDown [
	selectedSystemCategory ifNil: [^ self].
	systemOrganizer moveCategoryDown: selectedSystemCategory.
	self changed: #systemCategoryRoots
]

{ #category : 'system category functions' }
Browser >> moveSystemCategoryTop [
	selectedSystemCategory ifNil: [^ self].
	systemOrganizer moveCategoryTop: selectedSystemCategory.
	self changed: #systemCategoryRoots
]

{ #category : 'system category functions' }
Browser >> moveSystemCategoryUp [
	selectedSystemCategory ifNil: [^ self].
	systemOrganizer moveCategoryUp: selectedSystemCategory.
	self changed: #systemCategoryRoots
]

{ #category : 'refactorings' }
Browser >> moveToInstanceOrClassMethod [

	MoveToInstanceOrClassMethodApplier applyFrom: self
]

{ #category : 'class comment pane' }
Browser >> newClassComment: aText [
	"The user has just entered aText.
	It may be all red (a side-effect of replacing the default comment), so remove the color if it is."
	| theClass |
	theClass := self selectedClassOrMetaClass theNonMetaClass.
	theClass ifNotNil: [
		theClass classComment: aText asPlainString ].
	self changed: #classCommentText.
	^ true
]

{ #category : 'message category functions' }
Browser >> newMethodCategoryNameIfNone: aNoneBlock [
	
	^(MethodCategoriesPrompter 
		staringFrom: self selectedClassOrMetaClass 
		rejectingFirst: true) prompt: 'Add Category' ifNone: aNoneBlock
]

{ #category : 'system category functions' }
Browser >> newSystemCategoryNameIfNone: aNoneBlock [

	| newName |
	
	newName := self
		request: 'New category name?'
		initialAnswer: self newSystemCategoryNameInitialAnswer.
	
	^newName isEmpty
		ifTrue: aNoneBlock 
		ifFalse: [newName asSymbol].
]

{ #category : 'system category functions' }
Browser >> newSystemCategoryNameInitialAnswer [
	"Make a suggestion for a new sytems category.
	
	Can be redefined in subclasses to provide something meore meaningfull."

	^ 'Category-Name'.

]

{ #category : 'class functions' }
Browser >> plusButtonHit [
	"Cycle among definition and comment."

	editSelection == #editComment
		ifTrue: [
			self editSelection: #editClass.
			selectedClassName ifNil: [ ^self ].
			self changed: #editComment.
			self acceptedContentsChanged.
			^ self].
	self editComment.
	self changed: #instanceMessagesIndicated.
	self changed: #classCommentIndicated.
	self changed: #classMessagesIndicated.
]

{ #category : 'system category functions' }
Browser >> potentialClassNames [
	"Answer the names of all the classes that could be viewed in this browser.  This hook is provided so that HierarchyBrowsers can indicate their restricted subset.  For generic Browsers, the entire list of classes known to Smalltalk is provided, though of course that really only is accurate in the case of full system browsers."

	^ Smalltalk classNames
]

{ #category : 'refactorings - category' }
Browser >> pushDownCategorySelectorsToOneSubclass [
	
	self messageList do: [ :selectedSelector | selectedSelector ifNotNil: 
		[(PushDownMethodToOneSubclassApplier on: self for: self selectedClassOrMetaClass>>selectedSelector) value ]
	].
	
	self removeMessageCategory.
]

{ #category : 'refactorings - category' }
Browser >> pushDownCategorySelectorsToSubclasses [
	
		self messageList do: [ :selectedSelector | selectedSelector ifNotNil: 
			[(PushDownMethodToSubclassesApplier on: self for: self selectedClassOrMetaClass>>selectedSelector) value] 
		].
	
		self removeMessageCategory.

]

{ #category : 'refactorings' }
Browser >> pushDownInstanceVariable [

	self selectedClassOrMetaClass ifNotNil: [ :aClass |
		(PushDownInstanceVariableApplier on: self at: aClass ) value].
]

{ #category : 'refactorings' }
Browser >> pushDownSelectorToOneSubclass [
	
	PushDownMethodToOneSubclassApplier applyFrom: self.
]

{ #category : 'refactorings' }
Browser >> pushDownSelectorToSubclasses [
	
	PushDownMethodToSubclassesApplier applyFrom: self.
]

{ #category : 'refactorings - category' }
Browser >> pushUpCategorySelectors [

	| methodsOfCategory |

	methodsOfCategory := self messageList.
	
	methodsOfCategory do: [:selectedSelector |  (PushUpMethodApplier on: self for: self selectedClassOrMetaClass>>selectedSelector) value ].
	
	self removeMessageCategory 

]

{ #category : 'refactorings' }
Browser >> pushUpInstanceVariable [

	self selectedClassOrMetaClass ifNotNil: [ :aClass |
		(PushUpInstanceVariableApplier on: self at: aClass ) value].
]

{ #category : 'refactorings' }
Browser >> pushUpSelector [

	PushUpMethodApplier applyFrom: self.
]

{ #category : 'message category list' }
Browser >> rawMessageCategoryList [

	^ selectedClassName
		ifNil: [ #() ]
		ifNotNil: [ self classOrMetaClassOrganizer categories ]
]

{ #category : 'class list' }
Browser >> recent [
	"Let the user select from a list of recently visited classes.  11/96 stp.
	 12/96 di:  use class name, not classes themselves.
	 : dont fall into debugger in empty case"

	| className class recentList |
	recentList := self class recentClasses select: [:n | Smalltalk includesKey: n].
	recentList isEmpty ifTrue: [^ Smalltalk beep].
	className := (SelectionMenu selections: recentList) startUpMenu.
	className ifNil: [^ self].
	class := Smalltalk at: className.
	self setSelectedSystemCategory: class category.
	self selectClass: class
]

{ #category : 'message list' }
Browser >> reformulateList [
	"If the receiver has a way of reformulating its message list, here is a chance for it to do so"

	self messageListIndex: 0
]

{ #category : 'refactorings' }
Browser >> removeAllUnreferencedInstVar [

	self selectedClassOrMetaClass ifNotNil: [ :aClass |
		(RemoveAllUnreferencedInstanceVariablesApplier on: self for: aClass) value ].
]

{ #category : 'refactorings' }
Browser >> removeClass [

	self safelyRemoveClass 
]

{ #category : 'message category functions' }
Browser >> removeEmptyCategories [
	self selectedClassOrMetaClass organization removeEmptyCategories.
	self changed: #messageCategoryList

]

{ #category : 'refactorings' }
Browser >> removeInstVar [

	self selectedClassOrMetaClass ifNotNil: [ :aClass |
		(RemoveInstanceVariableApplier on: self for: aClass) value ].
]

{ #category : 'message functions' }
Browser >> removeMessage [
	"If a message is selected, create a Confirmer so the user can verify that  
	the currently selected message should be removed from the system. If 
	so,  
	remove it. If the Preference 'confirmMethodRemoves' is set to false, the 
	confirmer is bypassed."
	| messageName confirmation |
	selectedMessage ifNil: [ ^self ].
	messageName := self selectedMessageName.
	confirmation := Smalltalk confirmRemovalOf: messageName on: self selectedClassOrMetaClass.
	confirmation = 3
		ifTrue: [^ self].
	self selectedClassOrMetaClass removeSelector: self selectedMessageName.
	self reformulateList.
	self changed: #messageList.
	self setClassOrganizer.
	"In case organization not cached"
	confirmation = 2
		ifTrue: [Smalltalk browseAllCallsOn: messageName]
]

{ #category : 'message category functions' }
Browser >> removeMessageCategory [
	"If a message category is selected, create a Confirmer so the user can 
	verify that the currently selected message category should be removed
 	from the system. If so, remove it."

	| messageCategoryName |
	selectedMessageCategory ifNil: [^ self].
	messageCategoryName := self selectedMessageCategoryName.
	(self messageList isEmpty
		or: [self confirm: 'Are you sure you want to
remove this method category 
and all its methods?'])
		ifTrue: [
			self selectedClassOrMetaClass removeCategory: messageCategoryName.
			self messageCategoryListIndex: 0.
			self changed: #classSelectionChanged].
	self changed: #messageCategoryList.

]

{ #category : 'message functions' }
Browser >> removeMessageFromBrowser [
	"Our list speaks the truth and can't have arbitrary things removed"

	^ self changed: #flash
]

{ #category : 'message functions' }
Browser >> removeMessageInHierarchy [
	"If a message is selected, create a Confirmer so the user can verify that  
	the currently selected message should be removed from the system. If so,  
	remove it. If the Preference 'confirmMethodRemoves' is set to false, the 
	confirmer is bypassed."
	
	| messageName confirmation selectedClass |
	selectedMessage ifNil: [ ^self ].
	
	messageName := self selectedMessageName.
	selectedClass := self selectedClassOrMetaClass.
	confirmation := Smalltalk confirmRemovalOf: messageName on: selectedClass andSubclasses: true.
	confirmation = 3
		ifTrue: [^ self].

	selectedClass withAllSubclassesDo: [ :eachClass | eachClass removeSelector: messageName ].
	
	self reformulateList.
	self changed: #messageList.
	self setClassOrganizer.
	confirmation = 2
		ifTrue: [Smalltalk browseAllCallsOn: messageName]
]

{ #category : 'refactorings' }
Browser >> removeParameter [

	RefactoringApplier removeParameterApplier applyFrom: self
	
]

{ #category : 'system category functions' }
Browser >> removeSystemCategory [
	"If a class category is selected, create a Confirmer so the user can 
	verify that the currently selected class category and all of its classes
 	should be removed from the system. If so, remove it."

	selectedSystemCategory ifNil: [^ self].
	(self classList isEmpty
		or: [self confirm: 'Are you sure you want to
remove this system category 
and all its classes?'])
		ifTrue: [
			systemOrganizer removeSystemCategory: selectedSystemCategory.
			self setSelectedSystemCategory: nil.
			self changed: #systemCategoryRoots ]
]

{ #category : 'message category functions' }
Browser >> renameCategory [
	"Prompt for a new category name and add it before the
	current selection, or at the end if no current selection"
	| oldIndex oldName newName |
	selectedClassName ifNil: [^ self].
	selectedMessageCategory ifNil: [ ^self ].
	oldIndex := self messageCategoryListIndex.
	oldName := self selectedMessageCategoryName.
	newName := self
		request: 'New category name?'
		initialAnswer: oldName
		verifying: [:aString| aString notEmpty and: [aString ~= oldName]]
		do: [:aString|
			newName := aString asSymbol.
			self classOrMetaClassOrganizer
				renameCategory: oldName
				to: newName.
			self classListIndex: self classListIndex.
			self messageCategoryListIndex: oldIndex.
			self changed: #messageCategoryList]
]

{ #category : 'refactorings' }
Browser >> renameClass [

	self selectedClassOrMetaClass ifNotNil: [ :aBehavior |
		(RenameClassApplier for: aBehavior theNonMetaClass) value ].
]

{ #category : 'refactorings' }
Browser >> renameGlobal [
	
	(RenameGlobalApplier on: self for: '') value
]

{ #category : 'refactorings' }
Browser >> renameInstVar [

	self selectedClassOrMetaClass ifNotNil: [ :aClass |
		(RenameInstanceVariableApplier on: self at: aClass) value ].
]

{ #category : 'refactorings' }
Browser >> renameSelector [

	RefactoringApplier renameSelectorApplier applyFrom: self
	
]

{ #category : 'system category functions' }
Browser >> renameSystemCategory [
	"Prompt for a new category name and add it before the
	current selection, or at the end if no current selection
	
	21-Mar-2012 jmv Note: This is not recorded appropriately in change sets.
	The easiest solution is to trigger #classRecategorized for all classes in the category.
	But this is not a real solution, as the resulting changeset would not do a rename,
	but create a new category (that would go to the bottom) with all the classes.
	
	In the meantime, disable the menu entry. This is not so important after all.
	"
	
	selectedSystemCategory ifNil: [^ self].  "no selection"
	self renameSystemCategoryNamed: selectedSystemCategory
]

{ #category : 'system category functions' }
Browser >> renameSystemCategoryNamed: oldName [

	^ self
		request: 'New category name?'
		initialAnswer: oldName
		verifying: [:aString| aString notEmpty and: [aString ~= oldName]]
		do: [:aString| | newName |
			newName := aString asSymbol.
			systemOrganizer renameSystemCategory: oldName to: newName.
			self setSelectedSystemCategory: newName.
			self changed: #systemCategoryRoots]
]

{ #category : 'running tests' }
Browser >> runClassTests [

	self selectedClass ifNotNil: [ :selectedClass |
		(ProgressiveTestRunner for: (TestSuite forClass: selectedClass)) value ].

]

{ #category : 'running tests' }
Browser >> runMessageCategoryTests [
	
	selectedMessageCategory ifNotNil: [ | selectedClass suite |
		selectedClass := Smalltalk classNamed: selectedClassName.
		suite := TestSuite forMessageCategoryNamed: selectedMessageCategory of: selectedClass categorizedWith: classOrganizer.
		(ProgressiveTestRunner for: suite) value ]
	
	
]

{ #category : 'running tests' }
Browser >> runMethodTest [
	
	currentCompiledMethod ifNotNil: [ | suite |
		suite := TestSuite forCompiledMethod: currentCompiledMethod.
		(ProgressiveTestRunner for: suite) value ]
		
]

{ #category : 'running tests' }
Browser >> runSystemCategoryTests [
	
	selectedSystemCategory ifNotNil: [ | suite |
		suite := TestSuite forSystemCategoryNamed: selectedSystemCategory using: systemOrganizer.
		(ProgressiveTestRunner for: suite) value ]
	
	
]

{ #category : 'refactorings' }
Browser >> safelyRemoveClass [

	self selectedClassOrMetaClass ifNotNil: [ :aBehavior |
		(SafelyRemoveClassApplier on: self of: aBehavior theNonMetaClass) value ].
]

{ #category : 'class list' }
Browser >> selectClass: classNotMeta [

	self classListIndex: (self classListIndexOf: classNotMeta name)
]

{ #category : 'message category list' }
Browser >> selectOriginalCategoryForCurrentMethod [
	"private - Select the message category for the current method. 
	 
	 Note:  This should only be called when somebody tries to save  
	 a method that they are modifying while ALL is selected. 
	 
	 Returns: true on success, false on failure."
	| aSymbol selectorName |
	aSymbol := self categoryOfCurrentMethod.
	selectorName := self selectedMessageName.
	(aSymbol notNil and: [aSymbol ~~ ClassOrganizer allCategory])
		ifTrue: [
			selectedMessageCategory := aSymbol.
			selectedMessage := selectorName.
			self changed: #messageCategorySelectionChanged.
			self changed: #messageCategoryListIndex.	"update my selection"
			self changed: #messageList.
			self changed: #messageListIndex.
			^ true].
	^ false
]

{ #category : 'class list' }
Browser >> selectedClass [
	"Answer the class that is currently selected. Answer nil if no selection 
	exists."

	^selectedClassName ifNotNil: [
		Smalltalk at: selectedClassName ifAbsent: nil ]
]

{ #category : 'metaclass' }
Browser >> selectedClassOrMetaClass [
	"Answer the selected class or metaclass."

	| cls |
	self metaClassIndicated
		ifTrue: [^ (cls := self selectedClass) ifNil: [nil] ifNotNil: [cls class]]
		ifFalse: [^ self selectedClass]
]

{ #category : 'message list' }
Browser >> selectedMessage [
	"Answer a copy of the source code for the selected message."

	| class selector method |
	self showingDecompile ifTrue: [
		^ self decompiledSource ].

	class := self selectedClassOrMetaClass.
	selector := self selectedMessageName.
	method := class compiledMethodAt: selector ifAbsent: [^ ''].	"method deleted while in another project"
	currentCompiledMethod := method.

	^ self sourceStringPrettifiedAndDiffed
			copy
]

{ #category : 'message category list' }
Browser >> selectedMessageCategoryName [
	"Answer the name of the selected message category, if any. Answer nil 
	otherwise."

	^selectedMessageCategory
]

{ #category : 'message list' }
Browser >> selectedMessageName [
	"Answer the message selector of the currently selected message, if any. 
	Answer nil otherwise."
	^selectedMessage
]

{ #category : 'message list' }
Browser >> selectedMessageName: aSelector [
	"Make the given selector be the selected message name"

	| anIndex |
	anIndex := self messageList indexOf: aSelector.
	anIndex > 0 ifTrue:
		[self messageListIndex: anIndex]
]

{ #category : 'system category list' }
Browser >> selectedSystemCategory [
	"Answer the name of the selected system category or nil."

	^selectedSystemCategory
]

{ #category : 'message list' }
Browser >> selector: oldSelector refactoredTo: newSelector [

	self selectedMessageName = oldSelector ifTrue: [ self setSelector: newSelector ]
]

{ #category : 'initialization' }
Browser >> setClass: aBehavior selector: aSymbol [
	"Set the state of a new, uninitialized Browser."

	| isMeta aClass messageCatIndex |
	aBehavior ifNil: [^ self].
	aBehavior isMeta
		ifTrue: [
			isMeta := true.
			aClass := aBehavior soleInstance]
		ifFalse: [
			isMeta := false.
			aClass := aBehavior].
	self setSelectedSystemCategory: aClass category.
	self selectClass: aClass.
	self metaClassIndicated: isMeta.
	aSymbol ifNil: [^ self].
	messageCatIndex := aBehavior organization numberOfCategoryOfElement: aSymbol.
	self messageCategoryListIndex: (messageCatIndex > 0
		ifTrue: [messageCatIndex + 1]
		ifFalse: [0]).
	messageCatIndex = 0 ifTrue: [^ self].
	self messageListIndex: (
		(aBehavior organization listAtCategoryNumber: messageCatIndex)
			indexOf: aSymbol).
]

{ #category : 'metaclass' }
Browser >> setClassOrganizer [

	"Install whatever organization is appropriate"

	| theClass |

	classOrganizer := nil.
	metaClassOrganizer := nil.
	selectedClassName ifNil: [^ self].	
	theClass := self selectedClass ifNil: [ ^self ].
	classOrganizer := theClass organization.
	metaClassOrganizer := theClass class organization.
]

{ #category : 'system category list' }
Browser >> setSelectedSystemCategory: aStringOrNil [
	"Set the selected system category and update all other selections to be deselected."
	selectedSystemCategory := aStringOrNil.
	selectedClassName := nil.
	selectedMessageCategory := nil.
	selectedMessage := nil.
	self editSelection: (aStringOrNil isNil ifTrue: [#none] ifFalse: [#newClass]).
	metaClassIndicated := false.
	self setClassOrganizer.
	self changed: #selectedSystemCategory.
	self changed: #classList.
	self changed: #messageCategoryList.
	self changed: #messageList.
	self changed: #relabel.
	self changed: #instanceMessagesIndicated.
	self changed: #classCommentIndicated.
	self changed: #classMessagesIndicated.
	self acceptedContentsChanged
]

{ #category : 'system category list' }
Browser >> setSelectedSystemCategoryTreeItem: aSystemCategoryWrapper [
	aSystemCategoryWrapper ifNotNil: [self setSelectedSystemCategory: aSystemCategoryWrapper withoutListWrapper]
]

{ #category : 'initialization' }
Browser >> setSelector: aSymbol [
	"Make the receiver point at the given selector, in the currently chosen class"

	| aClass messageCatIndex |
	aSymbol ifNil: [^ self].
	(aClass := self selectedClassOrMetaClass) ifNil: [^ self].

	(selectedMessageCategory isNil or: [ selectedMessageCategory == ClassOrganizer allCategory ])
		ifTrue: [
			self messageListIndex:
				(aClass organization allMethodSelectors indexOf: aSymbol) ]
		ifFalse: [
			messageCatIndex := aClass organization numberOfCategoryOfElement: aSymbol.
			messageCatIndex = 0 ifFalse: [
				self messageCategoryListIndex: messageCatIndex + 1.
				self messageListIndex:
					((aClass organization listAtCategoryNumber: messageCatIndex)
						indexOf: aSymbol) ]].
]

{ #category : 'message category functions' }
Browser >> showHomeCategory [
	"Show the home category of the selected method.  This is only really useful if one is in a tool that supports the showing of categories.  Thus, it's good in browsers and hierarchy browsers but not in message-list browsers"

	| aSelector |
	(aSelector := self selectedMessageName) ifNotNil: [
		self selectOriginalCategoryForCurrentMethod.
		self selectedMessageName: aSelector]
]

{ #category : 'system category functions' }
Browser >> systemCategoryBrowser [
	"Create a new system category browser with initial textual 
	contents set to aString."

	| newBrowser |
	selectedSystemCategory
		ifNotNil: [
			newBrowser := Browser new.
			newBrowser setSelectedSystemCategory: self selectedSystemCategory.
			newBrowser setClass: self selectedClassOrMetaClass selector: self selectedMessageName.
			^newBrowser].
	^nil
]

{ #category : 'system category list' }
Browser >> systemCategoryList [
	"Answer the class categories modelled by the receiver."

	^systemOrganizer categories
]

{ #category : 'system category list' }
Browser >> systemCategoryRoots [
	"Answer the roots of the forest of system categories."
	| categories subcategories roots name |
	categories := self systemCategoryList.
	subcategories := Dictionary new.
	categories do: [:each|
		name := SystemOrganization rootCategoryOf: each.
		(subcategories at: name ifAbsentPut: [OrderedCollection new]) add: each].
	roots := OrderedCollection new.
	categories do: [:each|
		name := SystemOrganization rootCategoryOf: each.
		(roots includes: name) ifFalse:
			[(subcategories at: name) size > 1 ifTrue: [roots add: name] ifFalse: [roots add: (subcategories at: name) first]]].
	^ roots collect: [:each| SystemCategoryWrapper with: each name: each model: self]
]

{ #category : 'system category list' }
Browser >> systemCategorySingleton [

	^ selectedSystemCategory
		ifNil: [#()]
		ifNotNil: [Array with: selectedSystemCategory]
]

{ #category : 'initialization' }
Browser >> systemOrganizer: aSystemOrganizer [
	"Initialize the receiver as a perspective on the system organizer, 
	aSystemOrganizer. Typically there is only one--the system variable 
	SystemOrganization."

	systemOrganizer := aSystemOrganizer.
	selectedSystemCategory := nil.
	selectedClassName := nil.
	selectedMessageCategory := nil.
	selectedMessage := nil.
	metaClassIndicated := false.
	self setClassOrganizer.
	self editSelection: #none.
]

{ #category : 'breakpoints' }
Browser >> toggleBreakOnEntry [
	"Install or uninstall a halt-on-entry breakpoint"

	| selectedMethod |
	self selectedClassOrMetaClass ifNil: [ ^self ].
	selectedMethod := self selectedClassOrMetaClass >> self selectedMessageName.
	selectedMethod hasBreakpoint
		ifTrue:
			[ selectedMethod uninstall ]
		ifFalse:
			[ (BreakingMethodWrapper on: selectedMethod) install ].
	self
		changed: #messageList;
		changed: #annotation
]

{ #category : 'self-updating' }
Browser >> updateIfNeeded [
	super updateIfNeeded.
	self changed: #systemCategoryRoots
]

{ #category : 'system category functions' }
Browser >> updateSystemCategories [
	"The class categories were changed in another browser. The receiver must 
	reorganize its lists based on these changes."

	self changed: #systemCategoryRoots
]
