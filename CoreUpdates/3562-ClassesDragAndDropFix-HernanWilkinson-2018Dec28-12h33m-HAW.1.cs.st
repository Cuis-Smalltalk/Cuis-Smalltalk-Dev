'From Cuis 5.0 of 7 November 2016 [latest update: #3561] on 28 December 2018 at 12:44:11 pm'!

!Browser methodsFor: 'message category functions' stamp: 'HAW 12/28/2018 12:37:44'!
newMethodCategoryNameIfNone: aNoneBlock

	| labels lines menuIndex newName reject |
	
	labels _ OrderedCollection with: 'new...'.
	reject _ Set new.
	reject
		addAll: self selectedClassOrMetaClass organization categories;
		add: ClassOrganizer nullCategory;
		add: ClassOrganizer default.
	lines _ OrderedCollection new.
	self selectedClassOrMetaClass allSuperclasses do: [:cls | | cats |
		cls = Object ifFalse: [
			cats _ cls organization categories reject:
				 [:cat | reject includes: cat].
			cats isEmpty ifFalse: [
				lines add: labels size.
				labels addAll: cats asArray sort.
				reject addAll: cats]]].
	newName _ (labels size = 1 or: [
		menuIndex _ (PopUpMenu labelArray: labels lines: lines)
		startUpWithCaption: 'Add Category'.
		menuIndex = 0 ifTrue: [^ aNoneBlock value].
		menuIndex = 1])
			ifTrue: [
				self request: 'Please type new category name'
					initialAnswer: 'category name']
			ifFalse: [
				labels at: menuIndex].
	
	^ newName isEmpty
		ifTrue: aNoneBlock
		ifFalse: [newName asSymbol].
! !

!Browser methodsFor: 'system category functions' stamp: 'HAW 12/28/2018 12:37:19'!
newSystemCategoryNameIfNone: aNoneBlock

	| newName |
	
	newName _ self
		request: 'Please type new category name'
		initialAnswer: 'Category-Name'.
	
	^newName isEmpty
		ifTrue: aNoneBlock 
		ifFalse: [newName asSymbol].! !


!Browser methodsFor: 'message category functions' stamp: 'HAW 12/28/2018 12:37:56'!
addCategory
	"Present a choice of categories or prompt for a new category name and add it before the current selection, or at the end if no current selection"

	| oldIndex newName |

	selectedClassName ifNil: [ ^self ].
	
	oldIndex _ self messageCategoryListIndex.
	newName _ self newMethodCategoryNameIfNone: [ ^self ].
	
	self classOrMetaClassOrganizer
		addCategory: newName
		before: selectedMessageCategory.
	self changed: #messageCategoryList.
	self messageCategoryListIndex:
		(oldIndex = 0
			ifTrue: [self classOrMetaClassOrganizer categories size + 1]
			ifFalse: [oldIndex]).
	self changed: #messageCategoryList.
	
! !

!Browser methodsFor: 'message category functions' stamp: 'HAW 12/28/2018 12:38:40'!
categorizeUnderNewCategorySelector: aSelectorToCategorize

	self selectedClassOrMetaClass ifNotNil: [ :class | | newCategory |
		newCategory _ self newMethodCategoryNameIfNone: [ ^self ].
		class organization 
			addCategory: newCategory;
			classify: aSelectorToCategorize under: newCategory suppressIfDefault: false.
			
		self changed: #messageCategoryList.
		self changed: #messageList]! !

!Browser methodsFor: 'system category functions' stamp: 'HAW 12/28/2018 12:38:00'!
addSystemCategory
	"Prompt for a new category name and add it before the
	current selection, or at the end if no current selection"
	
	| oldIndex newName |
	
	oldIndex _ self systemCategoryListIndex.
	newName _ self newSystemCategoryNameIfNone: [ ^self ].
	
	systemOrganizer
		addCategory: newName
		before: selectedSystemCategory.
	self systemCategoryListIndex:
		(oldIndex = 0
			ifTrue: [self systemCategoryList size]
			ifFalse: [oldIndex]).
	self changed: #systemCategoryList.! !

!Browser methodsFor: 'system category functions' stamp: 'HAW 12/28/2018 12:38:10'!
categorizeUnderNewCategoryClass: aClassName

	| newCategory |
	
	newCategory _ self newSystemCategoryNameIfNone: [ ^self ].
	
	systemOrganizer
		addCategory: newCategory;
		classify: aClassName withBlanksTrimmed asSymbol under: newCategory.
		
	self changed: #systemCategoryList.
	self changed: #classList.! !

!methodRemoval: Browser #categorizeUnderCategoryAt:messageAt:!
Browser removeSelector: #categorizeUnderCategoryAt:messageAt:!
!methodRemoval: Browser #categorizeUnderNewCategoryMessageAt:!
Browser removeSelector: #categorizeUnderNewCategoryMessageAt:!
!methodRemoval: Browser #newCategoryNameIfNone:!
Browser removeSelector: #newCategoryNameIfNone:!

