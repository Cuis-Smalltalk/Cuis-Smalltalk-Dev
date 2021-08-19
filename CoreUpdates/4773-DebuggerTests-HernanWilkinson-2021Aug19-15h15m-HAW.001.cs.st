'From Cuis 5.0 [latest update: #4768] on 19 August 2021 at 3:37:26 pm'!
!classDefinition: #MethodCategoriesPrompter category: #'System-Text'!
Object subclass: #MethodCategoriesPrompter
	instanceVariableNames: 'startClass rejectingFirst prompt categories lines reject '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!

!MethodCategoriesPrompter methodsFor: 'initialization' stamp: 'HAW 8/19/2021 15:16:33'!
initializeStaringFrom: aClass rejectingFirst: aRejectingFirst

	startClass := aClass.
	rejectingFirst := aRejectingFirst.
	
	self initializeCategories ! !

!MethodCategoriesPrompter methodsFor: 'prompting' stamp: 'HAW 8/19/2021 15:17:29'!
prompt: aPrompt ifNone: aNoneBlock

	| selectedCategoryIndex categoryName |
	
	selectedCategoryIndex := self promptCategory: aPrompt.
	selectedCategoryIndex = 0 ifTrue: [^ aNoneBlock value].
	
	categoryName := selectedCategoryIndex = 1 ifTrue: [ self requestNewCategory ] ifFalse: [ categories at: selectedCategoryIndex ].
	categoryName isEmpty ifTrue: [ ^aNoneBlock value ].
	
	^categoryName
	! !

!MethodCategoriesPrompter methodsFor: 'prompting - private' stamp: 'HAW 8/19/2021 15:17:44'!
promptCategory: aPrompt
	
	| selectedLabelIndex |
	
	selectedLabelIndex := categories size = 1 
		ifTrue: [ 1 ]
	 	ifFalse: [ (PopUpMenu labelArray: categories lines: lines) startUpWithCaption: aPrompt ].
	
	^selectedLabelIndex! !

!MethodCategoriesPrompter methodsFor: 'accessing' stamp: 'HAW 8/19/2021 15:18:46'!
categories

	^categories! !

!MethodCategoriesPrompter methodsFor: 'accessing' stamp: 'HAW 8/19/2021 15:19:53'!
lines

	^lines ! !


!MethodCategoriesPrompter class methodsFor: 'instance creation' stamp: 'HAW 8/19/2021 15:16:16'!
staringFrom: aClass rejectingFirst: rejectingFirst 
	
	^self new initializeStaringFrom: aClass rejectingFirst: rejectingFirst 
! !


!CodeProvider methodsFor: 'categories' stamp: 'HAW 8/19/2021 15:15:23'!
categoryFromUserWithPrompt: aPrompt for: aClass
	"self new categoryFromUserWithPrompt: 'testing' for: SystemDictionary"

	^(MethodCategoriesPrompter 
		staringFrom: aClass 
		rejectingFirst: false) prompt: aPrompt ifNone: [ nil ]! !


!Browser methodsFor: 'message category functions' stamp: 'HAW 8/19/2021 15:15:47'!
newMethodCategoryNameIfNone: aNoneBlock
	
	^(MethodCategoriesPrompter 
		staringFrom: self selectedClassOrMetaClass 
		rejectingFirst: true) prompt: 'Add Category' ifNone: aNoneBlock! !


!Debugger methodsFor: 'context stack menu' stamp: 'HAW 8/19/2021 15:16:05'!
askForCategoryIn: aClass default: aDefaultCategory

	^(MethodCategoriesPrompter 
		staringFrom: aClass 
		rejectingFirst: false) prompt: 'Select category for the new method' ifNone: [ aDefaultCategory ]! !

!methodRemoval: MethodCategoriesPrompter class #staringFrom:rejectingFirst:prompting: stamp: 'HAW 8/19/2021 15:16:19'!
MethodCategoriesPrompter class removeSelector: #staringFrom:rejectingFirst:prompting:!
!methodRemoval: MethodCategoriesPrompter #promptCategory stamp: 'HAW 8/19/2021 15:17:29'!
MethodCategoriesPrompter removeSelector: #promptCategory!
!methodRemoval: MethodCategoriesPrompter #initializeStaringFrom:rejectingFirst:prompting: stamp: 'HAW 8/19/2021 15:16:35'!
MethodCategoriesPrompter removeSelector: #initializeStaringFrom:rejectingFirst:prompting:!
!methodRemoval: MethodCategoriesPrompter #valueIfNone: stamp: 'HAW 8/19/2021 15:17:04'!
MethodCategoriesPrompter removeSelector: #valueIfNone:!
!classDefinition: #MethodCategoriesPrompter category: #'System-Text'!
Object subclass: #MethodCategoriesPrompter
	instanceVariableNames: 'startClass rejectingFirst categories lines reject'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!

!MethodCategoriesPrompter reorganize!
('initialization' initializeStaringFrom:rejectingFirst:)
('prompting' prompt:ifNone:)
('categories initialization - private' addCategories addCategoriesOf: createCategories initializeCategories initializeCategoriesWhenMeta:)
('prompting - private' promptCategory: requestNewCategory)
('accessing' categories lines)
!

