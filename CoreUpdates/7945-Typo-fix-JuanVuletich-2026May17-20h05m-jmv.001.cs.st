'From Cuis7.7 [latest update: #7944] on 17 May 2026 at 8:07:18 pm'!

!MethodCategoriesPrompter methodsFor: 'initialization' stamp: 'jmv 17/May/2026 20:05:57'!
initializeStartingFrom: aClass rejectingFirst: aRejectingFirst

	startClass := aClass.
	rejectingFirst := aRejectingFirst.
	
	self initializeCategories ! !


!MethodCategoriesPrompter class methodsFor: 'instance creation' stamp: 'jmv 17/May/2026 20:06:22'!
startingFrom: aClass rejectingFirst: rejectingFirst 
	
	^self new initializeStartingFrom: aClass rejectingFirst: rejectingFirst 
! !


!CodeProvider methodsFor: 'categories' stamp: 'jmv 17/May/2026 20:06:27'!
categoryFromUserWithPrompt: aPrompt for: aClass
	"self new categoryFromUserWithPrompt: 'testing' for: SystemDictionary"

	^(MethodCategoriesPrompter 
		startingFrom: aClass 
		rejectingFirst: false) prompt: aPrompt ifNone: nil! !


!Browser methodsFor: 'message category functions' stamp: 'jmv 17/May/2026 20:06:32'!
newMethodCategoryNameIfNone: aNoneBlock
	
	^(MethodCategoriesPrompter 
		startingFrom: self selectedClassOrMetaClass 
		rejectingFirst: true) prompt: 'Add Category' ifNone: aNoneBlock! !


!Debugger methodsFor: 'context stack menu' stamp: 'jmv 17/May/2026 20:06:35'!
askForCategoryIn: aClass default: aDefaultCategory

	^(MethodCategoriesPrompter 
		startingFrom: aClass 
		rejectingFirst: false) prompt: 'Select category for the new method' ifNone: [ aDefaultCategory ]! !

!methodRemoval: MethodCategoriesPrompter class #staringFrom:rejectingFirst: stamp: 'jmv 17/May/2026 20:06:45'!
MethodCategoriesPrompter class removeSelector: #staringFrom:rejectingFirst:!
!methodRemoval: MethodCategoriesPrompter #initializeStaringFrom:rejectingFirst: stamp: 'jmv 17/May/2026 20:06:42'!
MethodCategoriesPrompter removeSelector: #initializeStaringFrom:rejectingFirst:!
