'From Cuis7.9 [latest update: #8100] on 29 July 2026 at 2:56:24 pm'!

!MethodCategoriesPrompter methodsFor: 'categories initialization - private' stamp: 'jmv 29/Jul/2026, 2:58:57 pm (UTC)'!
initializeClassSideCategories: classCategories

	categories add: Categorizer instanceCreation.
	classCategories remove: Categorizer instanceCreation ifAbsent: nil.
	reject add: Categorizer instanceCreation! !


!MethodCategoriesPrompter methodsFor: 'categories initialization - private' stamp: 'jmv 29/Jul/2026, 2:59:36 pm (UTC)'!
createCategories

	| classCategories |
	
	categories := OrderedCollection with: 'new ...'.
	lines := OrderedCollection with: 1. 
	classCategories := startClass methodCategoriesAsSortedCollection.
	
	reject := classCategories asSet.
	reject
		add: ClassOrganizer defaultMethodCategory.
	
	startClass isMeta ifTrue: [ self initializeClassSideCategories: classCategories].
	rejectingFirst ifFalse: [ categories addAll: classCategories ]! !

!methodRemoval: MethodCategoriesPrompter #initializeCategoriesWhenMeta: stamp: 'jmv 29/Jul/2026, 5:56:05 pm (UTC)'!
MethodCategoriesPrompter removeSelector: #initializeCategoriesWhenMeta:!
