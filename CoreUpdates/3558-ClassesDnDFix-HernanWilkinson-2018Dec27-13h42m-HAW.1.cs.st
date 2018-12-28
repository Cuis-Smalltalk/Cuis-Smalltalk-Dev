'From Cuis 5.0 of 7 November 2016 [latest update: #3544] on 27 December 2018 at 1:50:32 pm'!

!Browser methodsFor: 'system category functions' stamp: 'HAW 12/27/2018 13:48:49'!
categorizeUnderCategoryAt: aSystemCategoryIndex class: aClassName 

	systemOrganizer classify: aClassName withBlanksTrimmed asSymbol under: (self systemCategoryList at: aSystemCategoryIndex).
	self changed: #classList! !

!Browser methodsFor: 'system category functions' stamp: 'HAW 12/27/2018 13:48:56'!
categorizeUnderNewCategoryClass: aClassName

	| newCategory |
	
	newCategory _ self newCategoryNameIfNone: [ ^self ].
	
	systemOrganizer
		addCategory: newCategory;
		classify: aClassName withBlanksTrimmed asSymbol under: newCategory.
		
	self changed: #systemCategoryList.
	self changed: #classList.! !

