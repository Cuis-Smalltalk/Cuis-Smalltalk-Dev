'From Cuis 5.0 [latest update: #4438] on 5 November 2020 at 12:32:31 pm'!

!Browser methodsFor: 'system category functions' stamp: 'KLG 11/5/2020 12:23:10'!
newSystemCategoryNameInitialAnswer
	"Make a suggestion for a new sytems category.
	
	Can be redefined in subclasses to provide something meore meaningfull."

	^ 'Category-Name'.
! !


!SinglePackageBrowser methodsFor: 'system category functions' stamp: 'KLG 11/5/2020 12:28:53'!
newSystemCategoryNameInitialAnswer
	"Make a suggestion for a new sytems category.
	
	Provide something more usefull, e.g. the package name or a sensible derivate of it."

	| packageName | 
	^ self systemCategoryList includes: (packageName _ package packageName) ::
		ifTrue: [ packageName, ' - Sub-Category-Name' ]
		ifFalse: [ packageName ]
! !


!Browser methodsFor: 'system category functions' stamp: 'KLG 11/5/2020 12:23:23'!
newSystemCategoryNameIfNone: aNoneBlock

	| newName |
	
	newName _ self
		request: 'New category name?'
		initialAnswer: self newSystemCategoryNameInitialAnswer.
	
	^newName isEmpty
		ifTrue: aNoneBlock 
		ifFalse: [newName asSymbol].! !


!SinglePackageBrowser reorganize!
('accessing' defaultBrowserTitle package:)
('lists' defaultClassList messageCategoryList messageList rawMessageCategoryList systemCategoryList)
('system category functions' newSystemCategoryNameInitialAnswer)
!

