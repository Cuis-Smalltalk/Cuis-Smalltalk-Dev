'From Cuis7.3 [latest update: #7052] on 26 February 2025 at 3:52:49 pm'!

!ClassDefinitionNodeAnalyzer methodsFor: 'accessing' stamp: 'HAW 2/26/2025 15:49:17'!
systemCategory

	self isClassDefinition 	ifFalse: [ self error: 'Cannot get system category from metaclass definition' ].
	
	^(classCreationMessageNode arguments at: self class categoryPosition) literalValue ! !


!Browser methodsFor: 'system category functions' stamp: 'HAW 2/26/2025 15:39:05'!
renameSystemCategoryNamed: oldName

	^ self
		request: 'New category name?'
		initialAnswer: oldName
		verifying: [:aString| aString notEmpty and: [aString ~= oldName]]
		do: [:aString| | newName |
			newName := aString asSymbol.
			systemOrganizer renameSystemCategory: oldName to: newName.
			self setSelectedSystemCategory: newName.
			self changed: #systemCategoryRoots]! !


!Browser methodsFor: 'system category functions' stamp: 'HAW 2/26/2025 15:50:33'!
renameSystemCategory
	"Prompt for a new category name and add it before the
	current selection, or at the end if no current selection
	
	21-Mar-2012 jmv Note: This is not recorded appropriately in change sets.
	The easiest solution is to trigger #classRecategorized for all classes in the category.
	But this is not a real solution, as the resulting changeset would not do a rename,
	but create a new category (that would go to the bottom) with all the classes.
	
	In the meantime, disable the menu entry. This is not so important after all.
	"
	
	selectedSystemCategory ifNil: [^ self].  "no selection"
	self renameSystemCategoryNamed: selectedSystemCategory! !


!ClassDefinitionNodeAnalyzer reorganize!
('initialization' initializeFor:)
('testing' isAtCategory: isAtClassName: isAtClassNameInClassDefinition: isAtInstanceVariables: isAtSuperclass: isAtSuperclassInClassDefinition: isClassDefinition)
('accessing' superclass systemCategory)
('testing - private' is:atStringParameterNumber:)
('parameters positions' instanceVariableNamesPosition)
!

