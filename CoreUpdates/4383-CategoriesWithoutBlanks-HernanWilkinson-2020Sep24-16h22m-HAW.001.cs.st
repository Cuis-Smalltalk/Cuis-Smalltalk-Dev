'From Cuis 5.0 [latest update: #4382] on 24 September 2020 at 8:08:04 pm'!

!Categorizer methodsFor: 'accessing' stamp: 'HAW 9/24/2020 19:59:20'!
addCategory: catString before: nextCategory
	"Add a new category named heading.
	If default category exists and is empty, remove it.
	If nextCategory is nil, then add the new one at the end,
	otherwise, insert it before nextCategory."
	
	| index newCategory |
	
	newCategory _ catString withBlanksTrimmed.
	
	"heading already exists, so done"
	(categoryArray indexOf: newCategory) > 0 ifTrue: [^self].	
	
	index _ categoryArray indexOf: nextCategory ifAbsent: [categoryArray size + 1].
	categoryArray _ categoryArray
		copyReplaceFrom: index
		to: index-1
		with: (Array with: newCategory).
	categoryStops _ categoryStops
		copyReplaceFrom: index
		to: index-1
		with: (Array with: (index = 1
				ifTrue: [0]
				ifFalse: [categoryStops at: index-1])).

	"remove empty default category"
	(newCategory ~= Default and: [(self listAtCategoryNamed: Default) isEmpty])
		ifTrue: [self removeCategory: Default]! !

!Categorizer methodsFor: 'accessing' stamp: 'HAW 9/24/2020 20:02:50'!
classify: element under: heading suppressIfDefault: aBoolean
	"Store the argument, element, in the category named heading.   If aBoolean is true, then invoke special logic such that the classification is NOT done if the new heading is the Default and the element already had a non-Default classification -- useful for filein"

	| catName catIndex elemIndex realHeading |
	
	((heading = NullCategory) or: [heading == nil])
		ifTrue: [realHeading _ Default]
		ifFalse: [realHeading _ heading withBlanksTrimmed ].
	
	"done if already under that category"
	(catName _ self categoryOfElement: element) = realHeading ifTrue: [^ self].  

	catName ifNotNil: [
		"return if non-Default category already assigned in memory"
		(aBoolean and: [realHeading = Default]) ifTrue: [^ self].	 
		"remove if in another category"
		self removeElement: element].

	(categoryArray indexOf: realHeading) = 0 ifTrue: [self addCategory: realHeading].

	catIndex _ categoryArray indexOf: realHeading.
	elemIndex _ catIndex > 1
		ifTrue: [categoryStops at: catIndex - 1]
		ifFalse: [0].
	[(elemIndex _ elemIndex + 1) <= (categoryStops at: catIndex) 
		and: [element >= (elementArray at: elemIndex)]] whileTrue.

	"elemIndex is now the index for inserting the element. Do the insertion before it."
	elementArray _ elementArray copyReplaceFrom: elemIndex to: elemIndex-1 with: (Array with: element).

	"add one to stops for this and later categories"
	catIndex to: categoryArray size do: 
		[:i | categoryStops at: i put: (categoryStops at: i) + 1].

	(self listAtCategoryNamed: Default) size = 0 ifTrue: [self removeCategory: Default]! !

!Categorizer methodsFor: 'accessing' stamp: 'HAW 9/24/2020 20:04:28'!
renameCategory: oldCatString toBe: newCatString
	"Rename a category. No action if new name already exists, or if old name does not exist."
	
	| index newCategory |
	
	newCategory _ newCatString withBlanksTrimmed.
	"new name exists, so no action"
	(categoryArray indexOf: newCategory) > 0 ifTrue: [^ self].
	
	"old name not found, so no action"
	(index _ categoryArray indexOf: oldCatString) = 0 ifTrue: [^ self].	
	
	"need to change identity so smart list update will notice the change"
	categoryArray _ categoryArray copy.  
	categoryArray at: index put: newCategory! !


!ClassOrganizer methodsFor: 'accessing' stamp: 'HAW 9/24/2020 20:05:51'!
renameCategory: oldCatString toBe: newCatString

	| newCategory oldElementsBefore oldElementsAfter |
	
	newCategory _ newCatString withBlanksTrimmed.
	oldElementsBefore _ self listAtCategoryNamed: oldCatString.
	SystemChangeNotifier uniqueInstance doSilently: [
		super renameCategory: oldCatString toBe: newCatString].
	
	oldElementsAfter _ (self listAtCategoryNamed: oldCatString) asSet.
	oldElementsBefore do: [:each | (oldElementsAfter includes: each)
		ifFalse: [self notifyOfChangedSelector: each from: oldCatString to: newCategory]].
	
	self notifyOfChangedCategoryFrom: oldCatString to: newCategory.! !


!ClassBuilder methodsFor: 'class definition' stamp: 'HAW 9/24/2020 16:24:14'!
name: className subclassOf: newSuper type: type instanceVariableNames: instVarString classVariableNames: classVarString poolDictionaries: poolString category: category unsafe: unsafe
	"Define a new class.
	If unsafe is true do not run any validation checks.
	This facility is provided to implement important system changes."
	
	| oldClass newClass organization instVars classVars force needNew oldCategory copyOfOldClass newCategory |
	
	instVars _ Scanner new scanFieldNames: instVarString.
	classVars _ (Scanner new scanFieldNames: classVarString) collect: [:x | x asSymbol].

	"Validate the proposed name"
	unsafe ifFalse:[(self validateClassName: className) ifFalse:[^nil]].
	oldClass _ Smalltalk at: className ifAbsent: nil.
	oldClass isBehavior 
		ifFalse:[oldClass _ nil]. "Already checked in #validateClassName:"
	copyOfOldClass _ oldClass copy.

	unsafe ifFalse:[
		"Run validation checks so we know that we have a good chance for recompilation"
		(self validateSuperclass: newSuper forSubclass: oldClass) ifFalse:[^nil].
		(self validateInstvars: instVars from: oldClass forSuper: newSuper) ifFalse:[^nil].
		(self validateClassvars: classVars from: oldClass forSuper: newSuper) ifFalse:[^nil].
		(self validateSubclassFormat: type from: oldClass forSuper: newSuper extra: instVars size) ifFalse:[^nil]].

	"See if we need a new subclass"
	needNew _ self needsSubclassOf: newSuper type: type instanceVariables: instVars from: oldClass.
	needNew ifNil: [^nil]. "some error"

	(needNew and:[unsafe not]) ifTrue:[
		"Make sure we don't redefine any dangerous classes"
		(self tooDangerousClasses includes: oldClass name) ifTrue:[
			self error: oldClass name, ' cannot be changed'.
		].
		"Check if the receiver should not be redefined"
		(oldClass notNil and:[oldClass shouldNotBeRedefined]) ifTrue:[
			self notify: oldClass name asText allBold, 
						' should not be redefined!! \Proceed to store over it.' withNewLines]].

	needNew ifTrue:[
		"Create the new class"
		newClass _ self 
			newSubclassOf: newSuper 
			type: type 
			instanceVariables: instVars
			from: oldClass.
		newClass ifNil: [ ^nil]. "Some error"
		newClass setName: className.
	] ifFalse:[
		"Reuse the old class"
		newClass _ oldClass.
	].

	"Install the class variables and pool dictionaries... "
	force _ (newClass declare: classVarString) | (newClass sharing: poolString).

	"... classify ..."
	newCategory _ category withBlanksTrimmed.
	organization _ Smalltalk organization.
	oldClass ifNotNil: [oldCategory := (organization categoryOfElement: oldClass name) asSymbol].
	organization classify: newClass name under: newCategory.

	"... recompile ..."
	newClass _ self recompile: force from: oldClass to: newClass mutate: false.

	"... export if not yet done ..."
	(Smalltalk at: newClass name ifAbsent: nil) == newClass ifFalse:[
		[Smalltalk at: newClass name put: newClass]
			on: AttemptToWriteReadOnlyGlobal do:[:ex| ex resume: true].
		Smalltalk flushClassNameCache.
	].

	self doneCompiling: newClass.
	
	"... notify interested clients ..."
	oldClass ifNil: [
		SystemChangeNotifier uniqueInstance classAdded: newClass inCategory: newCategory.
		^ newClass].
	SystemChangeNotifier uniqueInstance classDefinitionChangedFrom: copyOfOldClass to: newClass.
	newCategory ~= oldCategory 
		ifTrue: [SystemChangeNotifier uniqueInstance classRecategorized: newClass from: oldCategory to: newCategory ].
	^newClass! !

