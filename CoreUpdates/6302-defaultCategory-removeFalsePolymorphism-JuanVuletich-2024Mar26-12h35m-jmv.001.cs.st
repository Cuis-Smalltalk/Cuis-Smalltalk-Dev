'From Cuis6.3 [latest update: #6298] on 26 March 2024 at 12:36:15 pm'!

!ClassOrganizer class methodsFor: 'class variables' stamp: 'jmv 3/26/2024 12:09:39'!
defaultMethodCategory
	^ Default! !


!SystemOrganizer class methodsFor: 'class variables' stamp: 'jmv 3/26/2024 12:15:06'!
defaultClassCategory
	^ Default! !


!Browser methodsFor: 'message category list' stamp: 'jmv 3/26/2024 12:09:55'!
categorizeAllUncategorizedMethods
	"Categorize methods by looking in parent classes for a method category."

	| organizer organizers |
	organizer := self classOrMetaClassOrganizer.
	organizers := self selectedClassOrMetaClass withAllSuperclasses collect: [:ea | ea organization].
	(organizer listAtCategoryNamed: ClassOrganizer defaultMethodCategory) do: [:sel | | found |
		found := (organizers collect: [ :org | org categoryOfElement: sel])
			detect: [:ea | ea ~= ClassOrganizer defaultMethodCategory and: [ ea notNil]]
			ifNone: nil.
		found ifNotNil: [organizer classify: sel under: found]].

	self changed: #messageCategoryList! !


!Debugger methodsFor: 'method creation' stamp: 'jmv 3/26/2024 12:11:38'!
implement: aMessage inClass: aClass context: aContext 
	
	self implement: aMessage inCategory: (self askForCategoryIn: aClass default: ClassOrganizer defaultMethodCategory) fromClass: aClass context: aContext! !


!ClassDescription methodsFor: 'compiling' stamp: 'jmv 3/26/2024 12:10:03'!
compile: code notifying: requestor 
	"Refer to the comment in Behavior|compile:notifying:." 

	^self compile: code
		 classified: ClassOrganizer defaultMethodCategory
		 notifying: requestor! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 3/26/2024 12:10:06'!
methods
	"Answer a ClassCategoryReader for compiling messages that are not classified, as in fileouts made with Smalltalk/V"

	^ ClassCategoryReader new setClass: self category: ClassOrganizer defaultMethodCategory! !

!ClassDescription methodsFor: 'organization' stamp: 'jmv 3/26/2024 12:15:46'!
category: requested 
	"Categorize the receiver under the system category, cat, removing it from 
	any previous categorization."

	| oldCategory newCategory |
	requested isString ifFalse: [^self errorCategoryName].
	oldCategory := self category.
	newCategory := (requested isNil or: [ requested isEmpty ])
		ifFalse: [requested withBlanksTrimmed]
		ifTrue: [SystemOrganization class defaultClassCategory].
	SystemOrganization classify: self name under: newCategory.
	SystemChangeNotifier uniqueInstance classRecategorized: self from: oldCategory to: newCategory.! !


!ClassBuilder methodsFor: 'class definition' stamp: 'jmv 3/26/2024 12:16:39'!
name: className subclassOf: newSuper type: type instanceVariableNames: instVarString classVariableNames: classVarString poolDictionaries: poolString category: category unsafe: unsafe
	"Define a new class.
	If unsafe is true do not run any validation checks.
	This facility is provided to implement important system changes."
	
	| oldClass newClass organization instVars classVars force needNew oldCategory copyOfOldClass newCategory behaviorClasses |
	
	instVars := Scanner new scanFieldNames: instVarString.
	classVars := (Scanner new scanFieldNames: classVarString) collect: [:x | x asSymbol].

	"Validate the proposed name"
	unsafe ifFalse:[(self validateClassName: className) ifFalse:[^nil]].
	oldClass := Smalltalk at: className ifAbsent: nil.
	oldClass isBehavior 
		ifFalse:[oldClass := nil]. "Already checked in #validateClassName:"
	copyOfOldClass := oldClass copy.

	unsafe ifFalse:[
		"Run validation checks so we know that we have a good chance for recompilation"
		(self validateSuperclass: newSuper forSubclass: oldClass) ifFalse:[^nil].
		(self validateInstvars: instVars from: oldClass forSuper: newSuper) ifFalse:[^nil].
		(self validateClassvars: classVars from: oldClass forSuper: newSuper) ifFalse:[^nil].
		(self validateSubclassFormat: type from: oldClass forSuper: newSuper extra: instVars size) ifFalse:[^nil]].

	"See if we need a new subclass"
	needNew := self needsSubclassOf: newSuper type: type instanceVariables: instVars from: oldClass.
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
		newClass := self 
			newSubclassOf: newSuper 
			type: type 
			instanceVariables: instVars
			from: oldClass.
		newClass ifNil: [ ^nil]. "Some error"
		newClass setName: className.
	] ifFalse:[
		"Reuse the old class"
		newClass := oldClass.
	].

	"Install the class variables and pool dictionaries... "
	force := (newClass declareClassVarString: classVarString) | (newClass declareSharedPoolString: poolString).

	"... classify ..."
	organization := Smalltalk organization.
	newCategory := (category isNil or: [ category isEmpty ])
		ifFalse: [category withBlanksTrimmed]
		ifTrue: [organization class defaultClassCategory].
	oldClass ifNotNil: [oldCategory := (organization categoryOfElement: oldClass name) asSymbol].
	organization classify: newClass name under: newCategory.

	"... recompile ..."
	(oldClass notNil and: [oldClass includesBehavior: Behavior])
		ifTrue: [
			"This is only true for Behavior, ClassDescription, Class and Metaclass. Special treatment."
			unsafe ifFalse: [
				behaviorClasses := (oldClass withAllSubclasses select: [ :c | c isMeta not ]) asArray.
				(Processor anyReceiverInStackInstanceOfClassIn: behaviorClasses) ifTrue: [
					self error: oldClass name,' has some subInstance running some CompiledMethod.'.
					^nil ]].
			"Please see comment at this method"
			newClass := self recompileSingleRecursion: force from: oldClass to: newClass ]
		ifFalse: [
			"Other classes"
			newClass := self recompile: force from: oldClass to: newClass mutate: false ].
	newClass isNil ifTrue: [ ^nil ].

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


!CompiledMethod class methodsFor: 'source code management aux' stamp: 'jmv 3/26/2024 12:17:27'!
classCategoryFrom: tokens

	^self field: #category: from: tokens ifAbsentOrNil: [ SystemOrganizer defaultClassCategory ]! !


!MethodCategoriesPrompter methodsFor: 'categories initialization - private' stamp: 'jmv 3/26/2024 12:10:10'!
createCategories

	| classCategories |
	
	categories := OrderedCollection with: 'new ...'.
	lines := OrderedCollection with: 1. 
	classCategories := startClass methodCategoriesAsSortedCollection.
	
	reject := classCategories asSet.
	reject
		add: ClassOrganizer defaultMethodCategory.
	
	startClass isMeta ifTrue: [ self initializeCategoriesWhenMeta: classCategories].
	rejectingFirst ifFalse: [ categories addAll: classCategories ]! !


!MethodReference methodsFor: 'services' stamp: 'jmv 3/26/2024 12:10:14'!
updateReferencesTo: oldClassName toBe: newClassName

	| src ts cm |
	src := self actualClass sourceCodeAt: methodSymbol.
	src := src copyReplaceAll: oldClassName with: newClassName.
	cm := self actualClass compiledMethodAt: methodSymbol.
	ts := cm timeStamp.
	ts = '' ifTrue: [ts := nil].
	self actualClass 
		compile: src
		classified: ClassOrganizer defaultMethodCategory
		withStamp: ts 
		notifying: nil.

	^true
! !

!methodRemoval: Categorizer class #default stamp: 'jmv 3/26/2024 12:35:38'!
Categorizer class removeSelector: #default!
