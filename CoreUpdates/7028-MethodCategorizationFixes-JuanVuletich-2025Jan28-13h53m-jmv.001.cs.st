'From Cuis7.3 [latest update: #7027] on 28 January 2025 at 1:55:34 pm'!

!Categorizer methodsFor: 'accessing' stamp: 'jmv 1/28/2025 13:52:24'!
initEmpty
	categoryArray :=#().
	categoryStops := #().
	elementArray := #().! !


!Categorizer class methodsFor: 'instance creation' stamp: 'jmv 1/28/2025 13:52:36'!
empty
	"Answer an instance of me with initial elements from the argument"

	^self new initEmpty! !


!ClassOrganizer methodsFor: 'accessing' stamp: 'jmv 1/28/2025 13:52:28'!
initEmpty
	SystemChangeNotifier uniqueInstance doSilently: [
		super initEmpty ].! !

!ClassOrganizer methodsFor: 'accessing' stamp: 'jmv 1/28/2025 12:29:42'!
unclassifiedSelectors: anArray

	SystemChangeNotifier uniqueInstance doSilently: [
		categoryArray := {self class defaultCategory}.
		categoryStops := {anArray size}.
		elementArray := anArray ].! !


!ClassOrganizer class methodsFor: 'instance creation' stamp: 'jmv 1/28/2025 12:42:38'!
withUnclassifiedSelectors: anArray 

	^self new unclassifiedSelectors: anArray! !


!PseudoClassOrganizer methodsFor: 'accessing' stamp: 'jmv 1/28/2025 13:52:32'!
initEmpty
	super initEmpty.
	self classComment: nil! !


!ClassDescription methodsFor: 'organization' stamp: 'jmv 1/28/2025 12:23:46'!
organization
	"Answer the instance of ClassOrganizer that represents the organization 
	of the messages of the receiver."

	organization ifNil:
		[self organization: (ClassOrganizer withUnclassifiedSelectors: self methodDict keys sort)].
	(organization isMemberOf: Array) ifTrue:
		[self recoverFromMDFaultWithTrace].
	
	"Making sure that subject is set correctly. It should not be necessary."
	organization ifNotNil: [organization setSubject: self].
	^ organization! !


!Categorizer methodsFor: 'accessing' stamp: 'jmv 1/28/2025 13:52:44'!
changeFromString: aString 
	"Parse the argument, aString, and make this be the receiver's structure."

	| categorySpecs |
	categorySpecs := Scanner new scanTokens: aString.
	"If nothing was scanned and I had no elements before, then default me"
	(categorySpecs isEmpty and: [elementArray isEmpty])
		ifTrue: [^ self initEmpty].

	^ self changeFromCategorySpecs: categorySpecs! !

!Categorizer methodsFor: 'accessing' stamp: 'jmv 1/28/2025 12:07:37'!
removeCategory: cat 
	"Remove the category named, cat. Create an error notificiation if the 
	category has any elements in it."

	| index lastStop |
	index := categoryArray indexOf: cat ifAbsent: [^self].
	lastStop := 
		index = 1
			ifTrue: [0]
			ifFalse: [categoryStops at: index - 1].
	(categoryStops at: index) - lastStop > 0 
		ifTrue: [^self error: 'cannot remove non-empty category'].
	categoryArray := categoryArray copyReplaceFrom: index to: index with: #().
	categoryStops := categoryStops copyReplaceFrom: index to: index with: #().
! !

!Categorizer methodsFor: 'accessing' stamp: 'jmv 1/28/2025 12:06:32'!
removeEmptyCategories
	"Remove empty categories."

	| categoryIndex currentStop keptCategories keptStops |
	keptCategories := WriteStream on: (Array new: 16).
	keptStops := WriteStream on: (Array new: 16).
	currentStop := categoryIndex := 0.
	[(categoryIndex := categoryIndex + 1) <= categoryArray size]
		whileTrue: 
			[(categoryStops at: categoryIndex) > currentStop
				ifTrue: 
					[keptCategories nextPut: (categoryArray at: categoryIndex).
					keptStops nextPut: (currentStop := categoryStops at: categoryIndex)]].
	categoryArray := keptCategories contents.
	categoryStops := keptStops contents.

	"ClassOrganizer allInstancesDo: [:co | co removeEmptyCategories]."! !


!CodeFileBrowser methodsFor: 'accessing' stamp: 'jmv 1/28/2025 13:47:03'!
contents: input notifying: aRequestor 
	"The retrieved information has changed and its source must now be 
	updated. The information can be a variety of things, depending on the 
	list selections (such as templates for class or message definition, methods) 
	or the user menu commands (such as definition, comment, hierarchy). 
	Answer the result of updating the source."

	| codeString theClass |
	codeString := input string.

	editSelection == #editComment 
		ifTrue: [
			theClass := self selectedClass.
			theClass ifNil: [
				self inform: 'You must select a class
before giving it a comment.'.
				^ false ].
			theClass comment: codeString. 
			^ true ].

	self inform:'You cannot change the current selection'.
	^false! !


!CodeFile methodsFor: 'accessing' stamp: 'jmv 1/28/2025 13:52:07'!
organization
	^ SystemOrganizer empty.! !


!PseudoClass methodsFor: 'accessing' stamp: 'jmv 1/28/2025 13:52:10'!
organization
	organization ifNil: [
		organization := PseudoClassOrganizer empty].

	"Making sure that subject is set correctly. It should not be necessary."
	organization setSubject: self.
	^ organization! !

!PseudoClass methodsFor: 'methods' stamp: 'jmv 1/28/2025 13:44:25'!
removeSelector: aSelector
	| catName |
	catName := self class removedCategoryName asPlainString.
	self organization addCategory: catName before:
		(self organization categories ifNotEmpty: [ :coll | coll first ]).
	self organization classify: aSelector under: catName.
	self sourceCodeAt: aSelector put:'methodWasRemoved' asText.! !

!methodRemoval: CodeFileBrowser #changeMessageCategories: stamp: 'jmv 1/28/2025 13:54:55'!
CodeFileBrowser removeSelector: #changeMessageCategories:!
!methodRemoval: PseudoClassOrganizer #setDefaultList: stamp: 'jmv 1/28/2025 13:54:56'!
PseudoClassOrganizer removeSelector: #setDefaultList:!
!methodRemoval: ClassOrganizer #setDefaultList: stamp: 'jmv 1/28/2025 13:54:56'!
ClassOrganizer removeSelector: #setDefaultList:!
!methodRemoval: Categorizer class #defaultList: stamp: 'jmv 1/28/2025 13:54:56'!
Categorizer class removeSelector: #defaultList:!
!methodRemoval: Categorizer #setDefaultList: stamp: 'jmv 1/28/2025 13:54:56'!
Categorizer removeSelector: #setDefaultList:!
