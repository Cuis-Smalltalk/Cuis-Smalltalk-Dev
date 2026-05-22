Class {
	#name : 'DynamicMenuBuilder',
	#superclass : 'Object',
	#instVars : [
		'title',
		'defaultTarget',
		'menuDefinitionSelector',
		'rootMenu',
		'itemsDefinition',
		'optionsChanger',
		'subMenuItemsDefinition'
	],
	#category : 'Morphic-Menus',
	#package : 'Morphic',
	#tag : 'Menus'
}

{ #category : 'building' }
DynamicMenuBuilder class >> buildTargeting: aDefaultTarget collectingMenuOptionsWith: aMenuDefinitionSelector [

	^(self targeting: aDefaultTarget collectingMenuOptionsWith: aMenuDefinitionSelector) build
]

{ #category : 'building' }
DynamicMenuBuilder class >> buildTitled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuDefinitionSelector [

	^(self titled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuDefinitionSelector) build
]

{ #category : 'building' }
DynamicMenuBuilder class >> buildTitled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuDefinitionSelector changingThemWith: anOptionsChangerBlock [

	^(self titled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuDefinitionSelector changingThemWith: anOptionsChangerBlock) build
]

{ #category : 'examples' }
DynamicMenuBuilder class >> exampleItemWithoutSubMenu [

	^ {
		#itemGroup 	-> 		10.
		#itemOrder 	-> 		10.
		#label 			-> 		'item without submenu'.
		#selector 		-> 		#printString.
		#icon 			-> 		#editFindReplaceIcon
	} asDictionary
]

{ #category : 'examples' }
DynamicMenuBuilder class >> exampleWithSubMenus [

	(self buildTitled: 'Example with Sub Menus' targeting: nil collectingMenuOptionsWith: #exampleWithSubMenusDefinition) openInWorld 
]

{ #category : 'examples' }
DynamicMenuBuilder class >> exampleWithSubMenusDefinition [

	| topItemWithSubMenus subItemWithSubMenu |
	
	topItemWithSubMenus := 'top item with submenu...'.
	subItemWithSubMenu := 'subitem with submenu...'.
	
	^ {
			self exampleItemWithoutSubMenu.
			{
				#itemGroup 	-> 		20.
				#itemOrder 	-> 		10.
				#label 			-> 		topItemWithSubMenus.
				#selector 		-> 		nil.
				#icon 			-> 		#listAddIcon
			} asDictionary.
			{
				#submenuOf -> topItemWithSubMenus .
				#itemGroup 	-> 		10.
				#itemOrder 	-> 		10.
				#label 			-> 		'subitem without submenu'.
				#selector 		-> 		#printString.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#submenuOf -> topItemWithSubMenus .
				#itemGroup 	-> 		10.
				#itemOrder 	-> 		20.
				#label 			-> 		subItemWithSubMenu.
				#selector 		-> 		#printString.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#submenuOf -> topItemWithSubMenus.
				#itemGroup 	-> 		20.
				#itemOrder 	-> 		10.
				#label 			-> 		'another subitem without submenu'.
				#selector 		-> 		#printString.
				#icon 			-> 		#listAddIcon
			} asDictionary.
			{
				#submenuOf -> subItemWithSubMenu.
				#itemGroup 	-> 		10.
				#itemOrder 	-> 	10.
				#label 			-> 		'sub sub item without submenu'.
				#selector 		-> 		#printString.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
		}
	
]

{ #category : 'defaults' }
DynamicMenuBuilder class >> noTitle [

	^nil
]

{ #category : 'instance creation' }
DynamicMenuBuilder class >> targeting: aDefaultTarget collectingMenuOptionsWith: aMenuDefinitionSelector [

	^self titled: self noTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuDefinitionSelector


]

{ #category : 'instance creation' }
DynamicMenuBuilder class >> titled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuDefinitionSelector [

	^self titled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuDefinitionSelector changingThemWith: [ :options | ]
]

{ #category : 'instance creation' }
DynamicMenuBuilder class >> titled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuDefinitionSelector changingThemWith: anOptionsChangerBlock [

	^self new initializeTitled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuDefinitionSelector changingThemWith: anOptionsChangerBlock


]

{ #category : 'building - private' }
DynamicMenuBuilder >> addGroupSeparationTo: aMenuMorph  [
	
	aMenuMorph addLine 
]

{ #category : 'building - private' }
DynamicMenuBuilder >> addGroupedItemsFrom: anItemsDefinition toMenu: aMenu  [

	| theGroups |
	
	theGroups := anItemsDefinition groupBy: [ :item | item at: #itemGroup ].
	theGroups keys asSortedCollection 
		do: [ :group | self addItemsOfGroup: (theGroups at: group) to: aMenu ]
		separatedBy: [ self addGroupSeparationTo: aMenu ].

]

{ #category : 'building - private' }
DynamicMenuBuilder >> addGroupedItemsToMenu [
	
	self addGroupedItemsFrom: self rootItemsDefinition toMenu: rootMenu .
	
]

{ #category : 'building - private' }
DynamicMenuBuilder >> addItemsOfGroup: group to: aMenu  [

	| groupMenuOptions addedMenuItem |
	
	groupMenuOptions := group asSortedCollection: [ :leftItem :rightItem | (leftItem at: #itemOrder) < (rightItem at: #itemOrder) ].
	
	groupMenuOptions do: [ :options | 
		addedMenuItem := aMenu addItemFromDictionaryOrNil: options.
		self addSubMenuTo: addedMenuItem].
]

{ #category : 'building - private' }
DynamicMenuBuilder >> addSubMenuTo: aMenuItem [

	subMenuItemsDefinition 
		at: aMenuItem contents
		ifPresent: [ :subMenuDefinition | self addSubMenuTo: aMenuItem from: subMenuDefinition ]
		ifAbsent: nil
]

{ #category : 'building - private' }
DynamicMenuBuilder >> addSubMenuTo: aMenuItem from: subMenuDefinition [

	| subMenu |

	subMenu := MenuMorph new.
	subMenu defaultTarget: defaultTarget.
	"Add the menu items recursively to the parent menu item - Hernan"
	self addGroupedItemsFrom: subMenuDefinition toMenu: subMenu .
	aMenuItem subMenu: subMenu
]

{ #category : 'building' }
DynamicMenuBuilder >> build [

	self 
		createRootMenu;
	 	collectMenuOptions;
		changeOptions;
		initializeSubMenusDefinition;
		addGroupedItemsToMenu.		
			
	^ rootMenu.
]

{ #category : 'building - private' }
DynamicMenuBuilder >> changeOptions [

	optionsChanger value: itemsDefinition
]

{ #category : 'building - private' }
DynamicMenuBuilder >> classesProvidingMenuOptions [

	^(Smalltalk allClassesImplementing: menuDefinitionSelector) select: [ :aClass | aClass isMeta ]
]

{ #category : 'building - private' }
DynamicMenuBuilder >> collectMenuOptions [

	itemsDefinition := OrderedCollection new.
	self classesProvidingMenuOptions do: [ :aClass | itemsDefinition addAll: (aClass soleInstance perform: menuDefinitionSelector) ].
	
]

{ #category : 'building - private' }
DynamicMenuBuilder >> createRootMenu [

	rootMenu := self hasTitle ifTrue: [ MenuMorph entitled: title] ifFalse: [ MenuMorph new ].
	rootMenu defaultTarget: defaultTarget
]

{ #category : 'testing' }
DynamicMenuBuilder >> hasTitle [

	^title ~= self class noTitle 
]

{ #category : 'building - private' }
DynamicMenuBuilder >> initializeSubMenusDefinition [

	^ subMenuItemsDefinition := (itemsDefinition select: [ :item | item includesKey: #submenuOf ]) groupBy: [ :item | item at: #submenuOf ]
]

{ #category : 'initialization' }
DynamicMenuBuilder >> initializeTitled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuOptionsSelector changingThemWith: anOptionsChangerBlock [

	title := aTitle.
	defaultTarget := aDefaultTarget.
	menuDefinitionSelector := aMenuOptionsSelector.
	optionsChanger := anOptionsChangerBlock 
]

{ #category : 'building - private' }
DynamicMenuBuilder >> rootItemsDefinition [

	^ itemsDefinition reject: [ :item | item includesKey: #submenuOf ]
]
