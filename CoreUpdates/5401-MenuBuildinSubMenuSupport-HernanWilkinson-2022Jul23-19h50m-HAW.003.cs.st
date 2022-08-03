'From Cuis 6.0 [latest update: #5400] on 25 July 2022 at 8:12:10 am'!
!classDefinition: #DynamicMenuBuilder category: #'Morphic-Menus'!
Object subclass: #DynamicMenuBuilder
	instanceVariableNames: 'title defaultTarget menuOptionsSelector menu items groups optionsChanger rootMenu menuDefinitionSelector subMenuItemsDefinition itemsDefinition '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!

!MenuMorph methodsFor: 'construction' stamp: 'HAW 7/25/2022 07:42:50'!
addItemFromDictionary: aMenuItemDefinition
	"A menu constructor utility that uses Dictionaries with elements:
		#label - the name that displays in the menu
		#object - the target object. If nil, use defaultTarget. If a Symbol, send it as message to defaultTarget to get real target.
		#selector - the selector called on object when the menu item is selected
		#arguments - optional collection of arguments passed to the selector
		#balloonText - optional 'tool tip' style help text
		#icon-	optional icon selector or Form
		#submenuOf - optional label of the parent menu this item should belong to"
		
	| realTarget  item |
	
	realTarget _ aMenuItemDefinition at: #object ifAbsent: [defaultTarget].
	realTarget isSymbol ifTrue: [ realTarget _ defaultTarget perform: realTarget ].
	
	item _ self addItemFromDictionary: aMenuItemDefinition targeting: realTarget.
	
	(Preferences at: #wantsMenuIcons) ifTrue: [ aMenuItemDefinition at: #icon ifPresent: [ :symbolOrFormOrNil | item setIcon: symbolOrFormOrNil ]].
	aMenuItemDefinition at: #balloonText ifPresent: [ :balloonText | item setBalloonText: balloonText ].
	
	^item! !

!MenuMorph methodsFor: 'construction' stamp: 'HAW 7/25/2022 07:42:50'!
addItemFromDictionary: aMenuItemDefinition targeting: realTarget

	| label selector argumentList |
	
	label := aMenuItemDefinition at: #label.
	selector := aMenuItemDefinition at: #selector.
	argumentList := aMenuItemDefinition at: #arguments ifAbsent: [ #() ].
	
	^ label isSymbol
		ifTrue: [
			self
				addUpdating: label
				target: realTarget
				action: selector
				argumentList: argumentList ]
		ifFalse: [
			self
				add: label
				target: realTarget
				action: selector
				argumentList: argumentList ]! !

!MenuMorph methodsFor: 'construction' stamp: 'HAW 7/25/2022 07:36:27'!
addItemFromDictionaryOrNil: aMenuItemDefinitionOrNil 
	"See addItemFromDictionary: for definition of the expected Dictionary.
	Note: nil elements will add a line."

	^aMenuItemDefinitionOrNil
		ifNil: [ self addLine ]
		ifNotNil: [ self addItemFromDictionary: aMenuItemDefinitionOrNil ].
	
	! !


!DynamicMenuBuilder methodsFor: 'building - private' stamp: 'HAW 7/23/2022 22:02:15'!
addGroupSeparationTo: aMenuMorph 
	
	aMenuMorph addLine ! !

!DynamicMenuBuilder methodsFor: 'building - private' stamp: 'HAW 7/25/2022 07:20:26'!
addGroupedItemsFrom: anItemsDefinition toMenu: aMenu 

	| theGroups |
	
	theGroups := anItemsDefinition groupBy: [ :item | item at: #itemGroup ].
	theGroups keys asSortedCollection 
		do: [ :group | self addItemsOfGroup: (theGroups at: group) to: aMenu ]
		separatedBy: [ self addGroupSeparationTo: aMenu ].
! !

!DynamicMenuBuilder methodsFor: 'building - private' stamp: 'HAW 7/25/2022 07:21:02'!
addGroupedItemsToMenu
	
	self addGroupedItemsFrom: self rootItemsDefinition toMenu: rootMenu .
	! !

!DynamicMenuBuilder methodsFor: 'building - private' stamp: 'HAW 7/25/2022 07:31:31'!
addItemsOfGroup: group to: aMenu 

	| groupMenuOptions addedMenuItem |
	
	groupMenuOptions := group asSortedCollection: [ :leftItem :rightItem | (leftItem at: #itemOrder) < (rightItem at: #itemOrder) ].
	
	groupMenuOptions do: [ :options | 
		addedMenuItem := aMenu addItemFromDictionaryOrNil: options.
		self addSubMenuTo: addedMenuItem].! !

!DynamicMenuBuilder methodsFor: 'building - private' stamp: 'HAW 7/25/2022 07:59:56'!
addSubMenuTo: aMenuItem

	subMenuItemsDefinition 
		at: aMenuItem contents
		ifPresent: [ :subMenuDefinition | self addSubMenuTo: aMenuItem from: subMenuDefinition ]
		ifAbsent: []! !

!DynamicMenuBuilder methodsFor: 'building - private' stamp: 'HAW 7/25/2022 07:15:31'!
addSubMenuTo: aMenuItem from: subMenuDefinition

	| subMenu |

	subMenu := MenuMorph new.
	"Add the menu items recursively to the parent menu item - Hernan"
	self addGroupedItemsFrom: subMenuDefinition toMenu: subMenu .
	aMenuItem subMenu: subMenu! !

!DynamicMenuBuilder methodsFor: 'building - private' stamp: 'HAW 7/25/2022 07:24:05'!
createRootMenu

	rootMenu := self hasTitle ifTrue: [ MenuMorph entitled: title] ifFalse: [ MenuMorph new ].
	rootMenu defaultTarget: defaultTarget! !

!DynamicMenuBuilder methodsFor: 'building - private' stamp: 'HAW 7/25/2022 07:59:56'!
initializeSubMenusDefinition

	^ subMenuItemsDefinition := (itemsDefinition select: [ :item | item includesKey: #submenuOf ]) groupBy: [ :item | item at: #submenuOf ]! !

!DynamicMenuBuilder methodsFor: 'building - private' stamp: 'HAW 7/25/2022 07:20:39'!
rootItemsDefinition

	^ itemsDefinition reject: [ :item | item includesKey: #submenuOf ]! !


!DynamicMenuBuilder class methodsFor: 'examples' stamp: 'HAW 7/25/2022 08:10:47'!
exampleItemWithoutSubMenu

	^ {
		#itemGroup 			-> 		10.
		#itemOrder 			-> 		10.
		#label 			-> 		'item without submenu'.
		#selector 			-> 		#printString.
		#icon 			-> 		#editFindReplaceIcon
	} asDictionary! !

!DynamicMenuBuilder class methodsFor: 'examples' stamp: 'HAW 7/25/2022 07:58:33'!
exampleWithSubMenus

	(self buildTitled: 'Example with Sub Menus' targeting: nil collectingMenuOptionsWith: #exampleWithSubMenusDefinition) openInWorld ! !

!DynamicMenuBuilder class methodsFor: 'examples' stamp: 'HAW 7/25/2022 08:11:37'!
exampleWithSubMenusDefinition

	| topItemWithSubMenus subItemWithSubMenu |
	
	topItemWithSubMenus := 'top item with submenu...'.
	subItemWithSubMenu := 'subitem with submenu...'.
	
	^ {
			self exampleItemWithoutSubMenu.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		topItemWithSubMenus.
				#selector 			-> 		nil.
				#icon 			-> 		#listAddIcon
			} asDictionary.
			{
				#submenuOf -> topItemWithSubMenus .
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'subitem without submenu'.
				#selector 		-> 		#printString.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#submenuOf -> topItemWithSubMenus .
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		20.
				#label 			-> 		subItemWithSubMenu.
				#selector 		-> 		#printString.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#submenuOf -> topItemWithSubMenus.
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'another subitem without submenu'.
				#selector 		-> 		#printString.
				#icon 			-> 		#listAddIcon
			} asDictionary.
			{
				#submenuOf -> subItemWithSubMenu.
				#itemGroup 	-> 		10.
				#itemOrder 		-> 	10.
				#label 			-> 		'sub sub item without submenu'.
				#selector 		-> 		#printString.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
		}
	! !


!MenuMorph methodsFor: 'construction' stamp: 'HAW 7/25/2022 07:43:35'!
addItemsFromDictionaries: aMenuItemsDefinition
	
	"see addItemFromDictionary: for expected dictionary definition"
	
	aMenuItemsDefinition do: [ :menuItemDefinition | self addItemFromDictionaryOrNil: menuItemDefinition ]! !

!MenuMorph methodsFor: 'construction' stamp: 'HAW 7/25/2022 07:48:03'!
addLine
	"Append a divider line to this menu. Suppress duplicate lines."

	| lastMorph |
	
	submorphs isEmpty ifTrue: [^ self].
	
	lastMorph := self lastSubmorph.
	lastMorph class == MenuLineMorph
		ifFalse: [
			lastMorph := MenuLineMorph new.
			self addMorphBack: lastMorph ].
		
	^lastMorph 
! !


!DynamicMenuBuilder methodsFor: 'initialization' stamp: 'HAW 7/25/2022 07:21:30'!
initializeTitled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuOptionsSelector changingThemWith: anOptionsChangerBlock

	title := aTitle.
	defaultTarget := aDefaultTarget.
	menuDefinitionSelector := aMenuOptionsSelector.
	optionsChanger := anOptionsChangerBlock ! !

!DynamicMenuBuilder methodsFor: 'building' stamp: 'HAW 7/25/2022 07:59:50'!
build

	self 
		createRootMenu;
	 	collectMenuOptions;
		changeOptions;
		initializeSubMenusDefinition;
		addGroupedItemsToMenu.		
			
	^ rootMenu.! !

!DynamicMenuBuilder methodsFor: 'building - private' stamp: 'HAW 7/25/2022 07:20:39'!
changeOptions

	optionsChanger value: itemsDefinition! !

!DynamicMenuBuilder methodsFor: 'building - private' stamp: 'HAW 7/25/2022 07:21:30'!
classesProvidingMenuOptions

	^(Smalltalk allClassesImplementing: menuDefinitionSelector) select: [ :aClass | aClass isMeta ]! !

!DynamicMenuBuilder methodsFor: 'building - private' stamp: 'HAW 7/25/2022 07:21:30'!
collectMenuOptions

	itemsDefinition := OrderedCollection new.
	self classesProvidingMenuOptions do: [ :aClass | itemsDefinition addAll: (aClass soleInstance perform: menuDefinitionSelector) ].
	! !


!DynamicMenuBuilder class methodsFor: 'instance creation' stamp: 'HAW 7/25/2022 07:21:54'!
targeting: aDefaultTarget collectingMenuOptionsWith: aMenuDefinitionSelector

	^self titled: self noTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuDefinitionSelector

! !

!DynamicMenuBuilder class methodsFor: 'instance creation' stamp: 'HAW 7/25/2022 07:22:10'!
titled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuDefinitionSelector

	^self titled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuDefinitionSelector changingThemWith: [ :options | ]! !

!DynamicMenuBuilder class methodsFor: 'instance creation' stamp: 'HAW 7/25/2022 07:22:23'!
titled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuDefinitionSelector changingThemWith: anOptionsChangerBlock

	^self new initializeTitled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuDefinitionSelector changingThemWith: anOptionsChangerBlock

! !

!DynamicMenuBuilder class methodsFor: 'building' stamp: 'HAW 7/25/2022 07:22:41'!
buildTargeting: aDefaultTarget collectingMenuOptionsWith: aMenuDefinitionSelector

	^(self targeting: aDefaultTarget collectingMenuOptionsWith: aMenuDefinitionSelector) build! !

!DynamicMenuBuilder class methodsFor: 'building' stamp: 'HAW 7/25/2022 07:22:47'!
buildTitled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuDefinitionSelector

	^(self titled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuDefinitionSelector) build! !

!DynamicMenuBuilder class methodsFor: 'building' stamp: 'HAW 7/25/2022 07:22:55'!
buildTitled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuDefinitionSelector changingThemWith: anOptionsChangerBlock

	^(self titled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuDefinitionSelector changingThemWith: anOptionsChangerBlock) build! !

!methodRemoval: DynamicMenuBuilder class #exampleMenuWithSubMenues stamp: 'HAW 7/25/2022 07:18:34'!
DynamicMenuBuilder class removeSelector: #exampleMenuWithSubMenues!
!methodRemoval: DynamicMenuBuilder class #exampleMenuWithSubMenuesDefinition stamp: 'HAW 7/25/2022 07:50:57'!
DynamicMenuBuilder class removeSelector: #exampleMenuWithSubMenuesDefinition!
!methodRemoval: DynamicMenuBuilder class #exampleWithSubMenues stamp: 'HAW 7/25/2022 07:58:03'!
DynamicMenuBuilder class removeSelector: #exampleWithSubMenues!
!methodRemoval: DynamicMenuBuilder class #example stamp: 'HAW 7/25/2022 07:51:14'!
DynamicMenuBuilder class removeSelector: #example!
!methodRemoval: DynamicMenuBuilder class #exampleWithSubMenuesDefinition stamp: 'HAW 7/25/2022 07:58:12'!
DynamicMenuBuilder class removeSelector: #exampleWithSubMenuesDefinition!
!methodRemoval: DynamicMenuBuilder class #itemWithoutSubMenu stamp: 'HAW 7/25/2022 08:09:03'!
DynamicMenuBuilder class removeSelector: #itemWithoutSubMenu!
!methodRemoval: DynamicMenuBuilder #addGroupedMenuOptionsToMenu stamp: 'HAW 7/25/2022 07:10:30'!
DynamicMenuBuilder removeSelector: #addGroupedMenuOptionsToMenu!
!methodRemoval: DynamicMenuBuilder #addGroupedMenuOptionsToMenu2 stamp: 'HAW 7/25/2022 07:11:07'!
DynamicMenuBuilder removeSelector: #addGroupedMenuOptionsToMenu2!
!methodRemoval: DynamicMenuBuilder #addMenuOptionsOfGroup:to:with: stamp: 'HAW 7/25/2022 07:05:49'!
DynamicMenuBuilder removeSelector: #addMenuOptionsOfGroup:to:with:!
!methodRemoval: DynamicMenuBuilder #addItemsOfGroup:to:with: stamp: 'HAW 7/25/2022 07:06:26'!
DynamicMenuBuilder removeSelector: #addItemsOfGroup:to:with:!
!methodRemoval: DynamicMenuBuilder #addMenuOptionsOfGroup: stamp: 'HAW 7/25/2022 07:11:27'!
DynamicMenuBuilder removeSelector: #addMenuOptionsOfGroup:!
!methodRemoval: DynamicMenuBuilder #initializeSubmenusDefinition stamp: 'HAW 7/25/2022 07:59:50'!
DynamicMenuBuilder removeSelector: #initializeSubmenusDefinition!
!methodRemoval: DynamicMenuBuilder #createMenu stamp: 'HAW 7/25/2022 07:24:05'!
DynamicMenuBuilder removeSelector: #createMenu!
!methodRemoval: DynamicMenuBuilder #addGroupedItemsFrom:toMenu:with: stamp: 'HAW 7/25/2022 07:05:26'!
DynamicMenuBuilder removeSelector: #addGroupedItemsFrom:toMenu:with:!
!methodRemoval: DynamicMenuBuilder #build2 stamp: 'HAW 7/25/2022 07:10:10'!
DynamicMenuBuilder removeSelector: #build2!
!methodRemoval: DynamicMenuBuilder #addGroupedMenuOptions:toMenu:with: stamp: 'HAW 7/25/2022 07:04:38'!
DynamicMenuBuilder removeSelector: #addGroupedMenuOptions:toMenu:with:!
!methodRemoval: DynamicMenuBuilder #initializeSubmenuesDefinition stamp: 'HAW 7/25/2022 07:59:19'!
DynamicMenuBuilder removeSelector: #initializeSubmenuesDefinition!
!methodRemoval: DynamicMenuBuilder #addGroupSeparation stamp: 'HAW 7/25/2022 07:10:33'!
DynamicMenuBuilder removeSelector: #addGroupSeparation!
!methodRemoval: MenuMorph #addMenuItemFrom:targeting: stamp: 'HAW 7/25/2022 07:42:50'!
MenuMorph removeSelector: #addMenuItemFrom:targeting:!
!methodRemoval: MenuMorph #addItemsFromDictionaries:with: stamp: 'HAW 7/25/2022 07:25:42'!
MenuMorph removeSelector: #addItemsFromDictionaries:with:!

!MenuMorph reorganize!
('accessing' addBlankIconsIfNecessary items itemsDo: label lastItem layerNumber stayUp stayUp:)
('construction' add:action: add:action:balloonText: add:action:icon: add:action:icon:enabled: add:subMenu: add:target:action: add:target:action:argument: add:target:action:argument:icon: add:target:action:argumentList: add:target:action:icon: add:targetHighlight:action:argumentList: addItemFromDictionary: addItemFromDictionary:targeting: addItemFromDictionaryOrNil: addItemsFromDictionaries: addLine addServices:for:extraLines: addStayUpIcons addTitle: addUpdating:action: addUpdating:target:action: addUpdating:target:action:argumentList: defaultTarget: labels:lines:selections:)
('control' activeSubmenu: deleteIfPopUp: popUpAdjacentTo:from: popUpAt:allowKeyboard: popUpAt:forHand:in: popUpAt:forHand:in:allowKeyboard: popUpForHand:in: popUpInWorld popUpInWorld: selectItem: wantsToBeDroppedInto:)
('dropping/grabbing' justDroppedInto:event:)
('events' keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Up:localPosition:)
('event handling testing' handlesKeyboard handlesMouseDown:)
('events-processing' handleFocusEvent:)
('geometry' fontPreferenceChanged)
('initialization' defaultBorderWidth defaultColor delete initialize intoWorld:)
('keyboard control' displayFiltered: keyboardFocusChange: moveSelectionDown:event:)
('menu' removeStayUpBox)
('modal control' invokeModal invokeModal: isModalInvokationDone isModalInvokationDone: modalSelection modalSelection:)
('testing' isIncludedInTaskbar)
('private' adjustSubmorphsLayout fitInWorld selectedItem)
('drawing' drawOn:)
('misc' activate:)
('modal progress dialog' displayAt:during: informUserAt:during:)
('halos and balloon help' addHalo:)
!

!classDefinition: #DynamicMenuBuilder category: #'Morphic-Menus'!
Object subclass: #DynamicMenuBuilder
	instanceVariableNames: 'title defaultTarget menuDefinitionSelector rootMenu itemsDefinition optionsChanger subMenuItemsDefinition'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!

!DynamicMenuBuilder reorganize!
('initialization' initializeTitled:targeting:collectingMenuOptionsWith:changingThemWith:)
('building' build)
('testing' hasTitle)
('building - private' addGroupSeparationTo: addGroupedItemsFrom:toMenu: addGroupedItemsToMenu addItemsOfGroup:to: addSubMenuTo: addSubMenuTo:from: changeOptions classesProvidingMenuOptions collectMenuOptions createRootMenu initializeSubMenusDefinition rootItemsDefinition)
!


!DynamicMenuBuilder class reorganize!
('instance creation' targeting:collectingMenuOptionsWith: titled:targeting:collectingMenuOptionsWith: titled:targeting:collectingMenuOptionsWith:changingThemWith:)
('defaults' noTitle)
('building' buildTargeting:collectingMenuOptionsWith: buildTitled:targeting:collectingMenuOptionsWith: buildTitled:targeting:collectingMenuOptionsWith:changingThemWith:)
('examples' exampleItemWithoutSubMenu exampleWithSubMenus exampleWithSubMenusDefinition)
!

