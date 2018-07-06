'From Cuis 5.0 of 7 November 2016 [latest update: #3348] on 5 July 2018 at 4:00:10 pm'!
!classDefinition: #DynamicMenuBuilder category: #'Morphic-Menus'!
Object subclass: #DynamicMenuBuilder
	instanceVariableNames: 'title defaultTarget menuOptionsSelector menu items groups '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!

!DynamicMenuBuilder methodsFor: 'initialization' stamp: 'HAW 7/5/2018 15:42:29'!
initializeTitled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuOptionsSelector

	title := aTitle.
	defaultTarget := aDefaultTarget.
	menuOptionsSelector := aMenuOptionsSelector ! !

!DynamicMenuBuilder methodsFor: 'building - private' stamp: 'HAW 7/5/2018 15:59:10'!
addGroupSeparation 

	menu addLine
	! !

!DynamicMenuBuilder methodsFor: 'building - private' stamp: 'HAW 7/5/2018 15:56:23'!
addGroupedMenuOptionsToMenu
	
	groups := items groupBy: [ :item | item at: #itemGroup ].
	groups keys asSortedCollection 
		do: [ :group | self addMenuOptionsOfGroup: group ]
		separatedBy: [ self addGroupSeparation ].
! !

!DynamicMenuBuilder methodsFor: 'building - private' stamp: 'HAW 7/5/2018 15:55:59'!
addMenuOptionsOfGroup: group

	| groupMenuOptions |
	
	groupMenuOptions := (groups at: group) asSortedCollection: [ :leftItem :rightItem | (leftItem at: #itemOrder) < (rightItem at: #itemOrder) ].
	menu addItemsFromDictionaries: groupMenuOptions.! !

!DynamicMenuBuilder methodsFor: 'building - private' stamp: 'HAW 7/5/2018 15:53:08'!
collectMenuOptions

	items := (Smalltalk allClassesImplementing: menuOptionsSelector)
		collect: [ :item | item isMeta ifTrue: [ item soleInstance perform: menuOptionsSelector ] ]
		thenSelect: [ :item | item notNil ].! !


!DynamicMenuBuilder class methodsFor: 'instance creation' stamp: 'HAW 7/5/2018 15:41:58'!
titled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuOptionsSelector

	^self new initializeTitled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuOptionsSelector

! !


!DynamicMenuBuilder methodsFor: 'building' stamp: 'HAW 7/5/2018 15:57:34'!
build

	self 
		createMenu;
	 	collectMenuOptions;
		addGroupedMenuOptionsToMenu.		
			
	^ menu.! !

!DynamicMenuBuilder methodsFor: 'building - private' stamp: 'HAW 7/5/2018 15:52:31'!
createMenu

	menu := MenuMorph entitled: title.
	menu 
		defaultTarget: defaultTarget; 
		addStayUpIcons! !


!TheWorldMenu methodsFor: 'construction' stamp: 'HAW 7/5/2018 15:47:16'!
openMenu
	"Build the open window menu for the world."

	^(DynamicMenuBuilder titled: 'Open...' targeting: self collectingMenuOptionsWith: #worldMenuForOpenGroup) build

	! !

!classDefinition: #DynamicMenuBuilder category: #'Morphic-Menus'!
Object subclass: #DynamicMenuBuilder
	instanceVariableNames: 'title defaultTarget menuOptionsSelector menu items groups'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!

!DynamicMenuBuilder reorganize!
('initialization' initializeTitled:targeting:collectingMenuOptionsWith:)
('building' build)
('building - private' addGroupSeparation addGroupedMenuOptionsToMenu addMenuOptionsOfGroup: collectMenuOptions createMenu)
!


!DynamicMenuBuilder class reorganize!
('instance creation' titled:targeting:collectingMenuOptionsWith:)
!

