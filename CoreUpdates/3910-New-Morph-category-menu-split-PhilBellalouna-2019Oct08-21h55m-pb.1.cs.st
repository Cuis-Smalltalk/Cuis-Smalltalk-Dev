'From Cuis 5.0 [latest update: #3904] on 8 October 2019 at 10:16:09 pm'!

!TheWorldMenu methodsFor: 'private' stamp: 'pb 10/8/2019 22:07:22'!
pvtAlphaSplitListDepth
	"Split factor.  A higher number results in fewer items in each submenu"
	^ 4! !

!TheWorldMenu methodsFor: 'private' stamp: 'pb 10/8/2019 22:07:30'!
pvtCategorySplitListDepth
	"Split factor.  A higher number results in fewer items in each submenu"
	^ 2.! !

!TheWorldMenu methodsFor: 'private' stamp: 'pb 10/8/2019 22:14:40'!
pvtMaxItemsPerCategorySubMenu
	"If the number of items exceeds this value, split the category submenu into sub-submenus"
	^ 15.! !

!TheWorldMenu methodsFor: 'private' stamp: 'pb 10/8/2019 22:00:02'!
pvtNewMenuForSplitLists: splitLists
| firstChar lastChar menu subMenu |
menu := MenuMorph new defaultTarget: self.
	1
		to: splitLists size
		do: [ :i |
			firstChar := i = 1
				ifTrue: [ $A ]
				ifFalse: [ 
					(splitLists at: i) first name first: 3 ].
			lastChar := i = splitLists size
				ifTrue: [ $Z ]
				ifFalse: [ 
					(splitLists at: i) last name first: 3 ].
			subMenu := MenuMorph new.
			(splitLists at: i) do: [ :cl |
				subMenu
					add: cl name
					target: self
					action: #newMorphOfClass:event:
					argument: cl ].
			menu
				add: firstChar asString , ' - ' , lastChar asString
				subMenu: subMenu ].
	^ menu.! !


!TheWorldMenu methodsFor: 'construction' stamp: 'pb 10/8/2019 22:07:54'!
alphabeticalMorphMenu
	| list splitLists |
	list := Morph withAllSubclasses select: [ :m |
		m includeInNewMorphMenu ].
	list := list asArray sort: [ :c1 :c2 |
		c1 name < c2 name ].
	splitLists := self
		splitNewMorphList: list
		depth: self pvtAlphaSplitListDepth .
	^ self pvtNewMenuForSplitLists: splitLists 
! !

!TheWorldMenu methodsFor: 'construction' stamp: 'pb 10/8/2019 22:11:22'!
newMorph
	"The user requested 'new morph' from the world menu.  Put up a menu that allows many ways of obtaining new morphs.  If the preference #classicNewMorphMenu is true, the full form of yore is used; otherwise, a much shortened form is used."
	| menu catDict |
	menu _ self menu: 'Add a new morph'.
	menu
		
			add: 'From Clipboard'
			target: myHand
			action: #pasteMorph;
		
			add: 'From Alphabetical List'
			subMenu: self alphabeticalMorphMenu.
	menu addLine.
	"Add 'Classic' New Morph menu"
	catDict _ Dictionary new.
	Morph allSubclassesDo: [ :eaSubclass |
		eaSubclass includeInNewMorphMenu ifTrue: [ | category |
			(eaSubclass respondsTo: #categoryInNewMorphMenu)
				ifTrue: [ category _ eaSubclass categoryInNewMorphMenu ]
				ifFalse: [ category _ 'Unknown' ].
			(catDict includesKey: category)
				ifTrue: [ (catDict at: category) add: eaSubclass ]
				ifFalse: [
					catDict
						at: category
						put: (OrderedCollection with: eaSubclass) ]]].
	catDict keys sort do: [ :categ | | morphsInCat |
		morphsInCat _ (catDict at: categ) asArray sort: [ :c1 :c2 |
			c1 name < c2 name ].
		morphsInCat size > self pvtMaxItemsPerCategorySubMenu
			ifTrue: [
				menu
					add: categ
					subMenu:
						(self pvtNewMenuForSplitLists:
							(self
								splitNewMorphList: morphsInCat
								depth: self pvtCategorySplitListDepth )) ]
			ifFalse: [ | subMenu |
				subMenu _ MenuMorph new.
				morphsInCat do: [ :cl |
					subMenu
						add: cl name
						target: self
						action: #newMorphOfClass:event:
						argument: cl ].
				menu
					add: categ
					subMenu: subMenu ]].
	self doPopUp: menu.! !

!methodRemoval: TheWorldMenu #pvtMaxItemsPerCategory!
TheWorldMenu removeSelector: #pvtMaxItemsPerCategory!

!TheWorldMenu reorganize!
('commands' cleanUpWorld fullScreenOff fullScreenOn garbageCollect newMorphOfClass:event: quitSession saveAndQuit saveWorldInFile splitNewMorphList:depth: startMessageTally vmStatistics)
('construction' alphabeticalMorphMenu buildWorldMenu changesMenu debugMenu helpMenu newMorph openMenu preferencesMenu)
('mechanics' menu: myWorld world:hand:)
('menu' openCommentGuide openTerseGuide)
('popups' doPopUp: openChangesMenu openDebugMenu openHelpMenu openOpenMenu openPreferencesMenu openWindowsMenu)
('windows & flaps menu' windowsMenu)
('private' pvtAlphaSplitListDepth pvtCategorySplitListDepth pvtMaxItemsPerCategorySubMenu pvtNewMenuForSplitLists:)
!

