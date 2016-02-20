'From Cuis 4.2 of 25 July 2013 [latest update: #2686] on 20 February 2016 at 6:42:04.595916 pm'!

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 2/20/2016 18:41'!
openMenu
	"Build the open window menu for the world."
	| menu items groups firstGroup itemsSorted itemsBase |
	menu _ self menu: 'Open...'.
	itemsBase _ (Smalltalk allClassesImplementing: #worldMenu:menuGroup:)
		collect: [ :item |
			item class = Metaclass ifTrue: [
				item soleInstance
					worldMenu: menu
					menuGroup: #worldOpenMenu ] ]
		thenSelect: [ :item |
			item notNil ].
	items _ OrderedCollection new.
	"A single class may add more than one item to a menu"
	itemsBase do: [ :item |
		item class = Dictionary
			ifTrue: [ items add: item ]
			ifFalse: [ items addAll: item ]].
	groups _ (items collect: [ :item |
		item at: #itemGroup ]) asSet asSortedCollection.
	itemsSorted _ OrderedCollection new.
	firstGroup _ true.
	groups do: [ :group |
		firstGroup
			ifTrue: [ firstGroup _ false ]
			ifFalse: [ itemsSorted add: nil ].
		((items select: [ :item |
			(item at: #itemGroup) = group ]) sort: [ :item1 :item2 |
			(item1 at: #itemOrder) < (item2 at: #itemOrder) ]) do: [ :item |
			itemsSorted add: item ]].
	self
		fillIn: menu
		fromDictionaries: itemsSorted.
	^ menu.! !

!methodRemoval: TheWorldMenu class #addExtraOpenCommands:!
TheWorldMenu class removeSelector: #addExtraOpenCommands:!

!TheWorldMenu reorganize!
('action' doMenuItem:with: staggerPolicyString toggleWindowPolicy)
('commands' changeBackgroundColor cleanUpWorld fullScreenOff fullScreenOn garbageCollect newMorphOfClass:event: openChangeSorter1 quitSession saveAndQuit saveWorldInFile setDisplayDepth splitNewMorphList:depth: startMessageTally startThenBrowseMessageTally vmStatistics)
('construction' alphabeticalMorphMenu buildWorldMenu changesMenu colorForDebugging: debugMenu fillIn:from: fillIn:fromDictionaries: helpMenu newMorph openMenu preferencesMenu saveMenu)
('mechanics' menu: world:hand:)
('popups' changesDo debugDo doPopUp: helpDo openWindow preferencesDo saveOptionsDo)
('windows & flaps menu' windowsDo windowsMenu)
('menu' openCommentGuide openTerseGuide)
!

