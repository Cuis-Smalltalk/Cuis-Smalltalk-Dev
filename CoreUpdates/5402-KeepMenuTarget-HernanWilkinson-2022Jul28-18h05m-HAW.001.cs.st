'From Cuis 6.0 [latest update: #5401] on 28 July 2022 at 6:05:47 pm'!

!DynamicMenuBuilder methodsFor: 'building - private' stamp: 'HAW 7/28/2022 18:05:11'!
addSubMenuTo: aMenuItem from: subMenuDefinition

	| subMenu |

	subMenu := MenuMorph new.
	subMenu defaultTarget: defaultTarget.
	"Add the menu items recursively to the parent menu item - Hernan"
	self addGroupedItemsFrom: subMenuDefinition toMenu: subMenu .
	aMenuItem subMenu: subMenu! !

