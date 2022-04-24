'From Cuis 6.0 [latest update: #5113] on 24 April 2022 at 2:00:40 pm'!

!TheWorldMenu methodsFor: 'private' stamp: 'jmv 4/24/2022 13:59:58'!
pvtNewMenuForSplitLists: splitLists
	| firstPrefix lastPrefix menu subMenu |
	menu _ MenuMorph new defaultTarget: self.
	1
		to: splitLists size
		do: [ :i |
			firstPrefix _ i = 1
				ifTrue: [ 'A' ]
				ifFalse: [ (splitLists at: i) first name first: 3 ].
			lastPrefix _ i = splitLists size
				ifTrue: [ 'Z' ]
				ifFalse: [ (splitLists at: i) last name first: 3 ].
			subMenu _ MenuMorph new.
			(splitLists at: i) do: [ :cl |
				subMenu
					add: cl name
					target: self
					action: #newMorphOfClass:event:
					argument: cl ].
			menu
				add: firstPrefix , ' - ' , lastPrefix
				subMenu: subMenu ].
	^ menu.! !

