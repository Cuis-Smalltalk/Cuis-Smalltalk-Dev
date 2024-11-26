'From Cuis7.1 [latest update: #6850] on 26 November 2024 at 11:12:38 am'!

!Morph class methodsFor: 'new-morph participation' stamp: 'jmv 11/26/2024 10:35:51'!
categoryInNewMorphMenu
	"Unknown"
	^ nil! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 11/26/2024 10:36:27'!
newMorph
	"The user requested 'new morph' from the world menu.  Put up a menu that allows many ways of obtaining new morphs.  If the preference #classicNewMorphMenu is true, the full form of yore is used; otherwise, a much shortened form is used."
	| menu catDict |
	menu := self menu: 'Add a new morph'.
	menu
		
			add: 'From Clipboard'
			target: myHand
			action: #pasteMorph;
		
			add: 'From Alphabetical List'
			subMenu: self alphabeticalMorphMenu.
	menu addLine.
	"Add 'Classic' New Morph menu"
	catDict := Dictionary new.
	Morph allSubclassesDo: [ :eaSubclass |
		eaSubclass includeInNewMorphMenu ifTrue: [ | category |
			category := eaSubclass categoryInNewMorphMenu ifNil: [ 'Unknown'].				
			(catDict includesKey: category)
				ifTrue: [ (catDict at: category) add: eaSubclass ]
				ifFalse: [
					catDict
						at: category
						put: (OrderedCollection with: eaSubclass) ]]].
	catDict keys sort do: [ :categ | | morphsInCat |
		morphsInCat := (catDict at: categ) asArray sort: [ :c1 :c2 |
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
				subMenu := MenuMorph new.
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

