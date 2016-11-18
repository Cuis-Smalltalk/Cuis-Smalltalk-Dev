'From Cuis 5.0 of 7 November 2016 [latest update: #2981] on 18 November 2016 at 10:49:39 am'!

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/3/2015 10:19'!
newMorph
	"The user requested 'new morph' from the world menu.  Put up a menu that allows many ways of obtaining new morphs.  If the preference #classicNewMorphMenu is true, the full form of yore is used; otherwise, a much shortened form is used."
	| menu subMenu catDict shortCat class |
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
	SystemOrganization categories do: [ :cat |
		((cat beginsWith: 'Morphic-') and: [ (#('Morphic-Menus' 'Morphic-Support' ) includes: cat) not ]) ifTrue: [
			shortCat _ (cat
				copyFrom: 'Morphic-' size + 1
				to: cat size).
			(SystemOrganization listAtCategoryNamed: cat) do: [ :cName |
				class _ Smalltalk at: cName.
				((class inheritsFrom: Morph) and: [ class includeInNewMorphMenu ]) ifTrue: [
					(catDict includesKey: shortCat)
						ifTrue: [ (catDict at: shortCat) addLast: class ]
						ifFalse: [
							catDict
								at: shortCat
								put: (OrderedCollection with: class) ]]]]].
	catDict keys sort do: [ :categ |
		subMenu _ MenuMorph new.
		((catDict at: categ) asArray sort: [ :c1 :c2 |
			c1 name < c2 name ]) do: [ :cl |
			subMenu
				add: cl name
				target: self
				selector: #newMorphOfClass:event:
				argument: cl ].
		menu
			add: categ
			subMenu: subMenu ].

	self doPopUp: menu.! !

