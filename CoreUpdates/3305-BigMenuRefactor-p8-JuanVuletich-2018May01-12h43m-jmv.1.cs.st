'From Cuis 5.0 of 7 November 2016 [latest update: #3303] on 1 May 2018 at 12:46:35 pm'!

!Morph methodsFor: 'menus' stamp: 'jmv 5/1/2018 11:26:39'!
addHaloActionsTo: aMenu 
	"Add items to aMenu representing actions requestable via halo"

	| subMenu |
	subMenu := MenuMorph new defaultTarget: self.
	subMenu addTitle: (self printStringLimitedTo: 40).
	subMenu addStayUpIcons.
	subMenu addLine.
	(subMenu add: 'delete' action: #dismissViaHalo)
		setBalloonText: 'Delete this object -- warning -- can be destructive!!' .
	self maybeAddCollapseItemTo: subMenu.
	(subMenu add: 'grab' action: #openInHand)
		setBalloonText: 'Pick this object up -- warning, since this removes it from its container, it can have adverse effects.' .
	subMenu addLine.
	(subMenu add: 'resize' action: #resizeFromMenu)
		setBalloonText: 'Change the size of this object'.
	(subMenu add: 'duplicate' action: #maybeDuplicateMorph)
		setBalloonText: 'Hand me a copy of this object'.
	(subMenu
		add: 'set color'
		target: self
		action: #changeColor)
			setBalloonText: 'Change the color of this object'.
	(subMenu
		add: 'inspect'
		target: self
		action: #inspect)
			setBalloonText: 'Open an Inspector on this object'.
	aMenu add: 'halo actions...' subMenu: subMenu! !

!Morph methodsFor: 'menus' stamp: 'jmv 5/1/2018 11:26:53'!
addToggleItemsToHaloMenu: aMenu
	"Add standard true/false-checkbox items to the memu"

	#(
		(stickinessString toggleStickiness 'whether I should be resistant to a drag done by mousing down on me')
		(lockedString lockUnlockMorph 'when "locked", I am inert to all user interactions')
	) do: [ :trip | 
			(aMenu addUpdating: trip first action: trip second)
				setBalloonText: trip third ]! !


!CodeWindow methodsFor: 'menu building' stamp: 'jmv 5/1/2018 11:24:50'!
addContentsTogglesTo: aMenu
	"Add updating menu toggles governing contents to aMenu."

	model contentsSymbolQuints do: [ :aQuint |
			aQuint == #-
				ifTrue: [
					aMenu addLine]
				ifFalse: [
					(aMenu addUpdating: aQuint third target: model action: aQuint second)
						setBalloonText: aQuint fifth ]]! !


!ChangeSorterWindow methodsFor: 'menu building' stamp: 'jmv 5/1/2018 11:24:14'!
changeSetMenu
	"Set up aMenu to hold commands for the change-set-list pane.  This could be for a single or double changeSorter"

	| aMenu isForBaseSystem |
	isForBaseSystem _ model changeSet isForBaseSystem.
	aMenu _ MenuMorph new defaultTarget: model.
	aMenu title: 'Change Set'.
	aMenu addStayUpIcons.

	(aMenu add: 'File out and remove (o)' 			action: #fileOutAndRemove) 	isEnabled: isForBaseSystem.
	aMenu add: 'File out and keep (k)' 				action: #fileOutAndKeep.
	aMenu addLine.

	(aMenu add: 'Rename change set (r)' 			action: #rename) 					isEnabled: isForBaseSystem.
	(aMenu add: 'Destroy change set (x)' 			action: #remove) 					isEnabled: isForBaseSystem.
	aMenu addLine.
	model currentCanHavePreambleAndPostscript ifTrue: [
		aMenu addLine.
		model currentHasPreamble
			ifTrue: [
				aMenu add: 'Edit preamble (p)' 		action: #addPreamble.
				aMenu add: 'Remove preamble' 	action: #removePreamble]
			ifFalse: [
				aMenu add: 'Add preamble (p)' 	action: #addPreamble].
		model currentHasPostscript
			ifTrue: [
				aMenu add: 'Edit postscript...' 		action: #editPostscript.
				aMenu add: 'Remove postscript' 	action: #removePostscript]
			ifFalse: [
				aMenu add: 'Add postscript...' 		action: #editPostscript].
	].
	aMenu addLine.

	"CONFLICTS SECTION"
	(aMenu add: 'conflicts with other change sets' target: self action: #browseMethodConflicts)
		setBalloonText: 'Browse all methods that occur both in this change set and in at least one other change set.'.
	aMenu addLine.

	"CHECKS SECTION"
	(aMenu add: 'trim history' action: #trimHistory)
		isEnabled: isForBaseSystem;
		setBalloonText: ' Drops any methods added and then removed, as well as renaming and reorganization of newly-added classes. ',
				'NOTE: can cause confusion if later filed in over an earlier version of these changes'.

	(aMenu add: 'view affected class categories' action: #viewAffectedClassCategories)
		setBalloonText: ' Show class categories affected by any contained change'.

	^ aMenu! !


!MenuMorph methodsFor: 'construction' stamp: 'jmv 5/1/2018 11:25:14'!
addList: aList
	"Add the given items to this menu, where each item is a pair (<string> <actionSelector>)..  If an element of the list is simply the symobl $-, add a line to the receiver.  The optional third element of each entry, if present, provides balloon help."

	| target item |
	aList do: [:tuple |
		tuple == #-
			ifTrue: [ self addLine ]
			ifFalse: [
				target _ tuple size > 3
					ifTrue: [ defaultTarget perform: tuple fourth ]
					ifFalse: [ defaultTarget ].
				item _ self add: tuple first target: target action: tuple second.
				tuple size > 2 ifTrue: [
					item setBalloonText: tuple third]]]! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 5/1/2018 11:24:38'!
addUpdating: aWordingSelector action: aSymbol

	^self addUpdating: aWordingSelector target: defaultTarget action: aSymbol argumentList: #()! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 5/1/2018 11:24:33'!
addUpdating: aWordingSelector target: aTarget action: aSymbol

	^self addUpdating: aWordingSelector target: aTarget action: aSymbol argumentList: #()! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/1/2018 12:45:58'!
fillIn: aMenu from: dataForMenu
	"A menu constructor utility by RAA.  dataForMenu is a list of items which mean:
			nil								Indicates to add a line

			first element is symbol		Add updating item with the symbol as the wording selector
			second element is a list		second element has the receiver and selector

			first element is a string		Add menu item with the string as its wording
			second element is a list		second element has the receiver and selector

			a third element exists			Use it as the balloon text
			a fourth element exists		Use it as the enablement selector (updating case only)"

	| item |

	dataForMenu do: [ :itemData |
		itemData ifNil: [aMenu addLine] ifNotNil: [
			item _ aMenu
				add: itemData first
				target: self 
				action: #doMenuItem:with: 
				argumentList: {itemData second}.
			itemData size >= 3 ifTrue: [
				item setBalloonText: itemData third.
			itemData size >= 4 ifTrue: [
				item enablementSelector: itemData fourth ]]]].

	^ aMenu! !

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/1/2018 11:27:29'!
fillIn: aMenu fromDictionaries: dataForMenuDicts
	"A menu constructor utility that uses Dictionaries with elements:
		#label - the name that displays in the menu
		#object - the target object
		#selector - the selector called on object when the menu item is selected
		#arguments - optional collection of arguments passed to the selector
		#balloonText - optional 'tool tip' style help text

	note, nil elements will add a line."
	| item |
	dataForMenuDicts do: [ :dict |
		dict
			ifNil: [ aMenu addLine ]
			ifNotNil: [
				item _ (dict at: #label) isSymbol
					ifTrue: [
						aMenu
							addUpdating: (dict at: #label)
							target: (dict at: #object)
							action: (dict at: #selector)
							argumentList:
								(dict
									at: #arguments
									ifAbsent: [ #() ]) ]
					ifFalse: [
						aMenu
							add: (dict at: #label)
							target: (dict at: #object)
							action: (dict at: #selector)
							argumentList:
								(dict
									at: #arguments
									ifAbsent: [ #() ]) ].
				dict
					at: #balloonText
					ifPresent: [ :balloonText |
						item setBalloonText: balloonText ]]].
	^ aMenu! !

!methodRemoval: CustomMenu #balloonTextForLastItem:!
CustomMenu removeSelector: #balloonTextForLastItem:!
!methodRemoval: MenuMorph #balloonTextForLastItem:!
MenuMorph removeSelector: #balloonTextForLastItem:!
