'From Cuis 5.0 of 7 November 2016 [latest update: #3300] on 1 May 2018 at 11:01:26 am'!

!MenuMorph methodsFor: 'construction' stamp: 'jmv 5/1/2018 10:59:27'!
add: aString target: target action: aSymbol argument: arg
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object with the given argument."

	^self add: aString
		target: target
		action: aSymbol
		argumentList: { arg }! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 5/1/2018 10:58:49'!
add: aString target: target action: aSymbol argumentList: argList
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object with the given arguments. If the selector takes one more argument than the number of arguments in the given list, then the triggering event is supplied as as the last argument.  Answer the appended menu item."

	| item |
	item _ MenuItemMorph new
		contents: aString;
		target: target selector: aSymbol arguments: argList asArray.
	self addMorphBack: item.
	^ item! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 5/1/2018 10:58:11'!
addUpdating: wordingSelector target: target action: aSymbol argumentList: argList
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object with the given arguments. If the selector takes one more argument than the number of arguments in the given list, then the triggering event is supplied as as the last argument.  In this variant, the wording of the menu item is obtained by sending the wordingSelector to the target,  Answer the item added."

	| item |
	item _ UpdatingMenuItemMorph new
		target: target selector: aSymbol arguments: argList asArray;
		wordingProvider: target wordingSelector: wordingSelector.
	self addMorphBack: item.
	^ item! !


!SimpleServiceEntry methodsFor: 'services menu' stamp: 'jmv 5/1/2018 10:55:06'!
addServiceFor: served toMenu: aMenu
	argumentProvider _ served.
	aMenu add: self label 
		target: self 
		action: #performService.
	self useLineAfter ifTrue: [ aMenu addLine ].! !


!CPUWatcher methodsFor: 'porcine capture' stamp: 'jmv 5/1/2018 11:00:03'!
openMorphicWindowForSuspendedProcess: aProcess
	| menu rule |
	menu _ MenuMorph new.
	"nickname  allow-stop  allow-debug"
	rule _ (ProcessBrowser rulesFor: aProcess) second.
	menu add: 'Dismiss this menu' target: menu action: #delete; addLine.
	menu add: 'Open Process Browser' target: ProcessBrowserWindow action: #openProcessBrowser.
	menu add: 'Resume'
		target: self
		action: #resumeProcess:fromMenu:
		argumentList: { aProcess . menu }.
	menu add: 'Terminate'
		target: self
		action: #terminateProcess:fromMenu:
		argumentList: { aProcess . menu }.
	rule ifTrue: [
		menu add: 'Debug at a lower priority'
			target: self
			action: #debugProcess:fromMenu:
			argumentList: { aProcess . menu }.
	].
	menu addTitle: aProcess identityHash asString,
		' ', aProcess name,
		' is taking too much time and has been suspended.
What do you want to do with it?'.
	menu stayUp.
	menu popUpInWorld
! !


!AbstractFont class methodsFor: 'instance accessing' stamp: 'jmv 5/1/2018 10:56:07'!
fromUser: priorFont
	"
	AbstractFont fromUser
	"
	"Present a menu of available fonts, and if one is chosen, return it.
	Otherwise return nil.
	Show only baseFonts i.e. FamilyName, pointSize (but do not include emphasis, such as italic or bold)"

	| fontList fontMenu active ptMenu label spec |
	fontList := AbstractFont familyNames.
	fontMenu := MenuMorph new defaultTarget: self.
	fontList do: [:fontName | 
			active := priorFont familyName sameAs: fontName.
			ptMenu := MenuMorph new defaultTarget: self.
			(AbstractFont pointSizesFor:fontName ) do: [ :pt | 
					(active and: [pt = priorFont pointSize]) 
						ifTrue: [label := '<on>']
						ifFalse: [label := '<off>'].
					label := label , pt printString , ' pt'.
					ptMenu 
						add: label
						target: fontMenu
						action: #modalSelection:
						argument: { 
								fontName.
								pt}].
			active ifTrue: [label := '<on>'] ifFalse: [label := '<off>'].
			label := label , fontName.
			fontMenu add: label subMenu: ptMenu].
	spec := fontMenu invokeModal.
	spec ifNil: [^nil].
	^AbstractFont familyName: spec first pointSize: spec last! !


!Morph methodsFor: 'debug and other' stamp: 'jmv 5/1/2018 10:55:33'!
buildDebugMenu: aHand 
	"Answer a debugging menu for the receiver.  The hand argument is seemingly historical and plays no role presently"

	| aMenu |
	aMenu := MenuMorph new defaultTarget: self.
	aMenu addStayUpIcons.
	self isKnownFailing
		ifTrue: [
			aMenu add: 'start drawing again' action: #resumeAfterDrawError.
			aMenu addLine].
	(self hasProperty: #errorOnStep) 
		ifTrue: [
			aMenu add: 'start stepping again' action: #resumeAfterStepError.
			aMenu addLine].
	aMenu add: 'inspect morph' action: #inspect.
	aMenu add: 'inspect owner chain' action: #inspectOwnerChain.
	self hasModel 
		ifTrue: [
			aMenu 
				add: 'inspect model'
				target: self model
				action: #inspect].
	aMenu 
		add: 'explore morph'
		target: self
		action: #explore.
	aMenu 
		add: 'copy to clipboard (c)'
		target: self
		action: #copyToClipboard.
	aMenu addLine.
	aMenu 
		add: 'browse morph class'
		target: self
		action: #browseClassHierarchy.
	self hasModel 
		ifTrue: [
			aMenu 
				add: 'browse model class'
				target: self model
				action: #browseClassHierarchy].
	aMenu addLine.
	aMenu
		add: 'edit balloon help' action: #editBalloonHelpText.
	^aMenu! !

!Morph methodsFor: 'meta-actions' stamp: 'jmv 5/1/2018 10:59:32'!
addEmbeddingMenuItemsTo: aMenu hand: aHandMorph
	| menu |
	menu _ MenuMorph new defaultTarget: self.
	self potentialEmbeddingTargets reverseDo: [:m | 
		menu 
			add: m class name asString 
			target: m 
			action: #addMorphFrontFromWorldPosition: 
			argumentList: {self}].
	aMenu ifNotNil:[
		menu submorphCount > 0 
			ifTrue:[aMenu add:'embed into' subMenu: menu].
	].
	^menu! !


!ObjectExplorerWindow methodsFor: 'building menus' stamp: 'jmv 5/1/2018 10:56:13'!
genericMenu
	"Borrow a menu from my inspector"
	
	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	model getCurrentSelection
		ifNil: [
			aMenu
				add: '*nothing selected*'
				target: self
				action: #yourself]
		ifNotNil: [
			aMenu addList: #(
				('inspect (i)'							inspectSelection)
				('explore (I)'							exploreSelection)
				('copy to clipboard (c)'				copySelectionToClipboard)
				('basic inspect'						inspectBasic)
				('references finder'					openReferencesFinder)
				('weight explorer'					openWeightExplorer)
				-
				('browse full (b)'						browseMethodFull)
				('browse hierarchy (h)'				browseHierarchy)
				('browse protocol (p)'				browseFullProtocol)).
			aMenu addLine;
				add: 'monitor changes'
				target: self
				action: #monitor:
				argument: model getCurrentSelection.
			model class == ReferencesExplorer ifTrue: [
				aMenu addLine;
					add: 'rescan'
					target: self
					action: #rescan ]].
	model basicMonitorList isEmptyOrNil
		ifFalse: [
			aMenu addLine;
				add: 'stop monitoring all'
				target: self
				action: #stopMonitoring ].
	^ aMenu! !


!TestRunnerWindow methodsFor: 'menu building' stamp: 'jmv 5/1/2018 10:56:21'!
listMenu
	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu title: 'Test Cases'.
	aMenu add: 'select all' target: model action: #selectAll.
	aMenu add: 'deselect all' target: model action: #deselectAll.
	aMenu add: 'toggle selections' target: model action: #invertSelections.
	aMenu add: 'filter' target: model action: #setFilter.
	model runButtonState ifTrue: [
		aMenu add: 'stop' target: model action: #terminateRun
		].
	model selectedSuite > 0 ifTrue: [ | cls |
		cls _ (model tests at: model selectedSuite ifAbsent: ['']) copyUpTo: Character space.
		cls _ cls asSymbol.
		cls _ (Smalltalk at: cls ifAbsent: nil).
		cls ifNotNil: [
			aMenu addLine.
			aMenu add: 'browse' target: self action: #browse: argument: cls.
		].
	].
	aMenu addLine.
	aMenu add: 'log to Transcript' target: model action: #showResult.
	^aMenu! !


!MenuMorph methodsFor: 'construction' stamp: 'jmv 5/1/2018 10:59:08'!
add: aString action: aSymbol
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the default target object."
	"Details: Note that the menu item added captures the default target object at the time the item is added; the default target can later be changed before added additional items without affecting the targets of previously added entries. The model is that each entry is like a button that knows everything it needs to perform its action."

	^self add: aString
		target: defaultTarget
		action: aSymbol
		argumentList: #()! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 5/1/2018 10:59:21'!
add: aString selector: aSymbol argument: arg

	^self add: aString
		target: defaultTarget
		action: aSymbol
		argumentList: { arg }! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 5/1/2018 10:59:24'!
add: aString target: aTarget action: aSymbol
	^self add: aString
		target: aTarget
		action: aSymbol
		argumentList: #()! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 5/1/2018 10:58:24'!
addUpdating: aWordingSelector action: aSymbol

	self addUpdating: aWordingSelector target: defaultTarget action: aSymbol argumentList: #()! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 5/1/2018 10:58:30'!
addUpdating: aWordingSelector target: aTarget action: aSymbol

	self addUpdating: aWordingSelector target: aTarget action: aSymbol argumentList: #()! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/1/2018 10:56:28'!
alphabeticalMorphMenu
	| list splitLists menu firstChar lastChar subMenu |
	list := Morph withAllSubclasses select: [ :m |
		m includeInNewMorphMenu ].
	list := list asArray sort: [ :c1 :c2 |
		c1 name < c2 name ].
	splitLists := self
		splitNewMorphList: list
		depth: 4.
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

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/1/2018 10:59:35'!
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
			item _ itemData first isSymbol
				ifTrue: [
					aMenu 
						addUpdating: itemData first 
						target: self 
						action: #doMenuItem:with: 
						argumentList: {itemData second}]
				 ifFalse: [
					aMenu 
						add: itemData first
						target: self 
						action: #doMenuItem:with: 
						argumentList: {itemData second}].
			itemData size >= 3 ifTrue: [
				aMenu balloonTextForLastItem: itemData third.
			itemData size >= 4 ifTrue: [
				item enablementSelector: itemData fourth ]]]].

	^ aMenu! !

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/1/2018 10:59:42'!
fillIn: aMenu fromDictionaries: dataForMenuDicts
	"A menu constructor utility that uses Dictionaries with elements:
		#label - the name that displays in the menu
		#object - the target object
		#selector - the selector called on object when the menu item is selected
		#arguments - optional collection of arguments passed to the selector
		#balloonText - optional 'tool tip' style help text

	note, nil elements will add a line."
	dataForMenuDicts do: [ :dict |
		dict
			ifNil: [ aMenu addLine ]
			ifNotNil: [
				(dict at: #label) isSymbol
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
						aMenu balloonTextForLastItem: balloonText ]]].
	^ aMenu.! !

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/1/2018 10:56:32'!
newMorph
	"The user requested 'new morph' from the world menu.  Put up a menu that allows many ways of obtaining new morphs.  If the preference #classicNewMorphMenu is true, the full form of yore is used; otherwise, a much shortened form is used."
	| menu subMenu catDict |
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
	catDict keys sort do: [ :categ |
		subMenu _ MenuMorph new.
		((catDict at: categ) asArray sort: [ :c1 :c2 |
			c1 name < c2 name ]) do: [ :cl |
			subMenu
				add: cl name
				target: self
				action: #newMorphOfClass:event:
				argument: cl ].
		menu
			add: categ
			subMenu: subMenu ].
	self doPopUp: menu.! !

!methodRemoval: MenuMorph #add:target:selector:!
MenuMorph removeSelector: #add:target:selector:!
!methodRemoval: MenuMorph #add:target:selector:argument:!
MenuMorph removeSelector: #add:target:selector:argument:!
!methodRemoval: MenuMorph #add:target:selector:argumentList:!
MenuMorph removeSelector: #add:target:selector:argumentList:!
!methodRemoval: MenuMorph #addUpdating:target:selector:argumentList:!
MenuMorph removeSelector: #addUpdating:target:selector:argumentList:!
