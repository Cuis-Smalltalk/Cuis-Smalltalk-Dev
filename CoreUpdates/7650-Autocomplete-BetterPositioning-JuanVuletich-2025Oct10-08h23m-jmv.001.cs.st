'From Cuis7.5 [latest update: #7649] on 10 October 2025 at 8:25:10 am'!
!classDefinition: #AutoCompleterMorph category: #'Tools-Autocompletion'!
BorderedBoxMorph subclass: #AutoCompleterMorph
	instanceVariableNames: 'completer selected firstVisible itemHeight lastActivity originalPosition showUp itemsPerPage downButtonForm upButtonForm '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!

!BoxMorph methodsFor: 'private' stamp: 'jmv 8/5/2021 11:43:50'!
fitInOwner

	| delta trialRect |
	trialRect := Rectangle origin: self morphPosition extent: extent.
	delta := trialRect amountToTranslateWithin: owner displayBounds.
	self morphPosition: trialRect origin + delta.! !


!AutoCompleterMorph methodsFor: 'actions' stamp: 'jmv 10/10/2025 08:21:08'!
initMenu
	| maxItemsPerPage |

	maxItemsPerPage := 13.
	itemsPerPage := self entryCount min: maxItemsPerPage.

	self firstVisible: 1.
	self selected: self firstSelectableEntryIndex.! !


!MenuMorph methodsFor: 'control' stamp: 'jmv 8/5/2021 11:44:30'!
popUpAdjacentTo: rightOrLeftPointInWorld from: sourceItem
	"Present this menu at the given point under control of the given hand.
	Used mostly for submenus."

	| trialRect |
	popUpOwner := sourceItem.
	sourceItem world addMorphFront: self position: rightOrLeftPointInWorld first.
	trialRect := rightOrLeftPointInWorld first extent: extent.
	trialRect right > sourceItem world morphWidth ifTrue: [
		self morphPosition: rightOrLeftPointInWorld second - (extent x@0)].
	self fitInOwner.! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 6/2/2022 19:26:33'!
popUpAt: aPoint allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	| evt hand world |
	self items isEmpty ifTrue: [^self].
	self addBlankIconsIfNecessary.
	(self submorphs select: [ :m | m is: #UpdatingMenuItemMorph ]) 
		do: [ :m | m updateContents].
	world := self runningWorld.
	world ifNil: [ ^ self ].
	hand := world activeHand.
	world addMorphFront: self position: aPoint - `2 @ 8`.
	self fitInOwner.
	"Acquire focus for valid pop up behavior"
	prevKbdFocus := hand keyboardFocus.
	prevMouseFocus := hand mouseFocus.
	hand newMouseFocus: self.
	aBoolean ifTrue: [ hand newKeyboardFocus: self ].
	evt := hand lastMouseEvent.
	(evt isKeyboard or: [ evt isMouse and: [ evt anyButtonPressed not ]]) 
		ifTrue: [
			"Select first item if button not down"
			self moveSelectionDown: 1 event: evt ]! !

!MenuMorph methodsFor: 'control' stamp: 'hlsf 6/19/2021 22:50:09'!
popUpAt: aPoint forHand: hand in: aWorld allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	self items isEmpty ifTrue: [ ^self ].
	self addBlankIconsIfNecessary.
	(self submorphs select: [ :m | m is: #UpdatingMenuItemMorph]) 
		do: [ :m | m updateContents].
	aWorld addMorphFront: self position: aPoint - `2 @ 8`.
	self fitInOwner.
	"Acquire focus for valid pop up behavior"
	prevKbdFocus := hand keyboardFocus.
	prevMouseFocus := hand mouseFocus.
	hand newMouseFocus: self.
	aBoolean ifTrue: [ hand newKeyboardFocus: self ]! !


!AutoCompleterMorph methodsFor: 'actions' stamp: 'jmv 10/10/2025 08:21:43'!
resetMenu

	| width newExtent |
	self initMenu.
	width := self calculateWidth.
	newExtent := width + 4 @ (self itemsPerPage * self itemHeight + 2).
	self extentInOwner: newExtent.
	self fitInOwner.
	"redraw is needed even if position and extent haven't changed"
	self redrawNeeded.! !

!AutoCompleterMorph methodsFor: 'initialization' stamp: 'jmv 10/10/2025 08:21:15'!
setCompleter: anAutoCompleter position: aPoint 

	completer := anAutoCompleter.
	self initMenu.
	self openInWorld.
	self morphPosition: aPoint.
	self resetMenu.! !

!methodRemoval: AutoCompleterMorph #calculateItemsPerPage stamp: 'jmv 10/10/2025 08:23:26'!
AutoCompleterMorph removeSelector: #calculateItemsPerPage!
!methodRemoval: AutoCompleterMorph #nextSelectableEntryIndexFromAndIncluding: stamp: 'jmv 10/10/2025 08:23:27'!
AutoCompleterMorph removeSelector: #nextSelectableEntryIndexFromAndIncluding:!
!methodRemoval: AutoCompleterMorph #maxItemsPerPage stamp: 'jmv 10/10/2025 08:23:41'!
AutoCompleterMorph removeSelector: #maxItemsPerPage!
!methodRemoval: MenuMorph #fitInWorld stamp: 'jmv 10/10/2025 08:24:28'!
MenuMorph removeSelector: #fitInWorld!
!classDefinition: #AutoCompleterMorph category: #'Tools-Autocompletion'!
BorderedBoxMorph subclass: #AutoCompleterMorph
	instanceVariableNames: 'completer selected firstVisible itemHeight lastActivity itemsPerPage downButtonForm upButtonForm'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!
