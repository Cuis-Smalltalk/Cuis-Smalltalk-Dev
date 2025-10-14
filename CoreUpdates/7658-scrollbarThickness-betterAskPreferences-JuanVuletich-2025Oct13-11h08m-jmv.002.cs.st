'From Cuis7.5 [latest update: #7652] on 13 October 2025 at 11:09:48 am'!

!ChangeListWindow methodsFor: 'GUI building' stamp: 'jmv 10/13/2025 10:48:01'!
buildMorphicWindow
	"Open a morphic view for the messageSet, whose label is labelString. 
	The listView may be either single or multiple selection type"

	| listPane desiredExtent desiredHeight wine availableHeight |
	listPane := (PluggableListMorphOfMany
		withModel: model
		listGetter: #list
		primarySelectionGetter: #listIndex
		primarySelectionSetter: #toggleListIndex:
		listSelectionGetter: #listSelectionAt:
		listSelectionSetter: #listSelectionAt:put:)
			menuNshortcutsKey: #listMenuSpec.
	desiredExtent := listPane desiredExtent.
	desiredHeight := desiredExtent y.
	wine := RealEstateAgent standardWindowExtent.
	availableHeight := wine y - self labelHeight - (borderWidth * 2).
	desiredExtent x > wine x ifTrue: [
		desiredHeight := desiredHeight + Theme current scrollbarThickness ].
	desiredHeight / availableHeight < 0.4
		ifTrue: [		"Number of items is small enough to make for a fixed list regardless of Window resize"
			self layoutMorph
				addMorph: listPane fixedHeight: desiredHeight;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 1.0 ]
		ifFalse: [		"Number of items is large enough to prefer a list proportional to Window resize"
			self layoutMorph
				addMorph: listPane proportionalHeight: 0.4;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.6.
			listPane limitLayoutHeight ].! !


!VersionsBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 10/13/2025 10:48:23'!
buildMorphicWindow
	"Open a morphic view for the messageSet, whose label is labelString. 
	The listView may be either single or multiple selection type"

	| listPane desiredExtent desiredHeight wine availableHeight |
	listPane := (PluggableListMorph
		withModel: model
		listGetter: #list
		indexGetter: #listIndex
		indexSetter: #toggleListIndex:)
			menuNshortcutsKey: #listMenuSpec.
	desiredExtent := listPane desiredExtent.
	desiredHeight := desiredExtent y.
	wine := RealEstateAgent standardWindowExtent.
	availableHeight := wine y - self labelHeight - (borderWidth * 2).
	desiredExtent x > wine x ifTrue: [
		desiredHeight := desiredHeight + Theme current scrollbarThickness ].
	desiredHeight / availableHeight < 0.4
		ifTrue: [		"Number of items is small enough to make for a fixed list regardless of Window resize"
			self layoutMorph
				addMorph: listPane fixedHeight: desiredHeight;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 1.0 ]
		ifFalse: [		"Number of items is large enough to prefer a list proportional to Window resize"
			self layoutMorph
				addMorph: listPane proportionalHeight: 0.4;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.6.
			listPane limitLayoutHeight ].! !


!MethodSetWindow methodsFor: 'GUI building' stamp: 'jmv 10/13/2025 10:48:28'!
buildMorphicWindow
	"Answer a morphic window with the given label that can display the receiver"

	| listPane desiredExtent desiredHeight wine availableHeight |
	listPane := self buildMorphicMessageList.
	desiredExtent := listPane desiredExtent.
	desiredHeight := desiredExtent y.
	wine := RealEstateAgent standardWindowExtent.
	availableHeight := wine y - self labelHeight - (borderWidth * 2).
	desiredExtent x > wine x ifTrue: [
		desiredHeight := desiredHeight + Theme current scrollbarThickness ].
	desiredHeight / availableHeight < 0.4
		ifTrue: [		"Number of items is small enough to make for a fixed list regardless of Window resize"
			self layoutMorph
				addMorph: listPane fixedHeight: desiredHeight;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 1.0 ]
		ifFalse: [		"Number of items is large enough to prefer a list proportional to Window resize"
			self layoutMorph
				addMorph: listPane proportionalHeight: 0.4;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.6.
			listPane limitLayoutHeight ].
	model changed: #editSelection! !


!AutoCompleterMorph methodsFor: 'drawing' stamp: 'jmv 10/13/2025 10:48:34'!
calculateWidth

	| width font |

	width := 120.
	font := self class listFont.

	1
		to: self entryCount
		do: [ :index | width := width max: (font widthOfString: (completer entries at: index) asPlainString)].

	self entryCount > self itemsPerPage ifTrue: [ width := width + Theme current scrollbarThickness ].

	^ width ! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'jmv 10/13/2025 10:48:39'!
downButtonPosition

	^ `0@0` + (extent - Theme current scrollbarThickness)! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'jmv 10/13/2025 10:48:44'!
drawScrollBarOn: aCanvas

	| scrollbarThickness width |

	width := extent x-2.
	self entryCount > self itemsPerPage  ifTrue: [
		scrollbarThickness := Theme current scrollbarThickness.
		width := width - scrollbarThickness.

		self drawScrollBarRectangleOn: aCanvas thickness: scrollbarThickness.
		self drawUpArrowOn: aCanvas thickness: scrollbarThickness.
		self drawDownArrowOn: aCanvas thickness: scrollbarThickness.
		self drawScrollBarMovementBarOn: aCanvas thickness: scrollbarThickness ].

	^width
! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'jmv 10/13/2025 10:48:58'!
upButtonPosition

	^extent x - Theme current scrollbarThickness@0! !

!AutoCompleterMorph methodsFor: 'events' stamp: 'jmv 10/13/2025 10:48:52'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

	(self includesPixel: aMouseButtonEvent eventPosition)
		ifTrue: [
			((self upButtonPosition extent: Theme current scrollbarThickness) containsPoint: localEventPosition)
				ifTrue: [ self stillActive; goUp. ^self ].
			((self downButtonPosition extent: Theme current scrollbarThickness) containsPoint: localEventPosition)
				ifTrue: [ self stillActive; goDown. ^self ].
			self selected: (localEventPosition y // self itemHeight) +  self firstVisible.
			completer insertSelected ]
		ifFalse: [ self delete. completer menuClosed ]! !


!BitBltCanvas methodsFor: 'drawing-icons' stamp: 'jmv 10/13/2025 10:49:04'!
drawDownIcon
	| icon |
	icon := BitBltCanvas arrowOfDirection: #down size: Theme current scrollbarThickness.
	self pvtDrawButtonFaceForm: icon.! !

!BitBltCanvas methodsFor: 'drawing-icons' stamp: 'jmv 10/13/2025 10:49:09'!
drawLeftIcon
	| icon |
	icon := BitBltCanvas arrowOfDirection: #left size: Theme current scrollbarThickness.
	self pvtDrawButtonFaceForm: icon.! !

!BitBltCanvas methodsFor: 'drawing-icons' stamp: 'jmv 10/13/2025 10:49:14'!
drawRightIcon
	| icon |
	icon := BitBltCanvas arrowOfDirection: #right size: Theme current scrollbarThickness.
	self pvtDrawButtonFaceForm: icon.! !

!BitBltCanvas methodsFor: 'drawing-icons' stamp: 'jmv 10/13/2025 10:49:21'!
drawUpIcon
	| icon |
	icon := BitBltCanvas arrowOfDirection: #up size: Theme current scrollbarThickness.
	self pvtDrawButtonFaceForm: icon.! !

