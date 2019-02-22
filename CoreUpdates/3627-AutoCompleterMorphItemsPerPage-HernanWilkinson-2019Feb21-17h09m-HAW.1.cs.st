'From Cuis 5.0 of 7 November 2016 [latest update: #3622] on 21 February 2019 at 9:48:42 pm'!
!classDefinition: #AutoCompleterMorph category: #'Tools-Autocompletion'!
BorderedRectMorph subclass: #AutoCompleterMorph
	instanceVariableNames: 'completer selected firstVisible itemHeight lastActivity selectorDocumentation originalPosition showUp itemsPerPage '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'HAW 2/21/2019 21:38:59'!
calculateItemsPerPage

	| itemsThatCanFit |

	itemsThatCanFit _ (DisplayScreen actualScreenSize y - originalPosition y - 2) // self itemHeight.
	itemsPerPage _ (itemsThatCanFit min: self maxItemsPerPage) min: completer entryCount.

! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'HAW 2/21/2019 21:31:17'!
maxItemsPerPage

	^13! !


!AutoCompleterMorph methodsFor: 'actions' stamp: 'HAW 2/21/2019 21:40:47'!
goDown

	"Wrap around"
	self selected = completer entryCount ifTrue: [ ^ self goHome ].

	self selected: self selected + 1.
	(self selected > self lastVisible and: [self selected <= completer entryCount]) ifTrue: [firstVisible := firstVisible + 1].

	self redrawNeeded! !

!AutoCompleterMorph methodsFor: 'actions' stamp: 'HAW 2/21/2019 21:40:58'!
goHome

	firstVisible := 1.
	self selected: firstVisible.

	self redrawNeeded! !

!AutoCompleterMorph methodsFor: 'actions' stamp: 'HAW 2/21/2019 21:42:25'!
goPageDown

	(self gotoPage: self currentPage + 1) ifFalse: [ self goToEnd ].

	self redrawNeeded! !

!AutoCompleterMorph methodsFor: 'actions' stamp: 'HAW 2/21/2019 21:42:30'!
goPageUp

	self gotoPage: self currentPage - 1.

	self redrawNeeded! !

!AutoCompleterMorph methodsFor: 'actions' stamp: 'HAW 2/21/2019 21:42:37'!
goToEnd

	self selected: completer entryCount.
	firstVisible := selected - itemsPerPage + 1 max: 1.

	self redrawNeeded! !

!AutoCompleterMorph methodsFor: 'actions' stamp: 'HAW 2/21/2019 20:46:55'!
goUp

	(self selected = 0 and: [self firstVisible = 1]) ifTrue: [^ self].
	"Wrap around"
	self selected = 1 ifTrue: [ ^self goToEnd ].

	self selected: self selected - 1.
	self selected < self firstVisible ifTrue: [firstVisible := firstVisible - 1].

	self redrawNeeded! !

!AutoCompleterMorph methodsFor: 'actions' stamp: 'HAW 2/21/2019 21:33:37'!
help

	TextModel new contents: AutoCompleter helpText; openLabel: 'uCompletion Keyboard Help'! !

!AutoCompleterMorph methodsFor: 'actions' stamp: 'HAW 2/21/2019 21:41:36'!
resetMenu

	| width newExtent |

	self hideSelectorDocumentation.
	self goHome.

	self calculateItemsPerPage.
	width _ self calculateWidth.
	newExtent _ width + 4 @ (itemsPerPage * self itemHeight + 2).

	self morphPosition: originalPosition extent: newExtent.
	"redraw is needed even if position and extent haven't changed"
	self redrawNeeded ! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'HAW 2/11/2019 20:13:14'!
adjust: aLocation ifOutOfScreenWith: anExtent xOffset: xOffset yOffset: yOffset

	| adjustedLocationX adjustedLocationY |

	adjustedLocationX := (self isXOutOfScreen: aLocation with: anExtent)
		ifTrue: [ aLocation x - anExtent x - xOffset ]
		ifFalse: [ aLocation x ].

	adjustedLocationX < 0 ifTrue: [ adjustedLocationX := aLocation x ].

	adjustedLocationY := (self isYOutOfScreen: aLocation with: anExtent)
		ifTrue: [ aLocation y - anExtent y - yOffset ]
		ifFalse: [ aLocation y ].

	^adjustedLocationX @ adjustedLocationY
	! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'HAW 2/21/2019 21:25:56'!
calculateWidth

	| width font |

	width _ 120.
	font _ self class listFont.

	1
		to: completer entryCount
		do: [ :index | width _ width max: (font widthOfString: (completer entries at: index) asString)].

	completer entryCount > itemsPerPage ifTrue: [ width _ width + ScrollBar scrollbarThickness ].

	^ width ! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'HAW 2/10/2019 23:51:54'!
drawItemsOn: aCanvas width: width

	| itemTop |

	itemTop _ 1.
	self firstVisible
		to: self lastVisible
		do: [ :index |
			self drawItemOf: index on: aCanvas width: width top: itemTop.
			itemTop _ itemTop + self itemHeight ].! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'HAW 2/21/2019 21:26:22'!
drawScrollBarOn: aCanvas

	| scrollbarThickness width |

	width _ extent x-2.
	completer entryCount > itemsPerPage  ifTrue: [
		scrollbarThickness _ ScrollBar scrollbarThickness.
		width _ width - scrollbarThickness.

		self drawScrollBarRectangleOn: aCanvas thickness: scrollbarThickness.
		self drawUpArrowOn: aCanvas thickness: scrollbarThickness.
		self drawDownArrowOn: aCanvas thickness: scrollbarThickness.
		self drawScrollBarMovementBarOn: aCanvas thickness: scrollbarThickness ].

	^width
! !

!AutoCompleterMorph methodsFor: 'events' stamp: 'HAW 2/21/2019 21:45:16'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

	(self morphContainsPoint: localEventPosition)
		ifTrue: [
			((self upButtonPosition extent: ScrollBar scrollbarThickness) containsPoint: localEventPosition)
				ifTrue: [ ^self stillActive; goUp ].
			((self downButtonPosition extent: ScrollBar scrollbarThickness) containsPoint: localEventPosition)
				ifTrue: [ ^self stillActive; goDown ].
			self selected: (localEventPosition y // self itemHeight) +  self firstVisible.
			completer insertSelected ]
		ifFalse: [ self delete. completer menuClosed ]! !

!AutoCompleterMorph methodsFor: 'paging' stamp: 'HAW 2/21/2019 21:26:13'!
currentPage

	^(self selected - 1 // itemsPerPage ) + 1.! !

!AutoCompleterMorph methodsFor: 'paging' stamp: 'HAW 2/21/2019 21:45:37'!
gotoPage: anInteger

	| item |

	item := ((anInteger - 1) * itemsPerPage) + 1.
	item >= completer entryCount ifTrue: [^false].

	item < 1 ifTrue: [item := 1].
	firstVisible := item.
	self selected: firstVisible.

	^ true! !

!AutoCompleterMorph methodsFor: 'paging' stamp: 'HAW 2/21/2019 21:28:22'!
pageCount

	| count |

	completer entryCount = itemsPerPage ifTrue: [^ 1].

	count _ completer entryCount // itemsPerPage.
	(completer entryCount \\ itemsPerPage) > 0 ifTrue: [ count _ count + 1].

	^count! !

!AutoCompleterMorph methodsFor: 'private' stamp: 'HAW 2/21/2019 21:45:53'!
firstVisible

	^firstVisible min: completer entryCount! !

!AutoCompleterMorph methodsFor: 'private' stamp: 'HAW 2/21/2019 21:25:00'!
lastVisible

	^ (self firstVisible + itemsPerPage - 1)  min: completer entryCount! !

!AutoCompleterMorph methodsFor: 'selector documentation' stamp: 'HAW 2/21/2019 21:28:49'!
selectorDefaultDocumentationLocation

	| relativeSelected |

	relativeSelected := (self selected - self firstVisible) min: itemsPerPage - 1 max: 0.

	^location externalizePosition: extent x@(relativeSelected * self itemHeight + 1).

	! !

!AutoCompleterMorph methodsFor: 'selector documentation' stamp: 'HAW 2/11/2019 16:18:05'!
selectorDocumentationExtent

	^`600@250`! !

!methodRemoval: AutoCompleterMorph class #itemsPerPage!
AutoCompleterMorph class removeSelector: #itemsPerPage!
!methodRemoval: AutoCompleterMorph class #maxItemsPerPage!
AutoCompleterMorph class removeSelector: #maxItemsPerPage!
!methodRemoval: AutoCompleterMorph #visibleItemsCount!
AutoCompleterMorph removeSelector: #visibleItemsCount!
!classDefinition: #AutoCompleterMorph category: #'Tools-Autocompletion'!
BorderedRectMorph subclass: #AutoCompleterMorph
	instanceVariableNames: 'completer selected firstVisible itemHeight lastActivity selectorDocumentation originalPosition showUp itemsPerPage'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!

!AutoCompleterMorph reorganize!
('accessing' itemHeight selected selected:)
('actions' goDown goHome goPageDown goPageUp goToEnd goUp help resetMenu)
('activity tracking' lastActivity stillActive timeOfLastActivity timeout updateColor)
('drawing' adjust:ifOutOfScreenWith:xOffset:yOffset: calculateItemsPerPage calculateWidth colorOf: downButtonPosition drawContainingRectangle: drawDownArrowOn:thickness: drawItemOf:on:width:top: drawItemsOn:width: drawOn: drawScrollBarMovementBarOn:thickness: drawScrollBarOn: drawScrollBarRectangleOn:thickness: drawUpArrowOn:thickness: maxItemsPerPage setDefaultColors upButtonPosition)
('events' mouseButton1Up:localPosition:)
('event handling testing' handlesMouseDown:)
('initialization' defaultBorderColor defaultBorderWidth defaultColor setCompleter:position:)
('paging' currentPage gotoPage: pageCount)
('stepping' stepAt: stepTime wantsSteps)
('private' firstVisible lastVisible)
('selector documentation' crPressedOnSelectorDocumentation hideSelectorDocumentation initializeSelectorDocumentation isShowingSelectorDocumentation methodDocumentationSeparator selectorDefaultDocumentationLocation selectorDocumentation selectorDocumentationExtent selectorDocumentationText selectorDocumentationTextForAllI: selectorDocumentationTextForAllImplementorsOf: selectorDocumentationTextOf:forAll: showSelectorDocumentation)
('submorphs-add/remove' delete)
('testing' isXOutOfScreen:with: isYOutOfScreen:with:)
!


!AutoCompleterMorph class reorganize!
('preferences' itemHeight listFont)
('instance creation' completer:position: initializedInstance)
!
