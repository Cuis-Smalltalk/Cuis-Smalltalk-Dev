'From Cuis 5.0 of 7 November 2016 [latest update: #3665] on 15 March 2019 at 10:06:54 pm'!

!AutoCompleterMorph methodsFor: 'accessing' stamp: 'AY 3/15/2019 21:55:46'!
selected: aNumber 

	"Set the value of selected"

	((aNumber between: 1 and: self entryCount) and: [ aNumber ~= selected ])
		ifTrue: [ 
			selected _ aNumber.
			self isShowingSelectorDocumentation ifTrue: [ self showSelectorDocumentation ]]! !

!AutoCompleterMorph methodsFor: 'actions' stamp: 'AY 3/15/2019 21:54:52'!
resetMenu

	| width newExtent |
	self hideSelectorDocumentation.

	self calculateItemsPerPage.
	
	self firstVisible: 1.
	self selected: self firstSelectableEntryIndex.
	
	width _ self calculateWidth.
	newExtent _ width + 4 @ (self itemsPerPage * self itemHeight + 2).

	self morphPosition: originalPosition extent: newExtent.
	"redraw is needed even if position and extent haven't changed"
	self redrawNeeded ! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'AY 3/15/2019 21:56:00'!
calculateItemsPerPage

	| itemsThatCanFit |

	itemsThatCanFit _ (DisplayScreen actualScreenSize y - originalPosition y - 2) // self itemHeight.
	itemsPerPage _ (itemsThatCanFit min: self maxItemsPerPage) min: self entryCount.

! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'AY 3/15/2019 21:56:10'!
calculateWidth

	| width font |

	width _ 120.
	font _ self class listFont.

	1
		to: self entryCount
		do: [ :index | width _ width max: (font widthOfString: (completer entries at: index) asString)].

	self entryCount > self itemsPerPage ifTrue: [ width _ width + ScrollBar scrollbarThickness ].

	^ width ! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'AY 3/15/2019 21:55:40'!
drawScrollBarMovementBarOn: aCanvas thickness: scrollbarThickness
	
	| height top bottom |

	height _ extent y - (2 * scrollbarThickness).
	top _ (1.0 * self firstVisible-1 / self entryCount * height) ceiling + 1 + scrollbarThickness-1.
	bottom _ (1.0 * self lastVisible / self entryCount * height) floor + 1 + scrollbarThickness -1.
	aCanvas
		fillRectangle: (extent x - scrollbarThickness+2@top corner:  extent x-2 @ bottom)
		color: `Color veryLightGray` ! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'AY 3/15/2019 21:54:24'!
drawScrollBarOn: aCanvas

	| scrollbarThickness width |

	width _ extent x-2.
	self entryCount > self itemsPerPage  ifTrue: [
		scrollbarThickness _ ScrollBar scrollbarThickness.
		width _ width - scrollbarThickness.

		self drawScrollBarRectangleOn: aCanvas thickness: scrollbarThickness.
		self drawUpArrowOn: aCanvas thickness: scrollbarThickness.
		self drawDownArrowOn: aCanvas thickness: scrollbarThickness.
		self drawScrollBarMovementBarOn: aCanvas thickness: scrollbarThickness ].

	^width
! !

!AutoCompleterMorph methodsFor: 'private' stamp: 'AY 3/15/2019 22:05:12'!
firstVisible

	^firstVisible min: self entryCount! !

!AutoCompleterMorph methodsFor: 'private' stamp: 'AY 3/15/2019 22:02:14'!
lastVisible
	
	^self firstVisible + self itemsPerPage - 1 min: self entryCount! !

!AutoCompleterMorph methodsFor: 'selector documentation' stamp: 'AY 3/15/2019 21:54:35'!
selectorDefaultDocumentationLocation

	| relativeSelected |

	relativeSelected := (self selected - self firstVisible) min: self itemsPerPage - 1 max: 0.

	^location externalizePosition: extent x@(relativeSelected * self itemHeight + 1).

	! !

