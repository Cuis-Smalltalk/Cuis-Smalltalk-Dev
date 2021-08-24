'From Cuis 5.0 [latest update: #4799] on 24 August 2021 at 11:59:48 am'!

!SystemWindow methodsFor: 'drawing' stamp: 'jmv 8/24/2021 10:58:39'!
addPossiblyUncoveredAreasIn: aRectangle to: aCollection
	"Answer an array of rectangles encompassing those areas in aRectangle not completely
	covered by self. These are the areas that might require further drawing (of morphs below us)
	All areas that might possibly be uncovered must be included."

	 | bounds |
	self isOrAnyOwnerIsRotated ifTrue: [
		aCollection add: aRectangle.
		^self ].

	color mightBeTranslucent ifTrue: [
		aCollection add: aRectangle.
		^self ].

	bounds _ self displayBounds.
	bounds ifNil: [
		aCollection add: aRectangle.
		^self ].

	"Solid rectangle.
	This will be the fastest in many cases. So, please disable rounded corners if on slow hardware!!"
	Theme current roundWindowCorners ifFalse: [
		aRectangle areasOutside: bounds do: [ :rect |  aCollection add: rect ].
		^self ].

	"The solid rectangle does not include the corners.
	Report a couple of rows (top and bottom) or columns (left and right) as uncovered areas.
	We could also try to be more careful and answer each rounded corner...
	Right now, report top and bottom rows as uncovered areas:
	- Rows are contiguous in Display memory
	- Redrawing title area wont trigger redrawing all windows contents."
	"
	radius _ Theme current roundedWindowRadius.
	aRectangle areasOutside: (bounds insetBy: 0@radius) do: [ :rect |  aCollection add: rect ].
	"
	"Alternative: just include window borders. Almost correct, and cheaper."
	aRectangle areasOutside: (bounds insetBy: Theme current windowBorderWidth) do: [ :rect |  aCollection add: rect ].

"Note: Doing this after the non-rounded-corner case gave bad results. Not letting the size of aCollection grow without bounds is more important than not answering extra areas.
	Theme current roundWindowCorners ifTrue: [
		r _ Theme current roundedWindowRadius.
		e _ r@r.
		aCollection add: (aRectangle intersect: (bounds topLeft extent: e)).
		aCollection add: (aRectangle intersect: (bounds topRight - (r@0) extent: e)).
		aCollection add: (aRectangle intersect: (bounds bottomLeft - (0@r)extent: e)).
		aCollection add: (aRectangle intersect: (bounds bottomRight - (r@r) extent: e)).
		]."! !

