'From Cuis 5.0 [latest update: #4199] on 3 June 2020 at 10:10:23 am'!

!Color class methodsFor: 'instance creation' stamp: 'jmv 6/3/2020 10:09:36'!
fromRgbOrRgba: list
	^ list size caseOf: {
		[0] -> [ `Color transparent` ].
		[3] -> [ Color r: list first g: list second b: list third ].
		[4] -> [ TranslucentColor r: list first g: list second b: list third alpha: list fourth ]
	}! !


!ColorForm methodsFor: 'fileIn/Out' stamp: 'jmv 6/3/2020 10:06:41'!
colorsFromArray: anArrayOfTriplesOrQuartets
	"Set my color palette to the given collection."

	| colorCount newColors |
	anArrayOfTriplesOrQuartets ifNil: [
		colors _ cachedDepth _ cachedColormap _ nil.
		^ self].

	colorCount _ anArrayOfTriplesOrQuartets size.
	newColors _ ColorArray new: (1 bitShift: self depth).
	1 to: newColors size do: [:i |
		i <= colorCount
			ifTrue: [newColors at: i put: (Color fromRgbOrRgba: (anArrayOfTriplesOrQuartets at: i))]
			ifFalse: [newColors at: i put: `Color transparent` ]].

	colors _ newColors.
	cachedDepth _ nil.
	cachedColormap _ nil.
! !


!ColorForm methodsFor: 'accessing' stamp: 'jmv 6/3/2020 09:57:14'!
colors: colorList
	"Set my color palette to the given collection."

	| colorArray colorCount newColors |
	colorList ifNil: [
		colors _ cachedDepth _ cachedColormap _ nil.
		^ self].

	colorArray _ colorList asArray.
	colorCount _ colorArray size.
	newColors _ ColorArray new: (1 bitShift: self depth).
	1 to: newColors size do: [:i |
		i <= colorCount
			ifTrue: [newColors at: i put: (colorArray at: i)]
			ifFalse: [newColors at: i put: `Color transparent` ]].

	colors _ newColors.
	cachedDepth _ nil.
	cachedColormap _ nil.
! !

!ColorForm methodsFor: 'private' stamp: 'jmv 6/3/2020 09:57:24'!
setColors: colorArray cachedColormap: aBitmap depth: anInteger
	"Semi-private. Set the color array, cached colormap, and cached colormap depth to avoid having to recompute the colormap when switching color palettes in animations."

	colors _ colorArray asColorArray.
	cachedDepth _ anInteger.
	cachedColormap _ aBitmap.
! !

