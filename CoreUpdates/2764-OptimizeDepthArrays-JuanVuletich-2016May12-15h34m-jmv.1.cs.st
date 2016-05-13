'From Cuis 4.2 of 25 July 2013 [latest update: #2763] on 12 May 2016 at 3:35:27.819477 pm'!

!GrafPort methodsFor: 'private' stamp: 'jmv 5/12/2016 14:52'!
cachedFontColormapFrom1BitTo: destDepth

	| map dstIndex |
	CachedFontColorMaps 
		ifNil: [CachedFontColorMaps _ Array new: 6].

	dstIndex _ destDepth highBit.
	(CachedFontColorMaps at: dstIndex) ifNotNil: [ :m | ^ m ].

	map _ (Color cachedColormapFrom: 1 to: destDepth) copy.
	CachedFontColorMaps at: dstIndex put: map.
	^ map! !

!GrafPort methodsFor: 'private' stamp: 'jmv 5/12/2016 14:55'!
colorConvertingMap: targetColor from: sourceDepth to: destDepth keepSubPixelAA: keepSubPix
	"Note: The color converting map for sourceDepth=16 and for sourceDepth=32 are the same"
	
	| srcIndex dstIndex map mapsForSource mapsForSourceAndDest |
	ColorConvertingMaps 
		ifNil: [ColorConvertingMaps _ (1 to: 6) collect: [:i | Array new: 6]].

	srcIndex _ sourceDepth highBit.
	sourceDepth > 8 ifTrue: [ srcIndex _ keepSubPix ifTrue: [5] ifFalse: [6] ].
	dstIndex _ destDepth highBit.
	
	mapsForSource _ ColorConvertingMaps at: srcIndex.
	(mapsForSourceAndDest _ mapsForSource at: dstIndex) ifNil: [
		mapsForSourceAndDest _ mapsForSource at: dstIndex put: Dictionary new ].
	
	map _ mapsForSourceAndDest at: targetColor ifAbsentPut: [
		Color 
			computeColorConvertingMap: targetColor 
			from: sourceDepth 
			to: destDepth 
			keepSubPixelAA: keepSubPix ].

	^ map! !


!Color class methodsFor: 'colormaps' stamp: 'jmv 5/12/2016 14:50'!
cachedColormapFrom: sourceDepth to: destDepth
	"Return a cached colormap for mapping between the given depths. Always return a real colormap, not nil; this allows the client to get an identity colormap that can then be copied and modified to do color transformations."
	"Note: This method returns a shared, cached colormap to save time and space. Clients that need to modify a colormap returned by this method should make a copy and modify that!!"
	"Note: The colormap cache may be cleared by evaluating 'Color shutDown'."
	"Note: The color maps for sourceDepth=16 and for sourceDepth=32 are the same"

	"Warning: The behavior is incorrect for 32bpp Forms with translucency.
	Color maps are RGB only, they don't map on alpha values. Alpha is ignored when using the color map. This means that the only value mapped as transparent is pixel value 0,
	that is R=0, G=0, B=0, Alpha=0.
	However, a 32bpp form could have, for instance R=255, G=0, B=0, Alpha=0, also meaning transparent. But this will be mapped as if the source was red, not transparent."

	| srcIndex dstIndex map |
	CachedColormaps
		ifNil: [CachedColormaps _ (1 to: 5) collect: [:i | Array new: 6]].

	srcIndex _ sourceDepth highBit min: 5.
	dstIndex _ destDepth highBit.

	((CachedColormaps at: srcIndex) at: dstIndex) ifNotNil: [ :m | ^ m ].

	map _ self computeColormapFrom: sourceDepth to: destDepth.
	(CachedColormaps at: srcIndex) at: dstIndex put: map.
	^ map! !

