'From Cuis 4.2 of 25 July 2013 [latest update: #2235] on 11 April 2015 at 12:08:36.026403 pm'!

!Form methodsFor: 'color mapping' stamp: 'jmv 9/23/2012 21:42'!
mapColor: oldColor to: newColor
	"Make all pixels of the given color in this Form to the given new color."
	"Warnings: This method modifies the receiver. It may lose some color accuracy on 32-bit Forms, since the transformation uses a color map with only 15-bit resolution."

	| map |
	map _ (Color cachedColormapFrom: self depth to: self depth) copy.
	map at: (oldColor indexInMap: map) put: (newColor pixelWordForDepth: self depth).
	(BitBlt toForm: self)
		sourceForm: self;
		sourceOrigin: 0@0;
		combinationRule: Form over;
		destX: 0 destY: 0 width: width height: height;
		colorMap: map;
		copyBits.
! !


!ColorForm methodsFor: 'color manipulation' stamp: 'jm 10/19/1998 10:52'!
mapColor: oldColor to: newColor
	"Replace all occurances of the given color with the given new color in my color map."

	self ensureColorArrayExists.
	1 to: colors size do: [:i | 
		(colors at: i) = oldColor ifTrue: [colors at: i put: newColor]].
	self clearColormapCache.
! !


!Form methodsFor: 'displaying-generic' stamp: 'jmv 4/11/2015 12:06'!
displayOn: aDisplayMedium at: aDisplayPoint rule: ruleInteger
	"Display the receiver located at aPoint with default setting for the 
	halftone and clippingBox."

	self displayOn: aDisplayMedium
		at: aDisplayPoint
		clippingBox: aDisplayMedium boundingBox
		rule: ruleInteger! !

!methodRemoval: ColorForm #replaceColor:with:!
ColorForm removeSelector: #replaceColor:with:!
!methodRemoval: Form #replaceColor:with:!
Form removeSelector: #replaceColor:with:!
