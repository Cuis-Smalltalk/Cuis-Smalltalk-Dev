'From Cuis 5.0 [latest update: #4241] on 26 June 2020 at 5:06:35 pm'!

!BitBlt commentStamp: 'jmv 6/26/2020 15:21:57' prior: 0!
WARNING: BitBlt's shape cannot be modified since WarpBlt relies on the exact layout. Important primitives will break if you fail to heed this warning.

I represent a block transfer (BLT) of pixels from one Form ( the sourceForm) into a rectangle (destX, destY, width, height) of the destinationForm, as modified by a combination rule, a possible halftoneForm and a possible color map.

The source of pixels may be a similar rectangle (at sourceX, sourceY) in the sourceForm, or the halftoneForm, or both.  If both are specified, their pixel values are combined by a logical AND function prior to any further combination rule processing. The halftoneForm may be an actual Form or a simple WordArray of 32 bit values usually intended to represent Color values. In either case the 'top' of the form is effectively aligned with the top of the destinationForm and for each scanline the destination y modulo the size of the halftoneForm gives the index of the word to use. This makes it easy to make horizontal stripes, for example.

In any case, the pixels from the source (AND'd with the halftone, remember) are combined with those of the destination by as specified by the combinationRules below- 
	name	rule		result 

					0	always 0
	and				1	src AND dst
					2	src AND not(dst)
	over				3	src only
	erase				4	not(src) AND dst
					5 	dst only
	reverse				6	src XOR dst
	under				7	src OR dst
					8	not(src) AND not(dst)
					9	not(src) XOR dst
					10	not(dst)
					11	src OR not(dst)
					12	not(src)
					13	not(src) OR dst
					14	not(src) OR not(dst)
					15	always 1
(You can find an interesting explanation of how this comes to be in http://dev-docs.atariforge.org/files/BLiTTER_1-25-1990.pdf - which interestingly fails to mention any connection to Smalltalk and PARC.)

Forms may be of different depths, see the comment in class Form.

In addition to the original 16 combination rules invented for monochrome Forms, this BitBlt supports
					16	fails (to simulate paint bits)
					17	fails (to simulate erase bits)
					18	sourceWord + destinationWord
					19	sourceWord - destinationWord
					20	rgbAdd: sourceWord with: destinationWord.  Sum of color components
					21	rgbSub: sourceWord with: destinationWord.  Difference of color components
					22	OLDrgbDiff: sourceWord with: destinationWord.  Sum of abs of differences in components
					23	OLDtallyIntoMap: destinationWord.  Tallies pixValues into a colorMap
							these old versions don't do bitwise dest clipping.  Use 32 and 33 now.
	blend				24	alphaBlend: sourceWord with: destinationWord.  32-bit source and dest only. Blend sourceWord 
							with destinationWord, assuming both are 32-bit pixels. The source is assumed to have 255*alpha
							in the high 8 bits of each pixel, while the high 8 bits of the destinationWord will be ignored.
							The blend produced is alpha*source + (1-alpha)*dest, with the computation being performed
							independently on each color component.  The high byte of the result will be 0.
	paint				25	pixPaint: sourceWord with: destinationWord.  Wherever the sourceForm is non-zero, it replaces
							the destination.  Can be used with a 1-bit source color mapped to (0, FFFFFFFF), and a fillColor 
							to fill the dest with that color wherever the source is 1.
	erase1BitShape				26	pixMask: sourceWord with: destinationWord.  Like pixPaint, but fills with 0.
					27	rgbMax: sourceWord with: destinationWord.  Max of each color component.
					28	rgbMin: sourceWord with: destinationWord.  Min of each color component.
					29	rgbMin: sourceWord bitInvert32 with: destinationWord.  Min with (max-source)
	blendAlpha				30	alphaBlendConst: sourceWord with: destinationWord.  alpha is an arg. works in 16 bits. Blend
							sourceWord with destinationWord using a constant alpha. Alpha is encoded as 0 meaning
							0.0, and 255 meaning 1.0. The blend produced is alpha*source + (1.0-alpha)*dest, with
							the computation being performed independently on each color component. 
	paintAlpha				31	alphaPaintConst: sourceWord with: destinationWord.  alpha is an arg. works in 16 bits.
					32	rgbDiff: sourceWord with: destinationWord.  Sum of abs of differences in components
					33	tallyIntoMap: destinationWord.  Tallies pixValues into a colorMap - Those tallied are exactly 
							those in the destination rectangle.  Note that the source should be specified == destination,
							in order for the proper color map checks  be performed at setup.
	blendAlphaScaled				34	alphaBlendScaled: srcWord with: dstWord. Alpha blend of scaled srcWord and destWord. In contrast
							to alphaBlend:with: the color produced is srcColor + (1-srcAlpha) * dstColor
					35	& 36 not used
	rgbMul				37	rgbMul: srcWord with: dstWord. 
					38	pixSwap: srcWord with: dstWord.
					39	pixClear: srcWord with: dstWord. Clear all pixels in destinationWord for which the pixels of 
							sourceWord have the same values. Used to clear areas of some constant color to zero.
					40	fixAlpha: srcWord with: dstWord. For any non-zero pixel value in destinationWord with zero alpha 
							channel take the alpha from sourceWord and fill it in. Intended for fixing alpha channels left at 
							zero during 16->32 bpp conversions.
					41	rgbComponentAlpha: srcWord with: dstWord.

Any transfer specified is further clipped by the specified clipping rectangle (clipX, clipY, clipWidth, clipHeight), and also by the bounds of the source and destination forms.
	To make a small Form repeat and fill a big form, use an InfiniteForm as the source.

Pixels copied from a source to a destination whose pixels have a different depth are converted based on the optional colorMap.  If colorMap is nil, then conversion to more bits is done by filling the new high-order bits with zero, and conversion to fewer bits is done by truncating the lost high-order bits.  

The colorMap, if specified, must be a either word array (ie Bitmap) with 2^n elements, where n is the pixel depth of the source, or a fully specified ColorMap which may contain a lookup table (ie Bitmap) and/or four separate masks and shifts which are applied to the pixels. For every source pixel, BitBlt will first perform masking and shifting and then index the lookup table, and select the corresponding pixelValue and mask it to the destination pixel size before storing.
	When blitting from a 32 or 16 bit deep Form to one 8 bits or less, the default is truncation.  This will produce very strange colors, since truncation of the high bits does not produce the nearest encoded color.  Supply a 512 long colorMap, and red, green, and blue will be shifted down to 3 bits each, and mapped.  The message copybits...stdColors will use the best map to the standard colors for destinations of depths 8, 4, 2 and 1.  Two other sized of colorMaps are allowed, 4096 (4 bits per color) and 32786 (five bits per color).
	Normal blits between 16 and 32 bit forms truncates or pads the colors automatically to provide the best preservation of colors.
	Colors can be remapped at the same depth.  Sometimes a Form is in terms of colors that are not the standard colors for this depth, for example in a GIF file.  Convert the Form to a MaskedForm and send colorMap: the list of colors that the picture is in terms of. (Note also that a Form can be copied to itself, and transformed in the process, if a non-nil colorMap is supplied.)!


!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'jmv 6/26/2020 16:01:09'!
standardWindowExtent
	"Answer the standard default extent for new windows.  "

	| grid allowedArea maxLevel |
	"NOTE: following copied from strictlyStaggeredInitialFrameFor:"
	allowedArea _ self maximumUsableArea insetBy: (
		self scrollBarSetback @ self screenTopSetback extent: `0@0`
	).
	"Number to be staggered at each corner (less on small screens)"
	maxLevel _ allowedArea area > 700000 ifTrue: [3] ifFalse: [2].
	"Amount by which to stagger (less on small screens)"
	grid _ allowedArea area > 700000 ifTrue: [40] ifFalse: [20].
	^ ((allowedArea extent - (grid*(maxLevel+1*2) + (grid//2))) min: `52@40` * Preferences standardCodeFont lineSpacing) rounded! !

