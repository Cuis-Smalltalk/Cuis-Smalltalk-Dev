'From Cuis 5.0 [latest update: #4263] on 19 July 2020 at 4:07:56 pm'!

!HaloMorph methodsFor: 'event handling' stamp: 'jmv 7/12/2020 21:28:50'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn."

	self
		invalidateDisplayRect:
			((self morphBoundsInWorld
				outsetBy: self class handleSize*2)
					extendBy: 0@(FontFamily defaultLineSpacing+self class handleSize))
		from: nil.
	"Label could be wider than us..."
	self submorphsDrawingOutsideReverseDo: [ :m | m redrawNeeded ]! !

