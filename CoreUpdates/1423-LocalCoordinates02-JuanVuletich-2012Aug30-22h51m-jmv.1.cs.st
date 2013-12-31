'From Cuis 4.0 of 21 April 2012 [latest update: #1422] on 30 August 2012 at 10:51:50 pm'!

!Morph methodsFor: 'drawing' stamp: 'jmv 8/30/2012 22:47'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."
	aCanvas
		zzfillRectangle: (0@0 extent: self morphExtent)
		colorOrInfiniteForm: self color! !


!BorderedRectMorph methodsFor: 'drawing' stamp: 'jmv 8/30/2012 22:48'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."
	aCanvas
		zzfillRectangle: (0@0 extent: self morphExtent)
		colorOrInfiniteForm: color
		borderWidth: borderWidth
		borderStyleSymbol: #simple
		baseColorForBorder: borderColor! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 8/30/2012 22:49'!
drawClassicFrameOn: aCanvas color: titleColor
	"Window border encompasses title area. No round corners. No title gradient."

	aCanvas zzfillRectangle: (0@0 extent: self morphExtent) colorOrInfiniteForm: color borderWidth: borderWidth borderStyleSymbol: #simple baseColorForBorder: self widgetsColor.

	aCanvas fillRectangle: self titleAreaInnerRect colorOrInfiniteForm: titleColor! !

