'From Cuis 5.0 of 7 November 2016 [latest update: #3188] on 29 September 2017 at 5:43:16 pm'!

!Form methodsFor: 'bordering' stamp: 'jmv 9/29/2017 16:59:58'!
border: aRectangle width: borderWidth borderHeight: borderHeight fillColor: aColor
	"Paint a border whose rectangular area is defined by aRectangle. The 
	width of the border of each side is borderWidth@borderHeight. Uses aHalfTone for 
	drawing the border."

	self border: aRectangle
		widthRectangle: 
			(Rectangle
				left: borderWidth
				right: borderWidth
				top: borderHeight
				bottom: borderHeight)
		rule: Form over
		fillColor: aColor! !

!Form methodsFor: 'bordering' stamp: 'jmv 9/29/2017 16:59:43'!
borderWidth: borderWidth borderHeight: borderHeight fillColor: aColor
	self border: self boundingBox width: borderWidth borderHeight: borderHeight fillColor: aColor! !

