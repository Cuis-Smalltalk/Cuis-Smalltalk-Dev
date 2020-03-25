'From Cuis 5.0 [latest update: #4073] on 24 March 2020 at 10:39:45 am'!

!LayoutMorph methodsFor: 'geometry' stamp: 'jmv 3/24/2020 10:23:22'!
minimumExtent
	"Answer size sufficient to frame my submorphs."
	
	| width height |
	width := self ySeparation.
	height := self xSeparation.
	(self direction =  #vertical)
		ifTrue: [
			self submorphsDo: [ :sm | | smMinExtent |
				smMinExtent := sm minimumLayoutExtent.
				"use maximum width across submorphs"
				width := width max: (smMinExtent x max: sm layoutSpec fixedOrMinimumLayoutWidth).
				"sum up submorph heights"
				height := height + (smMinExtent y max: sm layoutSpec fixedOrMinimumLayoutHeight) + self ySeparation.
			].
		     width := width + self xSeparation.
		]
		ifFalse: [
			self submorphsDo: [ :sm | | smMinExtent |
				smMinExtent := sm minimumLayoutExtent.
				"sum up submorphs width"
				width := width + (smMinExtent x max: sm layoutSpec fixedOrMinimumLayoutWidth) + self xSeparation.
				"use maximum height across submorph"
				height := height max: (smMinExtent y max: sm layoutSpec fixedOrMinimumLayoutHeight).
			].
			height := height + self ySeparation.
		].

	^ (width @ height) + self extentBorder! !

