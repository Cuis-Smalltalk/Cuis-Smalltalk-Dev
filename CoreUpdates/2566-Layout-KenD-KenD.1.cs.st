'From Cuis 4.2 of 25 July 2013 [latest update: #2563] on 29 October 2015 at 4:45:16.840423 pm'!

!LayoutMorph methodsFor: 'geometry' stamp: 'KenD 10/28/2015 20:38'!
calculateMinimumExtent
	"Answer size sufficient to frame my submorphs."
	
	| width height |
	width := self ySeparation.
	height := self xSeparation.
	(self direction =  #vertical)
		ifTrue: [
			self submorphsDo: [ :sm | | smMinExtent |
				smMinExtent := sm minimumExtent.
				"use maximum width across submorphs"
				width := width max: smMinExtent x. 
				"sum up submorph heights"
				height := height + smMinExtent y + self ySeparation. 
			].
		     width := width + self xSeparation.
		]
		ifFalse: [
			self submorphsDo: [ :sm | | smMinExtent |
				smMinExtent := sm minimumExtent.
				"sum up submorphs width"
				width := width + smMinExtent x + self xSeparation. 
				"use maximum height across submorph"
				height := height max: smMinExtent y. 
			].
			height := height + self xSeparation.
		].

	^ (width @ height) + self extentBorder.! !

