'From Cuis 5.0 [latest update: #4814] on 28 August 2021 at 7:29:54 pm'!

!ImageMorph methodsFor: 'accessing' stamp: 'jmv 8/28/2021 19:27:55'!
image: anImage 

	image _ anImage depth = 1 
		ifTrue: [ ColorForm mappingWhiteToTransparentFrom: anImage ]
		ifFalse: [ anImage ].
	self morphExtent: self minimumExtent.
	self redrawNeeded.! !

!methodRemoval: ImageMorph #morphExtent: stamp: 'jmv 8/28/2021 19:27:14'!
ImageMorph removeSelector: #morphExtent:!
