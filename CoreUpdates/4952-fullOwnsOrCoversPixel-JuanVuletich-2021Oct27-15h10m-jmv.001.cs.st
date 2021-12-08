'From Cuis 5.0 [latest update: #4928] on 27 October 2021 at 3:11:25 pm'!

!Morph methodsFor: 'geometry services' stamp: 'jmv 10/27/2021 15:10:52'!
fullOwnsOrCoversPixel: worldPoint
	"Answer true if worldPoint is in some submorph, even if not inside our shape.
	See comment at #ownsOrCoversPixel: for important notes on behavior."

	(self ownsOrCoversPixel: worldPoint) ifTrue: [ ^ true ].
	self submorphsMightProtrude ifTrue: [
		self submorphsDo: [ :m |
			(m fullOwnsOrCoversPixel: worldPoint) ifTrue: [ ^ true ]]].
	^ false.! !

!methodRemoval: WidgetMorph #fullOwnsOrCoversPixel: stamp: 'jmv 10/27/2021 15:11:03'!
WidgetMorph removeSelector: #fullOwnsOrCoversPixel:!
!methodRemoval: KernelMorph #fullOwnsOrCoversPixel: stamp: 'jmv 10/27/2021 15:11:05'!
KernelMorph removeSelector: #fullOwnsOrCoversPixel:!
