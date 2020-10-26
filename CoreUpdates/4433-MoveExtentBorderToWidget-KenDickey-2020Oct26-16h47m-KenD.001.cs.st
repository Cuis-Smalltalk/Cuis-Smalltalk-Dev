'From Cuis 5.0 [latest update: #4432] on 26 October 2020 at 4:48:09 pm'!

!WidgetMorph methodsFor: 'geometry' stamp: 'KenD 10/26/2020 12:16:38'!
extentBorder
	"This is the number of pixels to add to internal minimum to calculate
	my minimumExtent.  "
	
	^ 2 * borderWidth ! !

!methodRemoval: Morph #extentBorder stamp: 'KenD 10/26/2020 16:47:45'!
Morph removeSelector: #extentBorder!
