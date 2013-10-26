'From Cuis 4.0 of 21 April 2012 [latest update: #1483] on 14 November 2012 at 11:18:44 pm'!

!FormCanvas class methodsFor: 'cached arrow forms' stamp: 'jmv 11/14/2012 23:17'!
buildArrowOfDirection: aSymbolDirection size: finalSizeInteger
	"PRIVATE - create an arrow with aSymbolDirectionDirection,  
	finalSizeInteger and aColor  
	 
	aSymbolDirectionDirection = #up, #down. #left or #right
	 (self buildArrowOfDirection: #down size: 120) display
	 (self buildArrowOfDirection: #up size: 120) display
	"

	| resizeFactor resizedForm f e c bottomMargin topMargin |
	resizeFactor _ 4.
	e _ finalSizeInteger@finalSizeInteger*resizeFactor.
	f _ Form extent: e depth: 32.
	c _ Color gray: 0.5.
	topMargin _ finalSizeInteger * 3//4.
	bottomMargin _ finalSizeInteger * 4//4.
	0 to: e y-1-bottomMargin do: [ :y |
		0 to: e x -1 do: [ :x |
			(e x / 2 - 1 - x) abs * 2 + topMargin < y ifTrue: [
				f colorAt: x@y put: c
			]
		]
	].
	resizedForm _ f
		magnify: f boundingBox
		by: 1 / resizeFactor
		smoothing: 4.

	aSymbolDirection == #right ifTrue: [
		resizedForm _ resizedForm rotateBy: 90 ].
	aSymbolDirection == #down ifTrue: [
		resizedForm _ resizedForm rotateBy: 180 ].
	aSymbolDirection == #left ifTrue: [
		resizedForm _ resizedForm rotateBy:  270 ].
		
	aSymbolDirection == #up ifFalse: [
		resizedForm _ resizedForm
			copy: (resizedForm boundingBox insetBy: (resizedForm width - finalSizeInteger/ 2.0) rounded) ].
		
	^resizedForm! !

!methodRemoval: FormCanvas class #buildArrowIn:!
FormCanvas class removeSelector: #buildArrowIn:!
!methodRemoval: FormCanvas class #verticesForSimpleArrow:!
FormCanvas class removeSelector: #verticesForSimpleArrow:!
