'From Cuis 4.0 of 3 April 2012 [latest update: #4211] on 12 April 2012 at 8:58:08 am'!

!FormCanvas class methodsFor: 'cached arrow forms' stamp: 'jmv 4/12/2012 08:56'!
buildArrowOfDirection: aSymbolDirection size: finalSizeInteger
	"PRIVATE - create an arrow with aSymbolDirectionDirection,  
	finalSizeInteger and aColor  
	 
	aSymbolDirectionDirection = #up, #down. #left or #right
	 (self buildArrowOfDirection: #down size: 120) display
	"

	| resizeFactor outerBox arrowMorph resizedForm f |
	resizeFactor _ 4.
	outerBox _ RectangleMorph new.
	outerBox
		morphExtent: finalSizeInteger asPoint * resizeFactor;
		borderWidth: 0;
		color: Color transparent.
	
	arrowMorph _ self buildArrowIn: outerBox bounds.
	outerBox addMorphFront: arrowMorph.
	arrowMorph morphPositionInOwner: 12@8.	"not a clue why these numbers work..."
	
	
	f _ outerBox imageForm: 32.
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

