'From Cuis 5.0 [latest update: #4960] on 31 October 2021 at 7:42:03 pm'!

!InnerTextMorph methodsFor: 'geometry' stamp: 'jmv 10/31/2021 19:37:38'!
privateExtent: aPoint
	| newExtent |

	newExtent _ aPoint.
	(owner is: #ScrollPane) ifTrue: [
		"Resist changing the extent if no wordwrap. this should be checked."
		wrapFlag ifFalse: [ ^ false ].
		
		"Just update width. Height is set by ourselves. See #fit"
		newExtent _ aPoint x truncated @ extent y ].

	^ super privateExtent: newExtent! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 10/31/2021 19:41:54'!
fit
	"Adjust my bounds to fit the text.
	Required after the text changes,
	or if wrapFlag is true and the user attempts to change the extent."

	| newExtent newHeight newWidth oldExtent |
	(owner is: #ScrollPane) ifFalse: [
		^self ].

	newWidth _ extent x.
	"Adjust width only if we don't wrap text to own width!!"
	wrapFlag ifFalse: [
		newWidth _ self textComposition usedWidth max: 9 ].
	newHeight _ self textComposition usedHeight max: FontFamily defaultLineSpacing + 2.
	newExtent _ newWidth @ newHeight.
	extent = newExtent 
		ifTrue: [
			self redrawNeeded.
			"Too conservative: only text composition (because of changes in text or styles, etc)
			should cause invalidation.
			Try to avoid calling #fit unless needed."
		]
		ifFalse: [
			extent = newExtent ifFalse: [
				oldExtent _ extent.
				extent _ newExtent.
				self extentChanged: oldExtent.
				owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
				self redrawNeeded ]].

	(owner is: #ScrollPane) ifTrue: [
		owner innerHeight: newExtent y ].! !


!ReadOnlyTextMorph methodsFor: 'geometry' stamp: 'jmv 10/31/2021 18:55:58'!
adjustExtent
"So far, copied verbatim from InnerTextMorph."
	"This is just a suggestion. If we do wordwrap, the width will be honored.
	But the height is whatever is appropriate for the contents!! See #fit"
	self morphExtent: owner viewableExtent! !

!ReadOnlyTextMorph methodsFor: 'geometry' stamp: 'jmv 10/31/2021 19:37:21'!
privateExtent: aPoint
	| newExtent |
"So far, copied verbatim from InnerTextMorph."

	newExtent _ aPoint.
	(owner is: #ScrollPane) ifTrue: [
		"Resist changing the extent if no wordwrap. this should be checked."
		wrapFlag ifFalse: [ ^ false ].
		
		"Just update width. Height is set by ourselves. See #fit"
		newExtent _ aPoint x truncated @ extent y ].

	^ super privateExtent: newExtent! !

!ReadOnlyTextMorph methodsFor: 'private' stamp: 'jmv 10/31/2021 19:41:51'!
fit
	"Adjust my bounds to fit the text.
	Required after the text changes,
	or if wrapFlag is true and the user attempts to change the extent."

	| newExtent newHeight newWidth oldExtent |
	(owner is: #ScrollPane) ifFalse: [
		^self ].

	newWidth _ extent x.
	"Adjust width only if we don't wrap text to own width!!"
	wrapFlag ifFalse: [
		newWidth _ self textComposition usedWidth max: 9 ].
	newHeight _ self textComposition usedHeight max: FontFamily defaultLineSpacing + 2.
	newExtent _ newWidth @ newHeight.
	extent = newExtent 
		ifTrue: [
			self redrawNeeded.
			"Too conservative: only text composition (because of changes in text or styles, etc)
			should cause invalidation.
			Try to avoid calling #fit unless needed."
		]
		ifFalse: [
			extent = newExtent ifFalse: [
				oldExtent _ extent.
				extent _ newExtent.
				self extentChanged: oldExtent.
				owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
				self redrawNeeded ]].! !


!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 10/31/2021 19:34:05'!
textComposition: aTextComposition bounds: boundsRect color: c selectionColor: sc
	| displayScanner leftInRun line possibleVisibleLocalRect textTopLeft |

	textTopLeft _ boundsRect topLeft.
	possibleVisibleLocalRect _ currentTransformation boundsOfInverseTransformOf: self clipRect.

	displayScanner _ MorphicScanner new
		defaultFont: aTextComposition defaultFont;
		text: aTextComposition textComposed
		foreground: c.
	displayScanner canvas: self.

	leftInRun _ 0.
	"Take clipRect into account. Extrememly fast scrolls and redraws of huge files (like .sources)"
	(aTextComposition lineIndexForPoint: (possibleVisibleLocalRect topLeft - textTopLeft max: `0@0`) )
		to: (aTextComposition lineIndexForPoint: (possibleVisibleLocalRect bottomRight - textTopLeft min: boundsRect bottomRight))
		do: [ :i |
			line _ aTextComposition lines at: i.
			aTextComposition
				displaySelectionInLine: line
				on: self
				textTopLeft: textTopLeft
				selectionColor: sc.
			leftInRun _ displayScanner displayLine: line textTopLeft: textTopLeft leftInRun: leftInRun ]! !

