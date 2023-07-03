'From Cuis 6.0 [latest update: #5887] on 3 July 2023 at 11:21:14 am'!

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 7/3/2023 11:16:47'!
drawTextComposition: aTextComposition at: drawingPosition extent: anExtent color: c selectionColor: sc avoidOverhang: avoidLastNotFullyVisibleLine
	"strict boolean controls whether to avoid or include the last line, if it doesn't fully fit in visibleTextBounds"

	| displayScanner leftInRun line possiblyVisible firstLineIndex lastLineIndex |

	possiblyVisible := currentTransformation boundsOfInverseTransformOf: clipRect.
	possiblyVisible := (possiblyVisible translatedBy: drawingPosition negated) intersect: (0@0 extent: anExtent).

	displayScanner := MorphicScanner new
		defaultFont: aTextComposition defaultFont;
		text: aTextComposition textComposed
		foreground: c.
	displayScanner canvas: self.

	leftInRun := 0.
	"Take clipRect into account. Extrememly fast scrolls and redraws of huge files (like .sources)"
	firstLineIndex := aTextComposition lineIndexForPoint: (possiblyVisible topLeft max: `0@0`).
	lastLineIndex := aTextComposition lineIndexForPoint: (possiblyVisible bottomRight min: anExtent).
	avoidLastNotFullyVisibleLine ifTrue: [
		line := aTextComposition lines at: lastLineIndex.
		 line top + line baseline > anExtent y ifTrue: [
			lastLineIndex := lastLineIndex - 1 ]].
	firstLineIndex to: lastLineIndex do: [ :i |
		line := aTextComposition lines at: i.
		aTextComposition
			displaySelectionInLine: line
			on: self
			textTopLeft: drawingPosition
			selectionColor: sc.
		leftInRun := displayScanner displayLine: line textTopLeft: drawingPosition leftInRun: leftInRun ].! !


!HybridCanvas methodsFor: 'drawing-text' stamp: 'jmv 7/3/2023 11:09:49'!
drawTextComposition: aTextComposition at: drawingPosition extent: anExtent color: c selectionColor: sc avoidOverhang: avoidLastNotFullyVisibleLine

	drawingOnMorphIDs ifTrue: [
		^nil ].
	^super drawTextComposition: aTextComposition at: drawingPosition extent: anExtent color: c selectionColor: sc avoidOverhang: avoidLastNotFullyVisibleLine! !

