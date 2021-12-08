'From Cuis 5.0 [latest update: #4990] on 30 November 2021 at 2:05:56 pm'!

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 11/30/2021 13:58:15'!
textComposition: aTextComposition bounds: boundsRect color: c selectionColor: sc avoidOverhang: strict
	"strict boolean controls whether to avoid or include the last line, if it doesn't fully fit in boundsRect"

	| displayScanner leftInRun line possibleVisibleLocalRect textTopLeft |

	textTopLeft _ boundsRect topLeft.
	possibleVisibleLocalRect _ currentTransformation boundsOfInverseTransformOf: self clipRect.
	possibleVisibleLocalRect _ possibleVisibleLocalRect intersect: boundsRect.
	possibleVisibleLocalRect _ possibleVisibleLocalRect translatedBy: boundsRect topLeft negated.

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
			(strict not or: [ line top + line baseline < possibleVisibleLocalRect bottom ]) ifTrue: [
				aTextComposition
					displaySelectionInLine: line
					on: self
					textTopLeft: textTopLeft
					selectionColor: sc.
				leftInRun _ displayScanner displayLine: line textTopLeft: textTopLeft leftInRun: leftInRun ]]! !


!TextParagraphMorph methodsFor: 'drawing' stamp: 'jmv 11/30/2021 13:59:22'!
drawOn: aCanvas
	aCanvas
		fillRectangle: self morphLocalBounds color: color.
	aCanvas
		textComposition: textComposition
		bounds: (self morphLocalBounds insetBy: borderWidth * 2)
		color: Theme current text
		selectionColor: `Color red`
		avoidOverhang: true.
	aCanvas
		frameRectangle: self morphLocalBounds
		color: borderColor
		borderWidth: borderWidth
		borderStyleSymbol: nil! !


!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 11/30/2021 13:58:53'!
textComposition: aTextComposition bounds: boundsRect color: c selectionColor: sc

	self textComposition: aTextComposition bounds: boundsRect color: c selectionColor: sc avoidOverhang: false! !

