'From Cuis7.7 [latest update: #7854] on 7 April 2026 at 12:05:07 pm'!

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'FJG 7/Apr/2026 12:04:20'!
drawTextLine: text at: drawingPosition extent: anExtent defaultFont: aFont foregroundColor: color
	"Example:
	PluggableListMorph
		withModel: {
			(Text string: 'Bold' attributes: {TextEmphasis bold}) , (Text string: ' and red' attributes: {TextColor color: Color red}).
			(Text string: 'Blue ' attributes: {TextColor color: Color blue}), (Text string: 'italic' attributes: {TextEmphasis italic}).
			(Text string: 'Normal ' attributes: {TextEmphasis normal}) , (Text string: 'strikethrough' attributes: {TextEmphasis struckThrough})
		}
		listGetter: #yourself
		indexGetter: nil
		indexSetter: nil
		:: openInHand.
	"
	| displayScanner line |

	line := TextLine
		start: 1
		stop: text size
		internalSpaces: 0
		paddingWidth: 0.
	line
		rectangle: (0@0 extent: anExtent);
		lineHeight: aFont ascent + aFont descent baseline: aFont ascent.

	displayScanner := MorphicScanner new.
	displayScanner
		defaultFont: aFont;
		text: text foreground: color;
		canvas: self.
	displayScanner displayLine: line textTopLeft: drawingPosition leftInRun: 0! !


!InnerListMorph methodsFor: 'drawing' stamp: 'FJG 3/Apr/2026 23:08:40'!
draw: item atRow: row on: canvas
	"display the given item at row row"
	(item is: #Text)
		ifTrue: [
			canvas
				drawTextLine: item
				at: 0 @ (self drawYForRow: row)
				extent: extent
				defaultFont: self fontToUse
				foregroundColor: Theme current text ]
		ifFalse: [
			canvas
				drawString: item string
				at: 0 @ (self drawYForRow: row)
				font: self fontToUse
				color: Theme current text ]! !

