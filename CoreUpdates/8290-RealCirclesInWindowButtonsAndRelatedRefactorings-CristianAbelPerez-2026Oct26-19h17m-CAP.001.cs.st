'From Cuis7.9 [latest update: #8281] on 26 October 2026 at 8:23:24 pm'!

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'CAP 26/Oct/2026, 11:16:38 pm (UTC)'!
drawRoundWindowButtonBackground: aBaseColor
	"Draw the round background of the close, collapse, expand and menu window buttons: a disc filled with aBaseColor (darker while pressed or under the mouse) with a thin gray border, sized to currentMorph.
	Center and radius are in the 22 units grid used by the window button path data (see #windowButtonClosePathData:), so the disc lines up with the symbol drawn over it."

	| size scale |

	size := currentMorph morphWidth.
	scale := size / 22.0.
	self strokeWidth: size/30.0 color: `Color gray` fillColor: (self windowButtonFillColor: aBaseColor) do: [
		self circleCenter: (10.5 * scale) asPoint radius: 10.0 * scale ]! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'CAP 26/Oct/2026, 11:16:38 pm (UTC)'!
drawSquareWindowButtonBackground: aBaseColor
	"Draw the background of the square window buttons: a square filled with aBaseColor (darker while pressed or under the mouse) with a gray border, sized to currentMorph."

	| size |

	size := currentMorph morphWidth.
	self strokeWidth: size/20.0 color: `Color gray` fillColor: (self windowButtonFillColor: aBaseColor) do: [
		self pathSequence: (self class windowSquareButtonPathData: size) ]! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'CAP 26/Oct/2026, 11:16:38 pm (UTC)'!
drawWindowButtonFilledSymbol: aPathSequence
	"Draw the symbol of a window button (for example the triangle of the menu button) as a filled shape, over its background.
	aPathSequence is in #pathSequence: format, already scaled to currentMorph."

	self fillColor: self windowButtonSymbolColor do: [
		self pathSequence: aPathSequence ]! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'CAP 26/Oct/2026, 11:16:38 pm (UTC)'!
drawWindowButtonStrokedSymbol: aPathSequence
	"Draw the symbol of a window button (for example the cross of the close button) as lines, over its background.
	aPathSequence is in #pathSequence: format, already scaled to currentMorph."

	self strokeWidth: (currentMorph morphWidth/10.0 max: 1.0) color: self windowButtonSymbolColor do: [
		self pathSequence: aPathSequence ]! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'CAP 26/Oct/2026, 11:16:38 pm (UTC)'!
isWindowButtonHighlighted
	"Answer whether the window button being drawn is pressed or under the mouse"

	^ currentMorph isPressed or: [ currentMorph mouseIsOver ]! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'CAP 26/Oct/2026, 11:16:38 pm (UTC)'!
windowButtonFillColor: aBaseColor
	"Background color of the window button being drawn, whose own color is aBaseColor"

	^ self isWindowButtonHighlighted
		ifTrue: [ aBaseColor * 0.85 ]
		ifFalse: [ aBaseColor ]! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'CAP 26/Oct/2026, 11:16:38 pm (UTC)'!
windowButtonSymbolColor
	"Color of the symbol drawn over the background of the window button being drawn"

	^ self isWindowButtonHighlighted
		ifTrue: [ `Color black` ]
		ifFalse: [ `Color darkGray` ]! !


!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'CAP 26/Oct/2026, 11:16:38 pm (UTC)'!
drawCloseIcon

	self drawRoundWindowButtonBackground: `Color r: 0.996 g: 0.329 b: 0.329`.
	self drawWindowButtonStrokedSymbol: (self class windowButtonClosePathData: currentMorph morphWidth)! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'CAP 26/Oct/2026, 11:16:38 pm (UTC)'!
drawCollapseIcon

	self drawRoundWindowButtonBackground: `Color r: 0.988 g: 0.557 b: 0.102`.
	self drawWindowButtonStrokedSymbol: (self class windowButtonCollapsePathData: currentMorph morphWidth)! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'CAP 26/Oct/2026, 11:16:38 pm (UTC)'!
drawExpandIcon

	self drawRoundWindowButtonBackground: `Color r: 0.337 g: 0.604 b: 0.161`.
	self drawWindowButtonStrokedSymbol: (self class windowButtonExpandPathData: currentMorph morphWidth)! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'CAP 26/Oct/2026, 11:16:38 pm (UTC)'!
drawMenuIcon

	self drawRoundWindowButtonBackground: `Color r: 0.212 g: 0.439 b: 0.784`.
	self drawWindowButtonFilledSymbol: (self class windowButtonMenuPathData: currentMorph morphWidth)! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'CAP 26/Oct/2026, 11:20:24 pm (UTC)'!
drawSquareCloseIcon

	self drawSquareWindowButtonBackground: `Color r: 0.996 g: 0.329 b: 0.329`.
	self drawWindowButtonStrokedSymbol: (self class windowButtonClosePathData: currentMorph morphWidth)! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'CAP 26/Oct/2026, 11:16:38 pm (UTC)'!
drawSquareExpandIcon

	self drawSquareWindowButtonBackground: `Color r: 0.337 g: 0.604 b: 0.161`.
	self drawWindowButtonStrokedSymbol: (self class windowButtonExpandPathData: currentMorph morphWidth)! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'CAP 26/Oct/2026, 11:16:38 pm (UTC)'!
drawSquareMenuIcon

	self drawSquareWindowButtonBackground: `Color r: 0.212 g: 0.439 b: 0.784`.
	self drawWindowButtonFilledSymbol: (self class windowButtonMenuPathData: currentMorph morphWidth)! !

!methodRemoval: AbstractVectorCanvas class #windowButtonPathData: stamp: 'CAP 26/Oct/2026, 10:48:27 pm (UTC)'!
AbstractVectorCanvas class removeSelector: #windowButtonPathData:!
