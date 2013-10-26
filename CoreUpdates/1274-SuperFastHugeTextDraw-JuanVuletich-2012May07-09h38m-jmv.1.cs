'From Cuis 4.0 of 21 April 2012 [latest update: #1271] on 7 May 2012 at 9:41:05 am'!

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 5/7/2012 09:40'!
paragraph: aParagraph bounds: boundsInWorld color: c selectionColor: sc
	| displayScanner leftInRun line |

	self setPaintColor: c.

	displayScanner _ MorphicScanner new 
		text: aParagraph paragraphText
		foreground: (shadowColor ifNil: [ c ])
		ignoreColorChanges: self isShadowDrawing.
	displayScanner canvas: self.

	leftInRun _ 0.
	"Take clipRect into account. Extrememly fast scrolls and redraws of huge files (like .sources)"
	(aParagraph lineIndexForPoint: (0@0 max: clipRect origin- boundsInWorld origin))
		to: (aParagraph lineIndexForPoint: (boundsInWorld extent min: clipRect corner - boundsInWorld origin))
		do: [ :i |
			line _ aParagraph lines at: i.
			aParagraph
				displaySelectionInLine: line
				on: self
				paragraphTopLeft: boundsInWorld topLeft
				selectionColor: sc.
			leftInRun _ displayScanner displayLine: line paragraphTopLeft: boundsInWorld topLeft leftInRun: leftInRun  ]! !

