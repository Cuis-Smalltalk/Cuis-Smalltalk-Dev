'From Cuis 6.0 [latest update: #5875] on 22 June 2023 at 9:47:31 am'!

!TaskbarMorph methodsFor: 'initialization' stamp: 'KLG 6/21/2023 20:29:01'!
initialize
	super initialize.
	viewBox := LayoutMorph newRow color: self defaultColor.
	self
		addMorph: (clock := UpdatingLabelMorph initializedInstance)
		layoutSpec:  (LayoutSpec morphWidthProportionalHeight: 0.5).
	self
		addMorph: viewBox 
		layoutSpec: (LayoutSpec
			proportionalWidth: 1.0
			proportionalHeight: 1.0 
			offAxisEdgeWeight: #rightOrBottom).
	viewBox separation: self defaultHeight // 8.
	self setBalloonText: [ 
		'About this system:' bold, `String newLineString`,
		Smalltalk systemInformationString ].
	clock
		setProperty: #handlesMouseDown: toValue: true;
		setProperty: #'mouseButton1Down:localPosition:' 
		toValue: [ :evt : pos | | menu taskbarEdge point | 
			self runningWorld ifNotNil: [ :w |
				taskbarEdge := self morphPositionInWorld y.
				menu := (Preferences at: #worldMenu) getMenuIn: w at: w activeHand.	
				menu hide.
				w addMorph: menu. ": Otherwise we do not have a proper extent"
				point := 0 @ (taskbarEdge - menu morphExtentInWorld y).
				menu popUpAt: point allowKeyboard: false ] ]! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Theme current useTaskbar ifTrue: [self runningWorld hideTaskbar; showTaskbar.].!

