'From Cuis 5.0 [latest update: #4208] on 5 June 2020 at 10:41:48 am'!
!classDefinition: #BrowserCommentTextMorph category: #'Morphic-Views'!
TextModelMorph subclass: #BrowserCommentTextMorph
	instanceVariableNames: 'separator separatorHeight proportionalHeight '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views'!

!BrowserCommentTextMorph methodsFor: 'displaying' stamp: 'jmv 6/5/2020 10:38:23'!
hidePane

	self hide.
	separator visible ifTrue: [
		proportionalHeight _ self layoutSpec proportionalLayoutHeight ].
	separator layoutSpec fixedHeight: 0.
	self layoutSpec proportionalHeight: 0.
	separator ifNotNil: [ separator hide ]! !

!BrowserCommentTextMorph methodsFor: 'displaying' stamp: 'jmv 6/5/2020 10:38:45'!
showPane

	self show.
	proportionalHeight ifNotNil: [
		self layoutSpec proportionalHeight: proportionalHeight ].
	separator ifNotNil: [
		separator layoutSpec fixedHeight: Theme current layoutAdjusterThickness.
		separator show ]! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 6/5/2020 10:38:00'!
buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations comment separator |
	comment _ self buildMorphicCommentPane.
	separator _ LayoutAdjustingMorph new.
	comment separator: separator.
	codeAndButtons _ LayoutMorph newColumn.
	Preferences optionalButtons ifTrue: [
		codeAndButtons
			addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight;
			addAdjusterMorph ].
	codeAndButtons
		addMorph: self buildMorphicCodePane proportionalHeight: 2.0;
		addMorph: separator fixedHeight: Theme current layoutAdjusterThickness;
		addMorph: comment proportionalHeight: 2.0.
	Preferences showAnnotations ifFalse: [
		^codeAndButtons ].
	codeButtonsAndAnnotations _ LayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations! !


!LayoutMorph methodsFor: 'deprecated' stamp: 'jmv 6/5/2020 10:22:53'!
padding

	self deprecatedMethod. "use axisEdgeWeight"
	^ axisEdgeWeight! !

!LayoutMorph methodsFor: 'deprecated' stamp: 'jmv 6/5/2020 10:23:08'!
padding: aSymbolOrNumber
	"This sets how extra space is used when doing layout. For example, a column might have extra , unneded vertical space. #top means widgets are set close to the top, and extra space is at bottom. Conversely, #bottom means widgets are set close to the bottom, and extra space is at top. Valid values include #left and #right (for rows) and #center. Alternatively, any number between 0.0 and 1.0 might be used.
	self new padding: #center
	self new padding: 0.9
	"
	self deprecatedMethod. "use axisEdgeWeight:"
	axisEdgeWeight _ aSymbolOrNumber
		caseOf: {
			[ #top ] -> [ 0.0 ].
			[ #left ] -> [ 0.0 ].
			[ #center ] -> [ 0.5 ].
			[ #right ] -> [ 1.0 ].
			[ #bottom ] -> [ 1.0 ]
		}
		otherwise: [ aSymbolOrNumber ]! !


!LayoutSpec methodsFor: 'deprecated' stamp: 'jmv 6/5/2020 10:23:12'!
fixedOrMorphHeight

	self deprecatedMethod.
	
	^fixedHeight ifNil: [ morph morphHeight ]! !

!LayoutSpec methodsFor: 'deprecated' stamp: 'jmv 6/5/2020 10:23:15'!
minorDirectionPadding
	self deprecatedMethod. "use offAxisEdgeWeight"
	^offAxisEdgeWeight! !

!LayoutSpec methodsFor: 'deprecated' stamp: 'jmv 6/5/2020 10:23:19'!
minorDirectionPadding: aSymbolOrNumber
	"This sets how padding is done in the secondary direction. For instance, if the owning morph is set in a row, the row will control horizontal layout. But if there is unused vertical space, it will be used according to this parameter. For instance, #top sets the owning morph at the top. Same for #bottom and #center. If the owner is contained in a column, #left, #center or #right should be used. Alternatively, any number between 0.0 and 1.0 can be used.
	self new minorDirectionPadding: #center
	self new minorDirectionPadding: 0.9
	"
	self deprecatedMethod. "use offAxisEdgeWeight:"
	offAxisEdgeWeight _ aSymbolOrNumber
		caseOf: {
			[ #top ] -> [ 0.0 ].
			[ #left ] -> [ 0.0 ].
			[ #center ] -> [ 0.5 ].
			[ #right ] -> [ 1.0 ].
			[ #bottom ] -> [ 1.0 ]
		}
		otherwise: [ aSymbolOrNumber ]! !


!LayoutSpec class methodsFor: 'deprecated' stamp: 'jmv 6/5/2020 10:23:22'!
fixedWidth: aNumber fixedHeight: otherNumber minorDirectionPadding: aSymbolOrNumber
	self deprecatedMethod. "use fixedWidth:fixedHeight:offAxisEdgeWeight:"
	^ LayoutSpec new
		fixedWidth: aNumber;
		fixedHeight: otherNumber;
		minorDirectionPadding: aSymbolOrNumber! !

!LayoutSpec class methodsFor: 'deprecated' stamp: 'jmv 6/5/2020 10:23:25'!
fixedWidth: aNumber proportionalHeight: otherNumber minorDirectionPadding: aSymbolOrNumber
	self deprecatedMethod. "use fixedWidth:proportionalHeight:offAxisEdgeWeight:"
	^self new
		fixedWidth: aNumber;
		proportionalHeight: otherNumber;
		minorDirectionPadding: aSymbolOrNumber! !

!LayoutSpec class methodsFor: 'deprecated' stamp: 'jmv 6/5/2020 10:23:27'!
proportionalWidth: aNumber fixedHeight: otherNumber minorDirectionPadding: aSymbolOrNumber
	self deprecatedMethod. "proportionalWidth:fixedHeight:offAxisEdgeWeight:"
	^self new
		proportionalWidth: aNumber;
		fixedHeight: otherNumber;
		minorDirectionPadding: aSymbolOrNumber! !

!LayoutSpec class methodsFor: 'deprecated' stamp: 'jmv 6/5/2020 10:23:34'!
proportionalWidth: aNumber proportionalHeight: otherNumber minorDirectionPadding: aSymbolOrNumber
	self deprecatedMethod. "use proportionalWidth:proportionalHeight:offAxisEdgeWeight:
"
	^self new
		proportionalWidth: aNumber;
		proportionalHeight: otherNumber;
		minorDirectionPadding: aSymbolOrNumber! !


!Theme methodsFor: 'other options' stamp: 'jmv 6/5/2020 10:39:44'!
layoutAdjusterThickness

	self flag: #todo.  "Revisit this; consider moving proportional stuff out of Theme entirely."
	
	^ Preferences standardListFont pointSize // 3! !

!methodRemoval: PluggableButtonMorph #minLayoutWidth stamp: 'jmv 6/5/2020 10:41:29'!
PluggableButtonMorph removeSelector: #minLayoutWidth!
!methodRemoval: PluggableButtonMorph #minLayoutHeight stamp: 'jmv 6/5/2020 10:41:30'!
PluggableButtonMorph removeSelector: #minLayoutHeight!
!classDefinition: #BrowserCommentTextMorph category: #'Morphic-Views'!
TextModelMorph subclass: #BrowserCommentTextMorph
	instanceVariableNames: 'separator proportionalHeight'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views'!
