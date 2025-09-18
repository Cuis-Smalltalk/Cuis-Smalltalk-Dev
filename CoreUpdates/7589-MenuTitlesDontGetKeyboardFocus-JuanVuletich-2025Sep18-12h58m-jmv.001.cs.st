'From Cuis7.5 [latest update: #7587] on 18 September 2025 at 12:59:01 pm'!

!LayoutMorph methodsFor: 'event handling testing' stamp: 'jmv 9/18/2025 12:56:19'!
handlesKeyboardFocus
	"Some subclasses may want to answer true, though."

	^false! !


!MenuMorph methodsFor: 'construction' stamp: 'jmv 9/18/2025 12:51:58'!
addStayUpIcons
	| closeButton pinButton w buttonHW |
	(Preferences at: #optionalButtons) ifFalse: [ ^self ].
	(self valueOfProperty: #hasStayUpIcons ifAbsent: [ false ])
		ifTrue: [
		 	self removeProperty: #needsStayUpIcons.
			^self ].
	titleMorph ifNil: [
		"Title not yet there. Flag ourself, so this method is called again when adding title."
		self setProperty: #needsStayUpIcons toValue: true.
		^ self].
	buttonHW := Theme current titleBarButtonsExtent x.
	closeButton := PluggableButtonMorph model: self action: #delete.
	closeButton iconDrawSelector: #drawCloseIcon; noButtonShape.
	pinButton := PluggableButtonMorph model: self action: #stayUp.
	pinButton iconDrawSelector: #drawPushPinIcon; noButtonShape.
	w := (titleMorph hasSubmorphs ifTrue: [ titleMorph firstSubmorph morphWidth ] ifFalse: [ 0 ]) + (buttonHW*2).
	self addMorphFront: 
		(LinearLayoutMorph newRow
			"Make room for buttons"
			morphExtent: w @ (titleMorph morphHeight max: buttonHW * 12 // 10);
			color: `Color transparent`;
			addMorph: (ColoredBoxMorph new color: `Color transparent`) lock fixedWidth: buttonHW//10;
			addMorph: (closeButton layoutSpec: (LayoutSizeSpec fixedWidth: buttonHW fixedHeight: buttonHW));
			addMorph: (ColoredBoxMorph new color: `Color transparent`) lock fixedWidth: buttonHW//7;
			addMorph: titleMorph lock proportionalWidth: 1;
			addMorph: (ColoredBoxMorph new color: `Color transparent`) lock fixedWidth: buttonHW//7;
			addMorph: pinButton fixedWidth: buttonHW;
			addMorph: (ColoredBoxMorph new color: `Color transparent`) lock fixedWidth: buttonHW//3).

	self setProperty: #hasStayUpIcons toValue: true.
	self removeProperty: #needsStayUpIcons! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 9/18/2025 12:49:15'!
addTitle: aString
	"Add a title line at the top of this menu Make aString its initial 
	contents.  
	If aSelector is not nil, then periodically obtain fresh values for its 
	contents by sending aSelector to aTarget.."

	| s pp w newMorph |
	
	newMorph := ColoredBoxMorph new lock.
	newMorph color: Theme current menuTitleBar.
	pp := `8@2`.
	aString asPlainString linesDo: [ :line | | font |
		font := Preferences at: #standardMenuFont.
		s := LabelMorph new
			contents: line;
			font: (titleMorph
				ifNil: [ font bold ]
				ifNotNil: [ font italic ])..
		newMorph addMorphBack: s position: pp.
		pp := pp + (0@(s morphHeight+2)) ].
	w := newMorph submorphs inject: 0 into: [ :prev :each |
		prev max: each morphWidth ].
	newMorph morphExtent: (w + 16) @ (pp y).
	titleMorph
		 ifNil: [
			titleMorph := newMorph.
			self addMorphFront: titleMorph ]
		ifNotNil: [ self addMorphBack: newMorph ].
	
	(self hasProperty: #needsStayUpIcons) ifTrue: [ self addStayUpIcons ]! !

