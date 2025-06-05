'From Cuis7.5 [latest update: #7261] on 5 June 2025 at 12:00:20 pm'!

!MenuMorph methodsFor: 'construction' stamp: 'jmv 6/5/2025 11:57:45'!
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
		(LayoutMorph newRow
			"Make room for buttons"
			morphExtent: w @ (titleMorph morphHeight max: buttonHW * 12 // 10);
			color: `Color transparent`;
			addMorph: (ColoredBoxMorph new color: `Color transparent`) fixedWidth: buttonHW//10;
			addMorph: (closeButton layoutSpec: (LayoutSpec fixedWidth: buttonHW fixedHeight: buttonHW));
			addMorph: (ColoredBoxMorph new color: `Color transparent`) fixedWidth: buttonHW//7;
			addMorph: titleMorph proportionalWidth: 1;
			addMorph: (ColoredBoxMorph new color: `Color transparent`) fixedWidth: buttonHW//7;
			addMorph: pinButton fixedWidth: buttonHW;
			addMorph: (ColoredBoxMorph new color: `Color transparent`) fixedWidth: buttonHW//3).

	self setProperty: #hasStayUpIcons toValue: true.
	self removeProperty: #needsStayUpIcons! !

