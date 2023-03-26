'From Cuis 6.0 [latest update: #5720] on 26 March 2023 at 12:58:09 pm'!

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 3/26/2023 12:25:08'!
initializeLabelArea
	"Initialize the label area (titlebar) for the window."

	| topLeft spacing |
	topLeft := (self labelHeight + borderWidth - Theme current titleBarButtonsExtent / 2) ceiling asPoint.
	spacing := Theme current titleBarButtonsExtent x *13//10.
	self addMorph: self createCloseButton position: topLeft.
	self addMorph: self createCollapseButton position: spacing@0 + topLeft.
	self addMorph: self createExpandButton position: spacing*2@0 + topLeft.
	self addMorph: self createMenuButton position: spacing*3@0 + topLeft! !


!MenuMorph methodsFor: 'construction' stamp: 'jmv 3/26/2023 12:55:05'!
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
	closeButton iconDrawSelector: #drawCloseIcon; color: `Color transparent`.
	pinButton := PluggableButtonMorph model: self action: #stayUp.
	pinButton iconDrawSelector: #drawPushPinIcon; color: `Color transparent`.
	w := (titleMorph hasSubmorphs ifTrue: [ titleMorph firstSubmorph morphWidth ] ifFalse: [ 0 ]) + (buttonHW*2).
	self addMorphFront: 
		(LayoutMorph newRow
			"Make room for buttons"
			morphExtent: w @ (titleMorph morphHeight max: buttonHW * 12 // 10);
			color: `Color transparent`;
			addMorph: (BoxedMorph new noBorder color: `Color transparent`) fixedWidth: buttonHW//10;
			addMorph: (closeButton layoutSpec: (LayoutSpec fixedWidth: buttonHW fixedHeight: buttonHW));
			addMorph: (BoxedMorph new noBorder color: `Color transparent`) fixedWidth: buttonHW//7;
			addMorph: titleMorph proportionalWidth: 1;
			addMorph: (BoxedMorph new noBorder color: `Color transparent`) fixedWidth: buttonHW//7;
			addMorph: pinButton fixedWidth: buttonHW;
			addMorph: (BoxedMorph new noBorder color: `Color transparent`) fixedWidth: buttonHW//3).

	self setProperty: #hasStayUpIcons toValue: true.
	self removeProperty: #needsStayUpIcons! !

!MenuMorph methodsFor: 'drawing' stamp: 'jmv 3/26/2023 12:23:09'!
drawOn: aCanvas
	
	| roundCorners |
	roundCorners := Theme current roundWindowCorners.
	roundCorners
		ifTrue: [
			aCanvas roundRect: self morphLocalBounds color: color radius: Theme current roundedWindowRadius * 5//6 ]
		ifFalse: [
			aCanvas fillRectangle: self morphLocalBounds color: color borderWidth: borderWidth borderStyleSymbol: #raised baseColorForBorder: color ]! !

