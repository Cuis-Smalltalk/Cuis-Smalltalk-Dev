'From Cuis 4.0 of 21 April 2012 [latest update: #1309] on 23 May 2012 at 10:05:41 pm'!

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 5/23/2012 22:02'!
createAcceptButton
	"create the [accept] button"
	| result |
	result _ PluggableButtonMorph new
		 model: self;
		 color: Theme current acceptButton;
		 label: 'Accept';
		 action: #acceptClicked.
	self addMorph: result.
	result bounds: (29@90 corner: 122@117).
	^ result! !

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 5/23/2012 22:03'!
createCancelButton
	"create the [cancel] button"
	| result |
	result _ PluggableButtonMorph new
		 model: self;
		 color: Theme current cancelButton;
		 label: 'Cancel';
		 action: #cancelClicked.
	self addMorph: result.
	result bounds: (149@90 corner: 242@117).
	^ result! !


!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 5/23/2012 22:04'!
drawRegularLabelOn: aCanvas

	| w f center x y  availableW l labelMargin |

	f _ self fontToUse.
	center _ bounds center.

	label ifNotNil: [
		labelMargin _ 4.
		w _ f widthOfString: label.
		availableW _ bounds width-labelMargin-labelMargin-1.
		availableW >= w
			ifTrue: [
				x _ center x - (w // 2).
				l _ label ]
			ifFalse: [
				x _ bounds left + labelMargin.
				l _ label squeezedTo: (label size * availableW / w) rounded ].
		y _ center y - (f height // 2).
		self isPressed ifTrue: [
			x _ x + 1.
			y _ y + 1 ].
		aCanvas
			drawString: l
			in: (x@y extent: extent - (labelMargin*2-2@4))
			font: f
			color: Theme current buttonLabel ]! !

