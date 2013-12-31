'From Cuis 4.0 of 21 April 2012 [latest update: #1432] on 8 September 2012 at 12:28:09 am'!

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 9/8/2012 00:26'!
drawEmbossedLabelOn: aCanvas

	| availableW center colorForLabel f l labelMargin targetSize w x y |
	label ifNotNil: [
		colorForLabel _ Theme current buttonLabel.
		self isPressed
			ifFalse: [
				self mouseIsOver
					ifFalse: [ colorForLabel _ colorForLabel adjustSaturation: -0.10 brightness: 0.10 ]]
			ifTrue: [ colorForLabel _ colorForLabel adjustSaturation: 0.0 brightness: -0.07 ].
		f _ self fontToUse.
		center _ extent // 2.
		labelMargin _ 3.
		w _ f widthOfString: label.
		availableW _ extent x - labelMargin - labelMargin.
		availableW >= w
			ifTrue: [
				l _ label ]
			ifFalse: [
				x _ labelMargin.
				targetSize _ label size * availableW // w.
				l _ label squeezedTo: targetSize.
				(f widthOfString: l) > availableW ifTrue: [
					targetSize _ targetSize - 1.
					l _ label squeezedTo: targetSize ]].
		
		w _ f widthOfString: l.
		x _ center x - (w // 2).
		y _ center y - (f height // 2).
		aCanvas
			zzdrawStringEmbossed: l
			in: (x@y extent: extent - (labelMargin*2-2@4))
			font: f
			color: colorForLabel ]! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 9/8/2012 00:24'!
drawRegularLabelOn: aCanvas

	| w f center x y  availableW l labelMargin |

	f _ self fontToUse.
	center _ extent // 2.

	label ifNotNil: [
		labelMargin _ 4.
		w _ f widthOfString: label.
		availableW _ extent x - labelMargin - labelMargin - 1.
		availableW >= w
			ifTrue: [
				x _ center x - (w // 2).
				l _ label ]
			ifFalse: [
				x _ labelMargin.
				l _ label squeezedTo: (label size * availableW / w) rounded ].
		y _ center y - (f height // 2).
		self isPressed ifTrue: [
			x _ x + 1.
			y _ y + 1 ].
		aCanvas
			zzdrawString: l
			in: (x@y extent: extent - (labelMargin*2-2@4))
			font: f
			color: Theme current buttonLabel ]! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 9/7/2012 23:40'!
drawRoundGradientLookOn: aCanvas
	| r colorForButton rect bottomFactor topFactor |

	self isPressed
		ifFalse: [
			topFactor _ Theme current buttonGradientTopFactor.
			bottomFactor _ Theme current buttonGradientBottomFactor.
			self mouseIsOver
				ifTrue: [	
					colorForButton _ Color h: color hue s: color saturation * 1.3 v: color brightness * 0.9 ]
				ifFalse: [
					colorForButton _ color ]]
		ifTrue: [
			topFactor _ Theme current buttonGradientBottomFactor.
			bottomFactor _ Theme current buttonGradientTopFactor.
			colorForButton _ color adjustSaturation: 0.1 brightness: -0.1 ].

	colorForButton ifNotNil: [
		r _ Theme current roundedButtonRadius.
		Theme current useButtonGradient
			ifTrue: [
				rect _ (0@0 extent: extent) insetBy: 1@3.
				aCanvas
					zzroundRect: rect
					color: colorForButton
					radius: r
					gradientTop: topFactor
					gradientBottom: bottomFactor
					gradientHeight: Theme current buttonGradientHeight ]
			ifFalse: [
				rect _ (0@0 extent: extent) insetBy: 1@3.
				aCanvas zzroundRect: rect color: colorForButton radius: r ]
		].

	Theme current embossedButtonLabels
		ifTrue: [ self drawEmbossedLabelOn: aCanvas ]
		ifFalse: [ self drawRegularLabelOn: aCanvas ]! !

!methodRemoval: FormCanvas #drawString:from:to:in:font:color:!
FormCanvas removeSelector: #drawString:from:to:in:font:color:!
!methodRemoval: FormCanvas #drawString:in:font:color:!
FormCanvas removeSelector: #drawString:in:font:color:!
!methodRemoval: FormCanvas #drawStringEmbossed:from:to:in:font:color:!
FormCanvas removeSelector: #drawStringEmbossed:from:to:in:font:color:!
!methodRemoval: FormCanvas #drawStringEmbossed:in:font:color:!
FormCanvas removeSelector: #drawStringEmbossed:in:font:color:!
!methodRemoval: FormCanvas #fillOval:color:borderWidth:borderColor:!
FormCanvas removeSelector: #fillOval:color:borderWidth:borderColor:!
!methodRemoval: FormCanvas #fillRectangle:colorOrInfiniteForm:borderWidth:borderStyleSymbol:!
FormCanvas removeSelector: #fillRectangle:colorOrInfiniteForm:borderWidth:borderStyleSymbol:!
!methodRemoval: FormCanvas #fillRectangle:colorOrInfiniteForm:borderWidth:borderStyleSymbol:baseColorForBorder:!
FormCanvas removeSelector: #fillRectangle:colorOrInfiniteForm:borderWidth:borderStyleSymbol:baseColorForBorder:!
!methodRemoval: FormCanvas #roundRect:color:radius:gradientTop:gradientBottom:gradientHeight:!
FormCanvas removeSelector: #roundRect:color:radius:gradientTop:gradientBottom:gradientHeight:!
!methodRemoval: FormCanvas #windowFrame:color:radius:border:labelHeight:gradientTop:gradientBottom:insideColor:!
FormCanvas removeSelector: #windowFrame:color:radius:border:labelHeight:gradientTop:gradientBottom:insideColor:!
