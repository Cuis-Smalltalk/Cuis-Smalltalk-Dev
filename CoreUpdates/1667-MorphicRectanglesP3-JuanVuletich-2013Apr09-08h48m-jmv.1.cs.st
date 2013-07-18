'From Cuis 4.1 of 12 December 2012 [latest update: #1666] on 9 April 2013 at 9:33:38 am'!

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 4/9/2013 09:30'!
drawStringEmbossed: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor kern: kernOrNil

	aColor = Color black ifFalse: [ | topColor |
		topColor _ aColor alphaMixed: 0.25 with: Color black.
		self
			drawString: aString
			from: firstIndex
			to: lastIndex
			at: aPoint
			font: fontOrNil
			color: topColor
			kern: kernOrNil ].
	aColor = Color white ifFalse: [ | bottomColor |
		bottomColor _ aColor alphaMixed: 0.22 with: Color white.
		self
			drawString: aString
			from: firstIndex
			to: lastIndex
			at: aPoint  + (0@2)
			font: fontOrNil
			color: bottomColor
			kern: kernOrNil ].
	self
		drawString: aString
		from: firstIndex
		to: lastIndex
		at: aPoint  + (0@1)
		font: fontOrNil
		color: aColor
		kern: kernOrNil! !


!Morph methodsFor: 'geometry' stamp: 'jmv 4/9/2013 09:10'!
clippingRect
	"In own coordinates!!
	Return the bounds to which any submorphs should be clipped if the property is set"
	"Should be a region, like our shadow"
	self flag: #jmvVer2.
	^ 0@0 extent: self morphExtent! !


!BorderedRectMorph methodsFor: 'geometry' stamp: 'jmv 4/9/2013 09:11'!
clippingRect
	"In own coordinates!!
	Return the bounds to which any submorphs should be clipped if the property is set"
	"Should be a region, like our shadow"
	self flag: #jmvVer2.
	^ super clippingRect insetBy: borderWidth! !


!BitBlt methodsFor: 'accessing' stamp: 'jmv 4/9/2013 08:49'!
clipRect
	"
	
	Eliminar senders e implementors de clipRect y clipRect:
	Los de string, son analogos al scroller. los otros ver. limpiar todo esto que me molesta para m3.
	"

	"Answer the receiver's clipping area rectangle."

	^clipX @ clipY extent: clipWidth @ clipHeight! !


!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 4/9/2013 09:16'!
drawString: aString from: firstIndex to: lastIndex in: aRectangle font: fontOrNil color: aColor kern: kernOrNil

	self additionalClipRect: aRectangle while: [
		self
			drawString: aString
			from: firstIndex
			to: lastIndex
			at: aRectangle topLeft
			font: fontOrNil
			color: aColor
			kern: kernOrNil ]! !

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 4/9/2013 09:29'!
drawStringEmbossed: aStringOrText from: firstIndex to: lastIndex in: aRectangle font: fontOrNil color: aColor

	self additionalClipRect: aRectangle while: [
		self
			drawStringEmbossed: aStringOrText asString
			from: firstIndex
			to: lastIndex
			at: aRectangle topLeft
			font: fontOrNil
			color: aColor
			kern: nil ]! !

!FormCanvas methodsFor: 'clipping rectangle' stamp: 'jmv 4/9/2013 09:12'!
additionalClipRect: aRectangle while: aBlock
	"aRectangle is in MORPH coordinates.
	But ivar clipRect is relative to the form,
	For example, if we had previously been built like
		aCanvas on: someForm over: (100@100 extent 200@100)
	then our origin would be -100 @ -100.
	Then, a clipRect argument like (120@120 extent: 40@30) would mean affecting
	only (20@20 extent: 40@30) in our form"
	| oldClipRect |
	oldClipRect _ clipRect.
	self setClipRect: (clipRect intersect: (currentTransformation displayBoundsOfTransformOf: aRectangle)).
	aBlock value.
	self setClipRect: oldClipRect.! !


!Morph methodsFor: 'drawing' stamp: 'jmv 4/9/2013 09:12'!
drawSubmorphsOn: aCanvas 
	"Display submorphs back to front"

	submorphs isEmpty ifTrue: [ ^ self ].

	"No Clipping? Just draw and we're done."
	self clipsSubmorphs ifFalse: [
		submorphs reverseDo: [ :m |
			aCanvas fullDraw: m ].
		^self].

	"Draw with additional clipping done by us."
	aCanvas additionalClipRect: self clippingRect while: [
		submorphs reverseDo: [ :m |
			aCanvas fullDraw: m ]]! !

!methodRemoval: BorderedRectMorph #clippingBoundsInWorld!
BorderedRectMorph removeSelector: #clippingBoundsInWorld!
!methodRemoval: Morph #clippingBoundsInWorld!
Morph removeSelector: #clippingBoundsInWorld!
!methodRemoval: FormCanvas #drawStringEmbossed:from:to:at:font:color:!
FormCanvas removeSelector: #drawStringEmbossed:from:to:at:font:color:!
