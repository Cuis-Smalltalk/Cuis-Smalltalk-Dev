'From Cuis 4.1 of 12 December 2012 [latest update: #1650] on 1 April 2013 at 1:52:55 pm'!

!FormCanvas methodsFor: 'private' stamp: 'jmv 4/1/2013 10:25'!
setClipRect: aRectangle

	clipRect _ aRectangle.
	port clipRect: aRectangle! !


!FormCanvas methodsFor: 'copying' stamp: 'jmv 4/1/2013 10:28'!
copyClipRect: aRectangle
	| o |
	o _ self canvasOrigin.
	^ self clone
		resetGrafPort
			setOrigin: o;
			setClipRect: (clipRect intersect: (aRectangle translatedBy: o))
! !

!FormCanvas methodsFor: 'private' stamp: 'jmv 4/1/2013 10:25'!
setOrigin: aPoint

	transformations first setTranslation: aPoint! !


!FormCanvas class methodsFor: 'instance creation' stamp: 'jmv 4/1/2013 10:33'!
depth: depth over: aRectangle
	"Color black represents one for 1bpp"

	^ self new
		setForm: (Form extent: aRectangle extent depth: depth);
		setOrigin: aRectangle topLeft negated! !

!FormCanvas class methodsFor: 'instance creation' stamp: 'jmv 4/1/2013 10:33'!
on: aForm over: aRectangle
	"Warning: aForm extent must equal aRectangle extent"

	^ self new
		setForm: aForm;
		setOrigin: aRectangle topLeft negated! !


!Morph methodsFor: 'drawing' stamp: 'jmv 3/31/2013 20:03'!
drawSubmorphsOn: aCanvas 
	"Display submorphs back to front"
	submorphs isEmpty ifTrue: [ ^ self ].
	self clipsSubmorphs
		ifTrue: [
			aCanvas
				clipBy: self clippingBoundsInWorld
				during: [ :clippedCanvas | 
					submorphs reverseDo: [ :m |
						clippedCanvas fullDraw: m ] ] ]
		ifFalse: [
			submorphs reverseDo: [ :m |
				aCanvas fullDraw: m ]]! !

!Morph methodsFor: 'drawing' stamp: 'jmv 4/1/2013 10:38'!
fullDrawOn: aCanvas
	"Draw the full Morphic structure on the given Canvas"

	"Draw receiver itself"
	(aCanvas isVisible: self morphBoundsInWorld) ifTrue: [
		self drawOn: aCanvas ].	
	self drawSubmorphsOn: aCanvas! !

!Morph methodsFor: 'drawing' stamp: 'jmv 3/31/2013 19:58'!
ownShadowForm
	"Return a form representing the 'shadow' of the receiver, without including submorphs 
	regardless of clipping"
	| canvas |
	canvas _ FormCanvas forShadowOver: self morphBoundsInWorld.
	canvas into: self.
	self drawOn: canvas.
	^ canvas form! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 4/1/2013 13:51'!
processDropMorph: aDropEvent localPosition: localEventPosition
	"Handle a dropping morph."
	| aMorph |
	
	aDropEvent wasHandled ifTrue: [ ^self ]. "Do it just once, for one drop destination"
	
	aMorph _ aDropEvent contents.
	aDropEvent wasHandled: true.
	self acceptDroppingMorph: aMorph event: aDropEvent.
	aMorph justDroppedInto: self event: aDropEvent! !


!PasteUpMorph methodsFor: 'drawing' stamp: 'jmv 3/31/2013 19:57'!
drawOn: aCanvas

	"draw background image."
	backgroundImage
		ifNotNil: [
			aCanvas image: backgroundImage at: 0@0 ]
		ifNil: [
			"draw background fill"
			(self isWorldMorph and: [ aCanvas drawsOnDisplay ] and: [ color class == TranslucentColor ])
				ifTrue: [
					"Special case so a translucent background on the Display allows you to see through the main Squeak Window.
					Requires proper handling of translucent Display in the VM.
					Seems to work only on Linux when using a composing window manager."
					(BitBlt toForm: Display) clipRect: aCanvas clipRect;
						copy: Display boundingBox
						from: 0@0 in: nil
						fillColor: color rule: Form over]
				ifFalse: [ super drawOn: aCanvas ]]! !


!Transcript class methodsFor: 'displaying' stamp: 'jmv 4/1/2013 10:48'!
displayOn: aForm
	"
	Transcript displayOn: Display
	"
	| font count i string x y fh f bw r canvas |
	bw _ self borderWidth  .
	r _ innerRectangle outsetBy: bw + self padding.
	aForm border: r width: bw. 
	aForm fill: r fillColor: Color white.
	font _ StrikeFont default.
	
	fh _ font height.
	count _ innerRectangle height // fh-1.
	x _ innerRectangle left.
	y _ innerRectangle top.
	f _ firstIndex-1.
	firstIndex > lastIndex ifTrue: [ f _ f - self maxEntries ].
	i _ (lastIndex - count max: f) \\ self maxEntries + 1.
	canvas _ aForm getCanvas.
	canvas
		setOrigin: 0@0;
		setClipRect: innerRectangle.
	[
		string _ entries at: i.	
		canvas drawString: string at: x@y font: font color: Color veryDarkGray.
		y _ y + fh.
		i = lastIndex
	] whileFalse: [ i _ i \\ self maxEntries + 1 ].

	string _ unfinishedEntry contents.
	canvas drawString: string at: x@y font: font color: Color veryDarkGray! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 4/1/2013 10:48'!
displayUnfinishedEntryOn: aForm

	| font count string x y fh canvas |
	font _ StrikeFont default.
	
	fh _ font height.
	count _ innerRectangle height // fh-1.
	x _ innerRectangle left.

	string _ unfinishedEntry contents.
	y _ ((lastIndex - firstIndex \\ self maxEntries) min: count-1) + 1 * font height + innerRectangle top.
	canvas _ aForm getCanvas.
	canvas
		setOrigin: 0@0;
		setClipRect: innerRectangle;
		drawString: string at: x@y font: font color: Color veryDarkGray! !

!methodRemoval: FormCanvas #setOrigin:clipRect:!
FormCanvas removeSelector: #setOrigin:clipRect:!
