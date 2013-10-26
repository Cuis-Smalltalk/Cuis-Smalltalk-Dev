'From Cuis 4.0 of 21 April 2012 [latest update: #1428] on 4 September 2012 at 9:08 pm'!

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'jmv 9/4/2012 20:23'!
drawOn: aCanvas
	| rectangle w y0 h y1 y2 scrollbarThickness e |
	e _ self morphExtent.
	aCanvas zzframeAndFillRectangle: (0@0 extent: e) fillColor: self color borderWidth: borderWidth borderColor: borderColor.
	y0 _ 1.
	w _ e x-2.
	scrollbarThickness _ ScrollBar scrollbarThickness.
	completer entryCount > self class itemsPerPage  ifTrue: [
		w _ w - scrollbarThickness.
		aCanvas
			zzframeRectangle: (e x - scrollbarThickness@0
				extent: scrollbarThickness @ e y)
			borderWidth: 1
			color: borderColor.
		aCanvas
			zzimage: (FormCanvas arrowOfDirection: #up size: scrollbarThickness)
			at: e x - scrollbarThickness@0.
		aCanvas
			zzimage: (FormCanvas arrowOfDirection: #down size: scrollbarThickness)
			at: 0@0 + e - scrollbarThickness.
		h _ e y - (2 * scrollbarThickness).
		y1 _ (1.0 * self firstVisible-1 / completer entryCount * h) ceiling + y0 + scrollbarThickness-1.
		y2 _ (1.0 * self lastVisible / completer entryCount * h) floor + y0 + scrollbarThickness -1.
		aCanvas
			zzfillRectangle: (e x - scrollbarThickness+2@y1 corner:  e x-2 @ y2)
			colorOrInfiniteForm: Color veryLightGray ].
	self firstVisible
		to: self lastVisible
		do: [ :index |
			rectangle _ 1@y0 extent: w@self class itemHeight.
			index = self selected
				ifTrue: [
					aCanvas zzfillRectangle: rectangle colorOrInfiniteForm: (Theme current listHighlightFocused: true) ].
			aCanvas
				zzdrawString: (completer entries at: index) asString
				in: rectangle
				font: self class listFont
				color: Theme current text.
			y0 _ y0 + self itemHeight ]! !


!ImageMorph methodsFor: 'drawing' stamp: 'jmv 9/4/2012 20:49'!
drawOn: aCanvas

	aCanvas zzimage: image at: 0@0! !


!InnerTextMorph methodsFor: 'private' stamp: 'jmv 9/4/2012 20:00'!
selectionChanged
	self paragraph selectionRects do: [:r | self zzinvalidRect: r ]! !


!MagnifierMorph methodsFor: 'drawing' stamp: 'jmv 9/4/2012 20:50'!
drawOn: aCanvas
	RecursionLock == self ifFalse: [
		super drawOn: aCanvas.		"border and fill"
		aCanvas isShadowDrawing ifFalse: [
			"Optimize because #magnifiedForm is expensive"
			aCanvas zzimage: self magnifiedForm at: borderWidth@borderWidth]]! !


!PasteUpMorph methodsFor: 'drawing' stamp: 'jmv 9/4/2012 21:02'!
drawOn: aCanvas

	"draw background image."
	backgroundImage
		ifNotNil: [
			"self clipsSubmorphs ifTrue: [
				aCanvas clipBy: self clippingBounds
					during: [ :canvas | canvas image: backgroundImage multipliedBy: color at: bounds topLeft ]]
				ifFalse: [ aCanvas image: backgroundImage multipliedBy: color at: bounds topLeft ]"
			self clipsSubmorphs ifTrue: [
				aCanvas clipBy: self clippingBounds
					during: [ :canvas | canvas zzimage: backgroundImage at: 0@0 ]]
				ifFalse: [ aCanvas zzimage: backgroundImage at: 0@0 ]]

		ifNil: [
			"draw background fill"
			(self isWorldMorph and: [aCanvas drawsOnDisplay] and: [color class == TranslucentColor])
				ifTrue: [
					"Special case so a translucent background on the Display allows you to see through the main Squeak Window.
					Requires proper handling of translucent Display in the VM.
					Seems to work only on Linux when using a composing window manager."
					(BitBlt current toForm: Display) clipRect: aCanvas clipRect;
						copy: Display boundingBox
						from: 0@0 in: nil
						fillColor: color rule: Form over]
				ifFalse: [ super drawOn: aCanvas ]]! !


!Preferences class methodsFor: 'standard queries' stamp: 'jmv 9/4/2012 20:14'!
fastDragWindowForMorphic
	^ "self
		valueOfFlag: #fastDragWindowForMorphic
		ifAbsent: [ false ]"true not
		"resizeo, completar... O matar esta preferencia?"! !


!TranscriptMorph methodsFor: 'drawing' stamp: 'jmv 9/4/2012 21:03'!
drawOn: aCanvas
	"
	Transcript
		showOnDisplay: true;
		bounds: bounds;
		displayOn: aCanvas form.
	"
	Transcript
		showOnDisplay: true;
		morphBoundsInWorld: (0@0 extent: self morphExtentInWorld);
		displayOn: form;
		morphBoundsInWorld: self morphBoundsInWorld.
	aCanvas zzimage: form at: 0@0! !


!Transcripter methodsFor: 'accessing' stamp: 'jmv 9/4/2012 19:55'!
endEntry
	| c d cb |
	c _ self contents.
	Display extent ~= DisplayScreen actualScreenSize ifTrue: [
		"Handle case of user resizing physical window"
		DisplayScreen startUp.
		frame _ frame intersect: Display boundingBox.
		^ self clear; show: c].
	para
		setModel: (TextModel withText: c asText);
		extentForComposing: frame width-8 @9999.
	para composeAll.
	d _ para extent y - frame height.
	d > 0 ifTrue: [
		"Scroll up to keep all contents visible"
		cb _ para characterBlockAtPoint:
			0@0 + (0@(d+StrikeFont default height)).
		self on: (c copyFrom: cb stringIndex to: c size).
		readLimit _ position _ collection size.
		^ self endEntry].
	Display fill: (frame insetBy: -2) fillColor: self black;
			fill: frame fillColor: self white.
	Display getCanvas
		paragraph: para
		bounds: (4@4 + frame topLeft extent: Display extent)
		color: Color black
		selectionColor: Color blue! !


!WorldState methodsFor: 'drawing' stamp: 'jmv 9/4/2012 20:12'!
displayWorld: aWorld submorphs: submorphs
	"Update this world's display."

	| deferredUpdateMode worldDamageRects handsToDraw allDamage |
	self checkIfUpdateNeeded ifFalse: [ ^ self ].  "display is already up-to-date"
	deferredUpdateMode _ self doDeferredUpdatingFor: aWorld.
	deferredUpdateMode ifFalse: [ self assuredNonDisplayCanvas ].

	"repair world's damage on canvas"
	worldDamageRects _ self drawInvalidAreasWorld: aWorld submorphs: submorphs.

	"Check which hands need to be drawn (they are not the hardware mouse pointer)"
	handsToDraw _ self selectHandsToDrawForDamage: worldDamageRects.
	allDamage _ Array streamContents: [ :strm |
		strm nextPutAll: worldDamageRects.
		handsToDraw do: [ :h | 
			h savePatchFrom: canvas appendDamageTo: strm ]].

	"Draw hands (usually carying morphs) onto world canvas"
	handsToDraw reverseDo: [ :h | self drawHand: h ].

	"*make this true to flash damaged areas for testing*"
	Preferences debugShowDamage ifTrue: [ aWorld flashRects: allDamage ].

	"quickly copy altered rects of canvas to Display:"
	deferredUpdateMode
		ifTrue: [ self forceDamageToScreen: allDamage ]
		ifFalse: [ canvas showAt: aWorld viewBox origin invalidRects: allDamage ].

	"Restore world canvas under hands and their carried morphs"
	handsToDraw do: [ :h | h restoreSavedPatchOn: canvas ].
	Display deferUpdates: false; forceDisplayUpdate! !

!WorldState methodsFor: 'drawing' stamp: 'jmv 9/4/2012 20:14'!
drawHand: aHandMorph
	"Draw a hand that carries morphs, or needs to be drawn by us, because of not being the hardware mouse pointer."
	| bw r |
	Preferences fastDragWindowForMorphic ifTrue: [
		bw _ HandMorph fastDragBorderWidth // 2.
		r _ aHandMorph morphFullBoundsInWorld.
		canvas frameRectangle: r borderWidth: bw color: Color black.
		canvas frameRectangle: (r insetBy: bw) borderWidth: bw color: Color white.
		canvas clipBy: aHandMorph morphBoundsInWorld during: [ :c | aHandMorph drawOn: c ]]
	ifFalse: [
		 canvas fullDraw: aHandMorph]! !

!methodRemoval: InnerTextMorph #selectionRects!
InnerTextMorph removeSelector: #selectionRects!
