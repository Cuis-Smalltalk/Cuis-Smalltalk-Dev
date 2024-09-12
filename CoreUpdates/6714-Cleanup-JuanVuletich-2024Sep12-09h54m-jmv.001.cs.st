'From Cuis7.1 [latest update: #6712] on 12 September 2024 at 10:07:18 am'!

!PluggableScrollPane commentStamp: 'jmv 9/12/2024 10:06:27' prior: 0!
Allows viewing just part of a larger Morph. The scroll values vary from 0.0 to 1.0.

| p |
p := PluggableScrollPane new.
p scroller: BoxMorph new.
p openInWorld.

| p |
p := PluggableScrollPane new.
p scroller: Sample01Star new.
p openInWorld.!


!MethodInCallStackToBecomeInvalid methodsFor: 'exception handling' stamp: 'jmv 9/12/2024 09:54:14'!
defaultAction
	"defaultAction aborts the become or clas reshape.
	See references to MethodInCallStackToBecomeInvalid.
	To go ahead with the become or class reshape, do something like:
	[
		BorderedBoxMorph subclass: #PluggableMorph
			instanceVariableNames: 'model menuNshortcutsKey newIvarThatBreaksRunningMethods'
			classVariableNames: ''
			poolDictionaries: ''
			category: 'Morphic-Widgets'.
	] on: MethodInCallStackToBecomeInvalid do: [ :ex | ex resume: true ]
	"
	self resume: false! !


!ClassDescription methodsFor: 'instance variables' stamp: 'jmv 9/12/2024 09:54:29'!
allInstVarNamesEverywhere
	"Answer the set of inst var names used by the receiver, all superclasses, and all subclasses"

	| aList |
	aList := OrderedCollection new.
	(self allSuperclasses , self withAllSubclasses asOrderedCollection) do:
		[:cls | aList addAll: cls instVarNames].
	^ aList asSet

	"BorderedBoxMorph allInstVarNamesEverywhere"! !


!Morph methodsFor: 'geometry services' stamp: 'jmv 9/12/2024 10:04:38'!
collides: aMorph
	"Answer whether the pixels used by morphs touch at least at one place.
	Morphs not in the BoxMorph hierarchy should answer #true to #wantsContour,
	in order to use this service."

	privateDisplayBounds ifNil: [
		^false ].

	"Quick check with display bounds"
	aMorph displayFullBounds
		ifNil: [ ^false ]
		ifNotNil: [ :r |
			(self displayFullBounds
				ifNil: [ ^ false ]
				ifNotNil: [ :myFullBounds | myFullBounds intersects: r ]) ifFalse: [ ^false ]].

	"Precise check with contour, if available"
	(self valueOfProperty: #contour) ifNotNil: [ :contour | | contourTop contourBottom |
		contourTop := self valueOfProperty: #contourY0.
		contourBottom := self valueOfProperty: #contourY1.
		^ aMorph contourIntersects: contour top: contourTop bottom: contourBottom ].

	"If contour is not available, and both displayBounds overlap, answer true, as it is the best we can know."
	^ true! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 9/12/2024 10:05:10'!
coversPixel: worldPoint
	"Answer true if pixel worldPoint is covered by us (or a submorph of ours), even if we are not visible a it
	because of some other morph above us also covers it.

	WARNING: Might answer nil when we simply don't know. It is generally advisable to call this method only for
	morphs where the answer is not nil:
		- BoxMorphs. No special care needed. (Senders in the base Cuis image are of this kind).
		- Morphs drawn by VectorCanvas (i.e. they #requiresVectorCanvas). See notes below.
	
	Note: If your morph #requiresVectorCanvas, and depends on this general implementation, remember to 
	`removeProperty: #bitMask` whenever it becomes invalid due to changes in your morphs. You may consider
	using #ownsPixel: if appropriate, that doesn't require any maintenance and is cheaper (in cpu and memory).
	See #bitMask.

	See #ownsPixel:
	See #includesPixel:"

	self visible ifTrue: [
		"Expensive way for morphs with arbitrary shape in VectorGraphics.
		The base Cuis System doesn't use this.
		Use this in applications if the expense of maintaining #bitMask is worth it."
		self mainCanvas ifNotNil: [ :mainCanvas |
			mainCanvas canDoVectorGraphics ifTrue: [
				^(self bitMask pixelValueAt: worldPoint - self displayFullBounds topLeft) = 1 ]]].

	"If we don't know."
	^ nil.! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 9/12/2024 10:05:20'!
includesPixel: worldPoint
	"Answer true if the grabbing from the pixel argument pixel with the mouse should grab us:

	- Answer true if we own the pixel, i.e. we are the last morph drawn at worldPoint.

	- Some implementations (BoxMorph) may also answer true if we cover but don't own the pixel,
	meaning that some other morph was drawn later, covering us.

	- For other morphs, not BoxMorph, (i.e. they run this implementation), if VectorGraphics is not active,
	answer true for any point that lies within our bounds. If this is the case, consider using VectorGraphics: In addition to
	many other advantages, this method will always answer a strictly correct answer.

	Therefore, senders need to be aware that the answer is meaningful only when it is known that no other morph
	covers us. A common case is to find the topmost morph at some position. In that case, iterating front to back ensures
	that if any other morph covers us, it is found first.

	Note: This method is redefined by some subclasses. This implementation is only used for morphs drawn by VectorCanvas,
	either because the main canvas is a VectorCanvas, or because it is a HybridCanvas and we answer true to #requiresVectorCanvas.
	(See other implementors)

	Note: Also see #ownsPixel: and #coversPixel:"

	"Check if #ownsPixel:, if possible."
	self mainCanvas ifNotNil: [ :mainCanvas |
		mainCanvas canDoVectorGraphics ifTrue: [
			^ (mainCanvas morphIdAt: worldPoint) = self morphId ]].

	^ false.! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 9/12/2024 10:05:36'!
isCloserThan: maxDistance toPixel: worldPoint
	"Answer true if our closest point to aPoint is less than aNumber pixels away.
	In target surface (i.e. Display) coordinates.
	Uses precise testing of the morph contour if available:
	Morphs not in the BoxMorph hierarchy should answer #true to #wantsContour in order to use this service.
	Note: Considers only the external border. Any inner pixel is considered 'inside' regardless of us being transparent there.
	Note: Cheaper than #coversAnyPixelCloserThan:to: . Doesn't use #bitMask. Doesn't require maintenance."

	| center contourTop contourBottom |
	privateDisplayBounds ifNil: [
		^false ].
	center := privateDisplayBounds center.
	"Quick checks: If not even within aNumber distance to display bounds, fail"
	(center y - worldPoint y) abs < (privateDisplayBounds height  // 2 + maxDistance) ifFalse: [
		^false ].
	(center x - worldPoint x) abs < (privateDisplayBounds width // 2 + maxDistance) ifFalse: [
		^false ].
	"Precise check with contour, if available"
	(self valueOfProperty: #contour) ifNotNil: [ :contour | | y0 y1 x0 x1 |
		contourTop := self valueOfProperty: #contourY0.
		contourBottom := self valueOfProperty: #contourY1.
		"Contour rows to consider are those within requested distance."
		y0 := worldPoint y - maxDistance max: contourTop.
		y1 := worldPoint y + maxDistance min: contourBottom.
		y0 to: y1 do: [ :y |
			x0 := (contour at: (y - contourTop) * 2 + 1) - maxDistance.
			x1 := (contour at: (y - contourTop) * 2 + 2) + maxDistance.
			"If a vertical line of 2*aNumber height centered on aPoint is inside the contour, quick exit"
			(worldPoint x between: x0 and: x1) ifTrue: [ ^true ].
			"Check if aPoint is close enough to contour"
			(x0@y - worldPoint) r < maxDistance ifTrue: [ ^true ].
			(x1@y - worldPoint) r < maxDistance ifTrue: [ ^true ]].
		"Not inside, not close enough to contour"
		^ false ].
	"If contour is not available, and aPoint is close enough to displayBounds, answer true, as it is the best we can know."
	^ true! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 9/12/2024 10:06:07'!
wantsContour
	"True if a morph requires the Canvas to update a #morphContour property on us.
	Any subclass might answer true, if it #requiresVectorCanvas.
	But if #requiresVectorCanvas is false, #contour can not be computed.
	This is not a problem, as #contour is useless for BoxMorph.
	
	Also see #bitMask"

	"^self requiresVectorCanvas and: [ self isOwnedByWorldOrHand ]"
	^false! !

!Morph methodsFor: 'stepping' stamp: 'jmv 9/12/2024 10:05:43'!
stepAt: millisecondSinceLast
	"Do some periodic activity. Use startStepping/stopStepping to start and stop getting sent this message. 
	The desired time between steps is specified by this morph's answer to the stepTime message.
	The millisecondSinceLast parameter gives the time elapsed since the previous step."
	"
	m := BoxMorph new.
	m color: Color random.
	m openInWorld.
	m morphPosition: 10@10.
	t := 0.
	m when: #morphicStep evaluate: [ :delta |
		t := t + delta.
		t < 10000
			ifTrue: [
				(m owner is: #HandMorph) ifFalse: [
					m morphPosition: 3@2 * t // 100 ]]
			ifFalse: [ m stopStepping ]].
	m startSteppingStepTime: 20.
	"
	self step.
	self triggerEvent: #morphicStep with: millisecondSinceLast! !


!PlacedMorph methodsFor: 'geometry' stamp: 'jmv 9/12/2024 10:06:20'!
fitInto: aRectangle
	"Scale and position ourselves in such way that our #findFullBoundsInOwner are in aRectangle.
	BoxMorph, and maybe others will not scale, but resize, changing the actual extent."

	| currentBounds scale slackForAntiAliasing |
	currentBounds := self findFullBoundsInOwner.
	slackForAntiAliasing := 0.
	scale := (aRectangle width-slackForAntiAliasing / (currentBounds width-slackForAntiAliasing))
		min: (aRectangle height-slackForAntiAliasing / (currentBounds height-slackForAntiAliasing)).
	scale := scale asFloat.
	self scaleBy: scale.

	currentBounds := self findFullBoundsInOwner.
	self morphPosition: aRectangle origin - (currentBounds origin - self morphPosition) + (aRectangle extent - currentBounds extent // 2).! !

