'From Cuis 4.0 of 21 April 2012 [latest update: #1439] on 9 September 2012 at 11:42:09 pm'!

!FormCanvas methodsFor: 'drawing-images' stamp: 'jmv 9/9/2012 23:19'!
zzstencil: stencilForm at: aPoint color: aColor
	"Flood this canvas with aColor wherever stencilForm has non-zero pixels"
	^self zzstencil: stencilForm
		at: aPoint
		sourceRect: stencilForm boundingBox
		color: aColor! !

!FormCanvas methodsFor: 'drawing-images' stamp: 'jmv 9/9/2012 23:20'!
zzstencil: stencilForm at: aPoint sourceRect: sourceRect color: aColor
	"Flood this canvas with aColor wherever stencilForm has non-zero pixels"
	| p |
	p _ currentTransformation transform: aPoint.
	self setPaintColor: aColor.
	port colorMap: stencilForm maskingMap.
	port stencil: stencilForm
		at: p + origin
		sourceRect: sourceRect.! !

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 9/9/2012 23:02'!
zzdrawString: aString from: firstIndex to: lastIndex at: aPoint font: font color: c kern: kern

	| p1 |
	p1 _ currentTransformation transform: aPoint.
	port colorMap: nil.
	"Slight optimization when there's nothing to do."
	port clipHeight = 0 ifTrue: [^self].
	port clipWidth = 0 ifTrue: [^self].
	port installStrikeFont: font foregroundColor: (shadowColor ifNil: [ c ]).
	port
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: (origin + p1)
		strikeFont: font
		kern: kern! !

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 9/9/2012 22:50'!
zzparagraph: aParagraph bounds: boundsRect color: c selectionColor: sc
	| displayScanner leftInRun line boundsInWorld tl |

	tl _ boundsRect topLeft.
	boundsInWorld _ currentTransformation displayBoundsOfTransformOf: boundsRect.
	self setPaintColor: c.

	displayScanner _ MorphicScanner new 
		text: aParagraph paragraphText
		foreground: (shadowColor ifNil: [ c ])
		ignoreColorChanges: self isShadowDrawing.
	displayScanner canvas: self.

	leftInRun _ 0.
	"Take clipRect into account. Extrememly fast scrolls and redraws of huge files (like .sources)"
	(aParagraph lineIndexForPoint: (0@0 max: clipRect origin- boundsInWorld origin))
		to: (aParagraph lineIndexForPoint: (boundsInWorld extent min: clipRect corner - boundsInWorld origin))
		do: [ :i |
			line _ aParagraph lines at: i.
			aParagraph
				zzdisplaySelectionInLine: line
				on: self
				paragraphTopLeft: tl
				selectionColor: sc.
			leftInRun _ displayScanner zzdisplayLine: line paragraphTopLeft: tl leftInRun: leftInRun  ]! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:37'!
zzshadowForm
	"Return a 1-bit shadow of my submorphs.  Assumes submorphs is not empty"
	| bnds canvas |
	bnds _ Rectangle merging: (submorphs collect: [:m | m morphFullBoundsInWorld]).
	canvas _ Display defaultCanvasClass forShadowOver: bnds.
	canvas into: self.
	self drawSubmorphsOn: canvas.
	^ canvas form offset: bnds topLeft - self morphPositionInWorld! !


!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 9/9/2012 23:10'!
zzdisplayBulletIfAppropriateFor: textLine paragraphTopLeft: paragraphTopLeft
	"paragraphTopLeft is relative to the morph currently being drawn"

	| paragraphEnd count pattern |
	paragraphStyle ifNotNil: [
		(textLine isFirstLine and: [ paragraphStyle isListStyle ]) ifTrue: [
			pattern _ paragraphStyle listBulletPattern.
			"Count how many paragraphs before this one already used the pattern"
			count _ 0.
			paragraphEnd _ textLine first-1.
			[
			paragraphEnd > 0 and: [ ((text paragraphStyleOrNilAt: paragraphEnd) ifNotNil: [ :ps | ps listBulletPattern ]) = pattern ]] whileTrue: [
				count _ count + 1.
				paragraphEnd _ text string endOfParagraphBefore: paragraphEnd ].
			"Our number in the list, is one more than the count of previous contiguous paragraphs with this pattern"
			self
				zzdisplayBulletParagraphTopLeft: paragraphTopLeft
				number: count + 1]]! !

!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 9/9/2012 23:10'!
zzdisplayBulletParagraphTopLeft: paragraphTopLeft number: bulletNumber
	"paragraphTopLeft is relative to the morph currently being drawn"

	| pattern i c j s bullet bulletPos bulletSize prefix |
	pattern _ paragraphStyle listBulletPattern.
	bullet _ pattern.
	(i _ pattern indexOf: $%) > 0
		ifTrue: [ bullet _ bulletNumber asString]
		ifFalse: [
			(i _ pattern indexOf: $z) > 0
				ifTrue: [ bullet _ (Character value: 96 + bulletNumber) asString ]
				ifFalse: [
					(i _ pattern indexOf: $Z) > 0
						ifTrue: [ bullet _ (Character value: 64 + bulletNumber) asString ]]].
	prefix _ 0.
	i > 0 ifTrue: [
		c _ pattern at: i.
		j _ i.
		s _ pattern size.
		[ j <= s and: [ (pattern at: j) = c ] ] whileTrue: [ j _ j + 1 ].
		j _ j - 1.
		bulletSize _ j-i+1.
		prefix _ bulletSize - bullet size max: 0.
		bullet size > bulletSize ifTrue: [
			bullet _ bullet copyFrom: bullet size - bulletSize + 1 to: bullet size ].
		bullet _ (pattern copyFrom: 1 to: i-1), bullet, (pattern copyFrom: j+1 to: pattern size) ].
	bulletPos _ paragraphStyle firstIndent + paragraphTopLeft x + ((font widthOf: $9) * prefix)@destY.
	canvas
		zzdrawString: bullet
		from: 1
		to: bullet size
		at: bulletPos
		font: font
		color: foregroundColor
		kern: kern! !

!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 9/9/2012 23:14'!
zzdisplayLine: textLine paragraphTopLeft: paragraphTopLeft leftInRun: leftInRun
	"The call on the primitive (scanCharactersFrom:to:in:rightX:) will be interrupted according to an array of stop conditions passed to the scanner at which time the code to handle the stop condition is run and the call on the primitive continued until a stop condition returns true (which means the line has terminated).  leftInRun is the # of characters left to scan in the current run; when 0, it is time to call setStopConditions."

	"paragraphTopLeft is relative to the morph currently being drawn"
	| done stopCondition nowLeftInRun startIndex string lastPos priorFont |

	paraTopLeft _ paragraphTopLeft.
	line _ textLine.
	lineY _ line top + paragraphTopLeft y.
	lineHeight _ line lineHeight.
	rightMargin _ line rightMargin + paragraphTopLeft x.
	lastIndex _ line first.
	leftInRun <= 0 ifTrue: [self setStopConditions].
	leftMargin _ (line leftMarginForAlignment: alignment) + paragraphTopLeft x.
	destX _ runX _ leftMargin.
	destY _ lineY + line baseline - font ascent.

	textLine isEmptyLine ifTrue: [
		textLine paragraphStyle ifNotNil: [ :ps |
			ps = paragraphStyle ifFalse: [
				""
				foregroundColor _ paragraphColor.
				priorFont _ font.
				self setActualFont: ps font.
				ps color ifNotNil: [ :color | self textColor: color ].
				alignment _ ps alignment.
				paragraphStyle _ ps.
				priorFont ifNotNil: [ destX _ destX + priorFont descentKern ].
				destX _ destX - font descentKern.
				kern _ 0 - font baseKern.
				spaceWidth _ font widthOf: Character space.
				xTable _ font xTable.
				map _ font characterToGlyphMap.
				stopConditions _ DefaultStopConditions.
				text ifNotNil: [ destY _ lineY + line baseline - font ascent ]
				""
			]
		].
		self zzdisplayBulletIfAppropriateFor: textLine paragraphTopLeft: paragraphTopLeft.
		^leftInRun ].

	self zzdisplayBulletIfAppropriateFor: textLine paragraphTopLeft: paragraphTopLeft.

	lastIndex _ line first.
	leftInRun <= 0
		ifTrue: [nowLeftInRun _ text runLengthFor: lastIndex]
		ifFalse: [nowLeftInRun _ leftInRun].
	runStopIndex _ lastIndex + (nowLeftInRun - 1) min: line last.
	spaceCount _ 0.
	done _ false.
	string _ text string.

	self placeEmbeddedObject.
	[ done ] whileFalse: [
		startIndex _ lastIndex.
		lastPos _ destX@destY.
		stopCondition _ self
			scanCharactersFrom: lastIndex to: runStopIndex
			in: string rightX: rightMargin stopConditions: stopConditions
			kern: kern.
		lastIndex >= startIndex ifTrue: [
			canvas  
				zzdrawString: string
				from: startIndex
				to: lastIndex
				at: lastPos
				font: font
				color: foregroundColor
				kern: kern ].
		"see setStopConditions for stopping conditions for displaying."
		done _ self perform: stopCondition ].
	^ runStopIndex - lastIndex   "Number of characters remaining in the current run"! !


!Paragraph methodsFor: 'display' stamp: 'jmv 9/9/2012 22:54'!
zzdisplayInsertionMarkAtX: x top: top bottom: bottom emphasis: emphasis on: aCanvas paragraphLeft: paragraphLeft
	"x, top, bottom, paragraphLeft are relative to the morph currently being drawn."

	| caretColor x1 isBold isItalic x0 h w halfW r d |
	isBold _ emphasis allMask: 1.
	isItalic _ emphasis allMask: 2.
	caretColor _ Theme current insertionPoint.
	h _ bottom - top.
	w _ isBold
		ifTrue: [ h // 25 + 2 ]
		ifFalse: [ h // 30 + 1 ].
	halfW _ w // 2.
	isItalic
		ifTrue: [	
			"Keep tweaking if needed!! For italics with descenders (i.e. p), cursor shows a bit to the left..."
			d _ isBold ifTrue: [ h // 8 ] ifFalse: [ h // 9].
			x0 _ x- (h*5//24) + d.
			x1 _ x + d ]
		ifFalse: [
			x0 _ x.
			x1 _ x].
	x0-paragraphLeft < halfW ifTrue: [
		x1 _ x1 - x0 + halfW+paragraphLeft.
		x0 _ halfW+paragraphLeft ].
	r _ extentForComposing x-halfW-1.
	r < (x1-paragraphLeft) ifTrue: [
		x0 _ x0 + r - x1+paragraphLeft.
		x1 _ r +paragraphLeft].

	lastCaretRect _ x0-halfW@ top corner: x1+halfW+1 @ (bottom+1).
	aCanvas
		zzline: x0@(bottom-halfW) to: x1@(top+halfW)
		width: w color: caretColor! !

!Paragraph methodsFor: 'display' stamp: 'jmv 9/9/2012 22:51'!
zzdisplaySelectionInLine: line on: aCanvas paragraphTopLeft: paragraphTopLeft  selectionColor: sc

	"paragraphTopLeft is relative to the morph currently being drawn"
	selectionStartBlocks with: selectionStopBlocks do: [ :startBlock :stopBlock |
		self
			zzdisplaySelectionStartBlock: startBlock
			stopBlock: stopBlock
			InLine: line
			on: aCanvas
			paragraphTopLeft: paragraphTopLeft
			selectionColor: sc ]! !

!Paragraph methodsFor: 'display' stamp: 'jmv 9/9/2012 22:53'!
zzdisplaySelectionStartBlock: startBlock stopBlock: stopBlock InLine: line on: aCanvas paragraphTopLeft: paragraphTopLeft  selectionColor: sc
	"paragraphTopLeft is relative to the morph currently being drawn"

	| leftX rightX idx caretFont t b caretAttributes |

	startBlock ifNil: [^self].	"No selection"
	startBlock = stopBlock 
		ifTrue: [
			"Only show caret on line where clicked"
			startBlock textLine first = line first ifFalse: [
				^self ].
			leftX _ paragraphTopLeft x + startBlock left.
			idx _ startBlock stringIndex.
			caretAttributes _ editor ifNotNil: [ editor currentAttributes ].
			caretFont _ caretAttributes
				ifNil: [ model actualContents fontAt: idx ]
				ifNotNil: [ model actualContents fontIfApplying: caretAttributes ].
			b _ paragraphTopLeft y + line top + line baseline + caretFont descent-1.
			t _ paragraphTopLeft y + line top + line baseline - caretFont ascent.
			showCaret ifTrue: [
				self
					zzdisplayInsertionMarkAtX: leftX
					top: t
					bottom: b
					emphasis: caretFont emphasis
					on: aCanvas
					paragraphLeft: paragraphTopLeft x ]]
		ifFalse: [
			"Test entire selection before or after here"
			(stopBlock stringIndex < line first 
				or: [startBlock stringIndex > (line last + 1)])
					ifTrue: [^self].	"No selection on this line"
			(stopBlock stringIndex = line first 
				and: [stopBlock textLine ~= line])
					ifTrue: [^self].	"Selection ends on line above"
			(startBlock stringIndex = (line last + 1) 
				and: [stopBlock textLine ~= line])
					ifTrue: [^self].
			lastCaretRect _ nil.
			leftX _  paragraphTopLeft x + (startBlock stringIndex < line first 
				ifTrue: [ line ]
				ifFalse: [ startBlock ]) left.
			rightX _  paragraphTopLeft x + ((stopBlock stringIndex > (line last + 1) or: [
					stopBlock stringIndex = (line last + 1) 
						and: [stopBlock textLine ~= line]]) 
				ifTrue: [line right]
				ifFalse: [stopBlock left]).
			aCanvas
				zzfillRectangle: (leftX @ (line top +  paragraphTopLeft y) corner: rightX @ (line bottom +  paragraphTopLeft y))
				color: sc ].	"Selection begins on line below"! !


!FormCanvas methodsFor: 'drawing-images' stamp: 'jmv 9/9/2012 23:21'!
zzimage: aForm at: aPoint sourceRect: sourceRect
	"Draw a translucent image using the best available way of representing translucency.
	Note: This will be fixed in the future."
	| r p |
	p _ currentTransformation transform: aPoint.
	self isShadowDrawing ifTrue: [
		^self zzstencil: aForm at: aPoint sourceRect: sourceRect color: shadowColor ].
	r _ (self depth < 32 or: [ aForm mightBeTranslucent not]) 
		ifTrue: [
			"Rule Form paint treats pixels with a value of zero as transparent"
			Form paint]
		ifFalse: [ Form blend ].
	self image: aForm
		at: p rounded
		sourceRect: sourceRect
		rule: r! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:21'!
drawOn: aCanvas 
	"Draw the hand itself (i.e., the cursor)."
	"This method is only called when we are carrying morphs around..."
	 aCanvas
		zzstencil: Cursor move
		at: 0@0
		color: Color black! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:38'!
nonCachingFullDrawOn: aCanvas
	| shadowForm |
	"A HandMorph has unusual drawing requirements:
		1. the hand itself (i.e., the cursor) appears in front of its submorphs
		2. morphs being held by the hand cast a shadow on the world/morphs below
	The illusion is that the hand plucks up morphs and carries them above the world."
	"Note: This version does not cache an image of the morphs being held by the hand.
	 Thus, it is slower for complex morphs, but consumes less space."

	submorphs isEmpty ifTrue: [^ self drawOn: aCanvas].  "just draw the hand itself"

	"Note: We use a shadow form here to prevent drawing
	overlapping morphs multiple times using the transparent
	shadow color."
	shadowForm _ self zzshadowForm.

	"draw shadows"
	aCanvas zzstencil: shadowForm at: shadowForm offset  + self shadowOffset color: (Color black alpha: 0.5).
	
	"draw morphs in front of shadows"
	self drawSubmorphsOn: aCanvas.
	self drawOn: aCanvas.  "draw the hand itself in front of morphs"! !


!HoverHelpMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:04'!
drawOn: aCanvas

	| r |
	r _ 0@0 extent: extent.
	aCanvas zzroundRect: r color: self color radius: 4.
	aCanvas
		zzparagraph: paragraph
		bounds: (r insetBy: 4)
		color: Color black
		selectionColor: (Theme current textHighlightFocused: false)! !


!InnerTextMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:04'!
drawOn: aCanvas
	"Draw the receiver on a canvas"

	false ifTrue: [ self debugDrawLineRectsOn: aCanvas ].  "show line rects for debugging"

	aCanvas
		zzparagraph: self paragraph
		bounds: (0@0 extent: extent)
		color: color
		selectionColor: (Theme current textHighlightFocused: self hasKeyboardFocus)! !

!InnerTextMorph methodsFor: 'blinking cursor' stamp: 'jmv 9/9/2012 23:08'!
onBlinkCursor
	"Blink the cursor"
	paragraph ifNil: [ ^nil ].
	paragraph showCaret: paragraph showCaret not | pauseBlinking.
	pauseBlinking _ false.
	paragraph lastCaretRect ifNotNil: [ :r | self zzinvalidRect: r].! !


!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 9/9/2012 23:13'!
placeEmbeddedObject: anchoredFormOrMorph

	"This method should be redone calling reasonable protocol on the canvas"
	self flag: #jmvVer2.

	(super placeEmbeddedObject: anchoredFormOrMorph) ifFalse: [^ false].
	(anchoredFormOrMorph is: #Morph)
		ifTrue: [
			anchoredFormOrMorph morphPosition:
				((destX - anchoredFormOrMorph morphWidth)@
				(lineY+ line baseline - anchoredFormOrMorph morphHeight)) -
					paraTopLeft ]
		ifFalse: [
			destY _ lineY.
			runX _ destX.
			anchoredFormOrMorph 
				displayOn: canvas grafPort destForm 
				at: destX - anchoredFormOrMorph width @ (destY + line baseline - anchoredFormOrMorph height)
				clippingBox: canvas grafPort clipRect
				rule: Form blend
				fillColor: nil ].
	^ true! !


!String methodsFor: 'displaying' stamp: 'jmv 9/9/2012 23:16'!
displayOn: aDisplayMedium at: aPoint textColor: aColor
	"Show a representation of the receiver as a DisplayText at location aPoint on aDisplayMedium, rendering the text in the designated color"

	aDisplayMedium getCanvas zzdrawString: self at: aPoint font: nil color: aColor! !


!Transcript class methodsFor: 'displaying' stamp: 'jmv 9/9/2012 23:16'!
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
	canvas setOrigin: 0@0 clipRect: innerRectangle.
	[
		string _ entries at: i.	
		canvas zzdrawString: string at: x@y font: font color: Color veryDarkGray.
		y _ y + fh.
		i = lastIndex
	] whileFalse: [ i _ i \\ self maxEntries + 1 ].

	string _ unfinishedEntry contents.
	canvas zzdrawString: string at: x@y font: font color: Color veryDarkGray! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 9/9/2012 23:16'!
displayUnfinishedEntryOn: aForm

	| font count string x y fh canvas |
	font _ StrikeFont default.
	
	fh _ font height.
	count _ innerRectangle height // fh-1.
	x _ innerRectangle left.

	string _ unfinishedEntry contents.
	y _ ((lastIndex - firstIndex \\ self maxEntries) min: count-1) + 1 * font height + innerRectangle top.
	canvas _ aForm getCanvas.
	canvas setOrigin: 0@0 clipRect: innerRectangle.
	canvas zzdrawString: string at: x@y font: font color: Color veryDarkGray! !


!Transcripter methodsFor: 'accessing' stamp: 'jmv 9/9/2012 23:04'!
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
		zzparagraph: para
		bounds: (4@4 + frame topLeft extent: Display extent)
		color: Color black
		selectionColor: Color blue! !

!methodRemoval: Paragraph #displayInsertionMarkAtX:top:bottom:emphasis:on:paragraphLeft:!
Paragraph removeSelector: #displayInsertionMarkAtX:top:bottom:emphasis:on:paragraphLeft:!
!methodRemoval: Paragraph #displaySelectionInLine:on:paragraphTopLeft:selectionColor:!
Paragraph removeSelector: #displaySelectionInLine:on:paragraphTopLeft:selectionColor:!
!methodRemoval: Paragraph #displaySelectionStartBlock:stopBlock:InLine:on:paragraphTopLeft:selectionColor:!
Paragraph removeSelector: #displaySelectionStartBlock:stopBlock:InLine:on:paragraphTopLeft:selectionColor:!
!methodRemoval: MorphicScanner #displayBulletIfAppropriateFor:paragraphTopLeft:!
MorphicScanner removeSelector: #displayBulletIfAppropriateFor:paragraphTopLeft:!
!methodRemoval: MorphicScanner #displayBulletParagraphTopLeft:number:!
MorphicScanner removeSelector: #displayBulletParagraphTopLeft:number:!
!methodRemoval: MorphicScanner #displayLine:paragraphTopLeft:leftInRun:!
MorphicScanner removeSelector: #displayLine:paragraphTopLeft:leftInRun:!
!methodRemoval: HandMorph #shadowForm!
HandMorph removeSelector: #shadowForm!
!methodRemoval: Morph #shadowForm!
Morph removeSelector: #shadowForm!
!methodRemoval: Morph #zzshadowForm!
Morph removeSelector: #zzshadowForm!
!methodRemoval: FormCanvas #drawString:at:font:color:!
FormCanvas removeSelector: #drawString:at:font:color:!
!methodRemoval: FormCanvas #drawString:from:to:at:font:color:!
FormCanvas removeSelector: #drawString:from:to:at:font:color:!
!methodRemoval: FormCanvas #drawString:from:to:at:font:color:kern:!
FormCanvas removeSelector: #drawString:from:to:at:font:color:kern:!
!methodRemoval: FormCanvas #fillRectangle:color:!
FormCanvas removeSelector: #fillRectangle:color:!
!methodRemoval: FormCanvas #line:to:width:color:!
FormCanvas removeSelector: #line:to:width:color:!
!methodRemoval: FormCanvas #paragraph:bounds:color:selectionColor:!
FormCanvas removeSelector: #paragraph:bounds:color:selectionColor:!
!methodRemoval: FormCanvas #stencil:at:color:!
FormCanvas removeSelector: #stencil:at:color:!
!methodRemoval: FormCanvas #stencil:at:sourceRect:color:!
FormCanvas removeSelector: #stencil:at:sourceRect:color:!
