'From Cuis 5.0 [latest update: #4213] on 11 June 2020 at 10:10:34 am'!

!FontFamily class methodsFor: 'defaults' stamp: 'jmv 6/11/2020 09:58:10'!
defaultLineSpacing
	^FontFamily defaultFamilyAndPointSize lineSpacing! !


!Object methodsFor: 'private' stamp: 'jmv 6/11/2020 10:01:05'!
primitiveError: aString 
	"This method is called when the error handling results in a recursion in 
	calling on error: or halt or halt:."

	| context emergencyEvaluator lines r |
	r _ `10@10` extent: (Display extent -20 min: `700@1000`).
	lines _ r height // FontFamily defaultLineSpacing.
	emergencyEvaluator _ Transcripter newInFrame: r.
	emergencyEvaluator
		nextPutAll: '***System error handling failed***'; newLine;
		nextPutAll: aString; newLine;
		nextPutAll: '-------------------------------'; newLine.
	context _ thisContext sender sender.
	(30 min: lines - 10) timesRepeat: [context ifNotNil: [emergencyEvaluator print: (context _ context sender); newLine]].
	emergencyEvaluator
		nextPutAll: '-------------------------------'; newLine;
		nextPutAll: 'Type ''revert'' to revert your last method change.'; newLine;
		nextPutAll: 'Type ''exit'' to exit the emergency evaluator.'; newLine.
	emergencyEvaluator readEvalPrint! !


!Text methodsFor: 'emphasis' stamp: 'jmv 6/11/2020 10:07:59'!
fontIfApplying: textAttributes default: defaultFont
	"Answer the font for characters as specified by the argument."

	| fn ps |
	self withAttributeValues: textAttributes do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor |
		fn _ familyName ifNil: [ defaultFont ifNotNil: [ defaultFont familyName ] ifNil: [ FontFamily defaultFamilyName ]].
		ps _ pointSize ifNil: [ defaultFont ifNotNil: [ defaultFont pointSize ] ifNil: [ FontFamily defaultPointSize ]].
		^((FontFamily familyName: fn pointSize: ps) ifNil: [
			FontFamily familyName: fn aroundPointSize: ps])
			emphasized: emphasis ]! !


!Transcripter methodsFor: 'accessing' stamp: 'jmv 6/11/2020 10:01:22'!
endEntry
	| c d cb |
	c _ self contents.
	Display extent ~= DisplayScreen actualScreenSize ifTrue: [
		"Handle case of user resizing physical window"
		DisplayScreen startUp.
		frame _ frame intersect: Display boundingBox.
		^ self clear; show: c].
	textComposition
		setModel: (TextModel withText: c asText);
		extentForComposing: frame width-8 @9999.
	textComposition composeAll.
	d _ textComposition usedHeight - frame height.
	d > 0 ifTrue: [
		"Scroll up to keep all contents visible"
		cb _ textComposition characterBlockAtPoint:
			`0@0` + (0@(d+FontFamily defaultLineSpacing)).
		self on: (c copyFrom: cb stringIndex to: c size).
		readLimit _ position _ collection size.
		^ self endEntry].
	Display fill: (frame insetBy: -2) fillColor: self black;
			fill: frame fillColor: self white.
	Display getCanvas
		textComposition: textComposition
		bounds: (`4@4` + frame topLeft extent: Display extent)
		color: `Color black`
		selectionColor: `Color blue`.
	DisplayScreen screenUpdateRequired: nil! !


!ProgressInitiationException methodsFor: 'exceptionDescription' stamp: 'jmv 6/11/2020 10:03:05'!
defaultAction

	| delta textFrame barFrame outerFrame result range width filledWidth f h textWidth textForm innerBarFrame oldFilledWidth oldFilledWidth2 prevContents |
	f _ FontFamily defaultFamilyAndPointSize.
	h _ f lineSpacing * 3//2.
	textWidth _ (f widthOfString: progressTitle) + h.
	width _ 150 max: textWidth.

	textForm _ Form extent: width@h depth: 32.
	textForm fillBlack.
	textForm fillWhite: (textForm boundingBox insetBy: 2).
	progressTitle displayOn: textForm at: (width-textWidth+h//2@4).
	barFrame _ Rectangle center: aPoint extent: width@h.
	textFrame _ `0@0` extent: width@h.
	textFrame _ textFrame
					aligned: textFrame bottomCenter
					with: barFrame topCenter + `0@2`.
	outerFrame _ barFrame merge: textFrame.
	delta _ outerFrame amountToTranslateWithin: Display boundingBox.
	barFrame _ barFrame translatedBy: delta.
	textFrame _ textFrame translatedBy: delta.
	outerFrame _ outerFrame translatedBy: delta.
	prevContents _ Form fromDisplay: outerFrame.
	range _ maxVal = minVal ifTrue: [1] ifFalse: [maxVal - minVal].  "Avoid div by 0"
	innerBarFrame _ barFrame insetBy: 2.
	textForm displayAt: textFrame topLeft.
	Display fillBlack: barFrame.
	Display fillWhite: innerBarFrame.
	oldFilledWidth _ 0.
	oldFilledWidth2 _ 0.
	result _ workBlock value:  "Supply the bar-update block for evaluation in the work block"
		[ :barVal |
		barVal
			ifNotNil: [ currentVal _ barVal ]
			ifNil: [
				currentVal _ currentVal + 1.
				currentVal >= maxVal
					ifTrue: [ currentVal _ minVal ]].
		filledWidth _ ((barFrame width-4) asFloat * ((currentVal-minVal) asFloat / range min: 1.0)) asInteger.
		filledWidth  > oldFilledWidth ifTrue: [
			textForm displayAt: textFrame topLeft.
			Display fillBlack: barFrame.
			Display fillWhite: innerBarFrame.
			Display fillGray: (barFrame topLeft + `2@2` extent: filledWidth@(h-4)).
			filledWidth -200 > oldFilledWidth2
				ifFalse: [
					"Usually just request an update, to be done asynchronously."
					DisplayScreen screenUpdateRequired: outerFrame ]
				ifTrue: [
					"Once in a while, force a real screen update (warning: really slow on MacOS if done too often)"
					Display forceToScreen: outerFrame. oldFilledWidth2 _ filledWidth ].
			oldFilledWidth _ filledWidth ]].
	prevContents displayAt: outerFrame topLeft.
	self resume: result! !


!TextEditor methodsFor: 'initialization' stamp: 'jmv 6/11/2020 10:03:22'!
initialize
	selectionStartBlocks _ #().
	selectionStopBlocks _ #().
	initialSelectionStart _ nil.
	initialSelectionStop _ nil.
	doWordSelection _ false.
	doParagraphSelection _ false.
	defaultFont _ FontFamily defaultFamilyAndPointSize! !


!TextFontFamilyAndSize methodsFor: 'accessing' stamp: 'jmv 6/11/2020 10:08:22'!
font

	^ FontFamily familyName: familyName pointSize: pointSize! !


!Preferences class methodsFor: 'fonts' stamp: 'jmv 6/11/2020 10:07:40'!
setDefaultFonts: defaultFontsSpec
		
	| font |
	defaultFontsSpec do: [ :triplet |
		font _ FontFamily familyName: triplet second pointSize: triplet third.
		font ifNil: [ font _ FontFamily defaultFamilyAndPointSize ]. 
		triplet size > 3 ifTrue: [
			font _ font emphasized: triplet fourth ].
		self
			perform: triplet first
			with: font]! !


!Transcript class methodsFor: 'displaying' stamp: 'jmv 6/11/2020 10:03:24'!
displayOn: aForm in: aRectangle
	"
	1 to: 2000 do: [ :i | i print ].
	1 to: 750 do: [ :i | i print ].
	Transcript displayOn: Display in: bounds
	"
	| font count string x y fh canvas innerR index |
	aForm fill: aRectangle fillColor: `Color white`.
	font _ FontFamily defaultFamilyAndPointSize.

	innerR _ aRectangle insetBy: self padding.
	canvas _ aForm getCanvas.
	canvas newClipRect: innerR.
	x _ innerR left.
	y _ innerR top.
	fh _ font lineSpacing.
	count _ lastIndex - firstIndex \\ self maxEntries + 1 min: innerR height // fh - 1.
	index _ lastIndex - count \\ self maxEntries + 1.
	count timesRepeat: [
		string _ entries at: index.	
		canvas drawString: string at: x@y font: font color: `Color veryDarkGray`.
		y _ y + fh.
		index _ index \\ self maxEntries + 1 ].
	string _ unfinishedEntry contents.
	canvas drawString: string at: x@y font: font color: `Color veryDarkGray`! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 6/11/2020 10:03:26'!
displayUnfinishedEntryOn: aForm

	| font count string x y fh canvas r innerR |
	innerR _ bounds insetBy: self padding.
	lastDisplayPosition < innerR right ifTrue: [
		font _ FontFamily defaultFamilyAndPointSize.
		fh _ font lineSpacing.
		count _ innerR height // fh-1.
		x _ innerR left.
		string _ unfinishedEntry contents.
		y _ ((lastIndex - firstIndex \\ self maxEntries) min: count-1) + 1 * font lineSpacing + innerR top.
		canvas _ aForm getCanvas.
		r _ innerR left: lastDisplayPosition.
		canvas newClipRect: r.
		(canvas drawString: string at: x@y font: font color: `Color veryDarkGray`) ifNotNil: [ :lastPoint |
			lastDisplayPosition _ lastPoint x.
			^r ]].
	^nil! !


!AbstractFont methodsFor: 'measuring' stamp: 'jmv 6/11/2020 10:02:52'!
widthOfString: aString
	aString ifNil:[^0].
	^self widthOfString: aString from: 1 to: aString size.
"
	FontFamily defaultFamilyAndPointSize widthOfString: 'zort' 
"! !


!AbstractFont class methodsFor: 'instance accessing' stamp: 'jmv 5/5/2019 11:11:03'!
default
	"
	Compatibility.
	AbstractFont default
	"
	false ifTrue: [ self deprecatedMethod ]. "Too much senders to activate warnings right now!!"
	^FontFamily defaultFamilyAndPointSize! !

!AbstractFont class methodsFor: 'instance accessing' stamp: 'jmv 6/11/2020 10:09:27'!
fromUser: priorFont
	"
	AbstractFont fromUser
	"
	"Present a menu of available fonts, and if one is chosen, return it.
	Otherwise return nil.
	Show only baseFonts i.e. FamilyName, pointSize (but do not include emphasis, such as italic or bold)"

	| fontList fontMenu active ptMenu label spec |
	fontList := FontFamily familyNames.
	fontMenu := MenuMorph new defaultTarget: self.
	fontList do: [:fontName | 
			active := priorFont familyName sameAs: fontName.
			ptMenu := MenuMorph new defaultTarget: self.
			(FontFamily pointSizesFor:fontName ) do: [ :pt | 
					(active and: [pt = priorFont pointSize]) 
						ifTrue: [label := '<on>']
						ifFalse: [label := '<off>'].
					label := label , pt printString , ' pt'.
					ptMenu 
						add: label
						target: fontMenu
						action: #modalSelection:
						argument: { 
								fontName.
								pt}].
			active ifTrue: [label := '<on>'] ifFalse: [label := '<off>'].
			label := label , fontName.
			fontMenu add: label subMenu: ptMenu].
	spec := fontMenu invokeModal.
	spec ifNil: [^nil].
	^FontFamily familyName: spec first pointSize: spec last! !


!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 6/11/2020 10:03:13'!
glyphAt: character put: characterForm
	"Copy characterForm over the glyph for the argument, character."
	| ascii leftX rightX widthDif newGlyphs |
	ascii _ character numericValue.
	ascii < minAscii ifTrue: [^ self error: 'Cant store characters below min ascii'].
	ascii > maxAscii ifTrue:
		[(self confirm:
'This font does not accomodate ascii values higher than ' , maxAscii printString , '.
Do you wish to extend it permanently to handle values up to ' , ascii printString)
			ifTrue: [self extendMaxAsciiTo: ascii]
			ifFalse: [^ self error: 'No change made']].
	leftX _ xTable at: ascii + 1.
	rightX _ xTable at: ascii + 2.
	widthDif _ characterForm width - (rightX - leftX).
	widthDif ~= 0 ifTrue:
		["Make new glyphs with more or less space for this char"
		newGlyphs _ Form extent: (glyphs width + widthDif) @ glyphs height depth: glyphs depth.
		newGlyphs copy: (`0@0` corner: leftX@glyphs height)
			from: `0@0` in: glyphs rule: Form over.
		newGlyphs copy: ((rightX+widthDif)@0 corner: newGlyphs width@glyphs height)
			from: rightX@0 in: glyphs rule: Form over.
		glyphs _ newGlyphs.
		"adjust further entries on xTable"
		xTable _ xTable copy.
		ascii+2 to: xTable size
			do: [:i | xTable at: i put: (xTable at: i) + widthDif]].
	glyphs copy: (leftX @ 0 extent: characterForm extent)
		from: 0@0 in: characterForm rule: Form over
"
| f |  f _ FontFamily defaultFamilyAndPointSize.
f glyphAt: $  put: (Form extent: (f widthOf: $ )+10@f lineSpacing)
"! !


!TextModelMorph methodsFor: 'geometry' stamp: 'jmv 6/11/2020 10:01:19'!
scrollDeltaHeight
	"Return the increment in pixels which this pane should be scrolled."

	^ FontFamily defaultLineSpacing! !


!CodeWindow methodsFor: 'GUI building' stamp: 'jmv 6/11/2020 10:00:39'!
defaultAnnotationPaneHeight
	"Answer the receiver's preferred default height for new annotation panes."

	^ FontFamily defaultLineSpacing * 2 + 8! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 6/11/2020 10:00:16'!
buildMorphicClassColumnWith: classList

	| column |
	
	column _ LayoutMorph newColumn.
	column
		addMorphUseAll: classList;
		addAdjusterAndMorph: self buildMorphicSwitches fixedHeight:
			(Theme current minimalWindows
				ifTrue: [FontFamily defaultLineSpacing + 4]
				ifFalse: [FontFamily defaultLineSpacing *2-4]).
		
	^column! !


!StringRequestMorph methodsFor: 'initialization' stamp: 'jmv 6/11/2020 10:01:17'!
addTextPane
	| result |
	result _ (TextModelMorph
		textProvider: self
		textGetter: #response
		textSetter: #response:
		selectionGetter: #selectionInterval) emptyTextDisplayMessage: 'Enter response'.
	self
		valueOfProperty: #emptyTextDisplayMessage
		ifPresentDo: [ :msg |
			result emptyTextDisplayMessage: msg ].
	result
		hasUnacceptedEdits: true;
		acceptOnCR: true;
		escAction: [ self cancel ].
	"If the initial response is big or has multiple lines we make the text pane bigger and with scrollbars:"
	(response size > 20 or: [response includes: Character lf])
		ifTrue: [result morphExtent: 32 @ 3 * FontFamily defaultLineSpacing]
		ifFalse:
			[result hideScrollBarsIndefinitely;
				morphExtent: 18 @ 1 * FontFamily defaultLineSpacing + (0@8).].
	self addMorphBack: result.
	^ result.! !


!StringMorph methodsFor: 'accessing' stamp: 'jmv 6/11/2020 10:03:16'!
fontToUse
	| fontToUse |
	fontToUse := font ifNil: [FontFamily defaultFamilyAndPointSize].
	^(emphasis isNil or: [emphasis = 0]) 
		ifTrue: [ fontToUse]
		ifFalse: [ fontToUse emphasized: emphasis]! !


!ProgressMorph methodsFor: 'accessing' stamp: 'jmv 6/11/2020 10:03:58'!
label: aString subLabel: otherString
	| label subLabel n |
	label _ aString.
	subLabel _ otherString.
	layoutSpec notNil ifTrue: [
		n _ label size * self morphWidth * 0.95 // (FontFamily defaultFamilyAndPointSize widthOfString: label).
		label _ label squeezedTo: n.
		n _ subLabel size * self morphWidth * 0.95  // (FontFamily defaultFamilyAndPointSize widthOfString: subLabel).
		subLabel _ subLabel squeezedTo: n ].
	self whenUIinSafeState: [
		labelMorph contents: label.
		subLabelMorph contents: subLabel.
		self updatePositionAndExtent. ]! !

!ProgressMorph methodsFor: 'initialization' stamp: 'jmv 6/11/2020 10:03:55'!
initialize
	super initialize.
	self separation: 0.
	labelMorph _ StringMorph contents: '' font: FontFamily defaultFamilyAndPointSize.
	subLabelMorph _ StringMorph contents: '' font: FontFamily defaultFamilyAndPointSize.
	progress _ ProgressBarMorph new.
	progress morphExtent: 200 @ FontFamily defaultLineSpacing.
	self addMorphFront: labelMorph.
	self addMorphFront: subLabelMorph.
	self addMorph: progress fixedHeight: FontFamily defaultLineSpacing! !


!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 6/11/2020 10:02:59'!
font: newFont
	font _ newFont ifNil: [ FontFamily defaultFamilyAndPointSize ].
	self adjustExtent! !


!InnerTextMorph methodsFor: 'geometry' stamp: 'jmv 6/11/2020 10:00:48'!
minimumExtent

	^(9@(FontFamily defaultLineSpacing+2))! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 6/11/2020 10:00:45'!
fit
	"Adjust my bounds to fit the text.
	Required after the text changes,
	or if wrapFlag is true and the user attempts to change the extent."

	| newExtent newHeight newWidth |
	newWidth _ extent x.
	"Adjust width only if we don't wrap text to own width!!"
	wrapFlag ifFalse: [
		newWidth _ self textComposition usedWidth max: 9 ].
	newHeight _ self textComposition usedHeight max: FontFamily defaultLineSpacing + 2.
	newExtent _ newWidth @ newHeight.
	extent = newExtent 
		ifTrue: [
			self redrawNeeded.
			"Too conservative: only text composition (because of changes in text or styles, etc)
			should cause invalidation.
			Try to avoid calling #fit unless needed."
		]
		ifFalse: [
			extent = newExtent ifFalse: [
				self redrawNeeded.
				extent _ newExtent.
				self someSubmorphPositionOrExtentChanged.
				owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
				self redrawNeeded ]].

	owner innerHeight: newExtent y! !


!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 6/11/2020 10:00:42'!
sizeUnit
	^FontFamily defaultLineSpacing! !


!FontPicker class methodsFor: 'prompting user' stamp: 'jmv 6/11/2020 10:05:21'!
promptUser
	"Present a menu of font families, answer selection.
	FontPicker promptUser
	"
	^self promptUserWithFamilies: FontFamily familyNames.! !

!FontPicker class methodsFor: 'prompting user' stamp: 'jmv 6/11/2020 10:05:24'!
promptUserAndSetDefault
	"Present a menu of available font families, and if one is chosen, change to it."
	self promptUserAndSetDefaultWithFamilies: FontFamily familyNames.! !


!BitBltCanvas methodsFor: 'drawing-text' stamp: 'jmv 6/11/2020 10:02:56'!
drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor
	"Answer last affected pixel position
	Answer nil if nothing was done"

	| p1 font |
	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^nil ].

	p1 _ (currentTransformation transform: aPoint rounded) rounded.
	engine colorMap: nil.
	font _ fontOrNil ifNil: [ FontFamily defaultFamilyAndPointSize ].
	^font
		onBitBltCanvasEngine: engine
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p1
		color: aColor! !


!TextComposer methodsFor: 'accessing' stamp: 'jmv 6/11/2020 10:03:18'!
defaultFont
	^editor ifNil: [ FontFamily defaultFamilyAndPointSize ] ifNotNil: [ editor defaultFont ]! !


!TextComposition methodsFor: 'access' stamp: 'jmv 6/11/2020 10:03:20'!
defaultFont
	^editor ifNil: [ FontFamily defaultFamilyAndPointSize ] ifNotNil: [ editor defaultFont ]! !


!FontFamily class reorganize!
('accessing' defaultFamilyName defaultPointSize familyNames)
('initialization' addFamily: defaultFamilyName: defaultFamilyName:defaultPointSize: defaultPointSize:)
('accessing fonts' defaultFamilyAndPointSize defaultFamilyPointSize: familyName:aroundPointSize: familyName:pointSize: familyNamed: pointSizesFor:)
('file read write' readAdditionalTrueTypeFonts)
('cached state access' releaseClassCachedState)
('defaults' defaultLineSpacing)
!

