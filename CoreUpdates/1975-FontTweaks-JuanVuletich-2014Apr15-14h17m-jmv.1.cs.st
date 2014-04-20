'From Cuis 4.2 of 25 July 2013 [latest update: #1972] on 15 April 2014 at 2:19:27.320527 pm'!

!AbstractFont class methodsFor: 'instance accessing' stamp: 'jmv 4/15/2014 10:51'!
fromUser
	"
	 AbstractFont fromUser
	"
	^self fromUser: self default! !

!AbstractFont class methodsFor: 'instance accessing' stamp: 'jmv 4/15/2014 10:50'!
fromUser: priorFont
	"
	AbstractFont fromUser
	"
	"Present a menu of available fonts, and if one is chosen, return it.
	Otherwise return nil.
	Show only baseFonts i.e. FamilyName, pointSize (but do not include emphasis, such as italic or bold)"

	| fontList fontMenu active ptMenu label spec |
	fontList := AbstractFont familyNames.
	fontMenu := MenuMorph new defaultTarget: self.
	fontList do: [:fontName | 
			active := priorFont familyName sameAs: fontName.
			ptMenu := MenuMorph new defaultTarget: self.
			(AbstractFont pointSizesFor:fontName ) do: [ :pt | 
					(active and: [pt = priorFont pointSize]) 
						ifTrue: [label := '<on>']
						ifFalse: [label := '<off>'].
					label := label , pt printString , ' pt'.
					ptMenu 
						add: label
						target: fontMenu
						selector: #modalSelection:
						argument: { 
								fontName.
								pt}].
			active ifTrue: [label := '<on>'] ifFalse: [label := '<off>'].
			label := label , fontName.
			fontMenu add: label subMenu: ptMenu].
	spec := fontMenu invokeModal.
	spec ifNil: [^nil].
	^AbstractFont familyName: spec first pointSize: spec last! !


!Object methodsFor: 'private' stamp: 'jmv 4/15/2014 09:23'!
primitiveError: aString 
	"This method is called when the error handling results in a recursion in 
	calling on error: or halt or halt:."

	| context emergencyEvaluator lines r |
	r _ 10@10 extent: (Display extent -20 min: 700@1000).
	lines _ r height // AbstractFont default height.
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


!TextEditor methodsFor: 'attributes' stamp: 'jmv 4/15/2014 09:25'!
offerFontMenu
	"Present a menu of available fonts, and if one is chosen, apply it to the current selection.
	Note: use #baseFont. If emphasis is desired, add it separatedly."
	"This is a user command, and generates undo"

	| curFont newFont attr startIndex |
	startIndex _ self startIndex.
	curFont _ model actualContents fontAt: startIndex.
	newFont _ AbstractFont fromUser: curFont.
	newFont ifNil: [ ^self ].
	attr _ TextFontFamilyAndSize
			familyName: newFont familyName
			pointSize: newFont pointSize.
	self applyAttribute: attr.
	morph updateFromTextComposition.! !


!ProgressInitiationException methodsFor: 'as yet unclassified' stamp: 'jmv 4/15/2014 09:23'!
defaultAction

	| delta textFrame barFrame outerFrame result range width w1 f h textWidth textForm innerBarFrame |
	f _ AbstractFont default.
	h _ f height * 3//2.
	textWidth _ (f widthOfString: progressTitle) + h.
	width _ 150 max: textWidth.
	
	textForm _ Form extent: width@h depth: 32.
	textForm fillBlack.
	textForm fillWhite: (textForm boundingBox insetBy: 2).
	progressTitle displayOn: textForm at: (width-textWidth+h//2@4).
			
	barFrame _ Rectangle center: aPoint extent: width@h.
	textFrame _ 0@0 extent: width@h.
	textFrame _ textFrame
					aligned: textFrame bottomCenter
					with: barFrame topCenter + (0@2).
	outerFrame _ barFrame merge: textFrame.
	delta _ outerFrame amountToTranslateWithin: Display boundingBox.
	barFrame _ barFrame translatedBy: delta.
	textFrame _ textFrame translatedBy: delta.
	outerFrame _ outerFrame translatedBy: delta.
	range _ maxVal = minVal ifTrue: [1] ifFalse: [maxVal - minVal].  "Avoid div by 0"
	innerBarFrame _ barFrame insetBy: 2.
	result _ workBlock value:  "Supply the bar-update block for evaluation in the work block"
		[ :barVal |
		barVal 
			ifNotNil: [ currentVal _ barVal ]
			ifNil: [		
				currentVal _ currentVal + 1.
				currentVal >= maxVal
					ifTrue: [ currentVal _ minVal ]].
		w1 _ ((barFrame width-4) asFloat * ((currentVal-minVal) asFloat / range min: 1.0)) asInteger.
		textForm displayAt: textFrame topLeft.
		Display fillBlack: barFrame.
		Display fillWhite: innerBarFrame.
		Display fillGray: (barFrame topLeft + (2@2) extent: w1@17) ].
	"Not pretty at all!!"
	self runningWorld ifNotNil: [ :w | w fullRepaintNeeded ].
	self resume: result! !


!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 4/15/2014 10:35'!
drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor kern: kernOrNil

	| p1 font kern insideColor |
	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^self ].
	p1 _ currentTransformation intTransform: aPoint.
	port colorMap: nil.
	font _ fontOrNil ifNil: [ AbstractFont default ].
	kern _ kernOrNil ifNil: [ font baseKern ].
	insideColor _ shadowColor ifNil: [ aColor ].
	port
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p1
		strikeFont: font
		color: insideColor
		kern: kern! !


!TextModelMorph methodsFor: 'geometry' stamp: 'jmv 4/15/2014 09:25'!
scrollDeltaHeight
	"Return the increment in pixels which this pane should be scrolled."

	^ AbstractFont default height! !


!SystemWindow methodsFor: 'geometry' stamp: 'jmv 4/15/2014 09:25'!
minPaneHeightForReframe
	^AbstractFont default height + 10! !


!CodeWindow methodsFor: 'GUI building' stamp: 'jmv 4/15/2014 09:23'!
defaultAnnotationPaneHeight
	"Answer the receiver's preferred default height for new annotation panes."

	^ AbstractFont default height * 2 + 8! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 4/15/2014 09:23'!
buildMorphicClassColumn
	| column |
	column _ LayoutMorph newColumn.
	column
		addMorphUseAll: self buildMorphicClassList;
		addAdjusterAndMorph: self buildMorphicSwitches fixedHeight:  AbstractFont default height *2-4.
	^column! !


!FrameRateMorph methodsFor: 'drawing' stamp: 'jmv 4/15/2014 09:22'!
drawOn: aCanvas
	super drawOn: aCanvas.
	meanStepDelta ifNotNil: [
		aCanvas drawString: lastStepDelta rounded printString at: 0@0 font: AbstractFont default color: Color black.
		aCanvas drawString: meanStepDelta rounded printString at: 0@14 font: AbstractFont default color: Color black.
		"aCanvas drawString: lastStepStamp printString at: bounds topLeft + (0@28) font: AbstractFont default color: Color black "
		]! !


!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 4/15/2014 09:22'!
font: newFont
	font _ newFont ifNil: [ AbstractFont default ].
	self adjustExtent! !


!InnerTextMorph methodsFor: 'geometry' stamp: 'jmv 4/15/2014 09:22'!
minimumExtent

	^(9@(AbstractFont default height+2))! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 4/15/2014 09:22'!
fit
	"Adjust my bounds to fit the text.
	Required after the text changes,
	or if wrapFlag is true and the user attempts to change the extent."

	| newExtent |
	newExtent _ (self textComposition extent max: 9 @ AbstractFont default height) + (0 @ 2).
	extent = newExtent ifFalse: [
		self basicExtent: newExtent ].

	self redrawNeeded.	"Too conservative: only text composition
							should cause invalidation."
	owner innerHeight: newExtent y! !


!LayoutMorph methodsFor: 'private' stamp: 'jmv 4/15/2014 09:22'!
minPaneHeightForReframe
	^AbstractFont default height + 10! !


!MinimalStringMorph methodsFor: 'accessing' stamp: 'jmv 4/15/2014 09:22'!
fontToUse
	| fontToUse |
	fontToUse := font ifNil: [AbstractFont default].
	^(emphasis isNil or: [emphasis = 0]) 
		ifTrue: [fontToUse]
		ifFalse: [fontToUse emphasized: emphasis]! !


!OneLineEditorMorph methodsFor: 'accessing' stamp: 'jmv 4/15/2014 09:23'!
baseFont

	font ifNil: [ font _ AbstractFont default ].
	^font! !


!StringMorph methodsFor: 'accessing' stamp: 'jmv 4/15/2014 09:25'!
fontToUse
	| fontToUse |
	fontToUse := font ifNil: [AbstractFont default].
	^(emphasis isNil or: [emphasis = 0]) 
		ifTrue: [ fontToUse]
		ifFalse: [ fontToUse emphasized: emphasis]! !

!StringMorph methodsFor: 'menu' stamp: 'jmv 4/15/2014 09:25'!
changeFont
	| newFont |
	newFont _ AbstractFont fromUser: self fontToUse.
	newFont ifNotNil:[self font: newFont].! !


!UpdatingStringMorph class methodsFor: 'new-morph participation' stamp: 'jmv 4/15/2014 09:26'!
initializedInstance
	"Answer a digital clock"

	| newInst |
	newInst := self 
		contents: '' 
		font: (AbstractFont familyName: 'DejaVu' pointSize: 22) 
		emphasis: AbstractFont  boldCode.
	newInst 
		stepTime: 500; "half a second"
		target: [String streamContents: [ :strm | DateAndTime now printHMSOn: strm]] ;
		getSelector: #value.
		
	^ newInst! !


!Preferences class methodsFor: 'fonts' stamp: 'jmv 4/15/2014 09:23'!
setSystemFontTo: aFont
	"Establish the default text font and style"

	aFont ifNil: [^ self].
	AbstractFont default: aFont! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 4/15/2014 09:23'!
standardCodeFont
	"Answer the font to be used in code"

	 ^ Parameters at: #standardCodeFont ifAbsent:
		[Parameters at: #standardCodeFont put: AbstractFont default]! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 4/15/2014 09:23'!
standardListFont
	"Answer the font to be used in lists"

	 ^ Parameters at: #standardListFont ifAbsent:
		[Parameters at: #standardListFont put: AbstractFont default]! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 4/15/2014 09:23'!
standardMenuFont
	"Answer the font to be used in menus"

	 ^ Parameters at: #standardMenuFont ifAbsent:
		[Parameters at: #standardMenuFont put: AbstractFont default]! !


!Transcripter methodsFor: 'accessing' stamp: 'jmv 4/15/2014 09:25'!
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
	d _ textComposition extent y - frame height.
	d > 0 ifTrue: [
		"Scroll up to keep all contents visible"
		cb _ textComposition characterBlockAtPoint:
			0@0 + (0@(d+AbstractFont default height)).
		self on: (c copyFrom: cb stringIndex to: c size).
		readLimit _ position _ collection size.
		^ self endEntry].
	Display fill: (frame insetBy: -2) fillColor: self black;
			fill: frame fillColor: self white.
	Display getCanvas
		textComposition: textComposition
		bounds: (4@4 + frame topLeft extent: Display extent)
		color: Color black
		selectionColor: Color blue! !


!Transcript class methodsFor: 'displaying' stamp: 'jmv 4/15/2014 09:25'!
displayOn: aForm
	"
	Transcript displayOn: Display
	"
	| font count i string x y fh f bw r canvas |
	bw _ self borderWidth  .
	r _ innerRectangle outsetBy: bw + self padding.
	aForm border: r width: bw. 
	aForm fill: r fillColor: Color white.
	font _ AbstractFont default.
	
	fh _ font height.
	count _ innerRectangle height // fh-1.
	x _ innerRectangle left.
	y _ innerRectangle top.
	f _ firstIndex-1.
	firstIndex > lastIndex ifTrue: [ f _ f - self maxEntries ].
	i _ (lastIndex - count max: f) \\ self maxEntries + 1.
	canvas _ aForm getCanvas.
	canvas newClipRect: innerRectangle.
	[
		string _ entries at: i.	
		canvas drawString: string at: x@y font: font color: Color veryDarkGray.
		y _ y + fh.
		i = lastIndex
	] whileFalse: [ i _ i \\ self maxEntries + 1 ].

	string _ unfinishedEntry contents.
	canvas drawString: string at: x@y font: font color: Color veryDarkGray! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 4/15/2014 09:25'!
displayUnfinishedEntryOn: aForm

	| font count string x y fh canvas |
	font _ AbstractFont default.
	
	fh _ font height.
	count _ innerRectangle height // fh-1.
	x _ innerRectangle left.

	string _ unfinishedEntry contents.
	y _ ((lastIndex - firstIndex \\ self maxEntries) min: count-1) + 1 * font height + innerRectangle top.
	canvas _ aForm getCanvas.
	canvas
		newClipRect: innerRectangle;
		drawString: string at: x@y font: font color: Color veryDarkGray! !

!methodRemoval: StrikeFont class #fromUser!
StrikeFont class removeSelector: #fromUser!
!methodRemoval: StrikeFont class #fromUser:!
StrikeFont class removeSelector: #fromUser:!
!methodRemoval: StrikeFont #height!
StrikeFont removeSelector: #height!
!methodRemoval: StrikeFont #widthOfString:from:to:!
StrikeFont removeSelector: #widthOfString:from:to:!
