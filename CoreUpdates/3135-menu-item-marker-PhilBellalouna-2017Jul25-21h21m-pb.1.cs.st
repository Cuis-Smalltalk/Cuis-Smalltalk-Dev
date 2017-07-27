'From Cuis 5.0 of 7 November 2016 [latest update: #3130] on 25 July 2017 at 9:35:35 pm'!

!Boolean methodsFor: 'converting' stamp: 'pb 7/25/2017 21:35:09'!
asMenuItemMarkerString
	"If this markup is present in a menu item label, an on/off indicator will be displayed"
	^ self
		ifTrue: [ '<on>' ]
		ifFalse: [ '<off>' ].! !


!CodeProvider methodsFor: 'diffs' stamp: 'pb 7/25/2017 21:24:16'!
showingLineDiffsString
	"Answer a string representing whether I'm showing regular diffs"
	^ self showingLineDiffs asMenuItemMarkerString , 'lineDiffs'.! !

!CodeProvider methodsFor: 'diffs' stamp: 'pb 7/25/2017 21:24:39'!
showingPrettyLineDiffsString
	"Answer a string representing whether I'm showing pretty diffs"
	^ self showingPrettyLineDiffs asMenuItemMarkerString , 'linePrettyDiffs'.! !

!CodeProvider methodsFor: 'diffs' stamp: 'pb 7/25/2017 21:24:50'!
showingPrettyWordDiffsString
	"Answer a string representing whether I'm showing pretty diffs"
	^ self showingPrettyWordDiffs asMenuItemMarkerString , 'wordPrettyDiffs'.! !

!CodeProvider methodsFor: 'diffs' stamp: 'pb 7/25/2017 21:24:58'!
showingWordDiffsString
	"Answer a string representing whether I'm showing regular diffs"
	^ self showingWordDiffs asMenuItemMarkerString , 'wordDiffs'.! !

!CodeProvider methodsFor: 'what to show' stamp: 'pb 7/25/2017 21:23:37'!
prettyPrintString
	"Answer whether the receiver is showing pretty-print"
	^ (self contentsSymbol == #prettyPrint) asMenuItemMarkerString , 'prettyPrint'.! !

!CodeProvider methodsFor: 'what to show' stamp: 'pb 7/25/2017 21:23:33'!
showingByteCodesString
	"Answer whether the receiver is showing bytecodes"
	^ self showingByteCodes asMenuItemMarkerString , 'byteCodes'.! !

!CodeProvider methodsFor: 'what to show' stamp: 'pb 7/25/2017 21:23:51'!
showingDecompileString
	"Answer a string characerizing whether decompilation is showing"
	^ self showingDecompile asMenuItemMarkerString , 'decompile'.! !

!CodeProvider methodsFor: 'what to show' stamp: 'pb 7/25/2017 21:24:04'!
showingDocumentationString
	"Answer a string characerizing whether documentation is showing"
	^ self showingDocumentation asMenuItemMarkerString , 'documentation'.! !

!CodeProvider methodsFor: 'what to show' stamp: 'pb 7/25/2017 21:24:29'!
showingPlainSourceString
	"Answer a string telling whether the receiver is showing plain source"
	^ self showingPlainSource asMenuItemMarkerString , 'source'.! !


!TextEditor methodsFor: 'attributes' stamp: 'pb 7/25/2017 21:28:16'!
changeEmphasisOrAlignment
	"This is a user command, and generates undo"
	| menuStrings aList reply code align menuList startIndex attribute |
	startIndex _ self startIndex.
	aList _ #(#normal #bold #italic #underlined #struckThrough #leftFlush #centered #rightFlush #justified ).
	align _ model actualContents alignmentAt: startIndex.
	code _ model actualContents emphasisAt: startIndex.
	menuList _ WriteStream on: Array new.
	menuList nextPut: code isZero asMenuItemMarkerString , 'normal'.
	menuList nextPutAll:
		(#(#bold #italic #underlined #struckThrough #superscript #subscript #withST80Glyphs ) collect: [ :emph |
			(code anyMask: (TextEmphasis perform: emph) emphasisCode) asMenuItemMarkerString , emph asString ]).
	menuList nextPutAll:
		(#(#leftFlush #centered #rightFlush #justified ) withIndexCollect: [ :type :i |
			(align = (i - 1)) asMenuItemMarkerString 
				 , type asString 
				]).
	menuStrings _ menuList contents.
	aList _ #(#normal #bold #italic #underlined #struckThrough #superscript #subscript #withST80Glyphs #leftFlush #centered #rightFlush #justified ).
	reply _ (SelectionMenu
		labelList: menuStrings
		lines: #(1 8 )
		selections: aList) startUpWithoutKeyboard.
	reply ifNotNil: [
		(#(#leftFlush #centered #rightFlush #justified ) includes: reply)
			ifTrue: [ attribute _ TextAlignment perform: reply ]
			ifFalse: [ attribute _ TextEmphasis perform: reply ].
		((menuStrings at: (aList indexOf: reply)) beginsWith: '<on>')
			ifTrue: [ self unapplyAttributes: {attribute} ]
			ifFalse: [ self applyAttribute: attribute ]].
	^ true.! !

!TextEditor methodsFor: 'undo & redo' stamp: 'pb 7/25/2017 21:30:11'!
offerUndoHistory
	| index labels current |
	current _ model undoRedoCommandsPosition.
	labels _ model undoRedoCommands withIndexCollect: [ :each :i |
		(i = current) asMenuItemMarkerString , each printString ].
	labels isEmpty ifFalse: [
		index _ (PopUpMenu
			labelArray: labels
			lines: #()) startUpMenu.
		index = current ifTrue: [ ^ self ].
		index = 0 ifTrue: [ ^ self ].
		index < current
			ifTrue: [ current - index timesRepeat: [ self undo ]]
			ifFalse: [ index - current timesRepeat: [ self redo ]]].! !


!Preferences class methodsFor: 'misc' stamp: 'pb 7/25/2017 21:25:46'!
staggerPolicyString
	"Answer the string to be shown in a menu to represent the 
	stagger-policy status"
	^ (self valueOfFlag: #reverseWindowStagger) asMenuItemMarkerString , 'stagger windows'.! !


!AbstractFont class methodsFor: 'instance accessing' stamp: 'pb 7/25/2017 21:26:40'!
fromUser: priorFont
	"
	AbstractFont fromUser
	"
	"Present a menu of available fonts, and if one is chosen, return it.
	Otherwise return nil.
	Show only baseFonts i.e. FamilyName, pointSize (but do not include emphasis, such as italic or bold)"
	| fontList fontMenu active ptMenu label spec |
	fontList _ AbstractFont familyNames.
	fontMenu _ MenuMorph new defaultTarget: self.
	fontList do: [ :fontName |
		active _ priorFont familyName sameAs: fontName.
		ptMenu _ MenuMorph new defaultTarget: self.
		(AbstractFont pointSizesFor: fontName) do: [ :pt |
			label _ (active and: [ pt = priorFont pointSize ]) asMenuItemMarkerString.
			label _ label , pt printString , ' pt'.
			ptMenu
				add: label
				target: fontMenu
				selector: #modalSelection:
				argument: {fontName. pt} ].
		label _ active asMenuItemMarkerString.
		label _ label , fontName.
		fontMenu
			add: label
			subMenu: ptMenu ].
	spec _ fontMenu invokeModal.
	spec ifNil: [ ^ nil ].
	^ AbstractFont
		familyName: spec first
		pointSize: spec last.! !


!Morph methodsFor: 'menus' stamp: 'pb 7/25/2017 21:26:57'!
lockedString
	"Answer the string to be shown in a menu to represent the 
	'locked' status"
	^ self isLocked asMenuItemMarkerString , 'be locked'.! !

!Morph methodsFor: 'menus' stamp: 'pb 7/25/2017 21:25:34'!
stickinessString
	"Answer the string to be shown in a menu to represent the  
	stickiness status"
	^ self isSticky asMenuItemMarkerString , 'resist being picked up'.! !


!InnerTextMorph methodsFor: 'menu' stamp: 'pb 7/25/2017 21:25:11'!
wrapString
	"Answer the string to put in a menu that will invite the user to 
	switch word wrap mode"
	^ wrapFlag asMenuItemMarkerString , 'text wrap to bounds'.! !


!TheWorldMenu methodsFor: 'commands' stamp: 'pb 7/25/2017 21:30:06'!
setDisplayDepth
	"Let the user choose a new depth for the display. "
	| result oldDepth allDepths allLabels menu hasBoth |
	oldDepth _ Display nativeDepth.
	allDepths _ #(1 -1 2 -2 4 -4 8 -8 16 -16 32 -32 ) select: [ :d |
		Display supportsDisplayDepth: d ].
	hasBoth _ (allDepths anySatisfy: [ :d |
		d > 0 ]) and: [
		allDepths anySatisfy: [ :d |
			d < 0 ]].
	allLabels _ allDepths collect: [ :d |
		String streamContents: [ :s |
			s nextPutAll: (d = oldDepth) asMenuItemMarkerString.
			s print: d abs.
			hasBoth ifTrue: [
				s nextPutAll:
					(d > 0
						ifTrue: [ '  (big endian)' ]
						ifFalse: [ '  (little endian)' ]) ]]].
	menu _ SelectionMenu
		labels: allLabels
		selections: allDepths.
	result _ menu startUpWithCaption: 'Choose a display depth'.
	result ifNotNil: [ Display newDepth: result ].
	oldDepth _ oldDepth abs.! !


!Boolean reorganize!
('logical operations' & eqv: not |)
('controlling' and: and:and: and:and:and: and:and:and:and: ifFalse: ifFalse:ifTrue: ifTrue: ifTrue:ifFalse: or: or:or: or:or:or: or:or:or:or:)
('copying' shallowCopy)
('printing' isLiteral storeOn:)
('converting' asMenuItemMarkerString)
!

