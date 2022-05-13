'From Cuis 6.0 [latest update: #5146] on 12 May 2022 at 11:26:27 am'!

!PluggableTextModel methodsFor: 'misc' stamp: 'hlsf 5/12/2022 11:21:53'!
refetch
	"Answer true if actualContents was actually fetched."
	textGetter
		ifNil: [
			actualContents ifNil: [
				self actualContents: Text new ].
			^false ]
		ifNotNil: [
			self actualContents: (Text
				initialFont: (PreferenceNG at: #standardCodeFont)
				stringOrText: (textProvider perform: textGetter)).
			self changed: #refetched.
			^true ]! !


!TextProvider methodsFor: 'contents' stamp: 'hlsf 5/12/2022 11:21:57'!
acceptedContents
	^ Text
		initialFont: (PreferenceNG at: #standardCodeFont)
		stringOrText: self acceptedStringOrText! !


!Browser methodsFor: 'class functions' stamp: 'hlsf 5/12/2022 11:22:01'!
classCommentText
	"return the text to display for the comment of the currently selected class"
	| theClass |
	theClass _ self selectedClassOrMetaClass.
	^ Text
		initialFont: (PreferenceNG at: #standardCodeFont)
		stringOrText:
			((theClass notNil and: [ theClass hasComment ])
				ifTrue: [ theClass comment ]
				ifFalse: [ '' ]).! !


!ChangeList methodsFor: 'menu actions' stamp: 'hlsf 5/12/2022 11:22:06'!
compareToCurrentVersion
	"If the current selection corresponds to a method in the system, then spawn a window showing the diffs as text"

	| change class s1 s2 differDesc diffWords |
	listIndex = 0
		ifTrue: [^ self].
	change _ changeList at: listIndex.
	((class _ change changeClass) notNil
			and: [class includesSelector: change methodSelector])
		ifTrue: [
			s1 _ (class sourceCodeAt: change methodSelector) asPlainString.
			s2 _ change string.
			s1 = s2
				ifTrue: [^ self inform: 'Exact Match'].
			diffWords _ self shouldDiffWords.
			differDesc _ diffWords
				ifTrue: [ 'Words']
				ifFalse: [ 'Lines'].
			(TextModel
				withText: (
					(DifferenceFinder
						displayPatchFrom: s1 to: s2
						tryWords: diffWords
						prettyPrintedIn: (self showingAnyKindOfPrettyDiffs ifTrue: [class]))
							font: (PreferenceNG at: #standardCodeFont)))
				openLabel: 'Comparison to Current Version: ', differDesc, 
					(self showingAnyKindOfPrettyDiffs ifTrue: [', using prettyPrint'] ifFalse: [''])]
		ifFalse: [self flash]! !


!FileList methodsFor: 'volume list and pattern' stamp: 'hlsf 5/12/2022 11:18:05'!
fileNameFormattedFrom: entry namePad: namePad sizePad: sizePad sizeWithCommasPad: sizeWithCommasPad
	"entry is a 5-element array of the form:
		(name creationTime modificationTime dirFlag fileSize)"
	| sizeStr nameStr paddedNameStr dateStr someSpaces sizeDigits sizeDigitsAndCommas spacesToAdd font spaceWidth |
	font _ PreferenceNG at: #standardListFont.
	spaceWidth _ font widthOf: $ .
	nameStr _ entry isDirectory
		ifTrue: [ entry name , self folderString ]
		ifFalse: [ entry name ].
	spacesToAdd _ namePad - (font widthOfString: nameStr) // spaceWidth.
	paddedNameStr _ nameStr ,
		(String
			new: spacesToAdd
			withAll: $ ).
	dateStr _ (entry modificationTime date printFormat: #(3 2 1 $/ 1 1 2 )) , '  ' ,
		(String streamContents: [ :s |
			entry modificationTime time
				print24: true
				showSeconds: true
				on: s ]).
	sizeDigits _ entry fileSize printString size.
	sizeStr _ entry fileSize printStringWithCommas.
	sizeDigitsAndCommas _ sizeStr size.
	spacesToAdd _ sizeWithCommasPad - sizeDigitsAndCommas.
	"Usually a space takes the same space as a comma, and half the space of a digit.
	Pad with 2 spaces for each missing digit and 1 space for each missing comma"
	(font widthOf: Character space) ~= (font widthOf: $, )
		ifTrue: [spacesToAdd _ spacesToAdd + sizePad - sizeDigits max: 0].
	sizeStr _ (String new: spacesToAdd withAll: $ ) , sizeStr.
	someSpaces _ String new: 6 withAll: $ .
	"
	sortMode = #name ifTrue: [ ^ paddedNameStr , someSpaces , '( ' , dateStr , someSpaces , sizeStr , ' )' ].
	sortMode = #date ifTrue: [ ^ '( ' , dateStr , someSpaces , sizeStr , ' )' , someSpaces , nameStr ].
	sortMode = #size ifTrue: [ ^ '( ' , sizeStr , someSpaces , dateStr , ' )' , someSpaces , nameStr ].
	"
	^ paddedNameStr , someSpaces , '( ' , dateStr , someSpaces , sizeStr , ' )' .! !

!FileList methodsFor: 'volume list and pattern' stamp: 'hlsf 5/12/2022 11:18:32'!
listForSelectingPatterns: patternsThatSelect rejectingPatterns: patternsThatReject
	"Make the list be those file names which match the patterns."

	| sizePad selected newList namePad sizeWithCommasPad font |
	directory ifNil: [^#()].
	selected _ Set new.
	patternsThatSelect do: [ :pat |
		directory childrenDo: [ :entry |
			(entry isDirectory
				ifTrue: [ showDirsInFileList ]
				ifFalse: [ self doesPattern: pat allow: entry])
					ifTrue: [ selected add: entry ]]].
	newList _ selected copy.
	patternsThatReject do: [ :pat |
		selected do: [ :entry |
			(entry isDirectory not and: [ pat match: entry name]) ifTrue: [
				newList remove: entry ]]].
		
	newList _ newList asArray sort: self sortBlock.
	font _ PreferenceNG at: #standardListFont.
	namePad _ newList inject: 0 into: [ :mx :entry | mx max: (font widthOfString: entry name)].
	sizePad _ (newList inject: 0 into: [ :mx :entry | mx max: (entry fileSize)]) printString size.
	sizeWithCommasPad _ (newList inject: 0 into: [ :mx :entry | mx max: (entry fileSize)]) printStringWithCommas size.
	newList _ newList collect: [ :e |
		self fileNameFormattedFrom: e namePad: namePad sizePad: sizePad sizeWithCommasPad: sizeWithCommasPad ].
	^ newList! !


!Color class methodsFor: 'colormaps' stamp: 'hlsf 5/12/2022 10:51:53'!
computeRGBColorConvertingMap: targetColor to: destDepth keepSubPixelAA: keepSubPix
	"Builds a colormap intended to convert from subpixelAA black values to targetColor values.
	keepSubPix
		ifTrue: [ Answer colors that also include subpixelAA ]
		ifFalse: [ 
			Take fullpixel luminance level. Apply it to targetColor.
			I.e. answer colors with NO subpixelAA ]"

	| mask map c bitsPerColor r g b f v |

	destDepth > 8
		ifTrue: [bitsPerColor _ 5]  "retain maximum color resolution"
		ifFalse: [bitsPerColor _ 4].
	"Usually a bit less is enough, but make it configurable"
	bitsPerColor _ bitsPerColor min: (PreferenceNG at: #aaFontsColormapDepth).
	mask _ (1 bitShift: bitsPerColor) - 1.
	map _ Bitmap new: (1 bitShift: (3 * bitsPerColor)).
	0 to: map size - 1 do: [:i |
		r _ (i bitShift: 0 - (2 * bitsPerColor)) bitAnd: mask.
		g _ (i bitShift: 0 - bitsPerColor) bitAnd: mask.
		b _ (i bitShift: 0) bitAnd: mask.
		f _ 1.0 - (r + g + b / 3.0 / mask).
		c _ targetColor
			ifNotNil: [
				(keepSubPix and: [destDepth > 8]) ifTrue: [
						Color
							r: 1.0 - (r asFloat/mask) * targetColor red
							g: 1.0 - (g asFloat/mask) * targetColor green
							b: 1.0 - (b asFloat/mask) * targetColor blue
							alpha: f	* targetColor alpha "alpha will be ignored below, in #pixelValueForDepth: if destDepth ~= 32" ]
				ifFalse: [
					destDepth = 32
						ifTrue: [ targetColor * f alpha: f * targetColor alpha ]
						ifFalse: [ targetColor alphaMixed: f*1.5 with: `Color white` ]]]
			ifNil: [ Color r: r g: g b: b range: mask].	"This is currently used only to keep some SubPixelAA on destDepth = 8, using a single pass of rule 25"
		v _ destDepth = 32
			ifTrue: [ c pixelValueForDepth: destDepth]
			ifFalse: [
				f < 0.1
					ifTrue: [ 0 ]
					ifFalse: [ c pixelValueForDepth: destDepth ]].
		map at: i + 1 put: v ].
	^ map! !


!PreferenceNG class methodsFor: 'data' stamp: 'hlsf 5/12/2022 10:49:26'!
prefFont
	^ #( 
		#(aaFontsColormapDepth 4)
		#(cacheTrueTypeGlyphs true )
		#(guiElementsSize standardFonts #(tinyFonts verySmallFonts smallFonts standardFonts largeFonts veryLargeFonts hugeFonts) defaultFontSize: )
		#(loadOnlyLatinGlyphData false )
		#(properDisplayAlphaForFonts false )	
		#(subPixelRenderColorFonts true )
		#(subPixelRenderFonts true )	
	)! !


!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'hlsf 5/12/2022 11:22:21'!
standardWindowExtent
	"Answer the standard default extent for new windows.  "

	| grid allowedArea maxLevel |
	"NOTE: following copied from strictlyStaggeredInitialFrameFor:"
	allowedArea _ self maximumUsableArea insetBy: (
		self scrollBarSetback @ self screenTopSetback extent: `0@0`
	).
	"Number to be staggered at each corner (less on small screens)"
	maxLevel _ allowedArea area > 700000 ifTrue: [3] ifFalse: [2].
	"Amount by which to stagger (less on small screens)"
	grid _ allowedArea area > 700000 ifTrue: [40] ifFalse: [20].
	^ ((allowedArea extent - (grid*(maxLevel+1*2) + (grid//2))) 
		min: `52@40` * (PreferenceNG at: #standardCodeFont) lineSpacing) rounded! !


!BitBltCanvasEngine methodsFor: 'private' stamp: 'hlsf 5/12/2022 10:50:53'!
setRuleAndMapFor: sourceDepth foregroundColor: foregroundColor

	| targetColor destDepth |
	destDepth _ destForm depth.
	halftoneForm _ nil.	"Don't use fillColor. Use a more powerful ColorMap"

	sourceDepth = 1 ifTrue: [
		self combinationRule: Form paint.
		"Set up color map for a different source depth (color font)"
		"Uses caching for reasonable efficiency"
		colorMap _ self cachedFontColormapFrom1BitTo: destDepth.
		colorMap at: 1 put: (destForm pixelValueFor: `Color transparent`).
		colorMap at: 2 put: (destForm pixelValueFor: foregroundColor) ]
	
	ifFalse: [
		"Enable subpixel rendering if requested, but never for translucent text:
		This technique always draws opaque text. This could be added, by using an extra colormap for the rgbMul phase...
		So far, no need arised for doing so."
		(sourceDepth > 8 and: [
			(PreferenceNG at: #subPixelRenderFonts) and: [ foregroundColor = `Color black` or: [ 
				(PreferenceNG at: #subPixelRenderColorFonts) and: [ foregroundColor isOpaque ]]]]) ifTrue: [
			destDepth > 8 ifTrue: [
				"rgbMul is equivalent to component alpha blend if text is black (only faster, hehe)"
				self combinationRule: 37.		"rgbMul"
				colorMap _ (foregroundColor ~= `Color black` or: [
						destDepth = 32 and: [ destForm ~~ Display or: [PreferenceNG at: #properDisplayAlphaForFonts] ]]) ifTrue: [
					"rgbMul / rgbAdd IS component alpha blend for any color of text (neat trick, eh!!)"
					"This colorMap is to be used on the second pass with rule 20 (rgbAdd)
					See #displayString:from:to:at:strikeFont:color:"
					"Note: In 32bpp, if we want the correct alpha in the result, we need the second pass, as the destination could have transparent pixels, 
					and we need to add to the alpha channel"
					self colorConvertingMap: foregroundColor from: sourceDepth to: destDepth keepSubPixelAA: true]]
			ifFalse: [
				self combinationRule: 25.		"Paint"
				targetColor _ foregroundColor = `Color black` ifFalse: [ foregroundColor ].
				colorMap _ self colorConvertingMap: targetColor from: sourceDepth to: destDepth keepSubPixelAA: true]]
		ifFalse: [
			"Do not use rule 34 for 16bpp display."
			self combinationRule: (destDepth = 32 ifTrue: [34 "alphaBlendScaled"] ifFalse: [25 "Paint"]).
			colorMap _ self colorConvertingMap: foregroundColor from: sourceDepth to: destDepth keepSubPixelAA: false]]! !


!StringRequestMorph methodsFor: 'initialization' stamp: 'hlsf 5/12/2022 11:23:40'!
addTitle: aString
	| titleMorph s pp w |
	titleMorph _ BoxedMorph new noBorder.
	titleMorph color: Theme current menuTitleBar.
	pp _ `8@2`.
	aString asPlainString linesDo: [ :line |
		s _ LabelMorph new
			contents: line;
			font: (PreferenceNG at: #standardMenuFont) bold.
		titleMorph addMorphBack: s position: pp.
		pp _ pp + (0@(s morphHeight+2)) ].
	w _ titleMorph submorphs inject: 0 into: [ :prev :each |
		prev max: each morphWidth ].
	titleMorph morphExtent: (w + 24) @ (pp y).
	self addMorphKeepMorphHeight: titleMorph.
	^titleMorph morphWidth! !


!StringRequestMorph class methodsFor: 'private' stamp: 'hlsf 5/12/2022 11:25:02'!
deltaToTextPane
	"Answer a distance to translate an instance of the receiver by when it is opened in the world in order to have the hand be over the text pane (so the text pane has focus).
	Distance is relative to font size"
	| e |
	e _ (PreferenceNG at: #windowTitleFont) lineSpacing.
	^ (0)@(0.5 * e)! !


!TaskbarMorph methodsFor: 'initialization' stamp: 'hlsf 5/12/2022 11:25:07'!
defaultHeight

	^ ((PreferenceNG at: #windowTitleFont) lineSpacing * 2 * self scale) asInteger! !


!InnerListMorph methodsFor: 'initialization' stamp: 'hlsf 5/12/2022 11:19:03'!
initialize
	super initialize.
	self color: `Color black`.
	font _ PreferenceNG at: #standardListFont.
	listItems _ #().
	selectedRow _ nil.
	highlightedRow _ nil! !

!InnerListMorph methodsFor: 'geometry' stamp: 'hlsf 5/12/2022 11:18:54'!
fontPreferenceChanged

	super fontPreferenceChanged.
	self font: (PreferenceNG at: #standardListFont).! !


!IndentingListItemMorph methodsFor: 'geometry' stamp: 'hlsf 5/12/2022 11:19:08'!
fontPreferenceChanged

	super fontPreferenceChanged.
	self font: (PreferenceNG at: #standardListFont).! !

!IndentingListItemMorph methodsFor: 'initialization' stamp: 'hlsf 5/12/2022 11:19:17'!
initWithContents: anObject prior: priorMorph forList: hostList indentLevel: newLevel

	| o |
	container _ hostList.
	complexContents _ anObject.
	self initWithContents: anObject asString font: (PreferenceNG at: #standardListFont) emphasis: nil.
	indentLevel _ 0.
	isExpanded _ false.
 	nextSibling _ firstChild _ nil.
	priorMorph ifNotNil: [
		priorMorph nextSibling: self.
	].
	o _ anObject withoutListWrapper.
	icon _ o ifNotNil: [ (o respondsTo: #icon) ifTrue: [ o icon ] ].
	icon isSymbol ifTrue: [ icon _ Theme current perform: icon ].
	indentLevel _ newLevel.
! !


!UpdatingLabelMorph class methodsFor: 'new-morph participation' stamp: 'hlsf 5/12/2022 11:25:12'!
initializedInstance
	"Answer a digital clock"

	| newInst |
	newInst := self 
		contents: '' 
		font: (PreferenceNG at: #windowTitleFont)
		emphasis: AbstractFont  boldCode.
	newInst 
		stepTime: 500; "half a second"
		target: [String streamContents: [ :strm | DateAndTime now printHMSOn: strm]] ;
		getSelector: #value.
		
	^ newInst! !


!MenuItemMorph methodsFor: 'initialization' stamp: 'hlsf 5/12/2022 11:23:50'!
initialize
	"initialize the state of the receiver"
	super initialize.
	isEnabled _ true.
	subMenu _ nil.
	isSelected _ false.
	target _ nil.
	selector _ nil.
	arguments _ nil.
	font _ PreferenceNG at: #standardMenuFont.
	self contents: ''.! !


!PluggableButtonMorph methodsFor: 'drawing' stamp: 'hlsf 5/12/2022 11:20:32'!
fontToUse
	| fontToUse |
	fontToUse := font ifNil: [PreferenceNG at: #standardButtonFont].
	"
	Could add emphasis...
	^(emphasis isNil or: [emphasis = 0]) 
		ifTrue: [fontToUse]
		ifFalse: [fontToUse emphasized: emphasis]
	"
	^fontToUse! !

!PluggableButtonMorph methodsFor: 'geometry' stamp: 'hlsf 5/12/2022 11:25:17'!
minimumExtent
	
	| unit |
	unit :=  (PreferenceNG at: #windowTitleFont) pointSize.
	^label 
		ifNil: [ (unit+2) @ (unit+2) ] "Assume title button"
		ifNotNil: [ (4 * unit) @ (3 * unit) ] "Assure some space for text."
! !


!PluggableListMorph methodsFor: 'geometry' stamp: 'hlsf 5/12/2022 11:19:23'!
fontPreferenceChanged

	super fontPreferenceChanged.
	self font: (PreferenceNG at: #standardListFont).! !


!SystemWindow methodsFor: 'drawing' stamp: 'hlsf 5/12/2022 11:25:24'!
drawLabelOn: aCanvas

	| x0 y0 f w availableW l |
	f _ PreferenceNG at: #windowTitleFont.
	x0 _  f lineSpacing * 5 + borderWidth.
	y0 _ borderWidth * 6 // 10.
	availableW _ extent x - x0.
	l _ labelString.
	w _ f widthOfString: l.
	[ w > availableW ] whileTrue: [
		l _ l squeezedTo: (1.0 * l size * availableW / w) truncated.
		l isEmpty ifTrue: [ ^self ].
		w _ f widthOfString: l ].
	aCanvas
		drawString: l
		at: x0@y0
		font: f
		color: Theme current windowLabel
		embossed: Theme current embossedTitles! !

!SystemWindow methodsFor: 'label' stamp: 'hlsf 5/12/2022 11:25:31'!
labelHeight
	"Answer the height for the window label."
	^ (PreferenceNG at: #windowTitleFont) lineSpacing+1! !


!CodePackageListWindow methodsFor: 'GUI building' stamp: 'hlsf 5/12/2022 11:22:31'!
initialExtent

	^`540@400` * (PreferenceNG at: #standardCodeFont) lineSpacing // 14! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'hlsf 5/12/2022 11:22:37'!
buildNoSysCatMorphicWindow
	"A Browser without the class categories list"

	| mySingletonList upperPanes messageCatList messageList classColumn classList |
	mySingletonList _ PluggableListMorph
				model: model
				listGetter: #systemCategorySingleton
				indexGetter: #indexIsOne
				indexSetter: #indexIsOne:
				mainView: self
				menuGetter: #systemCatSingletonMenu
				keystrokeAction: #systemCatSingletonKey:from:.
	mySingletonList hideScrollBarsIndefinitely.

	classList _ self buildMorphicClassList.
	classColumn _ self buildMorphicClassColumnWith: classList.
	messageCatList _ self buildMorphicMessageCatList.
	messageList _ self buildMorphicMessageList.
	
	classList rightSibling: messageCatList.
	messageCatList leftSibling: classList rightSibling: messageList.
	messageList leftSibling: messageCatList.
	
	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: classColumn proportionalWidth: 0.3;
		addAdjusterAndMorph: messageCatList proportionalWidth: 0.3;
		addAdjusterAndMorph: messageList proportionalWidth: 0.4.

	messageList makeItemsDraggable.
	messageCatList 
		acceptDropsFrom: messageList 
		performing: #categorizeUnderCategoryAt:selector: 
		whenOutsideList: #categorizeUnderNewCategorySelector:.

	self layoutMorph
		addMorph: mySingletonList fixedHeight: (PreferenceNG at: #standardCodeFont) lineSpacing + 10;
		addAdjusterAndMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.

	model changed: #editSelection! !


!CodeFileBrowserWindow methodsFor: 'GUI building' stamp: 'hlsf 5/12/2022 11:22:42'!
buildMorphicWindow
	"Create a pluggable version of all the views for a Browser, using Morphic widgets."

	| sysCatList msgCatList upperPanes clsLayout clsList msgList |
	model setSelectedSystemCategory: model systemCategoryList first.
	sysCatList _ PluggableListMorph
			model: model 
			listGetter: #systemCategorySingleton
			indexGetter: #indexIsOne 
			indexSetter: #indexIsOne:
			mainView: self
			menuGetter: #codeFileListMenu
			keystrokeAction: #codeFileListKey:from:.
	sysCatList hideScrollBarsIndefinitely.
	
	msgCatList _ PluggableListMorph
			model: model 
			listGetter: #messageCategoryList
			indexGetter: #messageCategoryListIndex 
			indexSetter: #messageCategoryListIndex:
			mainView: self
			menuGetter: #messageCategoryMenu
			keystrokeAction: nil.

	clsList := self buildMorphicClassList.
	clsLayout := self buildMorphicClassColumnWith: clsList.
	msgList := self buildMorphicMessageList.
	sysCatList rightSibling: clsList.
	clsList leftSibling: sysCatList rightSibling: msgCatList.
	msgCatList leftSibling: clsList rightSibling: msgList.
	msgList leftSibling: msgCatList.

	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: clsLayout proportionalWidth: 0.3;
		addAdjusterAndMorph: msgCatList proportionalWidth: 0.3;
		addAdjusterAndMorph: msgList proportionalWidth: 0.4.

	self layoutMorph
		addMorph: sysCatList fixedHeight: (PreferenceNG at: #standardCodeFont) lineSpacing + 10;
		addAdjusterAndMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.
	model changed: #editSelection! !


!ChangeSorterWindow methodsFor: 'GUI building' stamp: 'hlsf 5/12/2022 11:22:47'!
initialExtent
	^`540@300` * (PreferenceNG at: #standardCodeFont) lineSpacing // 14! !


!PreDebugWindow methodsFor: 'GUI building' stamp: 'hlsf 5/12/2022 11:22:52'!
initialExtent
	^ `640 @ 320` * (PreferenceNG at: #standardCodeFont) lineSpacing // 14! !


!InspectorWindow methodsFor: 'GUI building' stamp: 'hlsf 5/12/2022 11:22:58'!
initialExtent

	^`600@325` * (PreferenceNG at: #standardCodeFont) lineSpacing // 14! !


!ObjectExplorerWindow methodsFor: 'GUI building' stamp: 'hlsf 5/12/2022 11:23:04'!
initialExtent

	^`300@500` * (PreferenceNG at: #standardCodeFont) lineSpacing // 14! !


!MenuMorph methodsFor: 'construction' stamp: 'hlsf 5/12/2022 11:23:57'!
addTitle: aString
	"Add a title line at the top of this menu Make aString its initial 
	contents.  
	If aSelector is not nil, then periodically obtain fresh values for its 
	contents by sending aSelector to aTarget.."

	| s pp w newMorph |
	
	newMorph _ BoxedMorph new noBorder.
	newMorph color: Theme current menuTitleBar.
	pp _ `8@2`.
	aString asPlainString linesDo: [ :line | | font |
		font _ PreferenceNG at: #standardMenuFont.
		s _ LabelMorph new
			contents: line;
			font: (titleMorph
				ifNil: [ font bold ]
				ifNotNil: [ font italic ])..
		newMorph addMorphBack: s position: pp.
		pp _ pp + (0@(s morphHeight+2)) ].
	w _ newMorph submorphs inject: 0 into: [ :prev :each |
		prev max: each morphWidth ].
	newMorph morphExtent: (w + 16) @ (pp y).
	titleMorph
		 ifNil: [
			titleMorph _ newMorph.
			self addMorphFront: titleMorph ]
		ifNotNil: [ self addMorphBack: newMorph ].
	
	(self hasProperty: #needsStayUpIcons) ifTrue: [ self addStayUpIcons ]! !


!AutoCompleterMorph class methodsFor: 'preferences' stamp: 'hlsf 5/12/2022 11:19:30'!
listFont

	^PreferenceNG at: #standardListFont! !


!Theme methodsFor: 'other options' stamp: 'hlsf 5/12/2022 11:20:41'!
buttonPaneHeight
	"Answer the user's preferred default height for button panes."

	^(PreferenceNG at: #standardButtonFont) lineSpacing * 14 // 8! !

!Theme methodsFor: 'other options' stamp: 'hlsf 5/12/2022 11:19:41'!
layoutAdjusterThickness

	self flag: #todo.  "Revisit this; consider moving proportional stuff out of Theme entirely."
	
	^ (PreferenceNG at: #standardListFont) pointSize // 3! !

!Theme methodsFor: 'other options' stamp: 'hlsf 5/12/2022 11:25:37'!
titleBarButtonsExtent
	"Answer the extent to use for close & other title bar buttons.  
	The label height is used to be proportional to the fonts preferences."
	| e |
	e _ (PreferenceNG at: #windowTitleFont) pointSize * 13 // 10.
	^e@e! !

!Theme methodsFor: 'other options' stamp: 'hlsf 5/12/2022 11:19:49'!
windowBorderWidth
	| w |
	w _ (PreferenceNG at: #standardListFont) pointSize / 11.
	w _ w * (self roundWindowCorners ifTrue: [4] ifFalse: [2]).
	^ w rounded max: 1! !


!Theme class methodsFor: 'user interface' stamp: 'hlsf 5/12/2022 11:08:20'!
changeFontSizes

	| options menu preference  |
	preference _ PreferenceNG instanceAt: #guiElementsSize.
	options _ #(
		#('Huge' #hugeFonts)
		#('Very Large' #veryLargeFonts)
		#('Large' #largeFonts)
		#('Default Size' #standardFonts)
		#('Small' #smallFonts)
		#('Very Small' #verySmallFonts)
		#('Tiny' #tinyFonts)).
	menu _ MenuMorph new.
	menu
		addTitle: 'Make GUI elements';
		addStayUpIcons;
		stayUp: true.
	options do: [ :pair |
		(menu add: pair first target: preference action: #value: argument: pair second)
			isSelected: preference value == pair second  ].
	menu popUpInWorld: self runningWorld.! !


!PopUpMenu methodsFor: 'accessing' stamp: 'hlsf 5/12/2022 11:24:04'!
frameHeight
	"Designed to avoid the entire frame computation (includes MVC form),
	since the menu may well end up being displayed in Morphic anyway."
	| nItems |
	nItems _ 1 + labelString lineCount.
	^ (nItems * (PreferenceNG at: #standardMenuFont) lineSpacing) + 4 "border width"! !

!PopUpMenu methodsFor: 'basic control sequence' stamp: 'hlsf 5/12/2022 11:24:09'!
startUpSegmented: segmentHeight withCaption: captionOrNil at: location allowKeyboard: aBoolean
	"This menu is too big to fit comfortably on the screen.
	Break it up into smaller chunks, and manage the relative indices.
	Inspired by a special-case solution by Reinier van Loon.  The boolean parameter indicates whether the menu should be given keyboard focus (if in morphic)"

"
(PopUpMenu labels: (String streamContents: [:s | 1 to: 100 do: [:i | s print: i; newLine]. s skip: -1])
		lines: (5 to: 100 by: 5)) startUpWithCaption: 'Give it a whirl...'.
"
	| nLines nLinesPer allLabels from to subset subLines index |
	allLabels := labelString lines.
	nLines _ allLabels size.
	lineArray ifNil: [lineArray _ Array new].
	nLinesPer _ segmentHeight // (PreferenceNG at: #standardMenuFont) lineSpacing - 5.
	from := 1.
	[ true ] whileTrue: [
		to := (from + nLinesPer) min: nLines.
		subset := (allLabels copyFrom: from to: to) asOrderedCollection.
		subset add: (to = nLines ifTrue: ['start over...'] ifFalse: ['more...'])
			before: subset first.
		subLines _ lineArray select: [:n | n >= from] thenCollect: [:n | n - (from-1) + 1].
		subLines _ (Array with: 1) , subLines.
		index := (PopUpMenu labels: subset printStringWithNewline lines: subLines)
					startUpWithCaption: captionOrNil at: location allowKeyboard: aBoolean.
		index = 1
			ifTrue: [from := to + 1.
					from > nLines ifTrue: [ from := 1 ]]
			ifFalse: [index = 0 ifTrue: [^ 0].
					^ from + index - 2]]! !

