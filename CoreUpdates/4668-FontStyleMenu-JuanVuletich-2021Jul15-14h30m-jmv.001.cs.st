'From Cuis 5.0 [latest update: #4666] on 15 July 2021 at 3:09:36 pm'!

!TextEditor methodsFor: 'attributes' stamp: 'jmv 7/15/2021 15:05:21'!
offerFontStyleMenu
	"This is a user command, and generates undo"

	| emphases menuStrings entries reply code startIndex attribute |
	startIndex _ self startIndex.
	code _ model actualContents emphasisAt: startIndex.
	emphases _ #(bold italic underlined struckThrough superscript subscript withST80Glyphs).
	menuStrings _ Array streamContents: [ :strm |
		strm nextPut:(code isZero ifTrue: ['<on>'] ifFalse: ['<off>']), 'normal'.
		emphases do: [ :emph |
			strm nextPut:
				((code anyMask: (TextEmphasis perform: emph) emphasisCode)
					ifTrue: ['<on>'] ifFalse: ['<off>']),
				emph asString ]].
	entries _ `#(normal)`, emphases.
	reply _ (SelectionMenu labelList: menuStrings lines: #(1) selections: entries) startUpMenu.
	reply ifNotNil: [
		attribute _ TextEmphasis perform: reply.
		((menuStrings at: (entries indexOf: reply)) beginsWith: '<on>')
			ifTrue: [ self unapplyAttributes: {attribute} ]
			ifFalse: [ self applyAttribute:  attribute ]].
	morph updateFromTextComposition.! !


!TextEditor methodsFor: 'menu' stamp: 'jmv 7/15/2021 15:03:50'!
addStyleMenuSectionTo: aMenu
	"Adds to the given menu text styiling related operations"
	
	aMenu
		addItemsFromDictionaries: 
	`{
		{
				#label 			-> 		'Toggle WordWrap'.
				#selector 			-> 		#wrapOnOff.
				#icon 			-> 		#genericTextIcon
			} asDictionary.
			{
				#label 			-> 		'Set Font... (k)'.
				#selector 			-> 		#offerFontMenu.
				#icon 			-> 		#preferencesDesktopFontIcon
			} asDictionary.
			{
				#label 			-> 		'Clear Font'.
				#selector 			-> 		#clearFont.
				#icon 			-> 		#newIcon
			} asDictionary.
			{
				#label 			-> 		'Set Font Style'.
				#selector 			-> 		#offerFontStyleMenu.
				#icon 			-> 		#preferencesDesktopFontIcon
			} asDictionary.
			{
				#label 			-> 		'Set Default Font...'.
				#selector 			-> 		#offerDefaultFontMenu.
				#icon 			-> 		#fontXGenericIcon
			} asDictionary.
			{
				#label 			-> 		'Set Alignment...'.
				#selector 			-> 		#chooseAlignment.
				#icon 			-> 		#formatJustifyLeftIcon
			} asDictionary.
		}`.
		^aMenu! !


!Preferences class methodsFor: 'halos' stamp: 'jmv 7/15/2021 15:07:49'!
iconicHaloSpecifications
	"Answer an array that characterizes the locations, colors, icons, and selectors of the halo handles that may be used in the iconic halo scheme"

	"
	Preferences resetHaloSpecifications
	"

^ #(
	"selector						horiz			vert			color info						icon key 						balloon help
	 ---------						------			-----------			--------------------------						--------------------------						---------------"
	(addCollapseHandle:						left			topCenter			(tan)						haloCollapseIcon 						'Collapse')
	(addDebugHandle:						right			topCenter			(orange)						haloDebugIcon 						'Debug')
	(addDismissHandle:						left			top			(red)						haloDismissIcon 						'Remove')
	(addRotateHandle:						left			bottom			(blue)						haloRotateIcon 						'Rotate')
	(addMenuHandle:						leftCenter			top			(blue lighter)						haloMenuIcon 						'Menu')
	(addGrabHandle:						center			top			(black)						haloGrabIcon 						'Pick up')
	(addDragHandle:						rightCenter			top			(brown)						haloDragIcon 						'Move')
	(addDupHandle:						right			top			(green)						haloDuplicateIcon 						'Duplicate')	
	(addHelpHandle:						center			bottom			(lightBlue)						haloHelpIcon 						'Help')
	(addResizeHandle:						right			bottom			(yellow)						haloResizeIcon 						'Change size')
	(addScaleHandle:						right			bottom			(blue)						haloScaleIcon 						'Change scale')
	"FIXME - Currently non-functional...
	(addRecolorHandle:						right			bottomCenter			(magenta darker)						haloColorIcon 						'Change color')
	"
)! !

!methodRemoval: InnerTextMorph #chooseFont stamp: 'jmv 7/15/2021 15:08:43'!
InnerTextMorph removeSelector: #chooseFont!
!methodRemoval: InnerTextMorph #chooseEmphasisOrAlignment stamp: 'jmv 7/15/2021 15:08:46'!
InnerTextMorph removeSelector: #chooseEmphasisOrAlignment!
!methodRemoval: InnerTextMorph #chooseEmphasis stamp: 'jmv 7/15/2021 15:02:15'!
InnerTextMorph removeSelector: #chooseEmphasis!
!methodRemoval: HaloMorph #chooseEmphasisOrAlignment stamp: 'jmv 7/15/2021 15:08:11'!
HaloMorph removeSelector: #chooseEmphasisOrAlignment!
!methodRemoval: HaloMorph #chooseFont stamp: 'jmv 7/15/2021 15:08:22'!
HaloMorph removeSelector: #chooseFont!
!methodRemoval: HaloMorph #addFontEmphHandle: stamp: 'jmv 7/15/2021 15:07:56'!
HaloMorph removeSelector: #addFontEmphHandle:!
!methodRemoval: HaloMorph #addFontSizeHandle: stamp: 'jmv 7/15/2021 15:08:01'!
HaloMorph removeSelector: #addFontSizeHandle:!
!methodRemoval: TextEditor #changeEmphasis stamp: 'jmv 7/15/2021 15:05:07'!
TextEditor removeSelector: #changeEmphasis!
!methodRemoval: TextEditor #changeEmphasisOrAlignment stamp: 'jmv 7/15/2021 15:08:52'!
TextEditor removeSelector: #changeEmphasisOrAlignment!
