'From Cuis 5.0 [latest update: #4786] on 22 August 2021 at 1:47:52 pm'!

!Object methodsFor: 'printing' stamp: 'jmv 8/22/2021 12:47:48'!
shortPrintString
	"Answer a String whose characters are a description of the receiver.
	This is a short one, good for showing users (for example, in ObjectExplorers).
	If you want to print without a character limit, use fullPrintString.

	This description is to be meaningful for a Smalltalk programmer and usually includes
	a hint on the class of the object.

	Usually you should not reimplement this method in subclasses, but #printOn:

	See the comments at:
		#printString
		#displayStringOrText
		#asString
		#storeString"

	^ self printStringLimitedTo: 64.! !


!Inspector methodsFor: 'user interface support' stamp: 'jmv 8/22/2021 12:47:43'!
displayLabel

	| label |
	object isObject ifFalse: [^ 'Inspect: ', self objectClass name].
	label := [object shortPrintString]
		on: UnhandledError
		do: [:ex | ex return: self objectClass printString, ' (printing failed)'].
	"Note this is ambiguous: what happens with (Array with: Array)?"
	(label includesSubString: self objectClass name) ifTrue: [^'Inspect: ', label].
	^ 'Inspect: ', self objectClass name, ': ', label! !


!MethodContext methodsFor: 'printing' stamp: 'jmv 8/22/2021 13:25:20'!
printDetails: strm
	"Put my class>>selector and instance variables and arguments and temporaries on the stream.  Protect against errors during printing."

	| pe str pos |
	self printOn: strm.
	strm newLine.
	strm tab; nextPutAll: 'Receiver: '.
	pe _ '<<error during printing>>'.
	strm nextPutAll: ([receiver shortPrintString] ifError: [:err :rcvr | pe]).

	strm newLine; tab; nextPutAll: 'Arguments and temporary variables: '; newLine.
	str _ [(self tempsAndValuesLimitedTo: 80 indent: 2) 
				padded: #right to: 1 with: $x] ifError: [:err :rcvr | pe].
	strm nextPutAll: (str allButLast).

	strm newLine; tab; nextPutAll: 'Receiver''s instance variables: '; newLine.
	pos _ strm position.
	[receiver longPrintOn: strm limitedTo: 80 indent: 2] ifError: [:err :rcvr | 
				strm nextPutAll: pe].
	pos = strm position ifTrue: ["normal printString for an Array (it has no inst vars)"
		strm nextPutAll: ([receiver shortPrintString] ifError: [:err :rcvr | pe])].
	strm peekLast isLineSeparator ifFalse: [strm newLine].! !


!Morph methodsFor: 'e-toy support' stamp: 'jmv 8/22/2021 13:23:01'!
unlockOneSubpart
	| unlockables aMenu reply |
	unlockables _ self submorphs select:
		[ :m | m isLocked].
	unlockables size <= 1 ifTrue: [^ self unlockContents].
	aMenu _ SelectionMenu labelList: (unlockables collect: [:m | m shortPrintString]) selections: unlockables.
	reply _ aMenu startUpWithCaption: 'Who should be be unlocked?'.
	reply ifNil: [^ self].
	reply unlock.! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 8/22/2021 13:22:30'!
editBalloonHelpContent: aString
	self
		request: 'Edit the balloon help text for ' , self shortPrintString
		initialAnswer: (aString ifNil: [self noHelpString] ifNotNil: [aString])
		do: [:reply|
			(reply isEmpty or: [reply asString = self noHelpString])
				ifTrue: [self setBalloonText: nil]
				ifFalse: [self setBalloonText: reply]]! !

!Morph methodsFor: 'menus' stamp: 'jmv 8/22/2021 13:25:37'!
addHaloActionsTo: aMenu 
	"Add items to aMenu representing actions requestable via halo"

	| subMenu |
	subMenu := MenuMorph new defaultTarget: self.
	subMenu addTitle: self shortPrintString.
	subMenu addStayUpIcons.
	subMenu addLine.
	(subMenu add: 'delete' action: #dismissViaHalo)
		setBalloonText: 'Delete this object -- warning -- can be destructive!!' .
	self maybeAddCollapseItemTo: subMenu.
	(subMenu add: 'grab' action: #openInHand)
		setBalloonText: 'Pick this object up -- warning, since this removes it from its container, it can have adverse effects.' .
	subMenu addLine.
	(subMenu add: 'duplicate' action: #maybeDuplicateMorph)
		setBalloonText: 'Hand me a copy of this object'.
	(subMenu
		add: 'set color'
		target: self
		action: #changeColor)
			setBalloonText: 'Change the color of this object'.
	(subMenu
		add: 'inspect'
		target: self
		action: #inspect)
			setBalloonText: 'Open an Inspector on this object'.
	aMenu add: 'halo actions...' subMenu: subMenu! !

!Morph methodsFor: 'menus' stamp: 'jmv 8/22/2021 13:26:12'!
addStandardHaloMenuItemsTo: aMenu hand: aHandMorph
	"Add standard halo items to the menu"

	| unlockables |

	self isWorldMorph ifTrue:
		[^ self addWorldHaloMenuItemsTo: aMenu hand: aHandMorph].

	aMenu add: 'send to back' action: #goBehind.
	aMenu add: 'bring to front' action: #comeToFront.
	self addEmbeddingMenuItemsTo: aMenu hand: aHandMorph.
	aMenu addLine.

	self addColorMenuItems: aMenu hand: aHandMorph.
	self addHaloActionsTo: aMenu.
	aMenu addLine.
	self addToggleItemsToHaloMenu: aMenu.
	aMenu addLine.
	self addCopyItemsTo: aMenu.
	self addExportMenuItems: aMenu hand: aHandMorph.
	self addDebuggingItemsTo: aMenu hand: aHandMorph.

	aMenu addLine.
	aMenu defaultTarget: self.

	aMenu addLine.

	unlockables _ self submorphs select:
		[:m | m isLocked].
	unlockables size = 1 ifTrue:
		[aMenu
			add: ('unlock "{1}"' format: unlockables first shortPrintString)
			action: #unlockContents].
	unlockables size > 1 ifTrue:
		[aMenu add: 'unlock all contents' action: #unlockContents.
		aMenu add: 'unlock...' action: #unlockOneSubpart].

	aMenu defaultTarget: aHandMorph.
! !

!Morph methodsFor: 'menus' stamp: 'jmv 8/22/2021 13:22:15'!
addTitleForHaloMenu: aMenu
	aMenu addTitle: self shortPrintString.! !


!WorldMorph methodsFor: 'menu & halo' stamp: 'jmv 8/22/2021 13:23:11'!
addWorldHaloMenuItemsTo: aMenu hand: aHandMorph
	"Add standard halo items to the menu, given that the receiver is a World"

	| unlockables |
	self addColorMenuItems: aMenu hand: aHandMorph.

"	aMenu addLine.
	self addWorldToggleItemsToHaloMenu: aMenu."
	aMenu addLine.
	self addCopyItemsTo: aMenu.
	self addExportMenuItems: aMenu hand: aHandMorph.

	self addDebuggingItemsTo: aMenu hand: aHandMorph.

	aMenu addLine.
	aMenu defaultTarget: self.

	aMenu addLine.

	unlockables _ self submorphs select:
		[ :m | m isLocked].
	unlockables size = 1 ifTrue: [
		aMenu add: ('unlock "{1}"' format:{unlockables first shortPrintString})action: #unlockContents].
	unlockables size > 1 ifTrue: [
		aMenu add: 'unlock all contents' action: #unlockContents.
		aMenu add: 'unlock...' action: #unlockOneSubpart].

	aMenu defaultTarget: aHandMorph.
! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 8/22/2021 13:23:24'!
addHandles: aDisplayRectangle
	"update my size. owner is world, therefore owner coordinates are Display coordinates."

	self morphPosition: aDisplayRectangle topLeft extent: aDisplayRectangle extent.
	haloBox _ self handlesBox.
	target addHandlesTo: self box: haloBox.
	self addNameString: target shortPrintString.
	self redrawNeeded.! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/22/2021 13:23:56'!
doDebug: evt with: menuHandle
	"Ask hand to invoke the a debugging menu for my inner target.  If shift key is down, immediately put up an inspector on the inner target"

	| menu |
	evt hand obtainHalo: self.
	evt shiftPressed ifTrue: [
		^ target inspect].

	menu _ target buildDebugMenu: evt hand.
	menu addTitle: target shortPrintString.
	menu popUpInWorld: self world! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/22/2021 13:24:15'!
doDup: evt with: dupHandle 
	"Ask hand to duplicate my target."

	| hand positionInHandCoordinates |
	target okayToDuplicate ifFalse: [^ self].
	hand _ evt hand.
	positionInHandCoordinates _ target morphPositionInWorld - hand morphPositionInWorld.
	"Duplicate has no meaningful position, as it is not in the world. Grab position from original!!"
	target _ target duplicateMorph: evt.
	self addNameString: target shortPrintString.	
	hand
		obtainHalo: self;
		grabMorph: target delta: positionInHandCoordinates! !


!ObjectExplorerWindow methodsFor: 'GUI building' stamp: 'jmv 8/22/2021 12:48:02'!
buildMorphicWindow

	| textMorph |
	listMorph _ HierarchicalListMorph
			model: model
			listGetter: #getList
			indexGetter: #getCurrentSelection
			indexSetter: #noteNewSelection:
			mainView: self
			menuGetter: #genericMenu
			keystrokeAction: #explorerKey:from:.
	listMorph autoDeselect: false.
	listMorph doubleClickSelector: #inspectSelection.
	textMorph _ ((TextModelMorph textProvider: model)
			askBeforeDiscardingEdits: false;
			emptyTextDisplayMessage: 'Smalltalk expression (self is selected item)').
	self layoutMorph
		addMorph: listMorph proportionalHeight: 0.8;
		addAdjusterAndMorph: textMorph proportionalHeight: 0.2.
	self setLabel: model rootObject shortPrintString.! !


!ObjectExplorerWrapper methodsFor: 'converting' stamp: 'jmv 8/22/2021 12:33:32'!
asString
	| explorerString |
	explorerString _ [ item shortPrintString ]
		on: UnhandledError 
		do: [:ex | ex return: '<error in printString: evaluate "' , itemName , ' printString" to debug>'].
	^itemName , ': ' , explorerString :: withBlanksCondensed! !


!TestCase methodsFor: 'assertions' stamp: 'jmv 8/22/2021 13:26:49'!
comparingStringBetween: expected and: actual
	^ String streamContents: [:stream |
		stream
			nextPutAll: 'Expected ';
			nextPutAll: expected shortPrintString;
			nextPutAll: ' but was ';
			nextPutAll: actual shortPrintString;
			nextPutAll: '.'
		]! !

