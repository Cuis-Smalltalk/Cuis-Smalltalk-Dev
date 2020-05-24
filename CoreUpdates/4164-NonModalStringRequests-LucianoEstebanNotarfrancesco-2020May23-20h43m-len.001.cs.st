'From Cuis 5.0 [latest update: #4163] on 23 May 2020 at 8:51:22 pm'!
!classDefinition: #StringRequestMorph category: #'Morphic-Widgets'!
BorderedRectMorph subclass: #StringRequestMorph
	instanceVariableNames: 'response acceptBlock cancelBlock validationBlock'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
!classDefinition: #ClassNameRequestMorph category: #'Morphic-Widgets'!
StringRequestMorph subclass: #ClassNameRequestMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!Object methodsFor: 'user interface' stamp: 'len 5/20/2020 04:00:44'!
request: queryString do: aBlock
	^ self request: queryString initialAnswer: '' verifying: [:aString| true] do: aBlock orCancel: []! !

!Object methodsFor: 'user interface' stamp: 'len 5/20/2020 06:32:35'!
request: queryString initialAnswer: defaultAnswer
	"This is deprecated because using it requires blocking the user interface until the user provides a response. Please use the variants that are not modal."
	^ self request: queryString initialAnswer: defaultAnswer orCancel: ['']! !

!Object methodsFor: 'user interface' stamp: 'len 5/20/2020 04:11:48'!
request: queryString initialAnswer: defaultAnswer do: aBlock
	^ self request: queryString initialAnswer: defaultAnswer verifying: [:aString| true] do: aBlock orCancel: []! !

!Object methodsFor: 'user interface' stamp: 'len 5/20/2020 03:59:45'!
request: queryString initialAnswer: defaultAnswer do: aBlock orCancel: cancelBlock
	^ self request: queryString initialAnswer: defaultAnswer verifying: [:aString| true] do: aBlock orCancel: cancelBlock! !

!Object methodsFor: 'user interface' stamp: 'len 5/20/2020 04:50:27'!
request: queryString initialAnswer: defaultAnswer orCancel: cancelBlock
	"This is deprecated because using it requires blocking the user interface until the user provides a response. Please use the variants that are not modal."
	^ StringRequestMorph request: queryString initialAnswer: defaultAnswer orCancel: cancelBlock! !

!Object methodsFor: 'user interface' stamp: 'len 5/20/2020 07:37:39'!
request: queryString initialAnswer: defaultAnswer verifying: validationBlock do: acceptBlock
	^ self request: queryString initialAnswer: defaultAnswer verifying: validationBlock do: acceptBlock orCancel: []! !

!Object methodsFor: 'user interface' stamp: 'len 5/20/2020 07:38:05'!
request: queryString initialAnswer: defaultAnswer verifying: validationBlock do: acceptBlock orCancel: cancelBlock
	^ StringRequestMorph request: queryString initialAnswer: defaultAnswer verifying: validationBlock do: acceptBlock orCancel: cancelBlock! !


!StringRequestMorph methodsFor: 'accessing' stamp: 'len 5/19/2020 06:58:15'!
emptyTextDisplayMessage: aString
	self
		setProperty: #emptyTextDisplayMessage
		toValue: aString.! !

!StringRequestMorph methodsFor: 'accessing' stamp: 'len 5/19/2020 08:23:39'!
response
	^ response
! !

!StringRequestMorph methodsFor: 'accessing' stamp: 'len 5/20/2020 04:06:50'!
response: aText
	"Sent when text pane accepts."
	response _ aText.
	validationBlock ifNotNil: [(validationBlock value: aText asString) ifFalse: [self flash. ^ false]].
	[acceptBlock ifNotNil: [acceptBlock value: aText asString]] ensure: [self delete].
	^ true! !

!StringRequestMorph methodsFor: 'drawing' stamp: 'len 5/19/2020 06:59:55'!
drawOn: aCanvas
	
	| roundCorners |
	roundCorners _ Theme current roundWindowCorners.
	roundCorners
		ifTrue: [
			aCanvas roundRect: self morphLocalBounds color: color radius: Theme current roundedWindowRadius ]
		ifFalse: [
			aCanvas fillRectangle: self morphLocalBounds color: color borderWidth: borderWidth borderStyleSymbol: #raised baseColorForBorder: color ]! !

!StringRequestMorph methodsFor: 'initialization' stamp: 'len 5/20/2020 04:03:27'!
acceptBlock: aBlock
	acceptBlock _ aBlock! !

!StringRequestMorph methodsFor: 'initialization' stamp: 'len 5/20/2020 09:00:16'!
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
		ifTrue: [result morphExtent: 32 @ 3 * AbstractFont default lineSpacing]
		ifFalse:
			[result hideScrollBarsIndefinitely;
				morphExtent: 18 @ 1 * AbstractFont default lineSpacing + (0@8).].
	self addMorphBack: result.
	^ result.! !

!StringRequestMorph methodsFor: 'initialization' stamp: 'len 5/19/2020 08:18:15'!
addTitle: aString
	| titleMorph s pp w |
	titleMorph _ RectangleLikeMorph new.
	titleMorph color: Theme current menuTitleBar.
	pp _ `8@2`.
	aString asString linesDo: [ :line |
		s _ StringMorph new
			contents: line;
			font: Preferences standardMenuFont bold.
		titleMorph addMorphBack: s position: pp.
		pp _ pp + (0@(s morphHeight+2)) ].
	w _ titleMorph submorphs inject: 0 into: [ :prev :each |
		prev max: each morphWidth ].
	titleMorph morphExtent: (w + 16) @ (pp y).
	self addMorphFront: titleMorph.! !

!StringRequestMorph methodsFor: 'initialization' stamp: 'len 5/20/2020 04:03:33'!
cancelBlock: aBlock
	cancelBlock _ aBlock! !

!StringRequestMorph methodsFor: 'initialization' stamp: 'len 5/19/2020 07:00:19'!
defaultBorderWidth
	^ (Theme current roundWindowCorners or: [Theme current minimalWindows])
		ifTrue: [0]
		ifFalse: [Preferences menuBorderWidth]! !

!StringRequestMorph methodsFor: 'initialization' stamp: 'len 5/19/2020 07:00:42'!
defaultColor
	^ Theme current menu! !

!StringRequestMorph methodsFor: 'initialization' stamp: 'len 5/20/2020 08:54:26'!
initialize
	super initialize.
	extent _ `20@10`.
	response _ ''! !

!StringRequestMorph methodsFor: 'initialization' stamp: 'len 5/19/2020 19:27:25'!
intoWorld: aWorld
	"The receiver has just appeared in a new world. Note:
		* aWorld can be nil (due to optimizations in other places)
		* owner is already set
		* owner's submorphs may not include receiver yet.
	Important: Keep this method fast - it is run whenever morphs are added."
	super intoWorld: aWorld.
	self adjustSubmorphsLayout.
"this doesnt work:	aWorld ifNotNil: [aWorld activeHand newKeyboardFocus: textPane]"! !

!StringRequestMorph methodsFor: 'initialization' stamp: 'len 5/19/2020 19:27:47'!
setQuery: queryString initialAnswer: initialAnswer
	response _ initialAnswer.
	self addTitle: queryString.
	self addTextPane! !

!StringRequestMorph methodsFor: 'initialization' stamp: 'len 5/20/2020 04:03:48'!
validationBlock: aBlock
	validationBlock _ aBlock! !

!StringRequestMorph methodsFor: 'private' stamp: 'len 5/19/2020 07:38:08'!
adjustSubmorphsLayout
	"Enlarge the width of submorphs as needed
	so all of them are have the same width, and no less than #minWidth.
	Also adjust their vertical position.
	Finally, set our own extent."
	
	| w p h |
	
	submorphs isEmpty ifTrue: [ ^self ].
	w _ submorphs inject: 0 into: [ :prev :each |
		prev max: each minItemWidth].

	w _ w + 1.
	p _ `5 @ 5`.
	submorphs do: [ :m |
		h _ m morphHeight.
		m morphPosition: p extent: w@h.
		p _ p + (0@(h + 1)) ].

	self morphExtent: w+4 @ p y + 5! !

!StringRequestMorph methodsFor: 'private' stamp: 'len 5/19/2020 08:55:51'!
cancel
	self delete.
	cancelBlock ifNotNil: [cancelBlock value]! !

!StringRequestMorph methodsFor: 'private' stamp: 'len 5/20/2020 05:01:41'!
getUserResponseOrCancel: aBlock
	"Wait for the user to accept or cancel, and answer the result string. Answers the empty string if the user cancels."
	"Details: This is invoked synchronously from the caller. In order to keep processing inputs and updating the screen while waiting for the user to respond, this method has its own version of the World's event loop."
	| w delay done canceled |
	w _ self world.
	w isNil ifTrue: [^ response asString].
	done _ false.
	canceled _ false.
"	textPane focusText."
	acceptBlock _ [:aString| done _ true].
	cancelBlock _ [done _ true. canceled _ true].
	delay _ Delay forMilliseconds: 10.
	[done not and: [self isInWorld]] whileTrue: [ w doOneMinimalCycleNow. delay wait ].
	self delete.
	w doOneMinimalCycleNow.
	canceled ifTrue: [^ aBlock value].
	^ response asString! !

!StringRequestMorph methodsFor: 'private' stamp: 'len 5/19/2020 06:58:15'!
selectionInterval
	^ 1 to: response size
! !


!StringRequestMorph class methodsFor: 'instance creation' stamp: 'len 5/20/2020 06:02:15'!
request: queryString centeredAt: aPoint initialAnswer: defaultAnswer validationBlock: validationBlock acceptBlock: acceptBlock cancelBlock: cancelBlock
	| answer |
	answer _ self new
		setQuery: queryString
		initialAnswer: defaultAnswer;
		validationBlock: validationBlock;
		acceptBlock: acceptBlock;
		cancelBlock: cancelBlock.
	self runningWorld addMorph: answer centeredNear: aPoint - self deltaToTextPane.
	^ answer! !

!StringRequestMorph class methodsFor: 'instance creation' stamp: 'len 5/20/2020 06:00:53'!
request: queryString initialAnswer: defaultAnswer do: acceptBlock
	^ self request: queryString centeredAt: self runningWorld activeHand morphPosition initialAnswer: defaultAnswer validationBlock: [:aString| true] acceptBlock: acceptBlock cancelBlock: []! !

!StringRequestMorph class methodsFor: 'instance creation' stamp: 'len 5/20/2020 04:48:06'!
request: queryString initialAnswer: defaultAnswer orCancel: cancelBlock
	| morph world |
	morph _ self new
		setQuery: queryString
		initialAnswer: defaultAnswer.
	(world _ self runningWorld) addMorph: morph centeredNear: world activeHand morphPosition - self deltaToTextPane.
	^ morph getUserResponseOrCancel: cancelBlock! !

!StringRequestMorph class methodsFor: 'instance creation' stamp: 'len 5/20/2020 07:38:12'!
request: queryString initialAnswer: defaultAnswer verifying: validationBlock do: acceptBlock orCancel: cancelBlock
	^ self request: queryString centeredAt: self runningWorld activeHand morphPosition initialAnswer: defaultAnswer validationBlock: validationBlock acceptBlock: acceptBlock cancelBlock: cancelBlock! !

!StringRequestMorph class methodsFor: 'private' stamp: 'len 5/19/2020 19:32:56'!
deltaToTextPane
	"Answer a distance to translate an instance of the receiver by when it is opened in the world in order to have the hand be over the text pane (so the text pane has focus)."
	^ 40@40! !


!CodeProvider methodsFor: 'categories' stamp: 'len 5/20/2020 08:08:08'!
categoryFromUserWithPrompt: aPrompt for: aClass
	"self new categoryFromUserWithPrompt: 'testing' for: SystemDictionary"

	|  labels myCategories reject lines newName menuIndex |
	labels _ OrderedCollection with: 'new...'.
	labels addAll: (myCategories _ aClass organization categories asArray copy sort:
		[ :a :b | a asLowercase < b asLowercase ]).
	reject _ myCategories asSet.
	reject
		add: ClassOrganizer nullCategory;
		add: ClassOrganizer default.
	lines _ OrderedCollection with: 1 with: (myCategories size + 1).

	aClass allSuperclasses do: [ :cls | | cats |
			cats _ cls organization categories reject: [ :cat | reject includes: cat].
			cats isEmpty ifFalse: [
				lines add: labels size.
				labels addAll: (cats asArray sort: [ :a :b | a asLowercase < b asLowercase]).
				reject addAll: cats]].

	(labels size = 1 or: [
		menuIndex _ (PopUpMenu labelArray: labels lines: lines)
		startUpWithCaption: aPrompt.
		menuIndex = 0 ifTrue: [^ nil].
		menuIndex = 1])
			ifTrue:[
				newName _ self request: 'New category name?' initialAnswer: 'Category-Name'.
				newName isEmpty ifTrue: [ ^nil ]]
			ifFalse: [ newName _ labels at: menuIndex ].
	^ newName ifNotNil: [ newName asSymbol ]! !


!Browser methodsFor: 'class functions' stamp: 'len 5/20/2020 07:23:01'!
copyClass
	| originalClass originalName copysName newDefinition newMetaDefinition newClass |
	selectedClassName ifNil: [^ self].
	originalClass _ self selectedClass.
	originalName _ originalClass name.
	self request: 'New class name?'
		initialAnswer: originalName
		verifying: [:aString| aString notEmpty and: [aString ~= originalName]]
		do: [:aString|
			copysName _ aString asSymbol.
			(Smalltalk includesKey: copysName)
				ifTrue: [self error: copysName , ' already exists'].
			newDefinition _ originalClass definition
				copyReplaceAll: originalName printString
				with: copysName printString.
			newClass _ Compiler evaluate: newDefinition logged: true.
			newMetaDefinition _ originalClass class definition
				copyReplaceAll: originalClass class name
				with: newClass class name.
			Compiler evaluate: newMetaDefinition logged: true.
			newClass copyAllCategoriesFrom: originalClass.
			newClass class copyAllCategoriesFrom: originalClass class.
			originalClass hasComment ifTrue: [newClass comment: originalClass comment].
			self classListIndex: 0.
			self changed: #classList]! !

!Browser methodsFor: 'message category functions' stamp: 'len 5/20/2020 07:00:28'!
newMethodCategoryNameIfNone: aNoneBlock

	| labels lines menuIndex newName reject |
	
	labels _ OrderedCollection with: 'new...'.
	reject _ Set new.
	reject
		addAll: self selectedClassOrMetaClass organization categories;
		add: ClassOrganizer nullCategory;
		add: ClassOrganizer default.
	lines _ OrderedCollection new.
	self selectedClassOrMetaClass allSuperclasses do: [:cls | | cats |
		cls = Object ifFalse: [
			cats _ cls organization categories reject:
				 [:cat | reject includes: cat].
			cats isEmpty ifFalse: [
				lines add: labels size.
				labels addAll: cats asArray sort.
				reject addAll: cats]]].
	newName _ (labels size = 1 or: [
		menuIndex _ (PopUpMenu labelArray: labels lines: lines)
		startUpWithCaption: 'Add Category'.
		menuIndex = 0 ifTrue: [^ aNoneBlock value].
		menuIndex = 1])
			ifTrue: [
				self request: 'New category name?'
					initialAnswer: 'Category-Name']
			ifFalse: [
				labels at: menuIndex].
	
	^ newName isEmpty
		ifTrue: aNoneBlock
		ifFalse: [newName asSymbol]! !

!Browser methodsFor: 'message category functions' stamp: 'len 5/20/2020 07:25:11'!
renameCategory
	"Prompt for a new category name and add it before the
	current selection, or at the end if no current selection"
	| oldIndex oldName newName |
	selectedClassName ifNil: [^ self].
	selectedMessageCategory ifNil: [ ^self ].
	oldIndex _ self messageCategoryListIndex.
	oldName _ self selectedMessageCategoryName.
	newName _ self
		request: 'New category name?'
		initialAnswer: oldName
		verifying: [:aString| aString notEmpty and: [aString ~= oldName]]
		do: [:aString|
			newName _ aString asSymbol.
			self classOrMetaClassOrganizer
				renameCategory: oldName
				toBe: newName.
			self classListIndex: self classListIndex.
			self messageCategoryListIndex: oldIndex.
			self changed: #messageCategoryList]! !

!Browser methodsFor: 'system category functions' stamp: 'len 5/20/2020 06:58:53'!
newSystemCategoryNameIfNone: aNoneBlock

	| newName |
	
	newName _ self
		request: 'New category name?'
		initialAnswer: 'Category-Name'.
	
	^newName isEmpty
		ifTrue: aNoneBlock 
		ifFalse: [newName asSymbol].! !

!Browser methodsFor: 'system category functions' stamp: 'len 5/20/2020 07:26:14'!
renameSystemCategory
	"Prompt for a new category name and add it before the
	current selection, or at the end if no current selection
	
	21-Mar-2012 jmv Note: This is not recorded appropriately in change sets.
	The easiest solution is to trigger #classRecategorized for all classes in the category.
	But this is not a real solution, as the resulting changeset would not do a rename,
	but create a new category (that would go to the bottom) with all the classes.
	
	In the meantime, disable the menu entry. This is not so important after all.
	"
	| oldIndex oldName newName |
	selectedSystemCategory ifNil: [ ^ self].  "no selection"
	oldIndex _ self systemCategoryListIndex.
	oldName _ selectedSystemCategory.
	newName _ self
		request: 'New category name?'
		initialAnswer: oldName
		verifying: [:aString| aString notEmpty and: [aString ~= oldName]]
		do: [:aString|
			newName _ aString asSymbol.
			systemOrganizer
				renameCategory: oldName
				toBe: newName.
			self systemCategoryListIndex: oldIndex.
			self changed: #systemCategoryList]! !


!MessageSet methodsFor: 'filtering' stamp: 'len 5/20/2020 06:47:08'!
filterToImplementorsOf
	"Filter the receiver's list down to only those items with a given selector."

	self request: 'Enter selector:' initialAnswer: '' verifying: [:aString| aString notEmpty] do: [:aString|
		Symbol hasInterned: aString withBlanksTrimmed ifTrue:
			[:aSymbol | 
				self filterFrom:
					[:aClass :aSelector |
						aSelector == aSymbol]]]! !

!MessageSet methodsFor: 'filtering' stamp: 'len 5/20/2020 06:44:54'!
filterToNotImplementorsOf
	"Filter the receiver's list down to only those items whose selector is NOT one solicited from the user."

	self request: 'Enter selector:' initialAnswer: '' verifying: [:aString| aString notEmpty] do: [:aString|
		Symbol hasInterned: aString withBlanksTrimmed ifTrue:
			[:aSymbol | 
				self filterFrom:
					[:aClass :aSelector |
						aSelector ~~ aSymbol]]]! !

!MessageSet methodsFor: 'filtering' stamp: 'len 5/20/2020 06:47:00'!
filterToNotSendersOf
	"Filter the receiver's list down to only those items which do not send a given selector."
	
	self request: 'Enter selector:' initialAnswer: '' verifying: [:aString| aString notEmpty] do: [:aString|
		Symbol hasInterned: aString withBlanksTrimmed ifTrue:
			[:aSymbol |
				self filterFrom:
					[:aClass :aSelector | | aMethod |
						(aMethod _ aClass compiledMethodAt: aSelector) isNil or:
							[(aMethod hasLiteralThorough: aSymbol) not]]]]! !

!MessageSet methodsFor: 'filtering' stamp: 'len 5/20/2020 06:47:19'!
filterToSendersOf
	"Filter the receiver's list down to only those items which send a given selector."
	
	self request: 'Enter selector:' initialAnswer: '' verifying: [:aString| aString notEmpty] do: [:aString|
		Symbol hasInterned: aString withBlanksTrimmed ifTrue:
			[:aSymbol | 
				self filterFrom:
					[:aClass :aSelector | | aMethod |
						(aMethod _ aClass compiledMethodAt: aSelector) notNil and:
							[aMethod hasLiteralThorough: aSymbol]]]]! !


!CodeFileBrowser methodsFor: 'class list' stamp: 'len 5/20/2020 07:28:17'!
renameClass
	| oldName newName |
	selectedClassName ifNil: [ ^self ].
	oldName _ self selectedClass name.
	self request: 'New class name?'
		initialAnswer: oldName
		verifying: [:aString| aString notEmpty and: [aString ~= oldName]]
		do: [:aString|
			newName _ aString asSymbol.
			(caseCodeSource classDictionary includesKey: newName)
				ifTrue: [self error: newName , ' already exists in the CodeFile'].
			systemOrganizer classify: newName under: selectedSystemCategory.
			systemOrganizer removeElement: oldName.
			caseCodeSource renameClass: self selectedClass to: newName.
			self changed: #classList.
			self classListIndex: ((systemOrganizer listAtCategoryNamed: selectedSystemCategory) indexOf: newName)]! !


!ChangeList methodsFor: 'menu actions' stamp: 'len 5/20/2020 04:12:13'!
fileOutCurrentVersionsOfSelections
	self request: 'Enter file name' initialAnswer: 'Filename.st' do: [:aString|
		aString asFileEntry writeStreamDo: [ :stream |
			stream timeStamp.
			self currentVersionsOfSelections do: [ :methodRef |
				methodRef actualClass
					printMethodChunk: methodRef methodSymbol
					withPreamble: true
					on: stream
					moveSource: false
					toFile: 0 ]]]! !

!ChangeList methodsFor: 'menu actions' stamp: 'len 5/20/2020 04:12:37'!
fileOutSelections
	self request: 'Enter file name' initialAnswer: 'Filename.st' do: [ :aString |
		aString asFileEntry writeStreamDo: [ :stream |
			stream timeStamp.
			listSelections with: changeList do: [ :selected :item |
				selected ifTrue: [ item fileOutOn: stream ]]]]! !


!ChangeSorter methodsFor: 'changeSet menu' stamp: 'len 5/20/2020 06:13:26'!
rename
	"Store a new name string into the selected ChangeSet.  reject duplicate name; allow user to back out"

	self request: 'New name for this change set' initialAnswer: myChangeSet name do: [:newName|
		(newName = myChangeSet name or: [newName size = 0])
			ifTrue: [Smalltalk beep]
			ifFalse:
				[(ChangeSet changeSetNamed: newName) notNil
					ifTrue: [self inform: 'Sorry that name is already used']
					ifFalse:
						[myChangeSet name: newName.
						self update.
						self changed: #mainButtonName.
						self changed: #relabel]]]! !


!Debugger methodsFor: 'context stack menu' stamp: 'len 5/20/2020 08:27:52'!
askForCategoryIn: aClass default: aDefaultCategory

	| categories index category classCategories |
	
	categories := OrderedCollection with: 'new ...'. 
	
	aClass isMeta ifTrue: [ categories add: Categorizer instanceCreation ].
	classCategories := aClass allMethodCategoriesIntegratedThrough: Object.
	aClass isMeta ifTrue: [ classCategories remove: Categorizer instanceCreation ifAbsent: []].
	
	categories addAll: classCategories.	
	index := PopUpMenu 
		withCaption: 'Please provide a good category for the new method!!'
		chooseFrom: categories.
	index = 0 ifTrue: [^ aDefaultCategory].
	category := index = 1 
		ifTrue: [self request: 'Enter category name:' initialAnswer: '']
		ifFalse: [categories at: index].
		
	^ category isEmpty ifTrue: [aDefaultCategory] ifFalse: [category]! !


!FileList methodsFor: 'file menu action' stamp: 'len 5/20/2020 06:39:43'!
addNew: aString byEvaluating: aBlock
	"A parameterization of earlier versions of #addNewDirectory and
	#addNewFile.  Fixes the bug in each that pushing the cancel button
	in the FillInTheBlank dialog gave a walkback."

	| newName index |
	self request: ('New {1} name?' format: {aString})
		initialAnswer: ('{1}Name' format: {aString})
		verifying: [:response| response notEmpty]
		do: [:response|
			newName _ response asFileName.
			aBlock value: newName.
			self updateFileList.
			index _(1 to:  list size) detect: [ :i |
				(list at: i) includesSubString: newName ] ifNone: [ 0 ].
			self fileListIndex: index]! !

!FileList methodsFor: 'file menu action' stamp: 'len 5/20/2020 06:39:51'!
addNewDirectory

	self 
		addNew: 'directory'
		byEvaluating: [ :newName | (directory / newName) assureExistence ].
	self updateDirectory.
	self changed: #initialDirectoryList! !

!FileList methodsFor: 'file menu action' stamp: 'len 5/20/2020 06:39:54'!
addNewFile

	self 
		addNew: 'file'
		byEvaluating: [ :newName | (directory // newName) assureExistence ]
! !

!FileList methodsFor: 'file menu action' stamp: 'len 5/20/2020 06:39:31'!
renameFile
	"Rename the currently selected file"
	listIndex = 0 ifTrue: [^ self].
	self request: 'New file name?'
 		initialAnswer: fileName
		verifying: [:response| response notEmpty and: [response asFileName ~= fileName]]
		do: [:response|
			| newName |
			newName _ response asFileName.
			directory // fileName rename: newName.
			self updateFileList.
			listIndex _ list findFirst: [:item | (self fileNameFromFormattedItem: item) = newName].
			listIndex > 0 ifTrue: [fileName _ newName].
			self changed: #fileListIndex.
			self triggerEvent: #updateButtonRow]! !


!TestRunner methodsFor: 'menus' stamp: 'len 5/20/2020 04:15:57'!
setFilter
	self
		request: 'Pattern for added test cases (#* OK)'
		initialAnswer: '*'
		do: [:aString|
			filter _ aString.
			(filter endsWith: '*') ifFalse: [ filter _ filter, '*' ].
			selectedSuites _ (tests asOrderedCollection with: selectedSuites collect: [ :ea :sel |
				sel or: [ filter match: ea asString ]
			]).
			selectedSuite _ selectedSuites indexOf: true ifAbsent: [0].
			self changed: #allSelections]! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'len 5/20/2020 06:20:10'!
condenseSources	
	"Move all the changes onto a compacted sources file."
	"Smalltalk condenseSources"

	| classCount oldChanges oldChangesLocalName oldChangesPathName newChangesPathName newSourcesName |
	self request: 'Please name the new sources file' initialAnswer: SourceFileVersionString verifying: [:newVersionString| newVersionString ~= SourceFileVersionString] do: [:newVersionString|
		SourceFileVersionString _ newVersionString.

		"Write all sources with fileIndex 1"
		newSourcesName _ self defaultSourcesName.
		newSourcesName asFileEntry writeStreamDo: [ :f |
			f timeStamp.
			'Condensing Sources File...'
				displayProgressAt: Sensor mousePoint
				from: 0 to: Smalltalk classNames size
				during: [ :barBlock |
					classCount _ 0.
					Smalltalk allClassesDo: [ :class |
						barBlock value: (classCount _ classCount + 1).
						class fileOutOn: f moveSource: true toFile: 1]]].

		CompiledMethod allInstancesDo: [ :e | 
			e isInstalled ifFalse: [ e destroySourcePointer ] ].

		"Make a new empty changes file"
		oldChanges _ SourceFiles at: 2.
		oldChangesPathName _ oldChanges name.
		oldChangesLocalName _ oldChanges localName.
		self closeSourceFiles.
		oldChangesPathName ifNotNil: [
			(oldChangesPathName, '.old') asFileEntry delete.
		oldChangesPathName asFileEntry rename: oldChangesLocalName, '.old' ].
		newChangesPathName _ self defaultChangesName.
		newChangesPathName asFileEntry writeStreamDo: [ :stream |
			stream timeStamp ].
		LastQuitLogPosition _ 0.

		self openSourceFiles.
		self inform: 'Source files have been rewritten!!
 
Check that all is well, and then save/quit.
 
Otherwise, remove new sources/changes,
replace them with the former ones, and
exit without saving the image.
 ']! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'len 5/20/2020 08:37:42'!
saveAs
	"Put up the 'saveAs' prompt, obtain a name, and save the image  under that new name."

	self request: 'New file name?' initialAnswer: self imageName asFileEntry name do: [:newName|
		((((self fullNameForImageNamed: newName) asFileEntry exists not
			and: [(self fullNameForChangesNamed: newName) asFileEntry exists not])
			or: [self confirm: ('{1} already exists. Overwrite?' format: {newName})])
				and: [self okayToSave])
					ifTrue:
						[self saveAs: newName andQuit: false clearAllClassState: false]]! !


!StandardFileStream class methodsFor: 'error handling' stamp: 'len 5/20/2020 08:30:22'!
fileDoesNotExistUserHandling: fullFileName

	| selection newName |
	selection _ (PopUpMenu labels:
'create a new file
choose another name
cancel')
			startUpWithCaption: fullFileName asFileEntry name, '
does not exist.'.
	selection = 1 ifTrue:
		[^ self new open: fullFileName forWrite: true].
	selection = 2 ifTrue:
		[ newName _ self request: 'Enter a new file name'
						initialAnswer:  fullFileName.
		^ FileIOAccessor default privateWriteableFile: newName asFileEntry ].
	self halt! !

!StandardFileStream class methodsFor: 'error handling' stamp: 'len 5/20/2020 08:30:28'!
fileExistsUserHandling: fullFileName
	| dir localName choice newName entry |
	entry _ fullFileName asFileEntry.
	dir _ entry parent.
	localName _ entry name.
	choice _ (PopUpMenu
		labels:
'overwrite that file\choose another name\cancel' withNewLines)
		startUpWithCaption: localName, '
already exists.'.

	choice = 1 ifTrue: [
		dir removeKey: localName
			ifAbsent: [self error: 'Could not delete the old version of that file'].
		^ self new open: fullFileName forWrite: true].

	choice = 2 ifTrue: [
		newName _ self request: 'Enter a new file name' initialAnswer: fullFileName.
		^ FileIOAccessor default privateNewFile: newName asFileEntry ].

	self error: 'Please close this to abort file opening'! !

!StandardFileStream class methodsFor: 'error handling' stamp: 'len 5/20/2020 08:31:13'!
readOnlyFileDoesNotExistUserHandling: fullFileName

	| dir files choices selection newName fileName |
	dir _ fullFileName asFileEntry parent.
	files _ dir fileNames.
	fileName _ fullFileName asFileEntry name.
	choices _ fileName correctAgainst: files.
	choices add: 'Choose another name'.
	choices add: 'Cancel'.
	selection _ (PopUpMenu labelArray: choices lines: (Array with: 5) )
		startUpWithCaption: fullFileName asFileEntry name, '
does not exist.'.
	selection = choices size ifTrue:["cancel" ^ nil "should we raise another exception here?"].
	selection < (choices size - 1) ifTrue: [
		newName _ (dir pathName , '/', (choices at: selection))].
	selection = (choices size - 1) ifTrue: [
		newName _ self request: 'Enter a new file name' initialAnswer: fileName.
		"If Cancel was pressed, no file should be opened - Hernan"
		newName isEmpty ifTrue: [ ^nil ]].
	newName = '' ifFalse: [^ FileIOAccessor default privateReadOnlyFile: newName asFileEntry ].
	^ self error: 'Could not open a file'! !


!SmartRefStream methodsFor: 'class changed shape' stamp: 'len 5/20/2020 08:30:12'!
writeClassRenameMethod: sel was: oldName fromInstVars: oldList 
	"The class coming is unknown.  Ask the user for the existing class it maps to.  If got one, write a method, and restart the obj fileIn.  If none, write a dummy method and get the user to complete it later.  "

	| tell choice newName answ code oldVer newList newVer instSel |
	self flag: #bobconv.
	tell := 'Reading an instance of ' , oldName 
				, '.
Which modern class should it translate to?'.
	answ := (PopUpMenu 
				labels: 'Let me type the name now
Let me think about it
Let me find a conversion file on the disk') 
					startUpWithCaption: tell.
	answ = 1 
		ifTrue: [
			tell := 'Name of the modern class {1} should translate to:' format: {oldName}.
			choice := self request: tell initialAnswer: ''.	"class name"
			choice size = 0 
				ifTrue: [answ := 'conversion method needed']
				ifFalse: 
					[newName := choice.
					answ := Smalltalk at: newName asSymbol
								ifAbsent: ['conversion method needed'].
					answ class == String 
						ifFalse: [renamed at: oldName asSymbol put: answ name]]].
	answ = 3 | (answ = 0) 
		ifTrue: [
			byteStream close.
			^'conversion method needed'].
	answ = 2 ifTrue: [answ := 'conversion method needed'].
	answ = 'conversion method needed' 
		ifTrue: [
			byteStream close.
			newName := 'PutNewClassHere'].
	answ class == String 
		ifFalse: 
			[oldVer := self versionSymbol: (structures at: oldName).
			newList := (Array with: answ classVersion) , answ allInstVarNames.
			newVer := self versionSymbol: newList.
			instSel := 'convert' , oldVer , ':' , newVer , ':'].
	code := WriteStream on: (String new: 500).
	code
		nextPutAll: sel;
		newLine.
	answ class == String 
		ifFalse: [
			code
				newLine;
				tab;
				nextPutAll: 'reshaped at: #' , oldName , ' put: #' , instSel , '.'.
			code
				newLine;
				tab;
				tab;
				nextPutAll: '"Be sure to define that conversion method in class ' 
							, answ name , '"'].
	code
		newLine;
		tab;
		nextPutAll: '^ ' , newName.	"Return new class"
	self class compile: code contents classified: 'conversion'.
	newName = 'PutNewClassHere' 
		ifTrue: [
			self 
				inform: 'Please complete the following method and 
then read-in the object file again.'.
			Smalltalk browseAllImplementorsOf: sel asSymbol].
	self flag: #violateBasicLayerPrinciples.
	"SmartRefStream should not refer to UI!!!!!!!!!! (sd)"

	"The class version number only needs to change under one specific circumstance.  That is when the first letters of the instance variables have stayed the same, but their meaning has changed.  A conversion method is needed, but this system does not know it.  
	If this is true for class Foo, define classVersion in Foo class.  
	Beware of previous object fileouts already written after the change in meaning, but before bumping the version number.  They have the old (wrong) version number, say 2.  If this is true, your method must be able to test the data and successfully read files that say version 2 but are really 3."
	^answ! !


!Parser methodsFor: 'error correction' stamp: 'len 5/20/2020 08:28:19'!
defineClass: className
	"prompts the user to define a new class,  
	asks for it's category, and lets the users edit further  
	the definition"
	| sym cat def d2 |
	sym := className asSymbol.
	cat := self request: 'Enter class category : ' initialAnswer: self encoder classEncoding theNonMetaClass category.
	cat
		ifEmpty: [cat := 'Unknown'].
	def := 'Object subclass: #' , sym , '
		instanceVariableNames: '''' 
		classVariableNames: ''''
		poolDictionaries: ''''
		category: ''' , cat , ''''.
	d2 := self request: 'Edit class definition : ' initialAnswer: def.
	d2
		ifEmpty: [d2 := def].
	Compiler evaluate: d2.
	^ encoder
		global: (Smalltalk associationAt: sym)
		name: sym! !


!TextEditor methodsFor: 'menu messages' stamp: 'len 5/20/2020 06:23:28'!
find
	"Prompt the user for a string to search for, and search the receiver from the current selection onward for it.  1/26/96 sw"

	self
		request: 'Find what?'
		initialAnswer: self class findText
		do: [:aString|
			aString isEmpty ifFalse:
				["Set focus on our text morph, so that cmd-g does the search again"
				morph world activeHand newKeyboardFocus: morph.
				self setSearch: aString.
				ChangeText _ self class findText.  "Implies no replacement to againOnce: method"
				(self findAndReplaceMany: false)
					ifFalse: [ self flash ]]].

"	morph installEditorToReplace: self"! !


!Utilities class methodsFor: 'identification' stamp: 'len 5/22/2020 04:30:45'!
setAuthor
	"Put up a dialog allowing the user to specify the author's initials.
	Utilities setAuthor
	"
	| authorName |
	AuthorInitials _ (self
		request: 'Please type your initials: '
		initialAnswer: (AuthorInitials ifNil: [''])) withBlanksTrimmed.
	authorName _ (Smalltalk knownInitialsAndNames
		detect: [ :pair |
			pair first = AuthorInitials ]
		ifNone: [
			AuthorName _ (self
				request: 'Please type your name:'
				initialAnswer: 'Your Name') withBlanksTrimmed.
			^ self ]) second withBlanksTrimmed.
	(self confirm: 'Are you ' , authorName , '?')
		ifTrue: [ AuthorName _ authorName ]
		ifFalse: [
			self inform: 'Please enter different initials, then'.
			self setAuthor ]! !


!Morph methodsFor: 'halos and balloon help' stamp: 'len 5/20/2020 05:09:58'!
editBalloonHelpContent: aString
	self
		request: 'Edit the balloon help text for ' , (self printStringLimitedTo: 40)
		initialAnswer: (aString ifNil: [self noHelpString] ifNotNil: [aString])
		do: [:reply|
			(reply isEmpty or: [reply asString = self noHelpString])
				ifTrue: [self setBalloonText: nil]
				ifFalse: [self setBalloonText: reply]]! !

!Morph methodsFor: 'menus' stamp: 'len 5/20/2020 06:16:32'!
exportAsBMP
	"Export the receiver's image as a BMP."
	self request: 'Enter file name'
		initialAnswer: (self printStringLimitedTo: 20),'.bmp'
		do: [:aString| (self imageForm: 32) writeBMPfileNamed: aString]! !

!Morph methodsFor: 'menus' stamp: 'len 5/20/2020 06:16:41'!
exportAsJPEG
	"Export the receiver's image as a JPEG."
	self request: 'Enter file name'
		initialAnswer: (self printStringLimitedTo: 20),'.jpeg'
		do: [:aString| (self imageForm: 32) writeJPEGfileNamed: aString]! !


!SystemWindow methodsFor: 'label' stamp: 'len 5/20/2020 06:20:49'!
relabel
	self request: 'New title for this window' initialAnswer: labelString verifying: [:aString| aString notEmpty] do: [:aString| self setLabel: aString]! !

!SystemWindow methodsFor: 'user interface' stamp: 'len 5/20/2020 06:57:02'!
saveContents
	"Prompts the user for a file name and saves the contents to the file"
	self hasSaveAs ifFalse: [^self].
	self request: 'Enter file name' initialAnswer: '' verifying: [:aString| aString notEmpty] do: [:fileName| self saveContentsTo: fileName]! !


!CodePackageListWindow methodsFor: 'commands' stamp: 'len 5/20/2020 04:08:36'!
createPackage
	self request: 'Name for new package?' do: [:aString|
		aString ifNotEmpty:
			[CodePackage
				named: aString
				createIfAbsent: true
				registerIfNew: true]]! !


!CodeWindow methodsFor: 'misc' stamp: 'len 5/20/2020 08:09:08'!
getSelectorAndSendQuery: querySelector to: queryPerformer with: queryArgs
	"Obtain a selector relevant to the current context, and then send the querySelector to the queryPerformer with the selector obtained and queryArgs as its arguments.  If no message is currently selected, then obtain a method name from a user type-in"

	| strm array |
	strm _ WriteStream on: (array _ Array new: queryArgs size + 1).
	strm nextPut: nil.
	strm nextPutAll: queryArgs.

	model selectedMessageName ifNil: [ | selector |
		selector _ self request: 'Type selector:' initialAnswer: 'flag:'.
		^ selector isEmpty ifFalse: [
			(Symbol hasInterned: selector
				ifTrue: [ :aSymbol |
					array at: 1 put: aSymbol.
					queryPerformer perform: querySelector withArguments: array])
				ifFalse: [ self inform: 'no such selector']
		]
	].

	self selectMessageAndEvaluate: [:selector |
		array at: 1 put: selector.
		queryPerformer perform: querySelector withArguments: array
	]! !

!CodeWindow methodsFor: 'misc' stamp: 'len 5/20/2020 08:15:43'!
sendQuery: querySelector to: queryPerformer
	"Apply a query to the primary selector associated with the current context.  If no such selection exists, obtain one by user type-in. Then send querySelector to queryPerformer with the selector as its argument."

	model selectedMessageName ifNotNil: [^ queryPerformer perform: querySelector with: querySelector].
	self request: 'Type selector:' initialAnswer: 'flag:' verifying: [:aString| aString notEmpty] do: [:aString|
		(Symbol hasInterned: aString ifTrue: [ :aSymbol | queryPerformer perform: querySelector with: aSymbol])
			ifFalse: [self inform: 'no such selector']]! !

!CodeWindow methodsFor: 'misc' stamp: 'len 5/20/2020 08:15:27'!
useSelector: incomingSelector orGetSelectorAndSendQuery: querySelector to: queryPerformer
	"If incomingSelector is not nil, use it, else obtain a selector from user type-in.   Using the determined selector, send the query to the performer provided."

	incomingSelector ifNotNil: [^ queryPerformer perform: querySelector with: incomingSelector].
	self request: 'Type selector:' initialAnswer: 'flag:' verifying: [:aString| aString notEmpty] do: [:aString|
		(Symbol hasInterned: aString ifTrue: [:aSymbol| queryPerformer perform: querySelector with: aSymbol])
			ifFalse: [self inform: 'no such selector']]! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'len 5/19/2020 15:57:32'!
buildMorphicWindow
	"Create a pluggable version of all the morphs for a Browser in Morphic"

	| upperPanes messageCatList messageList systemCatList classList |

	systemCatList _ self buildMorphicSystemCatList.
	classList _ self buildMorphicClassList.
	messageCatList _ self buildMorphicMessageCatList.
	messageList _ self buildMorphicMessageList.
	
	systemCatList rightSibling: classList.
	classList leftSibling: systemCatList rightSibling: messageCatList.
	messageCatList leftSibling: classList rightSibling: messageList.
	messageList leftSibling: messageCatList.
	
"	classList makeItemsDraggable."
	systemCatList 
		acceptDropsFrom: classList 
		performing: #categorizeUnderCategoryAt:class:
		whenOutsideList: #categorizeUnderNewCategoryClass:.
		
"	messageList makeItemsDraggable."
	messageCatList 
		acceptDropsFrom: messageList 
		performing: #categorizeUnderCategoryAt:selector: 
		whenOutsideList: #categorizeUnderNewCategorySelector:.
	
	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: systemCatList proportionalWidth: 0.2;
		addAdjusterAndMorph: (self buildMorphicClassColumnWith: classList) proportionalWidth: 0.2;
		addAdjusterAndMorph: messageCatList proportionalWidth: 0.2;
		addAdjusterAndMorph: messageList proportionalWidth: 0.4.

	self layoutMorph
		addMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.

	model changed: #editSelection! !

!BrowserWindow methodsFor: 'GUI building' stamp: 'len 5/19/2020 15:57:45'!
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

"	messageList makeItemsDraggable."
	messageCatList 
		acceptDropsFrom: messageList 
		performing: #categorizeUnderCategoryAt:selector: 
		whenOutsideList: #categorizeUnderNewCategorySelector:.

	self layoutMorph
		addMorph: mySingletonList fixedHeight: Preferences standardCodeFont lineSpacing + 10;
		addAdjusterAndMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.

	model changed: #editSelection! !


!BrowserWindow class methodsFor: 'GUI building' stamp: 'len 5/20/2020 06:06:03'!
findClassFrom: potentialClassNames ifFound: aBlock
	| classNames exactMatch foundClass index toMatch |
	ClassNameRequestMorph request: 'Class name or fragment?' initialAnswer: '' do: [:pattern|
		pattern isEmpty
			ifTrue: [self flash]
			ifFalse: 
				[toMatch _ (pattern copyWithout: $.) asLowercase withBlanksTrimmed.
				classNames _ (pattern last = $. or: [pattern last = $ ])
					ifTrue: [potentialClassNames select: [:nm |  nm asLowercase = toMatch]]
					ifFalse: [potentialClassNames select: [:n | n includesSubstring: toMatch caseSensitive: false]].
				classNames isEmpty
					ifTrue: [self flash]
					ifFalse:
						[exactMatch _ classNames detect: [ :each | each asLowercase = toMatch] ifNone: nil.

						index _ classNames size = 1
							ifTrue: [ 1 ]
							ifFalse: [ exactMatch
								ifNil: [(PopUpMenu labelArray: classNames lines: #()) startUpMenu]
								ifNotNil: [classNames addFirst: exactMatch.
									(PopUpMenu labelArray: classNames lines: #(1)) startUpMenu]].

						index = 0
							ifTrue: [self flash]
							ifFalse:
								[foundClass _ Smalltalk at: (classNames at: index) asSymbol.
 	
								aBlock value: foundClass]]]]! !


!CodeFileBrowserWindow methodsFor: 'commands' stamp: 'len 5/20/2020 06:54:25'!
findClass
	| pattern foundClass classNames index foundCodeFile |
	self okToChange ifFalse: [^ self flash].
	self request: 'Class name?' do: [:aString|
		aString isEmpty ifFalse:
			[pattern _ aString asLowercase.
			classNames _ Set new.
			classNames addAll: model caseCodeSource classDictionary keys.
			classNames _ classNames asArray select: 
				[:n | (n asLowercase indexOfSubCollection: pattern startingAt: 1) > 0].
			classNames isEmpty ifFalse:
				[index _ classNames size = 1
					ifTrue: [1]
					ifFalse: [(PopUpMenu labelArray: classNames lines: #()) startUpMenu].
				index = 0 ifFalse:
					[foundCodeFile _ nil.
					foundClass _ nil.
					(model caseCodeSource classDictionary includesKey: (classNames at: index))
						ifTrue:
							[foundClass := model caseCodeSource classDictionary at: (classNames at: index).
							foundCodeFile := model caseCodeSource].
					foundClass ifNotNil:
						[model systemCategoryListIndex: (model systemCategoryList indexOf: foundCodeFile name asSymbol).
						model classListIndex: (model classList indexOf: foundClass name)]]]]]! !


!MessageSetWindow class methodsFor: 'opening' stamp: 'len 5/20/2020 04:09:03'!
findInSourceCode
	self request: 'Text to search source code for?' do: [:aString|
		Smalltalk browseMethodsWithSourceString: aString]! !


!ChangeSelectorWizardStepWindow methodsFor: 'actions' stamp: 'len 5/20/2020 05:59:49'!
do: aBlock withEnteredClassLabeled: aLabel
	ClassNameRequestMorph
		request: aLabel
		initialAnswer: ''
		do: [:className| self withClassNamed: className do: aBlock]! !


!ChangeSelectorSendersStepWindow methodsFor: 'actions' stamp: 'len 5/20/2020 08:07:14'!
askAndAddSenderOf: classOfSenderToAdd 
		
	| senderSelector senderToAdd |
	
	senderSelector := self request: 'Selector of sender of #', self oldSelector initialAnswer: '' orCancel: [^self ].
	senderToAdd := classOfSenderToAdd 
		compiledMethodAt: senderSelector asSymbol
		ifAbsent: [ ^self inform: classOfSenderToAdd doesNotImplement: senderSelector asSymbol].
		
	(senderToAdd sendsOrRefersTo: self oldSelector) ifFalse: [ ^self inform: senderToAdd classAndSelector, ' does not refer to #', self oldSelector ].
	
	self addToList: senderToAdd ! !


!DebuggerWindow methodsFor: 'menu commands' stamp: 'len 5/20/2020 05:04:59'!
returnValue
	self request: 'Enter expression for return value:' do: [:aString| model returnValue: aString]! !


!InspectorWindow methodsFor: 'menu commands' stamp: 'len 5/20/2020 05:08:06'!
addEntry
	self request:
'Enter new key, then type RETURN.
(Expression will be evaluated for value.)
Examples:  #Fred    ''a string''   3+4'
		do: [:aString|
			model addEntry: (Compiler evaluate: aString)]! !

!InspectorWindow methodsFor: 'menu commands' stamp: 'len 5/20/2020 06:15:29'!
renameEntry
	self request: 
'Enter new key, then type RETURN.
(Expression will be evaluated for value.)
Examples:  #Fred    ''a string''   3+4'
		initialAnswer: model selectedKey printString
		do: [:aString| aString isEmpty ifFalse: [model renameEntryTo: (Compiler evaluate: aString)]]! !


!ProcessBrowserWindow methodsFor: 'menu commands' stamp: 'len 5/20/2020 07:34:54'!
changePriority
	| newPriority rule |
	rule _ (model class rulesFor: model selectedProcess) second.
	rule
		ifFalse: [self inform: 'Nope, won''t change priority of ' , model selectedProcess name.
			^ self].
	self request: 'New priority'
		initialAnswer: model selectedProcess priority asString
		verifying: [:aString| ([aString asNumber asInteger] on: Error do: []) isInteger]
		do: [:aString|
			newPriority _ aString asNumber asInteger.
			(newPriority between: 1 and: Processor highestPriority)
			ifTrue:
				[model class setProcess: model selectedProcess toPriority: newPriority.
				model updateProcessList]
			ifFalse: [self inform: 'Bad priority']]! !

!ProcessBrowserWindow methodsFor: 'menu commands' stamp: 'len 5/20/2020 07:35:42'!
findContext
	self request: 'Enter a string to search for in the process stack lists'
		initialAnswer: model searchString
		do: [:searchString| model findContext: searchString]! !


!ImageMorph methodsFor: 'menu commands' stamp: 'len 5/20/2020 06:17:35'!
readFromFile
	self request: 'Enter file name'
		initialAnswer: 'fileName'
		do: [:fileName| self image: (Form fromFileNamed: fileName)]! !


!TheWorldMenu methodsFor: 'commands' stamp: 'len 5/20/2020 04:09:37'!
saveWorldInFile
	"Save the world's submorphs, model, and stepList in a file.  "

	self request: 'File name for this morph?' do: [ :fileName |
		fileName isEmpty ifTrue: [^ self].  "abort"
		"Save only model, stepList, submorphs in this world"
		myWorld submorphsDo: [ :m | m allMorphsDo: [ :subM | subM prepareToBeSaved ]].	"Amen"
		(fileName, '.morph') asFileEntry writeStreamDo: [ :fileStream |
			fileStream fileOutObject: myWorld ]]! !


!TextComposition methodsFor: 'display' stamp: 'len 5/19/2020 05:56:06'!
displaySelectionStartBlock: startBlock stopBlock: stopBlock InLine: line on: aCanvas textTopLeft: textTopLeft  selectionColor: sc
	"textTopLeft is relative to the morph currently being drawn"

	| leftX rightX idx textCursorFont t b textCursorAttributes |

	startBlock ifNil: [^self].	"No selection"
	startBlock = stopBlock 
		ifTrue: [
			"Only show text cursor on line where clicked"
			startBlock textLine first = line first ifFalse: [
				^self ].
			showTextCursor ifTrue: [
				leftX _ textTopLeft x + startBlock left.
				idx _ startBlock stringIndex.
				textCursorAttributes _ editor ifNotNil: [ editor currentAttributes ].
				textCursorFont _ textCursorAttributes
					ifNil: [ model actualContents fontAt: idx default: self defaultFont ]
					ifNotNil: [ model actualContents fontIfApplying: textCursorAttributes default: self defaultFont ].
				b _ textTopLeft y + line top + line baseline + textCursorFont descent-1.
				t _ textTopLeft y + line top + line baseline - textCursorFont ascent+1.
				lastTextCursorRect _ nil.
				self
					displayTextCursorAtX: leftX
					top: t
					bottom: b
					emphasis: textCursorFont emphasis
					on: aCanvas
					textLeft: textTopLeft x ]]
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
			lastTextCursorRect _ nil.
			leftX _  textTopLeft x + (startBlock stringIndex < line first 
				ifTrue: [ line ]
				ifFalse: [ startBlock ]) left.
			rightX _  textTopLeft x + ((stopBlock stringIndex > (line last + 1) or: [
					stopBlock stringIndex = (line last + 1) 
						and: [stopBlock textLine ~= line]]) 
				ifTrue: [line right]
				ifFalse: [stopBlock left]).
			aCanvas
				fillRectangle: (leftX @ (line top +  textTopLeft y) corner: rightX @ (line bottom +  textTopLeft y))
				color: sc ].	"Selection begins on line below"! !


!CodeFile methodsFor: 'fileIn/fileOut' stamp: 'len 5/20/2020 04:15:19'!
fileOut
	self request: 'Enter the file name' initialAnswer: '' do: [ :aString |
		aString asFileEntry writeStreamDo: [ :stream | 
			sourceSystem isEmpty
				ifFalse: [ stream nextChunkPut: sourceSystem printString; newLine ].
			self fileOutOn: stream.
			stream newLine; newLine.
			classes do: [ :cls |
				cls needsInitialize
					ifTrue: [ stream newLine; nextChunkPut: cls name,' initialize']].
			stream newLine ]]! !


!RefactoringApplier methodsFor: 'request information' stamp: 'len 5/20/2020 05:00:03'!
request: aLabel initialAnswer: anAnswer
	^self request: aLabel initialAnswer: anAnswer orCancel: requestExitBlock ! !

!methodRemoval: RefactoringApplier #request:initialAnswer:onCancel: stamp: 'len 5/23/2020 20:43:50'!
RefactoringApplier removeSelector: #request:initialAnswer:onCancel:!
!methodRemoval: SystemDictionary #getFileNameFromUser stamp: 'len 5/23/2020 20:43:50'!
SystemDictionary removeSelector: #getFileNameFromUser!
!methodRemoval: Text class #fromUser stamp: 'len 5/23/2020 20:43:50'!
Text class removeSelector: #fromUser!
!methodRemoval: Browser #request:initialAnswer: stamp: 'len 5/23/2020 20:43:50'!
Browser removeSelector: #request:initialAnswer:!

!Object reorganize!
('Breakpoint' break)
('accessing' addInstanceVarNamed:withValue: at: at:put: basicAt: basicAt:put: basicSize customizeExplorerContents rawBasicAt: rawBasicAt:put: size yourself)
('as yet unclassified' revisar)
('associating' ->)
('binding' bindingOf:)
('casing' caseOf: caseOf:otherwise:)
('class membership' class isKindOf: isMemberOf: respondsTo:)
('comparing' = ~= hash literalEqual:)
('converting' adaptToFloat:andSend: adaptToFraction:andSend: adaptToInteger:andSend: as: asString complexContents copyAs: mustBeBoolean mustBeBooleanIn: withoutListWrapper)
('copying' copy copyForClipboard copyFrom: copySameFrom: postCopy shallowCopy veryDeepCopy)
('events-old protocol' addDependent: breakDependents changed changed: removeDependent: update:)
('error handling' assert: assert:description: caseError doesNotUnderstand: error: halt halt: handles: notYetImplemented notify: notify:at: primitiveFail primitiveFailed primitiveFailed: shouldBeImplemented shouldNotHappen shouldNotHappenBecause: shouldNotHappenBecauseErrorMessage shouldNotHappenErrorMessage shouldNotImplement subclassResponsibility)
('evaluating' value valueWithArguments: valueWithPossibleArgument:)
('events-accessing' actionForEvent: actionMap actionSequenceForEvent: actionsDo: hasActionForEvent: setActionSequence:forEvent: updateableActionMap)
('events-registering' when:evaluate: when:send:to: when:send:to:with: when:send:to:withArguments:)
('events-removing' releaseActionMap removeAction:forEvent: removeActionsForEvent: removeActionsSatisfying: removeActionsSatisfying:forEvent: removeActionsWithReceiver: removeActionsWithReceiver:forEvent:)
('events-triggering' triggerEvent: triggerEvent:with: triggerEvent:withArguments:)
('finalization' actAsExecutor executor finalizationRegistry finalize retryWithGC:until: toFinalizeSend:to:with:)
('inspecting' basicInspect copyToClipboard inspect inspectWithLabel: inspectorClass)
('macpal' flash)
('message handling' argumentName argumentNameSufix disableCode: executeMethod: perform: perform:with: perform:with:with: perform:with:with:with: perform:withArguments: perform:withArguments:inSuperclass: perform:withPossiblyWrongSizedArguments: with:executeMethod: with:with:executeMethod: with:with:with:executeMethod: with:with:with:with:executeMethod: withArgs:executeMethod:)
('object serialization' comeFullyUpOnReload: convertToCurrentVersion:refStream: objectForDataStream: readDataFrom:size: storeDataOn:)
('printing' displayStringOrText fullPrintString isLiteral longPrintOn: longPrintOn:limitedTo:indent: longPrintString longPrintStringLimitedTo: nominallyUnsent: print printAs:limitedTo: printOn: printString printStringLimitedTo: printText printTextLimitedTo: printWithClosureAnalysisOn: storeOn: storeString)
('system primitives' becomeForward: becomeForward:copyHash: className instVarAt: instVarAt:put: instVarAtPrim73: instVarAtPrim74:put: instVarNamed: instVarNamed:put: primitiveChangeClassTo: someObject)
('stepping' stepAt: wantsSteps)
('testing' is: isArray isBehavior isBlock isClosure isCollection isComplex isContext isFloat isFloatOrFloatComplex isFraction isInteger isInterval isMethodProperties isNumber isPoint isPseudoContext isString isSymbol isVariableBinding name renameTo:)
('translation support' inline: success: var:declareC:)
('user interface' browseClassHierarchy confirm: confirm:orCancel: explore hasContentsInExplorer inform: request:do: request:initialAnswer: request:initialAnswer:do: request:initialAnswer:do:orCancel: request:initialAnswer:orCancel: request:initialAnswer:verifying:do: request:initialAnswer:verifying:do:orCancel:)
('private' deprecatedMethod errorDescriptionForSubcriptBounds: errorImproperStore errorNonIntegerIndex errorNotIndexable errorSubscriptBounds: primitiveError: setPinned: species storeAt:inTempFrame:)
('tracing' inboundPointers inboundPointersExcluding: outboundPointers outboundPointersDo:)
('morphic' activeHand runningWorld)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
('profiler support' profilerFriendlyCall:)
('pinning' isPinned pin unpin)
('debugging-haltOnce' clearHaltOnce haltOnce haltOnceEnabled setHaltOnce toggleHaltOnce)
!

