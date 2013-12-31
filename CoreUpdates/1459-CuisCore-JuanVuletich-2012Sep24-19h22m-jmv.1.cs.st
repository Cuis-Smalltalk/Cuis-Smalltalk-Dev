'From Cuis 4.0 of 21 April 2012 [latest update: #1457] on 24 September 2012 at 8:20:30 pm'!

!FillInTheBlankMorph class methodsFor: 'instance creation' stamp: 'jmv 9/24/2012 19:32'!
request: queryString initialAnswer: defaultAnswer centerAt: aPoint onCancelReturn: returnOnCancel acceptOnCR: acceptBoolean answerExtent: answerExtent
	"Create an instance of me whose question is queryString with the given initial answer. Invoke it centered at the given point, and answer the string the user accepts.   If the user cancels, answer returnOnCancel."
	"
	FillInTheBlankMorph
		request: 'Type something, then type [Return].'
		initialAnswer: 'yo ho ho!!'
		centerAt: Display center
	"

	| aFillInTheBlankMorph |
	aFillInTheBlankMorph _ self new
		setQuery: queryString
		initialAnswer: defaultAnswer
		answerExtent: answerExtent
		acceptOnCR: acceptBoolean.
	aFillInTheBlankMorph responseUponCancel: returnOnCancel.
	self currentWorld addMorph: aFillInTheBlankMorph centeredNear: aPoint.
	^ aFillInTheBlankMorph getUserResponse
! !


!MenuMorph methodsFor: 'control' stamp: 'jmv 9/24/2012 20:06'!
popUpAt: aPoint forHand: hand allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	| evt |
	self items isEmpty ifTrue: [^self].
	Theme current decorateMenu: self.
	(self submorphs select: [:m | m isKindOf: UpdatingMenuItemMorph]) 
		do: [:m | m updateContents].
	self currentWorld addMorphFront: self.
	self 
		positionAt: aPoint
		relativeTo: (selectedItem ifNil: [self items first]).
	"Acquire focus for valid pop up behavior"
	hand newMouseFocus: self.
	aBoolean ifTrue: [hand newKeyboardFocus: self].
	evt := hand lastEvent.
	(evt isKeyboard or: [evt isMouse and: [evt anyButtonPressed not]]) 
		ifTrue: [
			"Select first item if button not down"
			self moveSelectionDown: 1 event: evt]! !

!MenuMorph methodsFor: 'modal control' stamp: 'jmv 9/24/2012 20:09'!
invokeModalAt: aPoint allowKeyboard: aBoolean
	"Invoke this menu and don't return until the user has chosen a value.
	See senders of this method for finding out how to use modal menu morphs."
	| w oldFocus actHand |
	w _ self currentWorld.
	actHand _ w activeHand.
	oldFocus _ actHand keyboardFocus.
	w doOneSubCycle.
	self	
		popUpAt: aPoint
		forHand: actHand 
		allowKeyboard: aBoolean.
	self isModalInvokationDone: false.
	[self isInWorld & self isModalInvokationDone not] whileTrue: [w doOneSubCycle].
	self delete.
	oldFocus ifNotNil: [ actHand newKeyboardFocus: oldFocus ].
	^ self modalSelection! !


!MVCMenuMorph methodsFor: 'invoking' stamp: 'jmv 9/24/2012 20:08'!
invokeAt: aPoint allowKeyboard: aBoolean
	"Add this menu to the given world centered at the given point. Wait for the user to make a selection and answer it. The selection value returned is an integer in keeping with PopUpMenu, if the menu is converted from an MVC-style menu."
	"Details: This is invoked synchronously from the caller. In order to keep processing inputs and updating the screen while waiting for the user to respond, this method has its own version of the World's event loop." 
	|actHand w oldFocus |
	self flag: #bob.		"is <aPoint> global or local?"
	self flag: #arNote.	"<aPoint> is local to aWorld"
	w _ self currentWorld.
	actHand _ w activeHand.
	oldFocus _ actHand keyboardFocus.
	w doOneSubCycle.
	self
		popUpAt: aPoint
		forHand: actHand
		allowKeyboard: aBoolean.
	done _ false.
	[self isInWorld & done not] whileTrue: [w doOneSubCycle].
	self delete.
	oldFocus ifNotNil: [ actHand newKeyboardFocus: oldFocus ].
	^ mvcSelection ! !


!Object methodsFor: 'macpal' stamp: 'jmv 9/24/2012 20:04'!
currentWorld
	"Answer a morphic world that is the current UI focus."

	"Should query all instances of world, looking for the one animated by the currently running process!! (and answer nil if this is not a Morphic process!!)"
	self flag: #jmvVer2.

	ActiveWorld ifNotNil:[^ActiveWorld].
	^World! !


!AbstractSound class methodsFor: 'sound library-file in/out' stamp: 'jmv 9/24/2012 19:42'!
fileInSoundLibrary
	"Prompt the user for a file name and the file in the sound library with that name."
	"AbstractSound fileInSoundLibrary"

	| fileName |
	fileName _ FillInTheBlankMorph request: 'Sound library file name?'.
	fileName isEmptyOrNil ifTrue: [^ self].
	(fileName endsWith: '.sounds') ifFalse: [fileName _ fileName, '.sounds'].
	self fileInSoundLibraryNamed: fileName.
! !

!AbstractSound class methodsFor: 'sound library-file in/out' stamp: 'jmv 9/24/2012 19:42'!
fileOutSoundLibrary: aDictionary
	"File out the given dictionary, which is assumed to contain sound and instrument objects keyed by their names."
	"Note: This method is separated out so that one can file out edited sound libraries, as well as the system sound library. To make such a collection, you can inspect AbstractSound sounds and remove the items you don't want. Then do: 'AbstractSound fileOutSoundLibrary: self' from the Dictionary inspector."

	| fileName refStream |
	(aDictionary isKindOf: Dictionary)
		ifFalse: [self error: 'arg should be a dictionary of sounds'].
	fileName _ FillInTheBlankMorph request: 'Sound library file name?'.
	fileName isEmptyOrNil ifTrue: [^ self].
	refStream _ SmartRefStream fileNamed: fileName, '.sounds'.
	[ refStream nextPut: aDictionary ]
		ensure: [ refStream close ]! !


!CodeFile methodsFor: 'fileIn/fileOut' stamp: 'jmv 9/24/2012 19:45'!
fileOut
	| fileName stream |
	fileName := FillInTheBlankMorph request: 'Enter the file name' initialAnswer:''.
	stream := FileStream newFileNamed: fileName.
	sourceSystem isEmpty ifFalse:[
		stream nextChunkPut: sourceSystem printString; newLine ].
	self fileOutOn: stream.
	stream newLine; newLine.
	self classes do: [ :cls |
		cls needsInitialize ifTrue: [
			stream newLine; nextChunkPut: cls name,' initialize']].
	stream newLine.
	stream close! !


!CodeProvider methodsFor: 'categories' stamp: 'jmv 9/24/2012 19:45'!
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
				newName _ FillInTheBlankMorph request: 'Please type new category name' initialAnswer: 'category name'.
				newName isEmpty ifTrue: [ ^nil ]]
			ifFalse: [ newName _ labels at: menuIndex ].
	^ newName ifNotNil: [ newName asSymbol ]! !


!Browser methodsFor: 'accessing' stamp: 'jmv 9/24/2012 19:45'!
request: prompt initialAnswer: initialAnswer

	^ FillInTheBlankMorph
		request: prompt
		initialAnswer: initialAnswer
! !


!ChangeList methodsFor: 'menu actions' stamp: 'jmv 9/24/2012 19:45'!
fileOutSelections 
	| f |
	f _ FileStream newFileNamed: (FillInTheBlankMorph request: 'Enter file name' initialAnswer: 'Filename.st').
	f ifNil: [^ self].
	f timeStamp.
	listSelections with: changeList do: 
		[:selected :item | selected ifTrue: [item fileOutOn: f]].
	f close! !


!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 9/24/2012 19:45'!
rename
	"Store a new name string into the selected ChangeSet.  reject duplicate name; allow user to back out"

	| newName |
	newName _ FillInTheBlankMorph request: 'New name for this change set'
						initialAnswer: myChangeSet name.
	(newName = myChangeSet name or: [newName size = 0]) ifTrue:
			[^ Beeper beep].

	(self class changeSetNamed: newName) ifNotNil:
			[^ Utilities inform: 'Sorry that name is already used'].

	myChangeSet name: newName.
	self update.
	self changed: #mainButtonName.
	self changed: #relabel.! !


!Debugger methodsFor: 'context stack menu' stamp: 'jmv 9/24/2012 19:43'!
askForCategoryIn: aClass default: aString
	| categories index category |
	categories := OrderedCollection with: 'new ...'. 
	categories addAll: (aClass allMethodCategoriesIntegratedThrough: Object).	
	index := PopUpMenu withCaption: 'Please provide a good category for the new method!!'
						chooseFrom: categories.
	index = 0 ifTrue: [^ aString].
	category := index = 1 ifTrue: [FillInTheBlankMorph request: 'Enter category name:']
						ifFalse: [categories at: index].
	^ category isEmpty ifTrue: [^ aString] ifFalse: [category]! !


!FileList methodsFor: 'file menu action' stamp: 'jmv 9/24/2012 19:45'!
addNew: aString byEvaluating: aBlock
	"A parameterization of earlier versions of #addNewDirectory and
	#addNewFile.  Fixes the bug in each that pushing the cancel button
	in the FillInTheBlank dialog gave a walkback."

	| response newName index ending |
	(response := FillInTheBlankMorph
						request: ('New {1} Name?' format: {aString})
						initialAnswer: ('{1}Name' format: {aString}))
		isEmpty ifTrue: [^ self].
	newName := response asFileName.
	Cursor wait showWhile: [
		aBlock value: newName].
	self updateFileList.
	index := list indexOf: newName.
	index = 0 ifTrue: [ending := ') ',newName.
		index := list findFirst: [:line | line endsWith: ending]].
	self fileListIndex: index.
! !

!FileList methodsFor: 'file menu action' stamp: 'jmv 9/24/2012 19:46'!
renameFile
	"Rename the currently selected file"
	| newName response |
	listIndex = 0 ifTrue: [^ self].
	(response _ FillInTheBlankMorph request: 'NewFileName?'
 					initialAnswer: fileName)
		isEmpty ifTrue: [^ self].
	newName _ response asFileName.
	newName = fileName ifTrue: [^ self].
	directory rename: fileName toBe: newName.
	self updateFileList.
	listIndex _ list findFirst: [:item | (self fileNameFromFormattedItem: item) = newName].
	listIndex > 0 ifTrue: [fileName _ newName].
	self changed: #fileListIndex.
! !


!MessageSet methodsFor: 'filtering' stamp: 'jmv 9/24/2012 19:46'!
filterToImplementorsOf
	"Filter the receiver's list down to only those items with a given selector"

	| aFragment inputWithBlanksTrimmed |

	aFragment _ FillInTheBlankMorph request: 'type selector:' initialAnswer: ''.
	aFragment  isEmptyOrNil ifTrue: [^ self].
	inputWithBlanksTrimmed _ aFragment withBlanksTrimmed.
	Symbol hasInterned: inputWithBlanksTrimmed ifTrue:
		[:aSymbol | 
			self filterFrom:
				[:aClass :aSelector |
					aSelector == aSymbol]]! !

!MessageSet methodsFor: 'filtering' stamp: 'jmv 9/24/2012 19:46'!
filterToNotImplementorsOf
	"Filter the receiver's list down to only those items whose selector is NOT one solicited from the user."

	| aFragment inputWithBlanksTrimmed |

	aFragment _ FillInTheBlankMorph request: 'type selector: ' initialAnswer: ''.
	aFragment  isEmptyOrNil ifTrue: [^ self].
	inputWithBlanksTrimmed _ aFragment withBlanksTrimmed.
	Symbol hasInterned: inputWithBlanksTrimmed ifTrue:
		[:aSymbol | 
			self filterFrom:
				[:aClass :aSelector |
					aSelector ~~ aSymbol]]! !

!MessageSet methodsFor: 'filtering' stamp: 'jmv 9/24/2012 19:46'!
filterToNotSendersOf
	"Filter the receiver's list down to only those items which do not send a given selector"

	| aFragment inputWithBlanksTrimmed aMethod |

	aFragment _ FillInTheBlankMorph request: 'type selector:' initialAnswer: ''.
	aFragment  isEmptyOrNil ifTrue: [^ self].
	inputWithBlanksTrimmed _ aFragment withBlanksTrimmed.
	Symbol hasInterned: inputWithBlanksTrimmed ifTrue:
		[:aSymbol | 
			self filterFrom:
				[:aClass :aSelector |
					(aMethod _ aClass compiledMethodAt: aSelector) isNil or:
						[(aMethod hasLiteralThorough: aSymbol) not]]]! !

!MessageSet methodsFor: 'filtering' stamp: 'jmv 9/24/2012 19:46'!
filterToSendersOf
	"Filter the receiver's list down to only those items which send a given selector"

	| aFragment inputWithBlanksTrimmed aMethod |

	aFragment _ FillInTheBlankMorph request: 'type selector:' initialAnswer: ''.
	aFragment  isEmptyOrNil ifTrue: [^ self].
	inputWithBlanksTrimmed _ aFragment withBlanksTrimmed.
	Symbol hasInterned: inputWithBlanksTrimmed ifTrue:
		[:aSymbol | 
			self filterFrom:
				[:aClass :aSelector |
					(aMethod _ aClass compiledMethodAt: aSelector) notNil and:
						[aMethod hasLiteralThorough: aSymbol]]]

! !


!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 9/24/2012 19:53'!
editBalloonHelpContent: aString
	| reply |
	reply _ FillInTheBlankMorph
		request: 'Edit the balloon help text for ' , (self printStringLimitedTo: 40)
		initialAnswer: (aString ifNil: [self noHelpString] ifNotNil: [aString]).
	reply ifNil: [^ self].  "User cancelled out of the dialog"
	(reply isEmpty or: [reply asString = self noHelpString])
		ifTrue: [self setBalloonText: nil]
		ifFalse: [self setBalloonText: reply]! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 9/24/2012 20:12'!
halo

	self world ifNotNil: [ :w |
		w haloMorphs do: [ :h |
			h target == self ifTrue: [^ h]]].
	^ nil! !

!Morph methodsFor: 'menus' stamp: 'jmv 9/24/2012 19:46'!
exportAsBMP
	| fName |
	fName _ FillInTheBlankMorph request:'Please enter the name' initialAnswer: (self printStringLimitedTo: 20),'.bmp'.
	fName isEmpty ifTrue:[^self].
	(self imageForm: 32) writeBMPfileNamed: fName.! !

!Morph methodsFor: 'menus' stamp: 'jmv 9/24/2012 19:46'!
exportAsJPEG
	"Export the receiver's image as a JPEG"

	| fName |
	fName _ FillInTheBlankMorph request: 'Please enter the name' initialAnswer: (self printStringLimitedTo: 20),'.jpeg'.
	fName isEmpty ifTrue: [^ self].
	(self imageForm: 32) writeJPEGfileNamed: fName! !

!Morph methodsFor: 'menus' stamp: 'jmv 9/24/2012 19:47'!
exportAsPNG
	| fName |
	fName _ FillInTheBlankMorph request:'Please enter the name' initialAnswer: (self printStringLimitedTo: 20),'.png'.
	fName isEmpty ifTrue:[^self].
	PNGReadWriter putForm: (self imageForm: 32) onFileNamed: fName.! !


!FillInTheBlankMorph class methodsFor: 'instance creation' stamp: 'jmv 9/24/2012 19:52'!
request: queryString
	"Create an instance of me whose question is queryString. Invoke it centered at the cursor, and answer the string the user accepts. Answer the empty string if the user cancels."
	"FillInTheBlankMorph request: 'What is your favorite color?'"

	^ self
		request: queryString
		initialAnswer: ''
		centerAt: World activeHand morphPosition
		onCancelReturn: ''
		acceptOnCR: true
		answerExtent: self defaultAnswerExtent! !

!FillInTheBlankMorph class methodsFor: 'instance creation' stamp: 'jmv 9/24/2012 19:52'!
request: queryString initialAnswer: defaultAnswer 
	"Create an instance of me whose question is queryString with the given initial answer. Invoke it centered at the given point, and answer the string the user accepts. Answer the empty string if the user cancels."
	"FillInTheBlankMorph
		request: 'What is your favorite color?'
		initialAnswer: 'red, no blue. Ahhh!!'"

	^ self
		request: queryString
		initialAnswer: defaultAnswer
		centerAt: World activeHand morphPosition
		onCancelReturn: ''
		acceptOnCR: true
		answerExtent: self defaultAnswerExtent! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 9/24/2012 20:08'!
basicBox
	| aBox minSide anExtent w |
	minSide _ 4 * self handleSize.
	anExtent _ ((extent x + self handleSize + 8) max: minSide) @
				((extent y + self handleSize + 8) max: minSide).
	aBox _ Rectangle center: self morphBoundsInWorld center extent: anExtent.
	w _ self world ifNil: [ target world ].
	^ w
		ifNil:
			[ aBox ]
		ifNotNil:
			[ aBox intersect: (w viewBox insetBy: 8@8) ]! !


!ImageMorph methodsFor: 'menu commands' stamp: 'jmv 9/24/2012 19:46'!
readFromFile
	| fileName |
	fileName _ FillInTheBlankMorph
		request: 'Please enter the image file name'
		initialAnswer: 'fileName'.
	fileName isEmpty ifTrue: [^ self].
	self image: (Form fromFileNamed: fileName).
! !


!MenuMorph methodsFor: 'menu' stamp: 'jmv 9/24/2012 19:43'!
addItem

	| string sel |
	string _ FillInTheBlankMorph request: 'Label for new item?'.
	string isEmpty ifTrue: [^ self].
	sel _ FillInTheBlankMorph request: 'Selector?'.
	sel isEmpty ifFalse: [sel _ sel asSymbol].
	self add: string action: sel.
! !

!MenuMorph methodsFor: 'menu' stamp: 'jmv 9/24/2012 19:43'!
addTitle

	| string |
	string _ FillInTheBlankMorph request: 'Title for this menu?'.
	string isEmpty ifTrue: [^ self].
	self addTitle: string.
! !

!MenuMorph methodsFor: 'modal control' stamp: 'jmv 9/24/2012 20:10'!
invokeModal: allowKeyboardControl
	"Invoke this menu and don't return until the user has chosen a value.  If the allowKeyboarControl boolean is true, permit keyboard control of the menu"

	^ self invokeModalAt: ActiveHand morphPosition allowKeyboard: allowKeyboardControl! !


!MVCMenuMorph methodsFor: 'invoking' stamp: 'jmv 9/24/2012 20:14'!
displayAt: aPoint during: aBlock
	"Add this menu to the Morphic world during the execution of the given block."

	self currentWorld addMorph: self centeredNear: aPoint.
	self world displayWorld.  "show myself"
	aBlock value.
	self delete! !

!MVCMenuMorph methodsFor: 'invoking' stamp: 'jmv 9/24/2012 20:14'!
informUserAt: aPoint during: aBlock
	"Add this menu to the Morphic world during the execution of the given block."

	| w titleString |

	titleString _ titleMorph submorphs first.
	self visible: false.
	w _ self currentWorld.
	aBlock value: [ :string |
		self visible ifFalse: [
			w addMorph: self centeredNear: aPoint.
			self visible: true].
		titleString contents: string.
		titleMorph morphWidth: titleString width + 8.
		self morphPosition: w activeHand morphPosition.
		self adjustSubmorphsLayout.
		self redrawNeeded.
		w displayWorld		 "show myself"
	]. 
	self delete.
	w displayWorld! !


!NetNameResolver class methodsFor: 'lookups' stamp: 'jmv 9/24/2012 19:47'!
promptUserForHostAddressDefault: defaultName
	"Ask the user for a host name and return its address. If the default name is the empty string, use the last host name as the default."
	"NetNameResolver promptUserForHostAddressDefault: ''"

	| default hostName serverAddr |
	defaultName isEmpty
		ifTrue: [default _ DefaultHostName]
		ifFalse: [default _ defaultName].
	hostName _ FillInTheBlankMorph
		request: 'Host name or address?'
		initialAnswer: default.
	hostName isEmpty ifTrue: [^ 0].
	serverAddr _ NetNameResolver addressForName: hostName timeout: 15.
	hostName size > 0 ifTrue: [DefaultHostName _ hostName].
	^ serverAddr! !


!ObjectExplorer methodsFor: 'monitoring' stamp: 'jmv 9/24/2012 20:04'!
monitor: anObjectExplorerWrapper
	"Start stepping and watching the given wrapper for changes."
	anObjectExplorerWrapper ifNil: [ ^self ].
	self currentWorld ifNotNil: [ :w |
		self monitorList at: anObjectExplorerWrapper put: anObjectExplorerWrapper asString.
		w startStepping: self at: Time millisecondClockValue selector: #step arguments: #() stepTime: 200 ]! !

!ObjectExplorer methodsFor: 'monitoring' stamp: 'jmv 9/24/2012 20:05'!
step
	"If there's anything in my monitor list, see if the strings have changed."
	| string changes |
	changes _ false.
	self monitorList keysAndValuesDo: [ :k :v |
		k ifNotNil: [
			k refresh.
			(string _ k asString) ~= v ifTrue: [ self monitorList at: k put: string. changes _ true ].
		]
	].
	changes ifTrue: [ | sel |
		sel _ currentSelection.
		self changed: #getList.
		self noteNewSelection: sel.
	].
	
	self monitorList isEmpty ifTrue: [
		self currentWorld ifNotNil: [ :w | w stopStepping: self selector: #step ]]! !

!ObjectExplorer methodsFor: 'monitoring' stamp: 'jmv 9/24/2012 20:05'!
stopMonitoring
	monitorList _ nil.
	self currentWorld ifNotNil: [ :w |
		w stopStepping: self selector: #step ]! !


!Parser methodsFor: 'error correction' stamp: 'jmv 9/24/2012 19:47'!
defineClass: className
	"prompts the user to define a new class,  
	asks for it's category, and lets the users edit further  
	the definition"
	| sym cat def d2 |
	sym := className asSymbol.
	cat := FillInTheBlankMorph request: 'Enter class category : ' initialAnswer: self encoder classEncoding theNonMetaClass category.
	cat
		ifEmpty: [cat := 'Unknown'].
	def := 'Object subclass: #' , sym , '
		instanceVariableNames: '''' 
		classVariableNames: ''''
		poolDictionaries: ''''
		category: ''' , cat , ''''.
	d2 := FillInTheBlankMorph request: 'Edit class definition : ' initialAnswer: def.
	d2
		ifEmpty: [d2 := def].
	Compiler evaluate: d2.
	^ encoder
		global: (Smalltalk associationAt: sym)
		name: sym! !


!PasteUpMorph methodsFor: 'WiW support' stamp: 'jmv 9/24/2012 20:13'!
shouldGetStepsFrom: aWorld

	(self isWorldMorph and: [owner notNil]) ifTrue: [
		^self world == aWorld
	].
	^super shouldGetStepsFrom: aWorld! !

!PasteUpMorph methodsFor: 'change reporting' stamp: 'jmv 9/24/2012 20:13'!
invalidateRect: damageRect
        "Clip damage reports to my bounds, since drawing is clipped to my bounds."

        self == self world 
                ifTrue: [ worldState recordDamagedRect: (damageRect intersect: (0@0 extent: self morphExtent) ) ]
                ifFalse: [ super invalidateRect: damageRect ]
! !

!PasteUpMorph methodsFor: 'change reporting' stamp: 'jmv 9/24/2012 20:13'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn."

        self == self world 
                ifTrue: [worldState doFullRepaint]
                ifFalse: [super redrawNeeded]
! !

!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 9/24/2012 20:12'!
displayWorld

	self world privateOuterDisplayWorld
! !


!PopUpMenu methodsFor: 'basic control sequence' stamp: 'jmv 9/24/2012 20:08'!
startUpWithCaption: captionOrNil at: location allowKeyboard: aBoolean 
	"Display the menu, with caption if supplied. Wait for the mouse button to go down, then track the selection as long as the button is pressed. When the button is released,
	Answer the index of the current selection, or zero if the mouse is not released over  any menu item. Location specifies the desired topLeft of the menu body rectangle. The final argument indicates whether the menu should seize the keyboard focus in order to allow the user to navigate it via the keyboard."

	| maxHeight |
	maxHeight := Display height * 3 // 4.
	self frameHeight > maxHeight 
		ifTrue: 
			[^self 
				startUpSegmented: maxHeight
				withCaption: captionOrNil
				at: location
				allowKeyboard: aBoolean].
	^Cursor normal showWhile: 
							[(MVCMenuMorph from: self title: captionOrNil) 
								invokeAt: location
								allowKeyboard: aBoolean]! !


!ProgressBarMorph methodsFor: 'menu' stamp: 'jmv 9/24/2012 19:47'!
changeProgressValue: evt
	| answer |
	answer _ FillInTheBlankMorph
		request: 'Enter new value (0 - 1.0)'
		initialAnswer: self value contents asString.
	answer isEmptyOrNil ifTrue: [^ self].
	self value: answer asNumber! !


!SmartRefStream methodsFor: 'class changed shape' stamp: 'jmv 9/24/2012 19:43'!
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
			choice := FillInTheBlankMorph request: tell.	"class name"
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
			self close.
			^'conversion method needed'].
	answ = 2 ifTrue: [answ := 'conversion method needed'].
	answ = 'conversion method needed' 
		ifTrue: [
			self close.
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


!Socket class methodsFor: 'tests' stamp: 'jmv 9/24/2012 19:47'!
sendTest
	"Send data to the 'discard' socket of the given host.
	Tests the speed of one-way data transfers across the
	network to the given host. Note that most hosts
	do not run a discard server."

	"Socket sendTest"

	| sock bytesToSend sendBuf bytesSent t serverName serverAddr |
	Transcript newLine; show: 'starting send test'; newLine.
	self initializeNetwork.
	serverName := FillInTheBlankMorph request: 'What is the destination server?' initialAnswer: 'create.ucsb.edu'.
	serverAddr := NetNameResolver addressForName: serverName timeout: 10.
	serverAddr ifNil: [
		^self inform: 'Could not find an address for ' , serverName].
	sock := self new.
	Transcript show: '---------- Connecting ----------';newLine.
	sock connectTo: serverAddr port: 9.
	sock isConnected ifFalse: [
		sock destroy.
		^self inform: 'could not connect'].
	Transcript show: 'connection established; sending data'; newLine.
	bytesToSend := 1000000.
	sendBuf := String new: 64 * 1024 withAll: $x.
	bytesSent := 0.
	t := Time millisecondsToRun: 
					[[bytesSent < bytesToSend] whileTrue: 
							[sock sendDone 
								ifTrue: [bytesSent := bytesSent + (sock sendSomeData: sendBuf)]]].
	sock waitForSendDoneFor: self standardTimeout.
	sock destroy.
	Transcript show: '---------- Connection Closed ----------'; newLine;
		show: 'send test done; time = ' , t printString; newLine;
		show: (bytesToSend asFloat / t roundTo: 0.01) printString, ' * 1000 bytes/sec'; newLine; endEntry! !


!StandardFileStream class methodsFor: 'error handling' stamp: 'jmv 9/24/2012 19:47'!
fileDoesNotExistUserHandling: fullFileName

	| selection newName |
	selection _ (PopUpMenu labels:
'create a new file
choose another name
cancel')
			startUpWithCaption: (FileDirectory localNameFor: fullFileName) , '
does not exist.'.
	selection = 1 ifTrue:
		[^ self new open: fullFileName forWrite: true].
	selection = 2 ifTrue:
		[ newName _ FillInTheBlankMorph request: 'Enter a new file name'
						initialAnswer:  fullFileName.
		^ self oldFileNamed:
			(self fullName: newName)].
	self halt! !

!StandardFileStream class methodsFor: 'error handling' stamp: 'jmv 9/24/2012 19:48'!
fileExistsUserHandling: fullFileName
	| dir localName choice newName newFullFileName |
	dir _ FileDirectory forFileName: fullFileName.
	localName _ FileDirectory localNameFor: fullFileName.
	choice _ (PopUpMenu
		labels:
'overwrite that file\choose another name\cancel' withNewLines)
		startUpWithCaption: localName, '
already exists.'.

	choice = 1 ifTrue: [
		dir deleteFileNamed: localName
			ifAbsent: [self error: 'Could not delete the old version of that file'].
		^ self new open: fullFileName forWrite: true].

	choice = 2 ifTrue: [
		newName _ FillInTheBlankMorph request: 'Enter a new file name' initialAnswer: fullFileName.
		newFullFileName _ self fullName: newName.
		^ self newFileNamed: newFullFileName].

	self error: 'Please close this to abort file opening'! !

!StandardFileStream class methodsFor: 'error handling' stamp: 'jmv 9/24/2012 19:48'!
readOnlyFileDoesNotExistUserHandling: fullFileName

	| dir files choices selection newName fileName |
	dir _ FileDirectory forFileName: fullFileName.
	files _ dir fileNames.
	fileName _ FileDirectory localNameFor: fullFileName.
	choices _ fileName correctAgainst: files.
	choices add: 'Choose another name'.
	choices add: 'Cancel'.
	selection _ (PopUpMenu labelArray: choices lines: (Array with: 5) )
		startUpWithCaption: (FileDirectory localNameFor: fullFileName), '
does not exist.'.
	selection = choices size ifTrue:["cancel" ^ nil "should we raise another exception here?"].
	selection < (choices size - 1) ifTrue: [
		newName _ (dir pathName , FileDirectory slash , (choices at: selection))].
	selection = (choices size - 1) ifTrue: [
		newName _ FillInTheBlankMorph 
							request: 'Enter a new file name' 
							initialAnswer: fileName].
	newName = '' ifFalse: [^ self readOnlyFileNamed: (self fullName: newName)].
	^ self error: 'Could not open a file'! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 9/24/2012 19:48'!
condenseSources	
	"Move all the changes onto a compacted sources file."
	"Smalltalk condenseSources"

	| f classCount dir newVersionString newSourcesName |
	dir _ FileDirectory default.
	newVersionString _ FillInTheBlankMorph request: 'Please designate the version
for the new source code file...' initialAnswer: SourceFileVersionString.
	newVersionString ifNil: [^ self].
	newVersionString = SourceFileVersionString ifTrue:
		[^ self error: 'The new source file must not be the same as the old.'].
	SourceFileVersionString _ newVersionString.

	"Write all sources with fileIndex 1"
	newSourcesName _ self newSourcesName.
	f _ FileStream newFileNamed: newSourcesName.
	f timeStamp.
'Condensing Sources File...'
	displayProgressAt: Sensor mousePoint
	from: 0 to: Smalltalk classNames size
	during:
		[:bar | classCount _ 0.
		Smalltalk allClassesDo:
			[:class | bar value: (classCount _ classCount + 1).
			class fileOutOn: f moveSource: true toFile: 1]].
	f close.

	CompiledMethod allInstancesDo:
		[ : e | 
		e isInstalled ifFalse: [ e destroySourcePointer ] ].

	"Make a new empty changes file"
	self closeSourceFiles.
	dir rename: self localChangesName
		toBe: self localChangesName , '.old'.
	(FileStream newFileNamed: self localChangesName)
		timeStamp; close.
	LastQuitLogPosition _ 0.

	self setMacFileInfoOn: self localChangesName.
	self setMacFileInfoOn: newSourcesName.
	self openSourceFiles.
	self inform: 'Source files have been rewritten!!
 
Check that all is well, and then save/quit.
 
Otherwise, remove new sources/changes,
replace them with the former ones, and
exit without saving the image.
 '! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 9/24/2012 19:48'!
saveAsEmbeddedImage
	"Save the current state of the system as an embedded image"

	| dir newName newImageName |
	dir _ FileDirectory default.
	newName _ FillInTheBlankMorph
		request: 'Select existing VM file'
		initialAnswer: (FileDirectory localNameFor: '').
	newName = '' ifTrue: [^ self].
	newName _ FileDirectory baseNameFor: newName asFileName.

	newImageName _ newName.
	(dir includesKey: newImageName) ifFalse:
		[^ self inform: 'Unable to find name ', newName, ' Please choose another name.'].

	self logChange: '----SAVEAS (EMBEDDED) ', newName, '----', Date dateAndTimeNow printString.
	self imageName: (dir fullNameFor: newImageName).
	LastImageName _ self imageName.
	self closeSourceFiles.
	self snapshot: true andQuit: true embedded: true
! !

!SystemDictionary methodsFor: 'toDeprecate' stamp: 'jmv 9/24/2012 19:48'!
getFileNameFromUser

	| newName |
	newName _ FillInTheBlankMorph
		request: 'New File Name?'
		initialAnswer: (FileDirectory localNameFor: self imageName).
	newName isEmpty ifTrue: [^nil].
	((FileDirectory default fileOrDirectoryExists: (self fullNameForImageNamed: newName)) or:
	 [FileDirectory default fileOrDirectoryExists: (self fullNameForChangesNamed: newName)]) ifTrue: [
		(self confirm: ('{1} already exists. Overwrite?' format: {newName})) ifFalse: [^nil]].
	^newName
! !


!SystemWindow methodsFor: 'label' stamp: 'jmv 9/24/2012 19:48'!
relabel
	| newLabel |
	newLabel _ FillInTheBlankMorph 
		request: 'New title for this window'
		initialAnswer: labelString.
	newLabel isEmpty ifTrue: [^self].
	self setLabel: newLabel! !


!CodePackageListWindow methodsFor: 'commands' stamp: 'jmv 9/24/2012 19:43'!
createPackage

	| pkName |
	pkName _ FillInTheBlankMorph request: 'Name for new package?'.
	CodePackage
		named: pkName
		createIfAbsent: true
		registerIfNew: true! !


!CodeWindow methodsFor: 'misc' stamp: 'jmv 9/24/2012 19:45'!
getSelectorAndSendQuery: querySelector to: queryPerformer with: queryArgs
	"Obtain a selector relevant to the current context, and then send the querySelector to the queryPerformer with the selector obtained and queryArgs as its arguments.  If no message is currently selected, then obtain a method name from a user type-in"

	| strm array |
	strm _ WriteStream on: (array _ Array new: queryArgs size + 1).
	strm nextPut: nil.
	strm nextPutAll: queryArgs.

	model selectedMessageName ifNil: [ | selector |
		selector _ FillInTheBlankMorph request: 'Type selector:' initialAnswer: 'flag:'.
		^ selector isEmptyOrNil ifFalse: [
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


!BrowserWindow methodsFor: 'commands' stamp: 'jmv 9/24/2012 19:42'!
findClass
	"Search for a class by name."
	| pattern foundClass classNames index toMatch exactMatch potentialClassNames |

	self okToChange ifFalse: [ ^self flash ].
	pattern _ FillInTheBlankMorph request: 'Class name or fragment?'.
	pattern isEmpty ifTrue: [^ self flash].
	toMatch _ (pattern copyWithout: $.) asLowercase withBlanksTrimmed.
	potentialClassNames _ model potentialClassNames asOrderedCollection.
	classNames _ (pattern last = $. or: [pattern last = $ ])
		ifTrue: [potentialClassNames select:
					[:nm |  nm asLowercase = toMatch]]
		ifFalse: [potentialClassNames select: 
					[:n | n includesSubstring: toMatch caseSensitive: false]].
	classNames isEmpty ifTrue: [^ self flash].
	exactMatch _ classNames detect: [ :each | each asLowercase = toMatch] ifNone: nil.

	index _ classNames size = 1
		ifTrue:	[1]
		ifFalse:	[exactMatch
			ifNil: [(PopUpMenu labelArray: classNames lines: #()) startUp]
			ifNotNil: [classNames addFirst: exactMatch.
				(PopUpMenu labelArray: classNames lines: #(1)) startUp]].
	index = 0 ifTrue: [^ self flash].
	foundClass _ Smalltalk at: (classNames at: index) asSymbol.
 	model selectCategoryForClass: foundClass.
	model selectClass: foundClass
! !


!CodeFileBrowserWindow methodsFor: 'commands' stamp: 'jmv 9/24/2012 19:43'!
findClass
	| pattern foundClass classNames index foundCodeFile |
	self okToChange ifFalse: [^ self flash].
	pattern _ (FillInTheBlankMorph request: 'Class Name?') asLowercase.
	pattern isEmpty ifTrue: [^ self].
	classNames := Set new.
	classNames addAll: model selectedCodeFile classes keys.
	classNames := classNames asArray select: 
		[:n | (n asLowercase indexOfSubCollection: pattern startingAt: 1) > 0].
	classNames isEmpty ifTrue: [^ self].
	index _ classNames size = 1
				ifTrue:	[1]
				ifFalse:	[(PopUpMenu labelArray: classNames lines: #()) startUp].
	index = 0 ifTrue: [^ self].
	foundCodeFile := nil.
	foundClass := nil.
		(model selectedCodeFile classes includesKey: (classNames at: index)) ifTrue:[
			foundClass := model selectedCodeFile classes at: (classNames at: index).
			foundCodeFile := model selectedCodeFile ].
	foundClass ifNotNil: [
	 	model systemCategoryListIndex: (model systemCategoryList indexOf: foundCodeFile name asSymbol).
		model classListIndex: (model classList indexOf: foundClass name) ]! !


!DebuggerWindow methodsFor: 'menu commands' stamp: 'jmv 9/24/2012 19:43'!
returnValue
	| expression |
	expression _ FillInTheBlankMorph request: 'Enter expression for return value:'.
	model returnValue: expression! !


!InspectorWindow methodsFor: 'menu commands' stamp: 'jmv 9/24/2012 19:43'!
addEntry

	| newKey string |
	string _ FillInTheBlankMorph request:
'Enter new key, then type RETURN.
(Expression will be evaluated for value.)
Examples:  #Fred    ''a string''   3+4'.
	newKey _ Smalltalk actualCompilerClass evaluate: string.
	model addEntry: newKey! !

!InspectorWindow methodsFor: 'menu commands' stamp: 'jmv 9/24/2012 19:46'!
renameEntry
	| string newKey |

	string _ FillInTheBlankMorph request: 
'Enter new key, then type RETURN.
(Expression will be evaluated for value.)
Examples:  #Fred    ''a string''   3+4'
		 initialAnswer: model selectedKey printString.

	string = '' ifTrue: [
		^self ].

	newKey _ Compiler evaluate: string.

	model renameEntryTo: newKey! !


!MessageSetWindow methodsFor: 'menu commands' stamp: 'jmv 9/24/2012 19:40'!
filterMessageList
	"Allow the user to refine the list of messages."

	| aMenu |
	model messageList size <= 1 
		ifTrue: [^self inform: 'this is not a propitious filtering situation'].

	aMenu := MenuMorph new defaultTarget: model.
	aMenu addTitle: 'Filter by only showing...'.
	aMenu addStayUpIcons.
	aMenu 
		addList: #(
			#('unsent messages' #filterToUnsentMessages 'filter to show only messages that have no senders')
			#-
			#('messages that send...' #filterToSendersOf 'filter to show only messages that send a selector I specify')
			#('messages that do not send...' #filterToNotSendersOf 'filter to show only messages that do not send a selector I specify')
			#-
			#('messages whose selector is...' #filterToImplementorsOf 'filter to show only messages with a given selector I specify')
			#('messages whose selector is NOT...' #filterToNotImplementorsOf 'filter to show only messages whose selector is NOT a seletor I specify')
			#-
			#('messages in any change set' #filterToAnyChangeSet 'filter to show only messages that occur in at least one change set')
			#('messages not in any change set' #filterToNotAnyChangeSet 'filter to show only messages that do not occur in any change set in the system')
			#-
			#('messages authored by me' #filterToCurrentAuthor 'filter to show only messages whose authoring stamp has my initials')
			#('messages not authored by me' #filterToNotCurrentAuthor 'filter to show only messages whose authoring stamp does not have my initials')
			#-
			#('messages logged in .changes file' #filterToMessagesInChangesFile 'filter to show only messages whose latest source code is logged in the .changes file')
			#('messages only in .sources file' #filterToMessagesInSourcesFile 'filter to show only messages whose latest source code is logged in the .sources file')
			#-
			#('messages with prior versions' #filterToMessagesWithPriorVersions 'filter to show only messages that have at least one prior version')
			#('messages without prior versions' #filterToMessagesWithoutPriorVersions 'filter to show only messages that have no prior versions')
			#-
			#('uncommented messages' #filterToUncommentedMethods 'filter to show only messages that do not have comments at the beginning')
			#('commented messages' #filterToCommentedMethods 'fileter to show only messages that have comments at the beginning')
		).
	aMenu popUpInWorld: self world! !


!ProcessBrowserWindow methodsFor: 'menu commands' stamp: 'jmv 9/24/2012 19:47'!
changePriority
	| str newPriority nameAndRules |
	nameAndRules _ model nameAndRulesForSelectedProcess.
	nameAndRules third
		ifFalse: [PopUpMenu inform: 'Nope, won''t change priority of ' , nameAndRules first.
			^ self].
	str _ FillInTheBlankMorph request: 'New priority' initialAnswer: model selectedProcess priority asString.
	newPriority _ str asNumber asInteger.
	newPriority
		ifNil: [^ self].
	(newPriority < 1
			or: [newPriority > Processor highestPriority])
		ifTrue: [PopUpMenu inform: 'Bad priority'.
			^ self].
	model class setProcess: model selectedProcess toPriority: newPriority.
	model updateProcessList! !

!ProcessBrowserWindow methodsFor: 'menu commands' stamp: 'jmv 9/24/2012 19:47'!
findContext
	| searchString |
	searchString _ FillInTheBlankMorph request: 'Enter a string to search for in the process stack lists' initialAnswer: model searchString.
	model findContext: searchString! !

!ProcessBrowserWindow methodsFor: 'menu commands' stamp: 'jmv 9/24/2012 19:47'!
messageTally
	| secString secs |
	secString _ FillInTheBlankMorph request: 'Profile for how many seconds?' initialAnswer: '4'.
	secs _ secString asNumber asInteger.
	(secs isNil
			or: [secs isZero])
		ifTrue: [^ self].
	[ TimeProfileBrowser spyOnProcess: model selectedProcess forMilliseconds: secs * 1000 ] forkAt: model selectedProcess priority + 1.! !


!RecentMessageSetWindow methodsFor: 'menu commands' stamp: 'jmv 9/24/2012 19:47'!
setRecentHistorySize
	"Let the user specify the recent history size"

	| aReply aNumber |
	aReply _ FillInTheBlankMorph request: 'How many recent methods
should be maintained?' initialAnswer: Utilities numberOfRecentSubmissionsToStore asString.
	aReply isEmptyOrNil ifFalse:
		[aNumber _ aReply asNumber rounded.
		(aNumber > 1 and: [aNumber <= 1000])
			ifTrue:
				[Utilities numberOfRecentSubmissionsToStore: aNumber.
				self inform: 'Okay, ', aNumber asString, ' is the new size of the recent method history']
			ifFalse:
				[self inform: 'Sorry, must be a number between 2 & 1000']]! !


!TestRunner methodsFor: 'menus' stamp: 'jmv 9/24/2012 19:48'!
setFilter
	filter _ FillInTheBlankMorph request: 'Pattern for added test cases (#* OK)' initialAnswer: '*'.
	(filter endsWith: '*') ifFalse: [ filter _ filter, '*' ].
	selectedSuites _ (tests asOrderedCollection with: selectedSuites collect: [ :ea :sel |
		sel or: [ filter match: ea asString ]
	]).
	selectedSuite _ selectedSuites indexOf: true ifAbsent: [0].
	self changed: #allSelections.
! !


!Text class methodsFor: 'instance creation' stamp: 'jmv 9/24/2012 19:44'!
fromUser
	"Answer an instance of me obtained by requesting the user to type a string."
	"Text fromUser"

	^ self fromString:
		(FillInTheBlankMorph request: 'Enter text followed by [Return]')
! !


!TextEditor methodsFor: 'menu messages' stamp: 'jmv 9/24/2012 19:48'!
find
	"Prompt the user for a string to search for, and search the receiver from the current selection onward for it.  1/26/96 sw"

	| reply |
	reply _ FillInTheBlankMorph request: 'Find what? ' initialAnswer: FindText.
	"Set focus on our text morph, so that cmd-g does the search again"
	morph world activeHand newKeyboardFocus: morph.
	reply size = 0 ifTrue: [
		^ self].
	self setSearch: reply.
	ChangeText _ FindText.  "Implies no replacement to againOnce: method"
	(self findAndReplaceMany: false)
		ifFalse: [ self flash ].

"	morph installEditorToReplace: self"! !


!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 9/24/2012 19:44'!
saveWorldInFile
	"Save the world's submorphs, model, and stepList in a file.  "

	| fileName fileStream aClass |
	fileName _ FillInTheBlankMorph request: 'File name for this morph?'.
	fileName isEmpty ifTrue: [^ self].  "abort"

	"Save only model, stepList, submorphs in this world"
	myWorld submorphsDo: [:m |
		m allMorphsDo: [:subM | subM prepareToBeSaved]].	"Amen"

	fileStream _ FileStream newFileNamed: fileName, '.morph'.
	aClass _ myWorld model ifNil: [nil] ifNotNil: [myWorld model class].
	fileStream fileOutClass: aClass andObject: myWorld.
! !


!Utilities class methodsFor: 'identification' stamp: 'jmv 9/24/2012 19:48'!
setAuthor
	"Put up a dialog allowing the user to specify the author's initials.
	Utilities setAuthor
	"
	| authorName |
	AuthorInitials _ (FillInTheBlankMorph
		request: 'Please type your initials: '
		initialAnswer: AuthorInitials) withBlanksTrimmed.
	authorName _ (Smalltalk knownInitialsAndNames
		detect: [ :pair |
			pair first = AuthorInitials ]
		ifNone: [
			AuthorName _ (FillInTheBlankMorph
				request: 'Please type your name:'
				initialAnswer: 'Your Name') withBlanksTrimmed.
			^ self ]) second withBlanksTrimmed.
	(self confirm: 'Are you ' , authorName , '?')
		ifTrue: [ AuthorName _ authorName ]
		ifFalse: [
			self inform: 'Please enter different initials, then'.
			self setAuthor ]! !

!methodRemoval: PasteUpMorph #dispatchEvent:localPosition:!
PasteUpMorph removeSelector: #dispatchEvent:localPosition:!
!methodRemoval: PasteUpMorph #runLocalStepMethods!
PasteUpMorph removeSelector: #runLocalStepMethods!
!methodRemoval: PasteUpMorph #step!
PasteUpMorph removeSelector: #step!
!methodRemoval: MVCMenuMorph #invokeAt:in:allowKeyboard:!
MVCMenuMorph removeSelector: #invokeAt:in:allowKeyboard:!
!methodRemoval: MenuMorph #invokeModalAt:in:allowKeyboard:!
MenuMorph removeSelector: #invokeModalAt:in:allowKeyboard:!
!methodRemoval: FillInTheBlankMorph class #request:initialAnswer:centerAt:!
FillInTheBlankMorph class removeSelector: #request:initialAnswer:centerAt:!
!methodRemoval: FillInTheBlankMorph class #request:initialAnswer:centerAt:inWorld:!
FillInTheBlankMorph class removeSelector: #request:initialAnswer:centerAt:inWorld:!
!methodRemoval: FillInTheBlankMorph class #request:initialAnswer:centerAt:inWorld:onCancelReturn:!
FillInTheBlankMorph class removeSelector: #request:initialAnswer:centerAt:inWorld:onCancelReturn:!
!methodRemoval: FillInTheBlankMorph class #request:initialAnswer:centerAt:inWorld:onCancelReturn:acceptOnCR:!
FillInTheBlankMorph class removeSelector: #request:initialAnswer:centerAt:inWorld:onCancelReturn:acceptOnCR:!
!methodRemoval: FillInTheBlankMorph class #request:initialAnswer:centerAt:inWorld:onCancelReturn:acceptOnCR:answerExtent:!
FillInTheBlankMorph class removeSelector: #request:initialAnswer:centerAt:inWorld:onCancelReturn:acceptOnCR:answerExtent:!
!methodRemoval: FillInTheBlankMorph class #request:initialAnswer:centerAt:inWorld:onCancelReturn:acceptOnCR:answerHeight:!
FillInTheBlankMorph class removeSelector: #request:initialAnswer:centerAt:inWorld:onCancelReturn:acceptOnCR:answerHeight:!
!methodRemoval: FillInTheBlankMorph class #request:initialAnswer:centerAt:onCancelReturn:!
FillInTheBlankMorph class removeSelector: #request:initialAnswer:centerAt:onCancelReturn:!
!methodRemoval: FillInTheBlankMorph class #request:initialAnswer:centerAt:onCancelReturn:acceptOnCR:!
FillInTheBlankMorph class removeSelector: #request:initialAnswer:centerAt:onCancelReturn:acceptOnCR:!
!methodRemoval: FillInTheBlankMorph class #request:initialAnswer:centerAt:onCancelReturn:acceptOnCR:answerHeight:!
FillInTheBlankMorph class removeSelector: #request:initialAnswer:centerAt:onCancelReturn:acceptOnCR:answerHeight:!
!methodRemoval: FillInTheBlankMorph #setQuery:initialAnswer:answerHeight:acceptOnCR:!
FillInTheBlankMorph removeSelector: #setQuery:initialAnswer:answerHeight:acceptOnCR:!
!methodRemoval: Morph #outermostWorldMorph!
Morph removeSelector: #outermostWorldMorph!
!methodRemoval: MessageSet #filterToMessagesThat!
MessageSet removeSelector: #filterToMessagesThat!
!classRemoval: #FillInTheBlank!
Smalltalk removeClassNamed: #FillInTheBlank!
