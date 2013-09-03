'From Cuis 4.2 of 25 July 2013 [latest update: #1813] on 2 September 2013 at 10:57:53.227496 am'!

!FileDirectory methodsFor: 'testing' stamp: 'jmv 9/2/2013 10:41'!
is: aSymbol
	^ aSymbol == #FileDirectory or: [ super is: aSymbol ]! !


!MenuItemMorph methodsFor: 'testing' stamp: 'jmv 9/2/2013 10:36'!
is: aSymbol
	^ aSymbol == #MenuItemMorph or: [ super is: aSymbol ]! !


!UpdatingMenuItemMorph methodsFor: 'testing' stamp: 'jmv 9/2/2013 10:42'!
is: aSymbol
	^ aSymbol == #UpdatingMenuItemMorph or: [ super is: aSymbol ]! !


!FileList methodsFor: 'volume list and pattern' stamp: 'jmv 9/2/2013 10:45'!
listForPatterns: anArray
	"Make the list be those file names which match the patterns."

	| sizePad newList namePad sizeWithCommasPad font |
	directory ifNil: [^#()].
	newList _ Set new.
	anArray do: [ :pat |
		newList addAll: (directory entries select: [ :entry |
			entry isDirectory
				ifTrue: [showDirsInFileList]
				ifFalse: [fileSelectionBlock value: entry value: pat]]) ].
	newList _ newList asArray sort: self sortBlock.
	font _ Preferences standardListFont.
	namePad _ newList inject: 0 into: [ :mx :entry | mx max: (font widthOfString: entry name)].
	sizePad _ (newList inject: 0 into: [ :mx :entry | mx max: (entry fileSize)]) printString size.
	sizeWithCommasPad _ (newList inject: 0 into: [ :mx :entry | mx max: (entry fileSize)]) asStringWithCommas size.
	newList _ newList collect: [ :e | self fileNameFormattedFrom: e namePad: namePad sizePad: sizePad sizeWithCommasPad: sizeWithCommasPad ].
	^ newList! !


!CompiledMethod methodsFor: 'comparing' stamp: 'jmv 9/2/2013 10:25'!
= method
	| numLits |
	"Answer whether the receiver implements the same code as the 
	argument, method."
	(method is: #CompiledMethod) ifFalse: [^false].
	self size = method size ifFalse: [^false].
	self header = method header ifFalse: [^false].
	self initialPC to: self endPC do:
		[:i | (self at: i) = (method at: i) ifFalse: [^false]].
	(numLits := self numLiterals) ~= method numLiterals ifTrue: [^false].
	"``Dont bother checking FFI and named primitives''
	 (#(117 120) includes: self primitive) ifTrue: [^ true]."
	1 to: numLits do:
		[:i| | lit1 lit2 |
		lit1 := self literalAt: i.
		lit2 := method literalAt: i.
		lit1 = lit2 ifFalse:
			[(i = 1 and: [#(117 120) includes: self primitive])
				ifTrue: [lit1 isArray
							ifTrue:
								[(lit2 isArray and: [lit1 allButLast = lit2 allButLast]) ifFalse:
									[^false]]
							ifFalse: "ExternalLibraryFunction"
								[(lit1 analogousCodeTo: lit2) ifFalse:
									[^false]]] ifFalse:
			[i = (numLits - 1) ifTrue: "properties"
				[(self properties analogousCodeTo: method properties) ifFalse:
					[^false]] ifFalse:
			 [lit1 isFloat
				ifTrue:
					["Floats match if values are close, due to roundoff error."
					(lit1 closeTo: lit2) ifFalse: [^false]. self flag: 'just checking'. self halt]
				ifFalse:
					["any other discrepancy is a failure"
					^ false]]]]].
	^true! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 9/2/2013 10:25'!
obsoleteMethodReferences
	"Smalltalk obsoleteMethodReferences"
	"Smalltalk browseObsoleteMethodReferences"
	"Open a browser on all referenced behaviors that are obsolete"
	| obsClasses obsRefs references |
	references _ WriteStream on: Array new.
	obsClasses _ self obsoleteBehaviors.
	'Scanning for methods referencing obsolete classes' displayProgressAt: Sensor mousePoint
		from: 1 to: obsClasses size during:[:bar|
	obsClasses keysAndValuesDo:[:index :each|
		bar value: index.
		obsRefs _ self pointersTo: each except: obsClasses.
		obsRefs do:[:ref|
			"Figure out if it may be a global"
			((ref isVariableBinding) and:[ ref key isString "or Symbol" ]) ifTrue:[
				(self pointersTo: ref) do:[:meth|
					(meth is: #CompiledMethod) ifTrue:[
						meth methodReference ifNotNil: [ :mref|
							references nextPut: mref]]]]]].
	].
	^references contents! !


!Compiler methodsFor: 'public access' stamp: 'jmv 9/2/2013 10:40'!
from: textOrStream class: aClass classified: aCategory context: aContext notifying: req

	sourceStream := (textOrStream is: #Stream)
						ifTrue: [textOrStream]
						ifFalse: [ReadStream on: textOrStream asString].
	class := aClass.
	context := aContext.
	requestor := req.
	category := aCategory
! !


!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'jmv 9/2/2013 10:25'!
pushConstant: obj
	"Print the Push Constant, obj, on Top Of Stack bytecode."

	self print: (String streamContents:
				[:s |
				s nextPutAll: 'pushConstant: '.
				(obj isKindOf: LookupKey)
					ifFalse: [obj printOn: s]
					ifTrue: [obj key
						ifNotNil: [s nextPutAll: '##'; nextPutAll: obj key]
						ifNil: [s nextPutAll: '###'; nextPutAll: obj value soleInstance name]]]).

	(obj is: #CompiledMethod) ifTrue:
		[obj longPrintOn: stream indent: self indent + 2.
		^self].! !


!FileDirectoryWrapper methodsFor: 'as yet unclassified' stamp: 'jmv 9/2/2013 10:41'!
hasContents
	"Return whether this directory has subfolders. The value is cached to 
	avoid a performance penalty.	Also for performance reasons, the code 
	below will just assume that the directory does indeed have contents in a 
	few of cases:  
	1. If the item is not a FileDirectory (thus avoiding the cost 
	of refreshing directories that are not local) 
	2. If it's the root directory of a given volume 
	3. If there is an error computing the FileDirectory's contents
	"
	hasContents
		ifNil: [hasContents := true. "default"
			["Best test I could think of for determining if this is a local directory "
			((item is: #FileDirectory)
					and: ["test to see that it's not the root directory"
						"there has to be a better way of doing this test -tpr"
						item pathParts size > 1])
				ifTrue: [hasContents := self contents notEmpty]]
				on: Error
				do: [hasContents := true]].
	^ hasContents! !


!MenuMorph methodsFor: 'accessing' stamp: 'jmv 9/2/2013 10:37'!
items

	^ submorphs select: [:m | m is: #MenuItemMorph]
! !

!MenuMorph methodsFor: 'accessing' stamp: 'jmv 9/2/2013 10:37'!
lastItem
	^ submorphs reverse
		detect: [ :m | m is: #MenuItemMorph ]
		ifNone: [ submorphs last ]! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 9/2/2013 10:46'!
addLine
	"Append a divider line to this menu. Suppress duplicate lines."

	submorphs isEmpty ifTrue: [^ self].
	(self lastSubmorph class == MenuLineMorph)
		ifFalse: [self addMorphBack: MenuLineMorph new].
! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 9/2/2013 10:42'!
popUpAt: aPoint forHand: hand allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	| evt |
	self items isEmpty ifTrue: [^self].
	Theme current decorateMenu: self.
	(self submorphs select: [ :m | m is: #UpdatingMenuItemMorph ]) 
		do: [ :m | m updateContents].
	self runningWorld addMorphFront: self.
	self positionAt: aPoint forHand: hand.
	"Acquire focus for valid pop up behavior"
	oldKeyboardFocus _ hand keyboardFocus.
	hand newMouseFocus: self.
	aBoolean ifTrue: [ hand newKeyboardFocus: self ].
	evt _ hand lastEvent.
	(evt isKeyboard or: [ evt isMouse and: [ evt anyButtonPressed not ]]) 
		ifTrue: [
			"Select first item if button not down"
			self moveSelectionDown: 1 event: evt ]! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 9/2/2013 10:47'!
popUpAt: aPoint forHand: hand in: aWorld allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	self items isEmpty ifTrue: [ ^self ].
	Theme current decorateMenu: self.
	(self submorphs select: [ :m | m is: #UpdatingMenuItemMorph]) 
		do: [ :m | m updateContents].
	aWorld addMorphFront: self.
	self positionAt: aPoint forHand: hand.
	"Acquire focus for valid pop up behavior"
	oldKeyboardFocus _ hand keyboardFocus.
	hand newMouseFocus: self.
	aBoolean ifTrue: [ hand newKeyboardFocus: self ]! !

!MenuMorph methodsFor: 'keyboard control' stamp: 'jmv 9/2/2013 10:36'!
displayFiltered: evt
	| matchStr allItems isMatch matches feedbackMorph |
	matchStr _ self valueOfProperty: #matchString.
	allItems _ self submorphs select: [ :m |
		m is: #MenuItemMorph ].
	matches _ allItems select: [ :m |
		isMatch _ matchStr isEmpty or: [
			m contents
				includesSubstring: matchStr
				caseSensitive: false ].
		m isEnabled: isMatch.
		isMatch ].
	feedbackMorph _ self valueOfProperty: #feedbackMorph.
	feedbackMorph ifNil: [
		feedbackMorph _ StringMorph new color: Color veryDarkGray.
		self addMorphBack: feedbackMorph lock.
		feedbackMorph morphPosition: (0@ -20).
		self
			setProperty: #feedbackMorph
			toValue: feedbackMorph ].
	feedbackMorph contents: '<' , matchStr , '>'.
	matchStr isEmpty ifTrue: [
		feedbackMorph delete.
		self removeProperty: #feedbackMorph ].
	matches notEmpty ifTrue: [
		self
			selectItem: matches first
			event: evt ].! !

!MenuMorph methodsFor: 'keyboard control' stamp: 'jmv 9/2/2013 10:37'!
moveSelectionDown: direction event: evt
	"Move the current selection up or down by one, presumably under keyboard control.
	direction = +/-1"

	| index m |
	index _ (submorphs indexOf: selectedItem ifAbsent: [1-direction]) + direction.
	submorphs do: "Ensure finite"
		[:unused | m _ submorphs atWrap: index.
		((m is: #MenuItemMorph) and: [m isEnabled]) ifTrue:
			[^ self selectItem: m event: evt].
		"Keep looking for an enabled item"
		index _ index + direction sign].
	^ self selectItem: nil event: evt! !


!HierarchicalListMorph methodsFor: 'updating' stamp: 'jmv 9/2/2013 10:32'!
update: aSymbol
	super update: aSymbol.
	aSymbol == getSelectionSelector 
		ifTrue: [
			self selection: self getCurrentSelectionItem.
			^self ].
	aSymbol == getListSelector 
		ifTrue: [
			self list: self getList.
			^self ].

	"Indeed not pretty"
	( aSymbol notEmpty and: [aSymbol first == #openPath]) 
		ifTrue: [
			^(scroller submorphs at: 1 ifAbsent: [^self]) 
				openPath: aSymbol allButFirst adaptor: #asString compare: #=]! !


!CodeWindow methodsFor: 'updating' stamp: 'jmv 9/2/2013 10:25'!
decorateForInheritance
	"Check to see if the currently-viewed method has a super send or an override, and if so, change screen feedback, unless the #decorateBrowserButtons says not to."

	| cm aColor aButton flags buttonColor |
	(aButton _ self inheritanceButton) ifNil: [^ self].
	buttonColor _ self buttonColor.

	Preferences decorateBrowserButtons
		ifFalse: [ ^aButton color: buttonColor ].
	cm _ model currentCompiledMethod.
	(cm is: #CompiledMethod)
		ifFalse: [ ^aButton color: buttonColor ].

	flags _ 0.
	model isThisAnOverride ifTrue: [ flags _ flags bitOr: 4 ].
	cm sendsToSuper ifTrue: [ flags _ flags bitOr: 2 ].
	model isThereAnOverride ifTrue: [ flags _ flags bitOr: 1 ].
	aColor _ {

		"This is NOTan override. There is no super implementation."
		buttonColor.							"no sends to super. there is not override in any subclass"
		Color tan.							"no sends to super. there is an override in some subclass"
		Color red.							"sends to super. there is no override in any subclass. Error: no super to call (or calls super with a different message)"
		Color red.							"sends to super. there is  an override in some subclass. Error: no super to call (or calls super with a different message)"

		"This is an override. There is some super implementation"
		Color red muchLighter.			"doesn't have sub; has super but doesn't call it"
		Color r: 0.94 g: 0.823 b: 0.673.		"has sub; has super but doesn't call it"
		Color green muchLighter.			"doesn't have sub; has super and callsl it"
		Color blue muchLighter.			"has sub; has super and callsl it"

	} at: flags + 1.
	aButton color: aColor! !


!IndentingListItemMorph methodsFor: 'initialization' stamp: 'jmv 9/2/2013 10:51'!
initWithContents: anObject prior: priorMorph forList: hostList indentLevel: newLevel

	| o |
	container _ hostList.
	complexContents _ anObject.
	self initWithContents: anObject asString font: Preferences standardListFont emphasis: nil.
	indentLevel _ 0.
	isExpanded _ false.
 	nextSibling _ firstChild _ nil.
	priorMorph ifNotNil: [
		priorMorph nextSibling: self.
	].
	o _ anObject withoutListWrapper.
	icon _ (o respondsTo: #icon) ifTrue: [ o icon ].
	indentLevel _ newLevel.
! !

