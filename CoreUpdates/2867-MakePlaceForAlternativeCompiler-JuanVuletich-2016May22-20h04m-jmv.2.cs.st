'From Cuis 4.2 of 25 July 2013 [latest update: #2766] on 22 May 2016 at 8:13:48.925475 pm'!
Smalltalk at: #UseZCompiler put: nil!
Smalltalk renameClassNamed: #LiteralDictionary as: #ZLiteralDictionary!
Smalltalk renameClassNamed: #SyntaxErrorNotification as: #ZSyntaxErrorNotification!
!classDefinition: #ZSyntaxErrorNotification category: #'Compiler-Exceptions'!
Error subclass: #ZSyntaxErrorNotification
	instanceVariableNames: 'inClass code category doitFlag errorMessage location'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Exceptions'!
Smalltalk renameClassNamed: #UndeclaredVariableWarning as: #ZUndeclaredVariableWarning!
Smalltalk renameClassNamed: #ParserNotification as: #ZParserNotification!
Smalltalk renameClassNamed: #UndeclaredVariable as: #ZUndeclaredVariable!
Smalltalk renameClassNamed: #UndefinedVariable as: #ZUndefinedVariable!
Smalltalk renameClassNamed: #UnknownSelector as: #ZUnknownSelector!
Smalltalk renameClassNamed: #UnusedVariable as: #ZUnusedVariable!
Smalltalk renameClassNamed: #ReparseAfterSourceEditing as: #ZReparseAfterSourceEditing!
Smalltalk renameClassNamed: #UndeclaredVariableReference as: #ZUndeclaredVariableReference!
Smalltalk renameClassNamed: #Decompiler as: #ZDecompiler!
Smalltalk renameClassNamed: #CompiledMethodWithNode as: #ZCompiledMethodWithNode!
Smalltalk renameClassNamed: #Compiler as: #ZCompiler!
Smalltalk renameClassNamed: #InstructionClient as: #ZInstructionClient!
Smalltalk renameClassNamed: #BlockLocalTempCounter as: #ZBlockLocalTempCounter!
Smalltalk renameClassNamed: #BlockStartLocator as: #ZBlockStartLocator!
Smalltalk renameClassNamed: #ClosureExtractor as: #ZClosureExtractor!
Smalltalk renameClassNamed: #InstVarRefLocator as: #ZInstVarRefLocator!
Smalltalk renameClassNamed: #InstructionPrinter as: #ZInstructionPrinter!
Smalltalk renameClassNamed: #RelativeInstructionPrinter as: #ZRelativeInstructionPrinter!
Smalltalk renameClassNamed: #ParseNode as: #ZParseNode!
Smalltalk renameClassNamed: #AssignmentNode as: #ZAssignmentNode!
Smalltalk renameClassNamed: #BlockNode as: #ZBlockNode!
Smalltalk renameClassNamed: #BraceNode as: #ZBraceNode!
Smalltalk renameClassNamed: #CascadeNode as: #ZCascadeNode!
Smalltalk renameClassNamed: #DecompilerConstructor as: #ZDecompilerConstructor!
Smalltalk renameClassNamed: #DecompilerConstructorForClosures as: #ZDecompilerConstructorForClosures!
Smalltalk renameClassNamed: #Encoder as: #ZEncoder!
Smalltalk renameClassNamed: #BytecodeEncoder as: #ZBytecodeEncoder!
Smalltalk renameClassNamed: #EncoderForV3 as: #ZEncoderForV3!
Smalltalk renameClassNamed: #EncoderForV3PlusClosures as: #ZEncoderForV3PlusClosures!
Smalltalk renameClassNamed: #LeafNode as: #ZLeafNode!
Smalltalk renameClassNamed: #LiteralNode as: #ZLiteralNode!
Smalltalk renameClassNamed: #SelectorNode as: #ZSelectorNode!
Smalltalk renameClassNamed: #VariableNode as: #ZVariableNode!
Smalltalk renameClassNamed: #InstanceVariableNode as: #ZInstanceVariableNode!
Smalltalk renameClassNamed: #MaybeContextInstanceVariableNode as: #ZMaybeContextInstanceVariableNode!
Smalltalk renameClassNamed: #LiteralVariableNode as: #ZLiteralVariableNode!
Smalltalk renameClassNamed: #TempVariableNode as: #ZTempVariableNode!
Smalltalk renameClassNamed: #RemoteTempVectorNode as: #ZRemoteTempVectorNode!
Smalltalk renameClassNamed: #UndeclaredVariableNode as: #ZUndeclaredVariableNode!
Smalltalk renameClassNamed: #MessageNode as: #ZMessageNode!
Smalltalk renameClassNamed: #MessageAsTempNode as: #ZMessageAsTempNode!
Smalltalk renameClassNamed: #MethodNode as: #ZMethodNode!
Smalltalk renameClassNamed: #NewArrayNode as: #ZNewArrayNode!
Smalltalk renameClassNamed: #ReturnNode as: #ZReturnNode!
Smalltalk renameClassNamed: #ParseNodeVisitor as: #ZParseNodeVisitor!
Smalltalk renameClassNamed: #OptimizedBlockLocalTempReadBeforeWrittenVisitor as: #ZOptimizedBlockLocalTempReadBeforeWrittenVisitor!
Smalltalk renameClassNamed: #ParseNodeEnumerator as: #ZParseNodeEnumerator!
Smalltalk renameClassNamed: #VariableScopeFinder as: #ZVariableScopeFinder!
Smalltalk renameClassNamed: #ParseStack as: #ZParseStack!
Smalltalk renameClassNamed: #Scanner as: #ZScanner!
Smalltalk renameClassNamed: #Parser as: #ZParser!

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/25/2016 16:40'!
clsEncoderForV3
	^self useOldCompiler
		ifTrue: [ ZEncoderForV3 ]
		ifFalse: []! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/25/2016 16:40'!
clsEncoderForV3PlusClosures
	^self useOldCompiler
		ifTrue: [ ZEncoderForV3PlusClosures ]
		ifFalse: []! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/22/2016 22:37'!
clsParser
	^self useOldCompiler
		ifTrue: [ ZParser ]
		ifFalse: []! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/22/2016 11:00'!
clsBlockStartLocator
	^self useOldCompiler
		ifTrue: [ ZBlockStartLocator ]
		ifFalse: []! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/22/2016 11:01'!
clsClosureExtractor
	^self useOldCompiler
		ifTrue: [ ZClosureExtractor ]
		ifFalse: []! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/22/2016 11:03'!
clsCompiledMethodWithNode
	^self useOldCompiler
		ifTrue: [ ZCompiledMethodWithNode ]
		ifFalse: []! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/22/2016 11:10'!
clsCompiler
	^self useOldCompiler
		ifTrue: [ ZCompiler ]
		ifFalse: []! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/22/2016 11:31'!
clsDecompiler
	^self useOldCompiler
		ifTrue: [ ZDecompiler ]
		ifFalse: []! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/22/2016 11:02'!
clsInstVarRefLocator
	^self useOldCompiler
		ifTrue: [ ZInstVarRefLocator ]
		ifFalse: []! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/22/2016 10:59'!
clsInstructionClient
	^self useOldCompiler
		ifTrue: [ ZInstructionClient ]
		ifFalse: []! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/22/2016 11:02'!
clsInstructionPrinter
	^self useOldCompiler
		ifTrue: [ ZInstructionPrinter ]
		ifFalse: []! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/22/2016 10:47'!
clsParseNode
	^self useOldCompiler
		ifTrue: [ ZParseNode ]
		ifFalse: []! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/22/2016 11:04'!
clsParseNodeEnumerator
	^self useOldCompiler
		ifTrue: [ ZParseNodeEnumerator ]
		ifFalse: []! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/22/2016 11:03'!
clsRelativeInstructionPrinter
	^self useOldCompiler
		ifTrue: [ ZRelativeInstructionPrinter ]
		ifFalse: []! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/22/2016 11:32'!
clsScanner
	^self useOldCompiler
		ifTrue: [ ZScanner ]
		ifFalse: []! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/22/2016 11:08'!
clsSyntaxErrorNotification
	^self useOldCompiler
		ifTrue: [ ZSyntaxErrorNotification ]
		ifFalse: []! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/22/2016 11:06'!
clsUndeclaredVariableWarning
	^self useOldCompiler
		ifTrue: [ ZUndeclaredVariableWarning ]
		ifFalse: []! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/22/2016 11:10'!
useOldCompiler
	UseZCompiler ifNil: [ UseZCompiler _ true ].
	^ UseZCompiler! !


!Object class methodsFor: 'instance creation' stamp: 'jmv 5/22/2016 11:10'!
readFrom: aStream
	"Create an object based on the contents of aStream."

	| object ok |
	ok _ (aStream is: #Stream) or: [ aStream isString ].
	(ok or: [ aStream is: #Text ]) ifFalse: [ ^ self error: 'expected String or Text' ].
	object _ Smalltalk clsCompiler evaluate: aStream.
	(object isKindOf: self) ifFalse: [ self error: self name, ' expected' ].
	^ object! !


!Browser methodsFor: 'class functions' stamp: 'jmv 5/22/2016 11:12'!
copyClass
	| originalClass originalName copysName  newDefinition newMetaDefinition newClass |
	selectedClassName ifNil: [^ self].
	originalClass := self selectedClass.
	originalName := originalClass name.
	copysName _ self request: 'Please type new class name' initialAnswer: originalName.
	copysName = '' ifTrue: [^ self].  " Cancel returns '' "
	copysName _ copysName asSymbol.
	copysName = originalName ifTrue: [^ self].
	(Smalltalk includesKey: copysName)
		ifTrue: [^ self error: copysName , ' already exists'].
	newDefinition := originalClass definition
		copyReplaceAll: originalName printString
		with: copysName printString.
	newClass _ Smalltalk clsCompiler evaluate: newDefinition logged: true.
	newMetaDefinition := originalClass class definition
		copyReplaceAll: originalClass class name
		with: newClass class name.
	Smalltalk clsCompiler evaluate: newMetaDefinition logged: true.
	newClass copyAllCategoriesFrom: originalClass.
	newClass class copyAllCategoriesFrom: originalClass class.
	originalClass hasComment ifTrue: [
		newClass comment: originalClass comment ].			
	self classListIndex: 0.
	self changed: #classList! !

!Browser methodsFor: 'class functions' stamp: 'jmv 5/22/2016 11:12'!
defineClass: defString notifying: aRequestor  
	"The receiver's textual content is a request to define a new class. The
	source code is defString. If any errors occur in compilation, notify
	aRequestor."
	| oldClass class newClassName defTokens keywdIx |
	oldClass _ self selectedClassOrMetaClass.
	defTokens _ defString findTokens: Character separators.
	keywdIx _ defTokens findFirst: [ :x | x beginsWith: 'category' ].
	keywdIx _ defTokens findFirst: [ :x | '*subclass*' match: x ].
	newClassName _ (defTokens at: keywdIx+1) copyWithoutAll: '#()'.
	((oldClass isNil or: [ oldClass theNonMetaClass name asString ~= newClassName ])
		and: [ Smalltalk includesKey: newClassName asSymbol ]) ifTrue: [
			"Attempting to define new class over existing one when
				not looking at the original one in this browser..."
			(self confirm: (newClassName bold , ' is an existing class in this system.
Redefining it might cause serious problems.
Is this really what you want to do?'))
				ifFalse: [ ^ false ]].
	"ar 8/29/1999: Use oldClass superclass for defining oldClass
	since oldClass superclass knows the definerClass of oldClass."
	oldClass ifNotNil: [ oldClass _ oldClass superclass ].
	class _ Smalltalk clsCompiler
				evaluate: defString
				notifying: aRequestor
				logged: true.
	(class isKindOf: Behavior)
		ifTrue: [
				self changed: #systemCategoryList.
				self changed: #classList.
				self changed: #clearUserEdits.
				self setClass: class selector: nil.
				"self clearUserEditFlag; editClass."
				^ true ]
		ifFalse: [ ^ false ]! !


!MessageSet methodsFor: 'private' stamp: 'jmv 5/22/2016 11:12'!
contents: aString notifying: aRequestor 
	"Compile the code in aString. Notify aRequestor of any syntax errors. 
	Answer false if the compilation fails. Otherwise, if the compilation 
	created a new method, deselect the current selection. Then answer true."

	| category selector class oldSelector |
	self okayToAccept ifFalse: [^ false].
	selectedMessage ifNil: [^ false].
	class _ selectedMessage actualClass.
	oldSelector _ selectedMessage methodSymbol.
	(oldSelector notNil and: [oldSelector first isUppercase]) ifTrue:
		[oldSelector == #Comment ifTrue:
			[class comment: aString stamp: Utilities changeStamp.
			self triggerEvent: #annotationChanged.
 			self changed: #clearUserEdits.
			^ false].
		oldSelector == #Definition ifTrue: [
			Smalltalk clsCompiler
				evaluate: aString
				notifying: aRequestor
				logged: true.
			self changed: #clearUserEdits.
 			^ false].
		oldSelector == #Hierarchy ifTrue:
			[self inform: 'To change the hierarchy, edit the class definitions'. 
			^ false]].
	"Normal method accept"
	category _ class organization categoryOfElement: oldSelector.
	selector _ class compile: aString
				classified: category
				notifying: aRequestor.
	selector
		ifNil: [^ false].
	selector == oldSelector ifFalse: [
		self reformulateListNoting: selector].
	self triggerEvent: #annotationChanged.
	^ true! !


!ChangeList methodsFor: 'scanning' stamp: 'jmv 5/22/2016 11:33'!
scanSpecificChangeRecordType
	"Scan anything that involves more than one chunk"

	| itemPosition item item2 tokens firstToken secondToken stamp anIndex def isMeta name record methodReference doItOnlyIfInBaseSystem feature |
	itemPosition _ file position.
	item _ file nextChunk.
	(((item includesSubString: 'commentStamp:')
	or: [item includesSubString: 'methodsFor:']
	or: [item includesSubString: 'classDefinition:']
	or: [item includesSubString: 'classRemoval:']
	or: [item includesSubString: 'methodRemoval:'])
	or: [item includesSubString: 'methodMoveToSomePackage:']
	or: [item includesSubString: 'classMoveToSomePackage:']
	or: [item includesSubString: 'provides']
	or: [item includesSubString: 'requires'])
		ifFalse: [
			"Maybe a preamble, but not one we recognize; bail out with the preamble trick"
			^ self addItem: (ChangeRecord new file: file position: itemPosition type: #preamble)
				 text: ('preamble: ' , item contractTo: 160)].

	tokens _ Smalltalk clsScanner new scanTokens: item.
	tokens size >= 2 ifTrue: [
		stamp _ ''.
		anIndex _ tokens indexOf: #stamp: ifAbsent: nil.
		anIndex ifNotNil: [stamp _ tokens at: (anIndex + 1)].
		firstToken _ tokens first.
		secondToken _ tokens second.

		firstToken == #classDefinition: ifTrue: [
			itemPosition _ file position.
			isMeta _ secondToken includesSubString: ' class'.
			name _ isMeta ifTrue: [secondToken substrings first] ifFalse: [secondToken].
			def _ file nextChunk.
			record _ ChangeRecord new file: file position: itemPosition type: #classDefinition
				class: name asSymbol category: tokens last meta: isMeta stamp: nil.
			self addItem: record text: 'classDefinition: ', def.
			^ self ].

		(firstToken == #classRemoval: or: [ firstToken == #classMoveToSomePackage: ]) ifTrue: [
			doItOnlyIfInBaseSystem _ firstToken == #classMoveToSomePackage:.
			itemPosition _ file position.
			item2 _ file nextChunk.
			item2 size > 0 ifTrue: [
				self 
					addItem: (ClassDeletionChangeRecord new
						clsName: secondToken;
						doItOnlyIfInBaseSystem: doItOnlyIfInBaseSystem)
					text: 
						(doItOnlyIfInBaseSystem ifTrue: ['clase move to some package: '] ifFalse: ['class removal: ']), secondToken ].
			^ self ].

		(firstToken == #methodRemoval: or: [ firstToken == #methodMoveToSomePackage: ]) ifTrue: [
			doItOnlyIfInBaseSystem _ firstToken == #methodMoveToSomePackage:.
			itemPosition _ file position.
			item2 _ file nextChunk.
			item2 size > 0 ifTrue: [
				isMeta _ tokens third == #class.
				isMeta ifTrue: [secondToken substrings first] ifFalse: [secondToken].
				methodReference _ (MethodReference new
					setClassSymbol: secondToken
					classIsMeta: isMeta
					methodSymbol: tokens last
					stringVersion: secondToken, ' ', (isMeta ifTrue: ['class '] ifFalse: ['']), tokens last).
				self
					addItem: (MethodDeletionChangeRecord new
						methodReference: methodReference;
						doItOnlyIfInBaseSystem: doItOnlyIfInBaseSystem)
					text: 
						(doItOnlyIfInBaseSystem ifTrue: ['method move to some package: '] ifFalse: ['method removal: ']), 
							methodReference stringVersion ].
			^ self ].
		
		secondToken == #methodsFor: ifTrue: [
			^ self scanCategory: tokens third class: firstToken meta: false stamp: stamp].

		tokens third == #methodsFor: ifTrue: [
			^ self scanCategory: tokens fourth class: firstToken meta: true stamp: stamp].

		secondToken == #commentStamp: ifTrue: [
			stamp _ tokens third.
			self addItem:
					(ChangeRecord new file: file position: file position type: #classComment
									class: firstToken category: nil meta: false stamp: stamp)
					text: 'class comment for ' , firstToken, 
						  (stamp isEmpty ifTrue: [''] ifFalse: ['; ' , stamp]).
			file nextChunk.
			^ self ].

		firstToken == #provides: ifTrue: [
			feature _ FeatureChangeRecord new
					type: #provides
					feature: (Feature name: secondToken version: tokens third revision: tokens fourth).
			self
				addItem: feature
				text: feature string ].

		firstToken == #requires: ifTrue: [
			feature _ FeatureChangeRecord new
					type: #requires
					feature: (FeatureRequirement name: secondToken minVersion: tokens third minRevision: tokens fourth maxVersion: (tokens size > 4 ifTrue: [tokens fifth])).
			self
				addItem: feature
				text: feature string ].
	]! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 5/22/2016 11:08'!
selectEquivalentMethods
	"Selects all method definitions for which there is already an equivalent method in the current image, 
	(meaning that the difference is cosmetic and not in behavior)"
	1 to: changeList size do: [ :i | 
		| change class |
		change _ changeList at: i.
		listSelections at: i put:
			((change changeType == #method and: [
				(class _ change changeClass) notNil]) and: [
					(class includesSelector: change methodSelector) and: [
						| cmWithNode |
						cmWithNode _ [class basicCompile: change string notifying: nil trailer: class defaultMethodTrailer ifFail: nil] 
							on: Smalltalk clsSyntaxErrorNotification do: [ :ex | ex return ].
						(cmWithNode notNil and: [
							| current inChange |
							current _ (class compiledMethodAt: change methodSelector) copyWithTrailerBytes: #(0).
							inChange _ cmWithNode method copyWithTrailerBytes: #(0).
							current = inChange or: [
								| currentCmWithNode |
								currentCmWithNode _ [class basicCompile: (class decompilerClass new decompile: change methodSelector in: class) decompileString
										notifying: nil trailer: class defaultMethodTrailer ifFail: nil] on: Smalltalk clsSyntaxErrorNotification do: [ :ex | ex return ].
								(currentCmWithNode notNil and: [
									current _ currentCmWithNode method copyWithTrailerBytes: #(0).
									current = inChange])
							]
						])
					]]
			)].
	self changed: #allSelections.
	self changed: #annotation! !


!VersionsBrowser methodsFor: 'init & update' stamp: 'jmv 5/22/2016 11:33'!
scanVersionsOf: method class: class meta: meta category: category selector: selector
	| position prevPos prevFileIndex preamble tokens sourceFilesCopy stamp |
	selectorOfMethod _ selector.
	currentCompiledMethod _ method.
	classOfMethod _ meta ifTrue: [class class] ifFalse: [class].
	changeList _ OrderedCollection new.
	list _ OrderedCollection new.
	self addedChangeRecord ifNotNil: [ :change |
		self addItem: change text: ('{1} (in {2})' format: { change stamp. change fileName }) ].
	listIndex _ 0.
	position _ method filePosition.
	sourceFilesCopy _ SourceFiles collect:
		[:x | x ifNotNil: [ x name asFileEntry readStream ]].
	method fileIndex = 0 ifTrue: [^ nil].
	file _ sourceFilesCopy at: method fileIndex.
	[position notNil & file notNil]
		whileTrue:
		[file position: (0 max: position-150).  "Skip back to before the preamble"
		[file position < (position-1)]  "then pick it up from the front"
			whileTrue: [
				preamble _ file nextChunk.
				file skipSeparators			"Skip any padding"
				].

		"Preamble is likely a linked method preamble, if we're in
			a changes file (not the sources file).  Try to parse it
			for prior source position and file index"
		prevPos _ nil.
		stamp _ ''.
		(preamble findString: 'methodsFor:' startingAt: 1) > 0
			ifTrue: [tokens _ Smalltalk clsScanner new scanTokens: preamble]
			ifFalse: [tokens _ Array new  "ie cant be back ref"].
		((tokens size between: 7 and: 8)
			and: [(tokens at: tokens size-5) = #methodsFor:])
			ifTrue: [
				(tokens at: tokens size-3) = #stamp:
				ifTrue: ["New format gives change stamp and unified prior pointer"
						stamp _ tokens at: tokens size-2.
						prevPos _ tokens last.
						prevFileIndex _ sourceFilesCopy fileIndexFromSourcePointer: prevPos.
						prevPos _ sourceFilesCopy filePositionFromSourcePointer: prevPos]
				ifFalse: ["Old format gives no stamp; prior pointer in two parts"
						prevPos _ tokens at: tokens size-2.
						prevFileIndex _ tokens last].
				(prevPos = 0 or: [prevFileIndex = 0]) ifTrue: [prevPos _ nil]].
		((tokens size between: 5 and: 6)
			and: [(tokens at: tokens size-3) = #methodsFor:])
			ifTrue: [
				(tokens at: tokens size-1) = #stamp:
				ifTrue: ["New format gives change stamp and unified prior pointer"
						stamp _ tokens at: tokens size]].
 		self addItem:
				(ChangeRecord new file: file position: position type: #method
						class: class name category: category meta: meta stamp: stamp)
			text: stamp , ' ' , class name , (meta ifTrue: [' class '] ifFalse: [' ']) , selector.
		position _ prevPos.
		prevPos notNil ifTrue: [
			file _ sourceFilesCopy at: prevFileIndex]].
	sourceFilesCopy do: [:x | x notNil ifTrue: [x close]].
	self clearSelections! !


!ClassCommentVersionsBrowser methodsFor: 'basic function' stamp: 'jmv 5/22/2016 11:33'!
scanVersionsOf: class
	"Scan for all past versions of the class comment of the given class"

	| oldCommentRemoteStr sourceFilesCopy position prevPos stamp preamble tokens prevFileIndex |

	classOfMethod _ class.
	oldCommentRemoteStr _ class  organization commentRemoteStr.
	currentCompiledMethod _ oldCommentRemoteStr.
	selectorOfMethod _ #Comment.
	changeList _ OrderedCollection new.
	list _ OrderedCollection new.
	listIndex _ 0.
	oldCommentRemoteStr ifNil:[^ nil] ifNotNil: [oldCommentRemoteStr sourcePointer].

	sourceFilesCopy _ SourceFiles collect: [ :x | x ifNotNil: [x name asFileEntry readStream]].
	position _ oldCommentRemoteStr position.
	file _ sourceFilesCopy at: oldCommentRemoteStr sourceFileNumber.
	[ position notNil & file notNil]  whileTrue: [
		file position: (0 max: position-150).  " Skip back to before the preamble"
		[file position < (position-1)]  "then pick it up from the front"
			whileTrue: [
				preamble _ file nextChunk.
				file skipSeparators			"Skip any padding"
				].

		prevPos _ nil.
		stamp _ ''.
		(preamble findString: 'commentStamp:' startingAt: 1) > 0
			ifTrue: [
				tokens _ Smalltalk clsScanner new scanTokens: preamble.
				(tokens at: tokens size-3) = #commentStamp: ifTrue: [
					"New format gives change stamp and unified prior pointer"
					stamp _ tokens at: tokens size-2.
					prevPos _ tokens last.
					prevFileIndex _ sourceFilesCopy fileIndexFromSourcePointer: prevPos.
					prevPos _ sourceFilesCopy filePositionFromSourcePointer: prevPos]]
			ifFalse: [
				"The stamp get lost, maybe after a condenseChanges"
				stamp _ '<historical>'].
 		self addItem:
				(ChangeRecord new file: file position: position type: #classComment
						class: class name category: nil meta: class isMeta stamp: stamp)
			text: stamp , ' ' , class name , ' class comment'. 
		prevPos = 0 ifTrue: [ prevPos _ nil ].
		position _ prevPos.
		prevPos notNil ifTrue: [ file _ sourceFilesCopy at: prevFileIndex ]].
	sourceFilesCopy do: [ :x | x notNil ifTrue: [ x close ]].
	self clearSelections! !


!ChangeSorter methodsFor: 'code pane' stamp: 'jmv 5/22/2016 11:33'!
contents: aString notifying: aRequestor 
	"Compile the code in aString. Notify aRequestor of any syntax errors. 
	Create an error if the category of the selected message is unknown. 
	Answer false if the compilation fails. Otherwise, if the compilation 
	created a new method, deselect the current selection. Then answer true."
	| category selector class oldSelector |

	(class _ self selectedClassOrMetaClass) ifNil:
		[(myChangeSet preambleString == nil or: [aString size = 0]) ifTrue: [ ^ false].
		(aString count: [:char | char == $"]) odd 
			ifTrue: [self inform: 'unmatched double quotes in preamble']
			ifFalse: [(Smalltalk clsScanner new scanTokens: aString) size > 0 ifTrue: [
				self inform: 'Part of the preamble is not within double-quotes.
To put a double-quote inside a comment, type two double-quotes in a row.
(Ignore this warning if you are including a doIt in the preamble.)']].
		myChangeSet preambleString: aString.
		self currentSelector: nil.  "forces update with no 'unsubmitted chgs' feedback"
		^ true].
	oldSelector _ self selectedMessageName.
	category _ class organization categoryOfElement: oldSelector.
	selector _ class compile: aString
				classified: category
				notifying: aRequestor.
	selector ifNil: [^ false].
	(self messageList includes: selector)
		ifTrue: [self currentSelector: selector]
		ifFalse: [self currentSelector: oldSelector].
	self update.
	^ true! !


!Debugger methodsFor: 'context stack menu' stamp: 'jmv 5/22/2016 11:12'!
returnValue: expression
	"Force a return of a given value to the previous context!!"

	| previous selectedContext value |
	contextStackIndex = 0 ifTrue: [^Smalltalk beep].
	selectedContext _ self selectedContext.
	value _ Smalltalk clsCompiler new 
				evaluate: expression
				in: selectedContext
				to: selectedContext receiver.
	previous _ selectedContext sender.
	self resetContext: previous.
	interruptedProcess popTo: previous value: value! !


!SmalltalkCompleter methodsFor: 'entries' stamp: 'jmv 5/22/2016 11:13'!
computeEntries

	| allSource contextClass id p last3Ranges range prevRange receiverClass prevPrevRange |
	allSource _ model actualContents string.
	p _ (model is: #hasTextProvider)
		ifTrue: [ model textProvider ]
		ifFalse: [ model ].
	contextClass _ (p respondsTo: #selectedClassOrMetaClass) ifTrue: [
		p selectedClassOrMetaClass ].

	"Instead of creating a new string, maybe we could pass the last position to parse to Shout..."
	parser _ SHParserST80 new.
	parser
		workspace: ((model isMemberOf: Workspace) ifTrue: [ model ]);
		classOrMetaClass: contextClass;
		source: (allSource copyFrom: 1 to: position).
	parser parse.
	last3Ranges _ parser last3Ranges.
	range _ last3Ranges third.
	range ifNil: [ ^entries _ #() ].

	"If parsing breaks before position, then we don't know what to suggest, therefore don't open Completion"
	range end = position ifFalse: [ ^entries _ #() ].

	prefix _ allSource copyFrom: range start to: range end.
	
	(parser isMessage: range rangeType) ifTrue: [
		"If previous range is a constant or a well known identifier, we might filter messages"
		prevRange _ last3Ranges second.
		prevPrevRange _ last3Ranges first.
		receiverClass _ nil.
		"3 if -> ifNil: but not ifTrue:
		3=4 -> ifNil: or ifTrue:"
		(prevRange notNil and: [ prevPrevRange isNil or: [ (#(binary keyword) includes: prevPrevRange rangeType) not]]) ifTrue: [
			id _ (allSource copyFrom: prevRange start to: prevRange end).
			receiverClass _ prevRange rangeType caseOf: {
				[ #globalVar ] -> [ (Smalltalk at: id asSymbol) class ].
				[ #self ] -> [ contextClass ].
				[ #super ] -> [ contextClass superclass ].
				[ #true ] -> [ True ].
				[ #false ] -> [ False ].
				[ #nil ] -> [ UndefinedObject ].
				[ #character ] -> [ id first class ].
				[ #number ] -> [ (Smalltalk clsCompiler evaluate: id) class ].
				[ #string ] -> [ (Smalltalk clsCompiler evaluate: id) class ].
				[ #symbol ] -> [ (Smalltalk clsCompiler evaluate: id) class ].
				[ #stringSymbol ] -> [ (Smalltalk clsCompiler evaluate: id) class ].
				"thisContext could mean ContextPart or BlockClosure..."
				"[ #thisContext ] -> [ ContextPart ]"
			} otherwise: [ nil ]
		].
		^self computeMessageEntries: receiverClass ].

	(parser isPartialOrFullIdentifier: range rangeType) ifTrue: [
		^self computeIdentifierEntries ].
	
	"If we don't know what to do, do nothing"
	entries _ #()! !


!Behavior methodsFor: 'accessing' stamp: 'jmv 5/22/2016 11:12'!
compilerClass
	"Answer a compiler class appropriate for source methods of this class."

	^Smalltalk clsCompiler! !

!Behavior methodsFor: 'private' stamp: 'jmv 5/22/2016 11:04'!
basicCompile: code notifying: requestor trailer: bytes ifFail: failBlock
	"Compile code without logging the source in the changes file"

	| methodNode |
	methodNode _ self compilerClass new
				compile: code
				in: self
				notifying: requestor
				ifFail: failBlock.
	methodNode encoder requestor: requestor.
	^ Smalltalk clsCompiledMethodWithNode generateMethodFromNode: methodNode trailer: bytes.! !


!ClassDescription methodsFor: 'instance variables' stamp: 'jmv 5/22/2016 11:32'!
checkForInstVarsOK: instVarString
	"Return true if instVarString does no include any names used in a subclass"
	| instVarArray |
	instVarArray _ Smalltalk clsScanner new scanFieldNames: instVarString.
	self allSubclasses do:
		[:cl | cl instVarNames do:
			[:n | (instVarArray includes: n)
				ifTrue: [self error: n , ' is already used in ' , cl name.
						^ false]]].
	^ true! !


!Class methodsFor: 'initialization' stamp: 'jmv 5/22/2016 11:33'!
declare: varString 
	"Declare class variables common to all instances. Answer whether 
	recompilation is advisable."

	| newVars conflicts |
	newVars _ 
		(Smalltalk clsScanner new scanFieldNames: varString)
			collect: [:x | x asSymbol].
	newVars do: [:var | 
		var first isLowercase
			ifTrue: [self error: var, ' class variable name should be capitalized; proceed to include anyway.']].
	conflicts _ false.
	classPool
		ifNotNil: [
			(classPool keys reject: [:x | newVars includes: x]) 
				do: [:var | self removeClassVarName: var]].
	(newVars reject: [:var | self classPool includesKey: var])
		do: [:var | "adding"
			"check if new vars defined elsewhere"
			(self bindingOf: var)
				ifNotNil: [ 
					self error: var , ' is defined elsewhere'.
					conflicts _ true]].
	newVars size > 0
		ifTrue: [
			classPool _ self classPool.
			"in case it was nil"
			newVars do: [:var | classPool declare: var from: Undeclared]].
	^conflicts! !

!Class methodsFor: 'initialization' stamp: 'jmv 5/22/2016 11:33'!
sharing: poolString 
	"Set up sharedPools. Answer whether recompilation is advisable."
	| oldPools |
	oldPools _ self sharedPools.
	sharedPools _ OrderedCollection new.
	(Smalltalk clsScanner new scanFieldNames: poolString) do: 
		[:poolName | 
		sharedPools add: (Smalltalk at: poolName asSymbol ifAbsent:[
			(self confirm: 'The pool dictionary ', poolName,' does not exist.',
						'\Do you want it automatically created?' withNewLines)
				ifTrue:[Smalltalk at: poolName asSymbol put: Dictionary new]
				ifFalse:[^self error: poolName,' does not exist']])].
	sharedPools isEmpty ifTrue: [sharedPools _ nil].
	^oldPools anySatisfy: [ :pool |
		self sharedPools noneSatisfy: [ :p | p == pool ]]! !


!BlockClosure methodsFor: 'printing' stamp: 'jmv 5/22/2016 11:31'!
decompile
	^Smalltalk clsDecompiler new decompileBlock: self! !

!BlockClosure methodsFor: 'testing' stamp: 'jmv 5/22/2016 11:12'!
isTrivialClosure
	"A trivial closure is one that doesn't really need the home context because:
		- It doesn't send messages to self or super
		- It doesn't access any instance variable
		- It doesn't access any outer temp
		- It doesn't do ^ return (return from method to caller)
	Therefore it doesn't close over a lexical scope, and in this sense they are trivial.
	They can also be called 'context free', 'clean' or 'simple block'.
	"

	| recreated source |
	source _ self decompile decompileString.

	"This catches any acess to outer context!!"
	recreated _ [ Smalltalk clsCompiler evaluate: source ] on: Smalltalk clsUndeclaredVariableWarning do: [ :ex | ^ false ].

	"Fail if returns from outer context, or uses self"
	Smalltalk
		eliotsClosureMeasurementsOn: recreated outerContext method
		over: [ :closuresCount :hasIndirectTemps :anyClosureHasCopied :anyClosureDoesMethodReturn :anyClosureUsesSelf |
			anyClosureDoesMethodReturn ifTrue: [ ^ false ].
			anyClosureUsesSelf ifTrue: [ ^ false ]].

	"Ok."
	^true! !


!Categorizer methodsFor: 'accessing' stamp: 'jmv 5/22/2016 11:33'!
changeFromString: aString 
	"Parse the argument, aString, and make this be the receiver's structure."

	| categorySpecs |
	categorySpecs _ Smalltalk clsScanner new scanTokens: aString.
	"If nothing was scanned and I had no elements before, then default me"
	(categorySpecs isEmpty and: [elementArray isEmpty])
		ifTrue: [^ self setDefaultList: Array new].

	^ self changeFromCategorySpecs: categorySpecs! !


!ChangeRecord methodsFor: 'access' stamp: 'jmv 5/22/2016 11:13'!
compilerClass
	^ (Smalltalk at: class ifAbsent: [^ Smalltalk clsCompiler]) compilerClass! !

!ChangeRecord methodsFor: 'initialization' stamp: 'jmv 5/22/2016 11:13'!
fileIn
	"File the receiver in.  If I represent a method or a class-comment, file the method in and make a note of it in the recent-submissions list; if I represent a do-it, then, well, do it."
	| s |
	type == #method ifTrue: [
		self changeClass ifNotNil: [ :methodClass |
			methodClass
				compile: self text
				classified: category
				withStamp: stamp
				notifying: nil ]].
	type == #doIt ifTrue: [
		((s _ self string) beginsWith: '----') ifFalse: [ Smalltalk clsCompiler evaluate: s ]].
	type == #classDefinition ifTrue: [ Smalltalk clsCompiler evaluate: self string ].
	type == #classComment ifTrue: [
		(Smalltalk at: class asSymbol)
			comment: self text
			stamp: stamp ]! !


!ClassBuilder methodsFor: 'class definition' stamp: 'jmv 5/22/2016 11:33'!
class: oldClass instanceVariableNames: instVarString unsafe: unsafe
	"This is the basic initialization message to change the definition of
	an existing Metaclass"
	| instVars newClass needNew copyOfOldClass |
	instVars _ Smalltalk clsScanner new scanFieldNames: instVarString.
	unsafe ifFalse:[
		"Run validation checks so we know that we have a good chance for recompilation"
		(self validateInstvars: instVars from: oldClass forSuper: oldClass superclass) ifFalse:[^nil].
		(self validateSubclassFormat: oldClass typeOfClass from: oldClass forSuper: oldClass superclass extra: instVars size) ifFalse:[^nil]].
	"See if we need a new subclass or not"
	needNew _ self needsSubclassOf: oldClass superclass type: oldClass typeOfClass instanceVariables: instVars from: oldClass.
	needNew ifNil:[^nil]. "some error"
	needNew ifFalse:[^oldClass]. "no new class needed"

	"Create the new class"
	copyOfOldClass _ oldClass copy.
	newClass _ self 
		newSubclassOf: oldClass superclass 
		type: oldClass typeOfClass
		instanceVariables: instVars
		from: oldClass.
		
	newClass _ self recompile: false from: oldClass to: newClass mutate: false.
	self doneCompiling: newClass.
	SystemChangeNotifier uniqueInstance classDefinitionChangedFrom: copyOfOldClass to: newClass.
	^newClass! !

!ClassBuilder methodsFor: 'class definition' stamp: 'jmv 5/22/2016 11:33'!
name: className subclassOf: newSuper type: type instanceVariableNames: instVarString classVariableNames: classVarString poolDictionaries: poolString category: category unsafe: unsafe
	"Define a new class.
	If unsafe is true do not run any validation checks.
	This facility is provided to implement important system changes."
	| oldClass newClass organization instVars classVars force needNew oldCategory copyOfOldClass newCategory |
	instVars _ Smalltalk clsScanner new scanFieldNames: instVarString.
	classVars _ (Smalltalk clsScanner new scanFieldNames: classVarString) collect: [:x | x asSymbol].

	"Validate the proposed name"
	unsafe ifFalse:[(self validateClassName: className) ifFalse:[^nil]].
	oldClass _ Smalltalk at: className ifAbsent: nil.
	oldClass isBehavior 
		ifFalse:[oldClass _ nil]. "Already checked in #validateClassName:"
	copyOfOldClass _ oldClass copy.

	unsafe ifFalse:[
		"Run validation checks so we know that we have a good chance for recompilation"
		(self validateSuperclass: newSuper forSubclass: oldClass) ifFalse:[^nil].
		(self validateInstvars: instVars from: oldClass forSuper: newSuper) ifFalse:[^nil].
		(self validateClassvars: classVars from: oldClass forSuper: newSuper) ifFalse:[^nil].
		(self validateSubclassFormat: type from: oldClass forSuper: newSuper extra: instVars size) ifFalse:[^nil]].

	"See if we need a new subclass"
	needNew _ self needsSubclassOf: newSuper type: type instanceVariables: instVars from: oldClass.
	needNew ifNil: [^nil]. "some error"

	(needNew and:[unsafe not]) ifTrue:[
		"Make sure we don't redefine any dangerous classes"
		(self tooDangerousClasses includes: oldClass name) ifTrue:[
			self error: oldClass name, ' cannot be changed'.
		].
		"Check if the receiver should not be redefined"
		(oldClass notNil and:[oldClass shouldNotBeRedefined]) ifTrue:[
			self notify: oldClass name asText allBold, 
						' should not be redefined!! \Proceed to store over it.' withNewLines]].

	needNew ifTrue:[
		"Create the new class"
		newClass _ self 
			newSubclassOf: newSuper 
			type: type 
			instanceVariables: instVars
			from: oldClass.
		newClass ifNil: [ ^nil]. "Some error"
		newClass setName: className.
	] ifFalse:[
		"Reuse the old class"
		newClass _ oldClass.
	].

	"Install the class variables and pool dictionaries... "
	force _ (newClass declare: classVarString) | (newClass sharing: poolString).

	"... classify ..."
	newCategory _ category asSymbol.
	organization _ Smalltalk organization.
	oldClass ifNotNil: [oldCategory := (organization categoryOfElement: oldClass name) asSymbol].
	organization classify: newClass name under: newCategory.

	"... recompile ..."
	newClass _ self recompile: force from: oldClass to: newClass mutate: false.

	"... export if not yet done ..."
	(Smalltalk at: newClass name ifAbsent: nil) == newClass ifFalse:[
		[Smalltalk at: newClass name put: newClass]
			on: AttemptToWriteReadOnlyGlobal do:[:ex| ex resume: true].
		Smalltalk flushClassNameCache.
	].

	self doneCompiling: newClass.
	
	"... notify interested clients ..."
	oldClass ifNil: [
		SystemChangeNotifier uniqueInstance classAdded: newClass inCategory: newCategory.
		^ newClass].
	SystemChangeNotifier uniqueInstance classDefinitionChangedFrom: copyOfOldClass to: newClass.
	newCategory ~= oldCategory 
		ifTrue: [SystemChangeNotifier uniqueInstance classRecategorized: newClass from: oldCategory to: category].
	^newClass! !


!CodeFile methodsFor: 'private' stamp: 'jmv 5/22/2016 11:34'!
classDefinition: string with: chgRec
	| tokens theClass |
	tokens := Smalltalk clsScanner new scanTokens: string.
	tokens size = 11 ifFalse:[^doIts add: chgRec].
	theClass := self getClass: (tokens at: 3).
	theClass definition: string.
	classOrder add: theClass.! !

!CodeFile methodsFor: 'private' stamp: 'jmv 5/22/2016 11:35'!
metaClassDefinition: string with: chgRec
	| tokens theClass |
	tokens := Smalltalk clsScanner new scanTokens: string.
	theClass := self getClass: (tokens at: 1).
	theClass metaClass definition: string.
	classOrder add: theClass metaClass.! !

!CodeFile methodsFor: 'private' stamp: 'jmv 5/22/2016 11:35'!
msgClassComment: string with: chgRec
	| tokens theClass |
	tokens := Smalltalk clsScanner new scanTokens: string.
	(tokens size = 3 and:[(tokens at: 3) class == String]) ifTrue:[
		theClass := self getClass: tokens first.
		^theClass classComment: tokens last].
	(tokens size = 4 and:[(tokens at: 3) asString = 'class' and:[(tokens at: 4) class == String]]) ifTrue:[
		theClass := self getClass: tokens first.
		theClass metaClass classComment: tokens last].
! !

!CodeFile methodsFor: 'private' stamp: 'jmv 5/22/2016 11:34'!
possibleSystemSource: chgRec
	| tokens |
	sourceSystem isEmpty ifTrue:[
		tokens := Smalltalk clsScanner new scanTokens: chgRec string.
		(tokens size = 1 and:[tokens first class == String]) ifTrue:[
			sourceSystem := tokens first.
			^self]].
	doIts add: chgRec.! !

!CodeFile methodsFor: 'private' stamp: 'jmv 5/22/2016 11:35'!
removedMethod: string with: chgRec
	| class tokens firstToken secondToken thirdToken |
	tokens _ Smalltalk clsScanner new scanTokens: string.
	tokens size >= 3 ifTrue: [
		firstToken _ tokens at: 1.
		secondToken _ tokens at: 2.
		thirdToken _ tokens at: 3.
		(tokens size = 3 and: [ secondToken == #removeSelector: or: [ secondToken == #removeSelectorIfInBaseSystem: ]]) ifTrue:[
			class _ self getClass: firstToken.
			^class perform: secondToken with: thirdToken.
		].
		(tokens size = 4 and: [ secondToken == #class and: [ thirdToken == #removeSelector: or: [ thirdToken == #removeSelectorIfInBaseSystem: ]]]) ifTrue:[
			class _ self getClass: firstToken.
			^class metaClass perform: thirdToken with: (tokens at: 4).
		].
	].
	doIts add: chgRec! !


!CodePackageFile methodsFor: 'change record types' stamp: 'jmv 5/22/2016 11:35'!
possibleDescription: chgRec
	| tokens prefix token |
	description isEmpty ifTrue:[
		tokens _ Smalltalk clsScanner new scanTokens: chgRec string.
		(tokens size = 1 and: [ (token _ tokens first) class == String ]) ifTrue: [
			prefix _ 'Description '.
			(token beginsWith: prefix) ifTrue: [
				description _ token copyFrom: prefix size + 1 to: token size.
				^self ]]].
	doIts add: chgRec.! !


!CodePackage methodsFor: 'testing' stamp: 'jmv 5/22/2016 11:35'!
changeRecordForOverriddenMethod: aMethodReference
	| sourceFilesCopy method position |
	method := aMethodReference actualClass compiledMethodAt: aMethodReference methodSymbol.
	position := method filePosition.
	sourceFilesCopy := SourceFiles collect:
		[:x | x isNil ifTrue: [ nil ]
				ifFalse: [x name asFileEntry readStream]].
	[ | file prevPos prevFileIndex chunk stamp methodCategory tokens |
	method fileIndex = 0 ifTrue: [^ nil].
	file := sourceFilesCopy at: method fileIndex.
	[position notNil & file notNil] whileTrue: [
		file position: (0 max: position-150).  "Skip back to before the preamble"
		[file position < (position-1)]  "then pick it up from the front"
			whileTrue: [ chunk _ file nextChunk ].

		"Preamble is likely a linked method preamble, if we're in
			a changes file (not the sources file).  Try to parse it
			for prior source position and file index"
		prevPos := nil.
		stamp := ''.
		(chunk findString: 'methodsFor:' startingAt: 1) > 0
			ifTrue: [tokens := Smalltalk clsScanner new scanTokens: chunk]
			ifFalse: [tokens := #()  "ie cant be back ref"].
		((tokens size between: 7 and: 8)
			and: [(tokens at: tokens size-5) = #methodsFor:])
			ifTrue:
				[(tokens at: tokens size-3) = #stamp:
				ifTrue: ["New format gives change stamp and unified prior pointer"
						stamp := tokens at: tokens size-2.
						prevPos := tokens last.
						prevFileIndex := sourceFilesCopy fileIndexFromSourcePointer: prevPos.
						prevPos := sourceFilesCopy filePositionFromSourcePointer: prevPos]
				ifFalse: ["Old format gives no stamp; prior pointer in two parts"
						prevPos := tokens at: tokens size-2.
						prevFileIndex := tokens last].
				(prevPos = 0 or: [prevFileIndex = 0]) ifTrue: [prevPos := nil]].
		((tokens size between: 5 and: 6)
			and: [(tokens at: tokens size-3) = #methodsFor:])
			ifTrue:
				[(tokens at: tokens size-1) = #stamp:
				ifTrue: ["New format gives change stamp and unified prior pointer"
						stamp := tokens at: tokens size]].
		methodCategory := (tokens after: #methodsFor:) ifNil: ['as yet unclassifed'].
		(self includesMethodCategory: methodCategory ofClass: aMethodReference actualClass) ifTrue:
			[methodCategory = (Smalltalk at: #Categorizer ifAbsent: [Smalltalk at: #ClassOrganizer]) default ifTrue: [methodCategory := methodCategory, ' '].
			^ ChangeRecord new file: file position: position type: #method
						class: aMethodReference classSymbol category: methodCategory meta: aMethodReference classIsMeta stamp: stamp].
		position := prevPos.
		prevPos notNil ifTrue: [
			file _ sourceFilesCopy at: prevFileIndex]].
		^ nil]
			ensure: [sourceFilesCopy do: [:x | x notNil ifTrue: [x close]]]
	! !


!Array methodsFor: 'converting' stamp: 'jmv 5/22/2016 11:11'!
evalStrings
	   "Allows you to construct literal arrays.
    #(true false nil '5@6' 'Set new' '''text string''') evalStrings
    gives an array with true, false, nil, a Point, a Set, and a String
    instead of just a bunch of Symbols"

    ^ self collect: [:each |  | it |
        it _ each.
        each == #true ifTrue: [it _ true].
		      each == #false ifTrue: [it _ false].
        each == #nil ifTrue: [it _ nil].
        each class == String ifTrue: [
			it _ Smalltalk clsCompiler evaluate: each].
        each class == Array ifTrue: [it _ it evalStrings].
        it]! !


!CompiledMethod methodsFor: 'printing' stamp: 'jmv 5/22/2016 11:02'!
longPrintOn: aStream indent: tabs
	"List of all the byte codes in a method with a short description of each" 

	self isQuick ifTrue: 
		[self isReturnSpecial ifTrue:
			[^ aStream tab: tabs; nextPutAll: 'Quick return ' , 
				(#('self' 'true' 'false' 'nil' '-1' '0' '1' '2') at: self primitive - 255)].
		^ aStream nextPutAll: 'Quick return field ' , self returnField printString , ' (0-based)'].

	self primitive = 0 ifFalse: [
		aStream tab: tabs.
		self printPrimitiveOn: aStream.
	].
	(Smalltalk clsInstructionPrinter on: self) indent: tabs; printInstructionsOn: aStream.
! !

!CompiledMethod methodsFor: 'printing' stamp: 'jmv 5/22/2016 11:03'!
longPrintRelativeOn: aStream indent: tabs
	"List of all the byte codes in a method with a short description of each" 

	self isQuick ifTrue: 
		[^self longPrintOn: aStream indent: tabs].
	self primitive = 0 ifFalse:
		[aStream tab: tabs. self printPrimitiveOn: aStream].
	(Smalltalk clsRelativeInstructionPrinter on: self)
		indent: tabs;
		printCode: false;
		printInstructionsOn: aStream.
! !

!CompiledMethod methodsFor: 'printing' stamp: 'jmv 5/22/2016 11:02'!
symbolicLinesDo: aBlock
	"Evaluate aBlock with each of the lines in the symbolic output."

	| aStream pc firstLine |
	aStream := ReadWriteStream on: (String new: 64).
	self isQuick ifTrue:
		[self longPrintOn: aStream.
		 aBlock value: 0 value: aStream contents.
		 ^self].

	self primitive ~= 0 ifTrue:
		[self printPrimitiveOn: aStream.
		 aBlock value: 1 value: aStream contents.
		 aStream resetContents].

	pc := self initialPC.
	(Smalltalk clsInstructionPrinter on: self)
		indent: 0;
		printPC: false; "explorer provides pc anyway"
		printInstructionsOn: aStream
		do:	[:printer :scanner :stream| | line index |
			line := stream contents allButLast.
			firstLine _ line lines first.
			firstLine size < line size ifTrue: [
				line _ firstLine, '...'' (continues)'].
			(index := line indexOf: $>) > 0 ifTrue:
				[[(line at: index + 1) isSeparator] whileTrue: [index := index + 1].
				 line := ((line copyFrom: 1 to: index) copyReplaceAll: (String with: Character tab) with: (String new: 8 withAll: Character space)),
						(line copyFrom: index + 1 to: line size)].
			aBlock value: pc value: line.
			pc := scanner pc.
			stream resetContents]! !

!CompiledMethod methodsFor: 'scanning' stamp: 'jmv 5/22/2016 11:14'!
hasInstVarRef
	"Answer whether the method references an instance variable."

	| scanner end printer |

	scanner := InstructionStream on: self.
	printer := Smalltalk clsInstVarRefLocator new.
	end := self endPC.

	[scanner pc <= end] whileTrue: [
		(printer interpretNextInstructionUsing: scanner) ifTrue: [^true].
	].
	^false! !

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 5/22/2016 11:11'!
sourceClass
	"Get my receiver class (method class) from the preamble of my source.  Return nil if not found."

	^ [(Smalltalk clsCompiler evaluate: (self sourceFileStream backChunk "blank"; backChunk "preamble")) theClass] on: Error do: nil! !

!CompiledMethod methodsFor: 'decompiling' stamp: 'jmv 5/22/2016 11:11'!
compilerClass
	^self methodClass 
		ifNil: [Smalltalk clsCompiler] 
		ifNotNil: [:class | class compilerClass].! !

!CompiledMethod methodsFor: 'decompiling' stamp: 'jmv 5/25/2016 16:41'!
methodNode
	"Return the parse tree that represents self"
	| aClass source |
	aClass := self methodClass.
	source := self
				getSourceFor: (self selector ifNil: [self defaultSelector])
				in: aClass.
	^(aClass parserClass new
		encoderClass: (self isBlueBookCompiled
						ifTrue: [Smalltalk clsEncoderForV3]
						ifFalse: [Smalltalk clsEncoderForV3PlusClosures]);
		parse: source class: aClass)
			sourceText: source;
			yourself! !

!CompiledMethod methodsFor: 'decompiling' stamp: 'jmv 5/22/2016 11:11'!
parserClass
	^self methodClass 
		ifNil: [Smalltalk clsCompiler parserClass] 
		ifNotNil: [:class | class parserClass].! !

!CompiledMethod methodsFor: 'debugger support' stamp: 'jmv 5/22/2016 11:29'!
abstractPCForConcretePC: concretePC
	"Answer the abstractPC matching concretePC."

	| abstractPC scanner client |
	self flag: 'belongs in DebuggerMethodMap?'.
	abstractPC := 1.
	scanner := InstructionStream on: self.
	client := Smalltalk clsInstructionClient new.
	[(scanner atEnd
	  or: [scanner pc >= concretePC]) ifTrue:
		[^abstractPC].
	 abstractPC := abstractPC + 1.
	 scanner interpretNextInstructionFor: client.
	 true] whileTrue! !

!CompiledMethod methodsFor: 'debugger support' stamp: 'jmv 5/22/2016 11:00'!
blockExtentsInto: aDictionary from: initialPC to: endPC scanner: scanner numberer: numbererBlock
	"Support routine for startpcsToBlockExtents"
	| extentStart blockSizeOrLocator |
	self flag: 'belongs in DebuggerMethodMap'.
	extentStart := numbererBlock value.
	[scanner pc <= endPC] whileTrue:
		[blockSizeOrLocator := scanner interpretNextInstructionFor: Smalltalk clsBlockStartLocator new.
		 blockSizeOrLocator isInteger ifTrue:
			[self
				blockExtentsInto: aDictionary
				from: scanner pc
				to: scanner pc + blockSizeOrLocator - 1
				scanner: scanner
				numberer: numbererBlock]].
	aDictionary at: initialPC put: (extentStart to: numbererBlock value).
	^aDictionary! !

!CompiledMethod methodsFor: 'debugger support' stamp: 'jmv 5/22/2016 11:29'!
pcPreviousTo: pc
	| scanner client prevPc |
	self flag: 'belongs in DebuggerMethodMap?'.
	pc > self endPC ifTrue: [^self endPC].
	scanner := InstructionStream on: self.
	client := Smalltalk clsInstructionClient new.
	[scanner pc < pc] whileTrue:
		[prevPc := scanner pc.
		 scanner interpretNextInstructionFor: client].
	^prevPc! !

!CompiledMethod methodsFor: 'closures' stamp: 'jmv 5/22/2016 11:01'!
embeddedBlockClosures
	"
	(CompiledMethod >> #embeddedBlockClosures)embeddedBlockClosures
	"
	| bms extractor scanner |
	bms := OrderedCollection new.
	scanner := self scanner.
	extractor := Smalltalk clsClosureExtractor withAction: [ :c | bms add: c ] andScanner: scanner.
	[ scanner pc <= self endPC ] whileTrue: [ scanner interpretNextInstructionFor: extractor ].
	^ bms! !

!CompiledMethod methodsFor: 'time stamp' stamp: 'jmv 5/22/2016 11:32'!
timeStamp
	"Answer the authoring time-stamp for the given method, retrieved from the sources or changes file. Answer the empty string if no time stamp is available."

	"(CompiledMethod compiledMethodAt: #timeStamp) timeStamp"

	| file preamble stamp tokens tokenCount |
	self fileIndex = 0 ifTrue: [^ String new].  "no source pointer for this method"
	file _ SourceFiles at: self fileIndex.
	file ifNil: [^ String new].  "sources file not available"
	"file does not exist happens in secure mode"
	file _ [file name asFileEntry readStream] on: FileDoesNotExistException do: [ :ex| nil ].
	file ifNil: [^ String new].
	preamble _ self getPreambleFrom: file at: (0 max: self filePosition).
	stamp _ String new.
	tokens _ (preamble findString: 'methodsFor:' startingAt: 1) > 0
		ifTrue: [Smalltalk clsScanner new scanTokens: preamble]
		ifFalse: [Array new  "ie cant be back ref"].
	(((tokenCount _ tokens size) between: 7 and: 8) and: [(tokens at: tokenCount - 5) = #methodsFor:])
		ifTrue:
			[(tokens at: tokenCount - 3) = #stamp:
				ifTrue: ["New format gives change stamp and unified prior pointer"
						stamp _ tokens at: tokenCount - 2]].
	((tokenCount between: 5 and: 6) and: [(tokens at: tokenCount - 3) = #methodsFor:])
		ifTrue:
			[(tokens at: tokenCount  - 1) = #stamp:
				ifTrue: ["New format gives change stamp and unified prior pointer"
					stamp _ tokens at: tokenCount]].
	file close.
	^ stamp! !

!CompiledMethod methodsFor: 'inspecting' stamp: 'jmv 5/22/2016 11:32'!
explorerContents
	"(CompiledMethod compiledMethodAt: #explorerContents) explore"
	
	^Array streamContents:
		[:s| | tokens |
		tokens := Smalltalk clsScanner new scanTokens: (self headerDescription readStream skipTo: $"; upTo: $").
		s nextPut: (ObjectExplorerWrapper
						with: ((0 to: tokens size by: 2) collect:
								[:i| i = 0 ifTrue: [self header] ifFalse: [{tokens at: i - 1. tokens at: i}]])
						name: 'header'
						model: self).
		(1 to: self numLiterals) do:
			[:key|
			s nextPut: (ObjectExplorerWrapper
							with: (self literalAt: key)
							name: ('literal', key printString contractTo: 32)
							model: self)].
		self isQuick
			ifTrue: [s nextPut: (ObjectExplorerWrapper
									with: self symbolic
									name: #symbolic
									model: self)]
			ifFalse:
				[self symbolicLinesDo:
					[:pc :line|
					pc <= 1
						ifTrue:
							[s nextPut: (ObjectExplorerWrapper
											with: line
											name: 'pragma'
											model: self)]
						ifFalse:
							[s nextPut: (ObjectExplorerWrapper
											with: line
											name: pc printString
											model: self)]]].
				"should be self numLiterals + 1 * Smalltalk wordSize + 1"
		self endPC + 1
			to: self basicSize
			do: [:key|
				s nextPut: (ObjectExplorerWrapper
								with: (self basicAt: key)
								name: key printString
								model: self)]]! !


!String methodsFor: 'converting' stamp: 'jmv 5/22/2016 11:32'!
findSelector
    "Revised to use scanner for better removal of extraneous stuff"

    | sel |

    sel := self withBlanksTrimmed.
    (sel includes: $:) ifTrue: [sel _ Smalltalk clsScanner findSelectorIn: sel].
    sel isEmpty ifTrue: [^ nil].
    Symbol hasInterned: sel ifTrue:
        [:aSymbol | ^ aSymbol].
    ^ nil! !


!Symbol methodsFor: 'printing' stamp: 'jmv 5/22/2016 11:32'!
storeOn: aStream 

	aStream nextPut: $#.
	(Smalltalk clsScanner isLiteralSymbol: self)
		ifTrue: [aStream nextPutAll: self]
		ifFalse: [super storeOn: aStream]! !


!Symbol class methodsFor: 'instance creation' stamp: 'jmv 5/22/2016 11:32'!
readFrom: strm
	"
	Symbol readFrom: '#abc' readStream
	"

	strm peek = $# ifFalse: [self error: 'Symbols must be introduced by #'].
	^ (Smalltalk clsScanner new scan: strm) advance  "Just do what the code scanner does"! !


!ContentPack methodsFor: 'importing' stamp: 'jmv 5/22/2016 11:11'!
decodeContentFrom: associationList

	| associations | 
	
	associations _ associationList collect: [ :assoc |
		(assoc at: 2) class = Array
			ifTrue: [ (assoc at: 1) -> (self decodeContentFrom: (assoc at: 2)) ]
			ifFalse: [ 
				(assoc at: 1) -> (Smalltalk clsCompiler 
					evaluate: (self 
						perform: 
							('object' , (assoc at: 2) 
								asString) asSymbol) base64Decoded)
			]
	].

	^ associations as: Dictionary! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 5/22/2016 11:04'!
browseEqEqSentToSmallIntegerConstants
	"
	Smalltalk browseEqEqSentToSmallIntegerConstants
	"
	| hasMatch visitor |
	hasMatch _ false.
	visitor _ Smalltalk clsParseNodeEnumerator
		ofBlock: [ :node |
			(node isMessageNode and: [
				(#(#== #~~ ) identityIncludes: node selector key) and: [
					node receiver isConstantNumber or: [ node arguments first isConstantNumber ]]]) ifTrue: [
						hasMatch _ true ]]
		select: [ :node |
			hasMatch not ].
	Smalltalk browseAllSelect: [ :method |
		hasMatch _ false.
		method decompile accept: visitor.
		hasMatch ].! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 5/22/2016 11:32'!
printStuffToCleanOnImageSave
	"So far, to serve experiments.
	Some day, when this is empty, we'll be closer to bootstrap from sources."
	| n nonNilVars m classInstVars v|
	n _ 0.
	""
	'--------' print.
	'Proceeses: ' print.
	((Process allSubInstances reject: [:each | each isTerminated ])
		sort: [ :a :b | a priority >= b priority ]) do: [ :p | p print ].
	'======' print.
	'Globals: ' print.
	Smalltalk globals associationsDo: [ :g | " {g key. g value class }" g  print ].
	'======' print.
	'ClassVars (notNil):  ' print.
	n _ 0.
	Smalltalk hierachySortedAllClassesDo: [ :cls |
		"Cleared, but come again for lazy init during this report.
		therefore, not required for bootstrap from sources"
		({ Smalltalk clsScanner } includes: cls) ifFalse: [
			nonNilVars _ cls classPool select: [ :var | var notNil ].
			nonNilVars notEmpty ifTrue: [
				n _ n + 1.
				{ cls. nonNilVars keys} print ]]].
	'======' print.
	'InstClassVars (notNil): ' print.
	m _ 0.
	Smalltalk hierachySortedAllClassesDo: [ :cls |
		classInstVars _ Dictionary new.
		cls class allRegularInstVarNames do: [ :nam |
			v _ cls instVarNamed: nam.
			v ifNotNil: [
				classInstVars at: nam put: v ]].
		classInstVars notEmpty ifTrue: [
			m _ m + 1.
			{ cls class. classInstVars keys }  print ]].
	'======' print.
	
	{ n. m}  print! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 5/22/2016 11:29'!
eliotsClosureMeasurements
	"
	Smalltalk eliotsClosureMeasurements
	From http://www.mirandabanda.org/cogblog/2008/11/14/mechanised-modifications-and-miscellaneous-measurements/
	by Eliot Miranda
	"
      | numMethods numMethodsWithClosure numMethodsWithIndirectTemps
         numClosures numClosuresWithCopiedValues numCopiedValuesForClosure
         numRemoteTemps numScopesWithRemoteTemps
         methodReturnsInClosure closureUsesSelfs methodReturnAndUsesSelfs numClean |

        numMethods := numMethodsWithClosure := numMethodsWithIndirectTemps :=
        numClosures := numClosuresWithCopiedValues := numCopiedValuesForClosure :=
        numRemoteTemps := numScopesWithRemoteTemps :=
        methodReturnsInClosure := closureUsesSelfs := methodReturnAndUsesSelfs := numClean := 0.
        self allSelect:
                 [:m| | s hasClosure hasIndirectTemps blkPc blkSz doesMethodReturn usesSelf hasCopied sel|
			sel _ false.
                 hasClosure := hasIndirectTemps := false.
                 s := InstructionStream on: m.
                 s scanFor:
                          [:b|
                          b = 143 "closure creation" ifTrue:
                                   [hasClosure := true.
                                   numClosures := numClosures + 1.
                                   s followingByte >= 16 ifTrue:
                                            [numClosuresWithCopiedValues := numClosuresWithCopiedValues + 1.
                                             numCopiedValuesForClosure := numCopiedValuesForClosure + (s followingByte >> 4)]].
                          (b = 138 "indirect temp vector creation"
                           and: [s followingByte <= 127]) ifTrue:
                                   [hasIndirectTemps := true.
                                    numScopesWithRemoteTemps := numScopesWithRemoteTemps + 1.
                                    numRemoteTemps := numRemoteTemps + s followingByte].
                          false].
                 numMethods := numMethods + 1.
                 hasClosure ifTrue:
                          [numMethodsWithClosure := numMethodsWithClosure + 1.
                           s pc: m initialPC; scanFor: [:b| b = 143].

"jmv-This looks like the correct place to do this"
                           hasCopied := s followingByte >= 16.

                           blkSz := s interpretNextInstructionFor: Smalltalk clsBlockStartLocator new.
                           blkPc := s pc.
                           doesMethodReturn := usesSelf := false.

"jmv-Doing this here looks like a bug. See the other comment"
                           hasCopied := s followingByte >= 16.

"jmv-Another bug. This only considers the first closure (and any nested closure in it), but not later ones"

                           s scanFor:
                                   [:b|
                                   s pc >= (blkPc + blkSz)
                                            ifTrue: [true]
                                            ifFalse:
                                                     [doesMethodReturn := doesMethodReturn or: [s willReturn and: [s willBlockReturn not]].
                                                      usesSelf := usesSelf or: [b = 112 "pushSelf"
                                                                                                  or: [b < 16 "pushInstVar"
                                                                                                  or: [(b = 128 and: [s followingByte <= 63]) "pushInstVar"
                                                                                                  or: [(b between: 96 and: 96 + 7) "storePopInstVar"
                                                                                                  or: [(b = 130 and: [s followingByte <= 63]) "storePopInstVar"
                                                                                                  or: [(b = 129 and: [s followingByte <= 63]) "storeInstVar"
                                                                                                  or: [b = 132 and: [s followingByte = 160]]]]]]]].
                                                     false]].
                           doesMethodReturn ifTrue:
                                   [methodReturnsInClosure := methodReturnsInClosure + 1].
                           usesSelf ifTrue:
                                   [closureUsesSelfs := closureUsesSelfs + 1].
                           (doesMethodReturn and: [usesSelf]) ifTrue:
                                   [methodReturnAndUsesSelfs := methodReturnAndUsesSelfs + 1].
                           (doesMethodReturn or: [usesSelf or: [hasCopied]]) ifFalse:
                                   [numClean := numClean + 1]].
                 hasIndirectTemps ifTrue: [numMethodsWithIndirectTemps := numMethodsWithIndirectTemps + 1].
                 sel].
^        { {'Methods'. numMethods}. {'MethodsWithClosure'. numMethodsWithClosure}. {'MethodsWithIndirectTemps'. numMethodsWithIndirectTemps}.
         {'Closures'. numClosures}. {'CopiedValuesForClosures'. numCopiedValuesForClosure}. {'ClosuresWithCopiedValues'. numClosuresWithCopiedValues}.
         {'RemoteTemps'. numRemoteTemps}. {'ScopesWithRemoteTemps'. numScopesWithRemoteTemps}.
         {'MethodsWithMethodReturnsInClosures'. methodReturnsInClosure}. {'MethodsWithReferencesToSelfInClosures'. closureUsesSelfs}. {'Both'. methodReturnAndUsesSelfs}.
         {'MethodsWithOnlyCleanClosures'. numClean} }! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 5/22/2016 11:29'!
eliotsClosureMeasurementsOn: m over: aFiveArgBlock
	"
	See senders.
	Or try something like:
		Smalltalk
			eliotsClosureMeasurementsOn: FileList >> #defaultContents
			over: [ :closuresCount :hasCopiedValuesForClosure :hasIndirectTemps :anyClosureHasCopied :anyClosureDoesMethodReturn :anyClosureUsesSelf |
				(Array with: closuresCount with: hasCopiedValuesForClosure with: hasIndirectTemps with: anyClosureHasCopied with: anyClosureDoesMethodReturn with: anyClosureUsesSelf)]

	From http://www.mirandabanda.org/cogblog/2008/11/14/mechanised-modifications-and-miscellaneous-measurements/
	by Eliot Miranda
	
	Note: This could perhaps be refactored to use the newer #embeddedBlockClosures and testing methods on the closures themselves.
	"
	| s nextScanStart thisClosureHasCopied closuresCount hasIndirectTemps blkPc blkSz anyClosureHasCopied anyClosureDoesMethodReturn anyClosureUsesSelf analyzedClosures |
	closuresCount := 0.
	hasIndirectTemps := false.
	anyClosureHasCopied :=  anyClosureDoesMethodReturn := anyClosureUsesSelf := false.
	s := InstructionStream on: m.
	s scanFor: [ :b |
		b = 16r8F "16r8F = 143 closure creation" ifTrue: [
			closuresCount := closuresCount + 1].
		(b = 16r8A "16r8A = 138indirect temp vector creation" and: [ s followingByte <= 127]) ifTrue: [
				hasIndirectTemps := true].
		false].
	nextScanStart := m initialPC.
	analyzedClosures := 0.
	[ analyzedClosures < closuresCount ] whileTrue: [
		s pc: nextScanStart; scanFor: [ :b | b = 16r8F ].	"16r8F = 143 Search for first closure"
		analyzedClosures := analyzedClosures + 1.
		thisClosureHasCopied := s followingByte >= 16r10.
		anyClosureHasCopied := anyClosureHasCopied | thisClosureHasCopied.
		blkSz := s interpretNextInstructionFor: Smalltalk clsBlockStartLocator new.		"Findout size of first closure"
		blkPc := s pc.
		s scanFor: [ :b |
			s pc >= (blkPc + blkSz)
				ifTrue: [
					nextScanStart := s pc.
					true]
				ifFalse: [
					b = 16r8F ifTrue: [			
						thisClosureHasCopied := s followingByte >= 16r10.
						anyClosureHasCopied := anyClosureHasCopied | thisClosureHasCopied.
						analyzedClosures := analyzedClosures + 1 ].
					anyClosureDoesMethodReturn := anyClosureDoesMethodReturn or: [s willReturn and: [s willBlockReturn not]].
					anyClosureUsesSelf := anyClosureUsesSelf or: [b = 16r70 "pushSelf"
										or: [b < 16r10 "pushInstVar"
										or: [(b = 16r80 and: [s followingByte <= 16r3F]) "pushInstVar"
										or: [(b between: 16r60 and: 16r60 + 7) "storePopInstVar"
										or: [(b = 16r82 and: [s followingByte <= 63]) "storePopInstVar"
										or: [(b = 16r81 and: [s followingByte <= 63]) "storeInstVar"
										or: [b = 16r84 and: [s followingByte = 160]]]]]]]].
					false]]].
	^aFiveArgBlock valueWithArguments: (Array
			with: closuresCount
			with: hasIndirectTemps
			with: anyClosureHasCopied
			with: anyClosureDoesMethodReturn
			with: anyClosureUsesSelf)! !

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 5/22/2016 11:11'!
processCommandLineArgument: rawArgStream storeStartUpScriptArgsOn: startUpScriptArgs
	"
	Smalltalk processCommandLineArguments
	
	A possible example (duplicated single quotes: '' should be double quotes, but not allowed in a Smalltalk comment):
	Squeak.exe Cuis4.2-2211x.image -r RequiredFeature1 -rRequiredFeature2 -d ''Transcript show: 'popo1'; newLine'' -d''Transcript show: 'popo2'; newLine'' -s smalltalkScript.st paramAlScript1 paramAlSCript2 ''parametro al script ->>>--// 3''
	"
	| p data entry |
	p _ rawArgStream next.

	(p first = $- and: [ p size > 1 ]) ifTrue: [
		"If the command is not included in p, it is next argument"
		p size = 2
			ifTrue: [
				"as in 		-r RequiredFeature1"
				data _ rawArgStream next ]
			ifFalse: [
				"as in 		-rRequiredFeature2"
				data _ p copyFrom: 3 to: p size ].
		p second caseOf: {
			[ $r ] -> [		"as in 		-rRequiredFeature2"
				{ 'Feature require: '. data } print.
				[ Feature require: data ] on: Error do: [] ].
			[ $d ] -> [		"as in 		-d ''Transcript show: 'popo1'; newLine'' -d''Transcript show: 'popo2'; newLine''        (duplicated singleQuotes should read doubleQuote)"
				{ 'Compiler evaluate: '. data } print.
				[ Smalltalk clsCompiler evaluate: data ] on: Error do: [] ].
			[ $s ] -> [		"as in 		-s smalltalkScript.st paramAlScript1 paramAlSCript2 ''parametro al script ->>>--// 3'' 			(duplicated singleQuotes should read doubleQuote)"
				[ rawArgStream atEnd ] whileFalse: [
					startUpScriptArgs nextPut: rawArgStream next ].
				"Can use 'Smalltalk startUpScriptArguments' inside the startUp script
				{ 'Compiler evaluate contents of file named: '. data. ' arguments: '. Smalltalk startUpScriptArguments } print."
				entry _ data asFileEntry.
				entry exists ifTrue: [
					entry readStreamDo: [ :stream |
						[ Smalltalk clsCompiler evaluate: stream contentsOfEntireFile ] on: Error do: []]].
				"Maybe we decide to clear them after startup script execution
				startUpScriptArguments _ nil" ]
		}
		otherwise: []
	]! !


!DebuggerMethodMap methodsFor: 'source mapping' stamp: 'jmv 5/22/2016 11:31'!
abstractSourceMap
	"Answer with a Dictionary of abstractPC <Integer> to sourceRange <Interval>."
	| theMethodToScan rawSourceRanges concreteSourceRanges abstractPC scanner client |
	abstractSourceRanges ifNotNil: [
		^abstractSourceRanges].
	"If the methodNode hasn't had a method generated it doesn't have pcs set in its
	 nodes so we must generate a new method and might as well use it for scanning."
	[
		methodNode rawSourceRangesAndMethodDo: [ :ranges :method |
			rawSourceRanges := ranges.
		 	theMethodToScan := method ]]
		on: Smalltalk clsUndeclaredVariableWarning
		do: [ :ex | ex resume ].
	concreteSourceRanges := Dictionary new.
	rawSourceRanges keysAndValuesDo: [ :node :range |
		node pc ~= 0 ifTrue: [
			concreteSourceRanges at: node pc put: range ]].
	abstractPC := 1.
	abstractSourceRanges := Dictionary new.
	scanner := InstructionStream on: theMethodToScan.
	client := Smalltalk clsInstructionClient new.
	[
		(concreteSourceRanges includesKey: scanner pc) ifTrue: [
			abstractSourceRanges at: abstractPC put: (concreteSourceRanges at: scanner pc)].
		 abstractPC := abstractPC + 1.
		 scanner interpretNextInstructionFor: client.
		 scanner atEnd ] whileFalse.
	^abstractSourceRanges! !


!DebuggerMethodMap class methodsFor: 'instance creation' stamp: 'jmv 5/22/2016 11:06'!
forMethod: aMethod "<CompiledMethod>"
	"Answer a DebuggerMethodMap suitable for debugging activations of aMethod.
	 Answer an existing instance from the cache if it exists, cacheing a new one if required."
	
	| n |
	^self protected: [ 
		MapCache
			at: aMethod
			ifAbsent: [
				[ n _ aMethod methodNode ] 
					on: Smalltalk clsUndeclaredVariableWarning
					do: [ :ex | ex resume ].
				self
						cacheDebugMap:
							(self
								forMethod: aMethod
								methodNode: n)
						forMethod: aMethod] ]! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 5/22/2016 11:12'!
compileSelectionFor: anObject in: evalContext
	| methodNode |
	methodNode _ [
	Smalltalk clsCompiler new
		sourceStreamGetter: #selectionAsStream;		"Cuis specific. Do not remove!!"
		
		compileNoPattern: self selectionAsStream
		in: anObject class
		context: evalContext
		notifying: self
		ifFail: [ ^ nil ]]
		on: OutOfScopeNotification
		do: [ :ex |
			ex resume: true ].
	^ methodNode generateWithTempNames.! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 5/22/2016 11:12'!
evaluateSelectionAndDo: aBlock ifFail: failBlock
	"Treat the current selection as an expression; evaluate it and return the result
	3 +4
	"
	| provider result rcvr ctxt |
	self lineSelectAndEmptyCheck: [^ ''].

	provider _ self codeProvider.
	(provider respondsTo: #doItReceiver) 
		ifTrue: [
				rcvr _ provider doItReceiver.
				ctxt _ provider doItContext]
		ifFalse: [rcvr _ ctxt _ nil].
	result _ [
		Smalltalk clsCompiler new
			sourceStreamGetter: #selectionAsStream;		"Cuis specific. Do not remove!!"
			evaluate: self selectionAsStream
			in: ctxt
			to: rcvr
			notifying: self
			ifFail: [^ failBlock value]
			logged: true.
	] 
		on: OutOfScopeNotification 
		do: [ :ex | ex resume: true].
	morph formatAndStyleIfNeeded.		"Needed to re-shout workspaces, that might have new variables binded."
	^ aBlock value: result! !


!InstructionStream methodsFor: 'testing' stamp: 'jmv 5/22/2016 10:51'!
willBlockReturn

	^ (self method at: pc) = ZEncoder blockReturnCode! !

!InstructionStream methodsFor: 'testing' stamp: 'jmv 5/22/2016 10:51'!
willJustPop

	^ (self method at: pc) = ZEncoder popCode! !


!BlockContext methodsFor: 'accessing' stamp: 'jmv 5/22/2016 11:30'!
hasInstVarRef
	"Answer whether the receiver references an instance variable."

	| method scanner end printer |

	home ifNil: [^false].
	method := self method.
	end := self endPC.
	scanner := InstructionStream new method: method pc: startpc.
	printer := Smalltalk clsInstVarRefLocator new.

	[scanner pc <= end] whileTrue: [
		(printer interpretNextInstructionUsing: scanner) ifTrue: [^true].
	].
	^false! !


!ZDecompiler methodsFor: 'control' stamp: 'jmv 5/22/2016 11:00'!
doClosureCopyCopiedValues: blockCopiedValues numArgs: numArgs blockSize: blockSize
	| startpc savedTemps savedTempVarCount savedNumLocalTemps
	  jump blockArgs blockTemps blockTempsOffset block |
	savedTemps := tempVars.
	savedTempVarCount := tempVarCount.
	savedNumLocalTemps := numLocalTemps.
	jump := blockSize + (startpc := pc).
	numLocalTemps := ZBlockLocalTempCounter tempCountForBlockAt: pc - 4 in: method.
	blockTempsOffset := numArgs + blockCopiedValues size.
	(blockStartsToTempVars notNil "implies we were intialized with temp names."
	 and: [blockStartsToTempVars includesKey: pc])
		ifTrue:
			[tempVars := blockStartsToTempVars at: pc]
		ifFalse:
			[blockArgs := (1 to: numArgs) collect:
							[:i| (constructor
									codeTemp: i - 1
									named: 'arg', (tempVarCount + i) printString)
								  beBlockArg].
			blockTemps := (1 to: numLocalTemps) collect:
							[:i| constructor
									codeTemp: i + blockTempsOffset - 1
									named: 'temp', (tempVarCount + i + numArgs) printString].
			tempVars := blockArgs, blockCopiedValues, blockTemps].
	numLocalTemps timesRepeat:
		[self interpretNextInstructionFor: self.
		 stack removeLast].
	tempVarCount := tempVarCount + numArgs + numLocalTemps.
	block := self blockTo: jump.
	stack addLast: ((constructor
						codeArguments: (tempVars copyFrom: 1 to: numArgs)
						temps: (tempVars copyFrom: blockTempsOffset + 1 to: blockTempsOffset + numLocalTemps)
						block: block)
							pc: startpc;
							yourself).
	tempVars := savedTemps.
	tempVarCount := savedTempVarCount.
	numLocalTemps := savedNumLocalTemps! !

!ZDecompiler methodsFor: 'instruction decoding' stamp: 'jmv 5/22/2016 10:48'!
popIntoTemporaryVariable: offset
	| maybeTVTag tempVector start |
	maybeTVTag := stack last.
	((maybeTVTag isMemberOf: Association)
	 and: [maybeTVTag key == #pushNewArray]) ifTrue:
		[blockStartsToTempVars notNil "implies we were intialized with temp names."
			ifTrue: "Use the provided temps"
				[self assert: ((tempVector := tempVars at: offset + 1 ifAbsent: [ZParseNode basicNew]) isTemp
							 and: [tempVector isIndirectTempVector
							 and: [tempVector remoteTemps size = maybeTVTag value size]])]
			ifFalse: "Synthesize some remote temps"
				[tempVector := maybeTVTag value.
				 offset + 1 <= tempVars size
					ifTrue:
						[start := 2.
						 tempVector at: 1 put: (tempVars at: offset + 1)]
					ifFalse:
						[tempVars := (Array new: offset + 1)
										replaceFrom: 1
										to: tempVars size
										with: tempVars.
						start := 1].
				 start to: tempVector size do:
					[:i|
					tempVector
						at: i
						put: (constructor
								codeTemp: numLocalTemps + offset + i - 1
								named: 'temp', (tempVarCount + i) printString)].
				tempVars at: offset + 1 put: (constructor codeRemoteTemp: offset + 1 remoteTemps: tempVector)].
		 tempVarCount := tempVarCount + maybeTVTag value size.
		 stack removeLast.
		 ^self].
	self pushTemporaryVariable: offset; doStore: statements! !

!ZDecompiler methodsFor: 'private' stamp: 'jmv 5/22/2016 11:30'!
blockScopeRefersOnlyOnceToTemp: offset
	| nRefs byteCode extension scanner scan |
	scanner := InstructionStream on: method.
	nRefs := 0.
	scan := offset <= 15
				ifTrue:
					[byteCode := 16 + offset.
					 [:instr |
					  instr = byteCode ifTrue:
						[nRefs := nRefs + 1].
					  nRefs > 1]]
				ifFalse:
					[extension := 64 + offset.
					 [:instr |
					  (instr = 128 and: [scanner followingByte = extension]) ifTrue:
						[nRefs := nRefs + 1].
					   nRefs > 1]].
	self scanBlockScopeFor: pc from: method initialPC to: method endPC with: scan scanner: scanner.
	^nRefs = 1! !

!ZDecompiler methodsFor: 'private' stamp: 'jmv 5/22/2016 10:51'!
constructorForMethod: aMethod
	^(aMethod isBlueBookCompiled
		ifTrue: [ZDecompilerConstructor]
		ifFalse: [ZDecompilerConstructorForClosures]) new! !

!ZDecompiler methodsFor: 'private' stamp: 'jmv 5/22/2016 10:48'!
convertToDoLoop
	"If statements contains the pattern
		var := startExpr.
		[var <= limit] whileTrue: [...statements... var := var + incConst]
	then replace this by
		startExpr to: limit by: incConst do: [:var | ...statements...]"
	| initStmt toDoStmt limitStmt |
	(stack notEmpty
	 and: [(stack last == #CaseFlag) not and: [ stack last isAssignmentNode]])
		ifTrue:
			[initStmt := stack last.
			 (toDoStmt := statements last toDoFromWhileWithInit: initStmt) ifNil:
				[^self].
			 stack removeLast; addLast: toDoStmt.
			 statements removeLast]
		ifFalse:
			[statements size < 2 ifTrue:
				[^self].
			initStmt := statements at: statements size-1.
			(toDoStmt := statements last toDoFromWhileWithInit: initStmt) ifNil:
				[^self].
			statements removeLast; removeLast; addLast: toDoStmt].
	initStmt variable scope: -1.  "Flag arg as block temp"

	"Attempt further conversion of the pattern
		limitVar := limitExpr.
		startExpr to: limitVar by: incConst do: [:var | ...statements...]
	to
		startExpr to: limitExpr by: incConst do: [:var | ...statements...].
	The complication here is that limitVar := limitExpr's value may be used, in which case it'll
	be statements last, or may not be used, in which case it'll be statements nextToLast."
	statements size < 2 ifTrue: [^ self].
	limitStmt := statements last.
	((limitStmt isMemberOf: ZAssignmentNode)
		and: [limitStmt variable isTemp
		and: [limitStmt variable == toDoStmt arguments first]]) ifFalse:
			[limitStmt := statements at: statements size-1.
			((limitStmt isMemberOf: ZAssignmentNode)
				and: [limitStmt variable isTemp
				and: [limitStmt variable == toDoStmt arguments first]]) ifFalse:
					[^self]].

	(self blockScopeRefersOnlyOnceToTemp: limitStmt variable fieldOffset) ifFalse:
		[^self].
	toDoStmt arguments at: 1 put: limitStmt value.
	limitStmt variable scope: -2.  "Flag limit var so it won't print"
	statements last == limitStmt
		ifTrue: [statements removeLast]
		ifFalse: [statements removeLast; removeLast; addLast: toDoStmt]! !

!ZDecompiler methodsFor: 'private' stamp: 'jmv 5/22/2016 11:30'!
methodRefersOnlyOnceToTemp: offset
	| nRefs byteCode extension scanner |
	nRefs := 0.
	offset <= 15
		ifTrue:
			[byteCode := 16 + offset.
			(InstructionStream on: method) scanFor:
				[:instr | instr = byteCode ifTrue: [nRefs := nRefs + 1].
				nRefs > 1]]
		ifFalse:
			[extension := 64 + offset.
			scanner := InstructionStream on: method.
			scanner scanFor:
				[:instr | (instr = 128 and: [scanner followingByte = extension])
							ifTrue: [nRefs := nRefs + 1].
				nRefs > 1]].
	^ nRefs = 1
! !

!ZDecompiler methodsFor: 'private' stamp: 'jmv 5/22/2016 10:51'!
sawBlueBookBlock
	constructor isForClosures ifTrue:
		[constructor primitiveChangeClassTo: ZDecompilerConstructor new]! !

!ZDecompiler methodsFor: 'private' stamp: 'jmv 5/22/2016 10:51'!
sawClosureBytecode
	constructor isForClosures ifFalse:
		[constructor primitiveChangeClassTo: ZDecompilerConstructorForClosures new]! !

!ZDecompiler methodsFor: 'private' stamp: 'jmv 5/22/2016 11:01'!
scanBlockScopeFor: refpc from: startpc to: endpc with: scan scanner: scanner
	| bsl maybeBlockSize |
	bsl := ZBlockStartLocator new.
	scanner pc: startpc.
	[scanner pc <= endpc] whileTrue:
		[refpc = scanner pc ifTrue:
			[scanner pc: startpc.
			 [scanner pc <= endpc] whileTrue:
				[(scan value: scanner firstByte) ifTrue:
					[^endpc].
				 (maybeBlockSize := scanner interpretNextInstructionFor: bsl) isInteger ifTrue:
					[scanner pc: scanner pc + maybeBlockSize]].
			   ^self].
		 (maybeBlockSize := scanner interpretNextInstructionFor: bsl) isInteger ifTrue:
			[refpc <= (scanner pc + maybeBlockSize)
				ifTrue: [^self scanBlockScopeFor: refpc from: scanner pc to: scanner pc + maybeBlockSize with: scan scanner: scanner]
				ifFalse: [scanner pc: scanner pc + maybeBlockSize]]]! !


!ZDecompiler class methodsFor: 'testing' stamp: 'jmv 5/22/2016 11:31'!
recompileAllTest
	"[Decompiler recompileAllTest]"
	"decompile every method and compile it back; if the decompiler is correct then the system should keep running.  :)"
	
	Smalltalk allBehaviorsDo: [ :behavior |
		Utilities informUser: (behavior printString) during: [
			behavior selectors do: [ :sel |
				| decompiled ast compiled |
				decompiled := ZDecompiler new decompile: sel in: behavior.
				ast := ZCompiler new compile: decompiled in: behavior notifying: nil ifFail: [ self error: 'failed' ].
				compiled := ast generate: (behavior compiledMethodAt: sel) trailer.
				behavior addSelector: sel withMethod: compiled. ] ] ]! !


!InspectorWindow methodsFor: 'menu commands' stamp: 'jmv 5/22/2016 11:13'!
addEntry

	| newKey string |
	string _ FillInTheBlankMorph request:
'Enter new key, then type RETURN.
(Expression will be evaluated for value.)
Examples:  #Fred    ''a string''   3+4'.
	newKey _ Smalltalk clsCompiler evaluate: string.
	model addEntry: newKey! !

!InspectorWindow methodsFor: 'menu commands' stamp: 'jmv 5/22/2016 11:13'!
renameEntry
	| string newKey |

	string _ FillInTheBlankMorph request: 
'Enter new key, then type RETURN.
(Expression will be evaluated for value.)
Examples:  #Fred    ''a string''   3+4'
		 initialAnswer: model selectedKey printString.

	string = '' ifTrue: [
		^self ].

	newKey _ Smalltalk clsCompiler evaluate: string.

	model renameEntryTo: newKey! !


!PseudoClass methodsFor: 'accessing' stamp: 'jmv 5/22/2016 11:11'!
compilerClass
	^ (Smalltalk at: name ifAbsent: [^ Smalltalk clsCompiler]) compilerClass! !

!PseudoClass methodsFor: 'private' stamp: 'jmv 5/22/2016 11:11'!
evaluate: aString
	^Smalltalk clsCompiler evaluate: aString for: nil logged: true! !

!PseudoClass methodsFor: 'private' stamp: 'jmv 5/22/2016 11:11'!
parserClass

	^ Smalltalk clsCompiler parserClass! !


!RemoteString methodsFor: 'accessing' stamp: 'jmv 5/22/2016 11:11'!
compilerClass
	"Answer a compiler class appropriate for source methods of this class."

	^Smalltalk clsCompiler! !


!SHParserST80 methodsFor: 'character testing' stamp: 'jmv 5/22/2016 11:33'!
isSelectorCharacter: aCharacter
	"Actually meaning 'isBinarySelectorCharacter'. i.e. can aCharacter be part of a binary selector? "
	aCharacter isValidInIdentifiers ifTrue: [^false].
	aCharacter isSeparator ifTrue: [^false].
	"$- is specified here as NOT being a selector char, but it can appear as the 
	first char in a binary selector. That case is handled specially elsewhere"
	('"#$'':().;[]{}^_-'  includes: aCharacter) 
		ifTrue:[^false].
	aCharacter asciiValue = Smalltalk clsScanner doItCharacterValue ifTrue: [^false "the doIt char"].
	aCharacter asciiValue = 0 ifTrue: [^false].
	"Any other char is ok as a binary selector char."
	^true
! !


!PositionableStream methodsFor: 'fileIn/Out' stamp: 'jmv 5/22/2016 11:12'!
fileInAnnouncing: announcement
	"This is special for reading expressions from text that has been formatted 
	with exclamation delimitors. The expressions are read and passed to the 
	Compiler. Answer the result of compilation.  Put up a progress report with
     the given announcement as the title."

	| val chunk |
	announcement 
		displayProgressAt: Sensor mousePoint
		from: 0
		to: self size
		during: [ :bar | 
			[ self atEnd ] whileFalse: [
					bar value: self position.
					self skipSeparators.
					
					[
						val := (self peekFor: $!!) 
								ifTrue: [
									chunk := self nextChunk.
									"These are the ones that should do nothing, 
									because next line is a doit that does the stuff
									(or because it is handled elsewhere)"
									(chunk beginsWith: 'description:  ')
									| (chunk beginsWith: 'provides: ')
									| (chunk beginsWith: 'requires: ')
									| (chunk beginsWith: 'classDefinition: ')
									| (chunk beginsWith: 'classRemoval: ')
									| (chunk beginsWith: 'methodRemoval: ')
									| (chunk beginsWith: 'classMoveToSomePackage: ')
									| (chunk beginsWith: 'methodMoveToSomePackage: ')
										ifFalse: [(Smalltalk clsCompiler evaluate: chunk logged: false) scanFrom: self]]
								ifFalse: [
									chunk := self nextChunk.
									self checkForPreamble: chunk.
									[ Smalltalk clsCompiler evaluate: chunk logged: true ]
										on: Error
										do: [ :ex |
											ex print.
											('while evaluating: ', chunk) print.
											ex pass ]
										]] 
							on: InMidstOfFileinNotification
							do: [ :ex | ex resume: true ] ].
			].
	"Note:  The main purpose of this banner is to flush the changes file."
	Smalltalk logChange: '----End fileIn of ' , self name , '----'.
	^val! !


!Transcripter methodsFor: 'command line' stamp: 'jmv 5/22/2016 11:12'!
readEvalPrint
	| line |
	[ #('quit' 'exit' 'done' ) includes: (line _ self request: '>') ] whileFalse: [
		self
			newLine;
			show:
				([ Smalltalk clsCompiler evaluate: line ] ifError: [ :err :ex |
					err ]) ]! !


!TextDoIt methodsFor: 'as yet unclassified' stamp: 'jmv 5/22/2016 11:11'!
actOnClickFor: anObject
	"Note: evalString gets evaluated IN THE CONTEXT OF anObject
	 -- meaning that self and all instVars are accessible"
	Smalltalk clsCompiler evaluate: evalString for: anObject logged: false.
	^ true ! !


!Utilities class methodsFor: 'summer97 additions' stamp: 'jmv 5/22/2016 11:33'!
isLegalInstVarName: aString
	"Answer whether aString is a legal instance variable name."

	^ ((Smalltalk clsScanner isLiteralSymbol: aString) and: [(aString includes: $:) not]) and:
		[(self inviolateInstanceVariableNames includes:  aString) not]! !


!ZCompiler methodsFor: 'error handling' stamp: 'jmv 5/22/2016 11:08'!
notify: aString at: location
	"Refer to the comment in Object|notify:."

	^requestor == nil
		ifTrue: [ZSyntaxErrorNotification
					inClass: class
					category: category
					withCode: 
						(sourceStream contents
							copyReplaceFrom: location
							to: location - 1
							with: aString)
					doitFlag: false
					errorMessage: aString
					location: location]
		ifFalse: [requestor
					notify: aString
					at: location
					in: sourceStream]! !


!ZCompiler class methodsFor: 'accessing' stamp: 'jmv 5/22/2016 11:31'!
decompilerClass
	^ZDecompiler! !

!ZCompiler class methodsFor: 'accessing' stamp: 'jmv 5/22/2016 11:35'!
parserClass
	"Answer a parser class to use for parsing methods compiled by instances of the receiver."

	^ZParser! !


!ZBlockLocalTempCounter methodsFor: 'initialization' stamp: 'jmv 5/22/2016 11:30'!
tempCountForBlockAt: pc in: method
	"Compute the number of local temporaries in a block.
	 If the block begins with a sequence of push: nil bytecodes then some of
	 These could be initializing local temps.  We can only reliably disambuguate
	 them from other uses of nil by parsing the stack and seeing what the offset
	 of the stack pointer is at the end of the block.

	 There are short-cuts.  The ones we take here are
		- if there is no sequence of push nils there can be no local temps
		- we follow forward jumps to shorten the amount of scanning"
	stackPointer := 0.
	scanner := InstructionStream new method: method pc: pc.
	scanner interpretNextInstructionFor: self.
	blockEnd ifNil:
		[self error: 'pc is not that of a block'].
	scanner nextByte = ZEncoder pushNilCode ifTrue:
		[joinOffsets := Dictionary new.
		 [scanner pc < blockEnd] whileTrue:
			[scanner interpretNextInstructionFor: self]].
	^stackPointer! !

!ZBlockLocalTempCounter methodsFor: 'initialization' stamp: 'jmv 5/22/2016 11:30'!
testTempCountForBlockAt: startPc in: method
	"Compute the number of local temporaries in a block.
	 If the block begins with a sequence of push: nil bytecodes then some of
	 These could be initializing local temps.  We can only reliably disambuguate
	 them from other uses of nil by parsing the stack and seeing what the offset
	 of the stack pointer is at the end of the block.There are short-cuts.  The only
	 one we take here is
		- if there is no sequence of push nils there can be no local temps"

	| symbolicLines line prior thePc |
	symbolicLines := Dictionary new.
	method symbolicLinesDo:
		[:pc :lineForPC| symbolicLines at: pc put: lineForPC].
	stackPointer := 0.
	scanner := InstructionStream new method: method pc: startPc.
	scanner interpretNextInstructionFor: self.
	blockEnd ifNil:
		[self error: 'pc is not that of a block'].
	scanner nextByte = ZEncoder pushNilCode ifTrue:
		[joinOffsets := Dictionary new.
		 [scanner pc < blockEnd] whileTrue:
			[line := symbolicLines at: scanner pc.
			 prior := stackPointer.
			 thePc := scanner pc.
			 scanner interpretNextInstructionFor: self.
			 Transcript newLine; print: prior; nextPutAll: '->'; print: stackPointer;  tab; print: thePc; tab; nextPutAll: line]].
	^stackPointer! !


!ZInstructionPrinter methodsFor: 'initialization' stamp: 'jmv 5/22/2016 11:30'!
printInstructionsOn: aStream 
	"Append to the stream, aStream, a description of each bytecode in the
	 instruction stream."
	
	| end |
	stream := aStream.
	scanner := InstructionStream on: method.
	end := method endPC.
	oldPC := scanner pc.
	innerIndents := Array new: end withAll: 0.
	[scanner pc <= end] whileTrue:
		[scanner interpretNextInstructionFor: self]! !

!ZInstructionPrinter methodsFor: 'initialization' stamp: 'jmv 5/22/2016 11:30'!
printInstructionsOn: aStream do: aBlock
	"Append to the stream, aStream, a description of each bytecode in the
	 instruction stream. Evaluate aBlock with the receiver, the scanner and
	 the stream after each instruction."

	| end |
	stream := aStream.
	scanner := InstructionStream on: method.
	end := method endPC.
	oldPC := scanner pc.
	innerIndents := Array new: end withAll: 0.
	[scanner pc <= end] whileTrue:
		[scanner interpretNextInstructionFor: self.
		 aBlock value: self value: scanner value: stream]! !


!ZParseNode methodsFor: 'visiting' stamp: 'jmv 5/22/2016 11:05'!
nodesDo: aBlock
	self accept: (ZParseNodeEnumerator ofBlock: aBlock)! !

!ZParseNode methodsFor: 'converting' stamp: 'jmv 5/22/2016 10:56'!
asReturnNode

	^ZReturnNode new expr: self! !

!ZParseNode methodsFor: 'testing' stamp: 'jmv 5/22/2016 11:05'!
isOnlySubnodeOf: aSubtree "<ParseNode>" in: aParseTree "<ParseNode>"
	"Answer if the receiver only occurs within aSubtree of aParseTree, not in the rest of aParseTree.
	 Assumes that aSubtree is in fact a subnode of aParseTree."
	| isSubnode |
	isSubnode := false.
	aSubtree accept: (ZParseNodeEnumerator
							ofBlock: [:node| node == self ifTrue: [isSubnode := true]]).
	isSubnode ifFalse:
		[^false].
	aParseTree accept: (ZParseNodeEnumerator
							ofBlock: [:node| node == self ifTrue: [^false]]
							select: [:node| node ~= aSubtree]).
	^true! !


!ZParseNode class methodsFor: 'instance creation' stamp: 'jmv 5/22/2016 10:54'!
new
	LdInstType ifNil: [
		ZParseNode initClassCachedState.
		ZVariableNode initClassCachedState ].
	^super new! !


!ZAssignmentNode methodsFor: 'initialization' stamp: 'jmv 5/22/2016 10:57'!
toDoIncrement: var
	var = variable ifFalse: [^ nil].
	(value isMemberOf: ZMessageNode) 
		ifTrue: [^ value toDoIncrement: var]
		ifFalse: [^ nil]! !

!ZAssignmentNode methodsFor: 'initialization' stamp: 'jmv 5/22/2016 10:57'!
variable: aVariable value: expression from: encoder

	(aVariable isMemberOf: ZMessageAsTempNode)
		ifTrue: ["Case of remote temp vars"
				^ aVariable store: expression from: encoder].
	variable := aVariable.
	value := expression! !


!ZBlockNode methodsFor: 'code generation (closures)' stamp: 'jmv 5/22/2016 10:56'!
addRemoteTemp: aTempVariableNode rootNode: rootNode "<MethodNode>"
	"Add aTempVariableNode to my actualScope's sequence of
	 remote temps.  If I am an optimized block then the actual
	 scope is my actualScopeIfOptimized, otherwise it is myself."
	remoteTempNode == nil ifTrue:
		[remoteTempNode := ZRemoteTempVectorNode new
								name: self remoteTempNodeName
								index: arguments size + temporaries size
								type: LdTempType
								scope: 0.
		 actualScopeIfOptimized
			ifNil:
				[self addTempNode: remoteTempNode.
				 remoteTempNode definingScope: self]
			ifNotNil: [actualScopeIfOptimized addHoistedTemps: { remoteTempNode }]].
	remoteTempNode addRemoteTemp: aTempVariableNode encoder: rootNode encoder.
	"use remove:ifAbsent: because the deferred analysis for optimized
	 loops can result in the temp has already been hoised into the root."
	self removeTempNode: aTempVariableNode ifAbsent: [
		self actualScope removeTempNode: aTempVariableNode ifAbsent: ["should not happen"]].
	^remoteTempNode! !

!ZBlockNode methodsFor: 'code generation (closures)' stamp: 'jmv 5/22/2016 10:47'!
computeCopiedValues: rootNode
	| referencedValues |
	referencedValues := rootNode referencedValuesWithinBlockExtent: blockExtent.
	^(referencedValues reject: [:temp| temp isDefinedWithinBlockExtent: blockExtent])
		asArray sort: ZParseNode tempSortBlock! !

!ZBlockNode methodsFor: 'code generation (closures)' stamp: 'jmv 5/22/2016 10:58'!
constructClosureCreationNode: encoder
	copiedValues := self computeCopiedValues: encoder rootNode.
	encoder supportsClosureOpcodes ifTrue:
		[^self closureCreationNode].
	"Without the bytecode we can still get by."
	^ZMessageNode new
		receiver: (encoder encodeVariable: 'thisContext')
		selector: #closureCopy:copiedValues:
		arguments: (Array
						with: (encoder encodeLiteral: arguments size)
						with: (copiedValues isEmpty
								ifTrue: [NodeNil]
								ifFalse: [ZBraceNode new elements: copiedValues]))
		precedence: 3
		from: encoder! !

!ZBlockNode methodsFor: 'code generation (closures)' stamp: 'jmv 5/22/2016 11:04'!
nilReadBeforeWrittenTemps
	| visitor readBeforeWritten |
	self accept: (visitor := ZOptimizedBlockLocalTempReadBeforeWrittenVisitor new).
	readBeforeWritten := visitor readBeforeWritten.
	temporaries reverseDo:
		[:temp|
		((readBeforeWritten includes: temp)
		 and: [temp isRemote not]) ifTrue:
			[statements addFirst: (ZAssignmentNode new variable: temp value: NodeNil)]]! !

!ZBlockNode methodsFor: 'accessing' stamp: 'jmv 5/22/2016 10:53'!
closureCreationNode
	closureCreationNode ifNil:
		[closureCreationNode := ZLeafNode new
									key: #closureCreationNode
									code: nil].
	^closureCreationNode! !


!ZBraceNode methodsFor: 'code generation (new scheme)' stamp: 'jmv 5/22/2016 10:57'!
sizeCodeForValue: encoder

	(encoder supportsClosureOpcodes
		"Hack; we have no way of knowing how much stack space is available"
	 and: [elements size <= self maxElementsForConsArray]) ifTrue:
		[^(elements inject: 0 into: [:sum :node| sum + (node sizeCodeForValue: encoder)])
		  + (encoder sizePushConsArray: elements size)].
	emitNode := elements size <= 4
		ifTrue: ["Short form: Array braceWith: a with: b ... "
				ZMessageNode new
					receiver: (encoder encodeVariable: #Array)
					selector: (self selectorForShortForm: elements size)
					arguments: elements precedence: 3 from: encoder]
		ifFalse: ["Long form: (Array braceStream: N) nextPut: a; nextPut: b; ...; braceArray"
				ZCascadeNode new
					receiver: (ZMessageNode new
								receiver: (encoder encodeVariable: #Array)
								selector: #braceStream:
								arguments: (Array with: (encoder encodeLiteral: elements size))
								precedence: 3 from: encoder)
					messages: ((elements collect: [:elt | ZMessageNode new receiver: nil
														selector: #nextPut:
														arguments: (Array with: elt)
														precedence: 3 from: encoder])
								copyWith: (ZMessageNode new receiver: nil
														selector: #braceArray
														arguments: (Array new)
														precedence: 1 from: encoder))].
	^emitNode sizeCodeForValue: encoder! !


!ZDecompilerConstructor methodsFor: 'constructor' stamp: 'jmv 5/22/2016 10:55'!
codeAnyLitInd: association

	^ZVariableNode new
		name: association key
		key: association
		index: 0
		type: LdLitIndType! !

!ZDecompilerConstructor methodsFor: 'constructor' stamp: 'jmv 5/22/2016 10:53'!
codeAnyLiteral: value

	^ZLiteralNode new
		key: value
		index: 0
		type: LdLitType! !

!ZDecompilerConstructor methodsFor: 'constructor' stamp: 'jmv 5/22/2016 10:53'!
codeAnySelector: selector

	^ZSelectorNode new
		key: selector
		index: 0
		type: SendType! !

!ZDecompilerConstructor methodsFor: 'constructor' stamp: 'jmv 5/22/2016 10:48'!
codeAssignTo: variable value: expression

	^ZAssignmentNode new variable: variable value: expression! !

!ZDecompilerConstructor methodsFor: 'constructor' stamp: 'jmv 5/22/2016 10:49'!
codeBlock: statements returns: returns
	^ ZBlockNode statements: statements returns: returns! !

!ZDecompilerConstructor methodsFor: 'constructor' stamp: 'jmv 5/22/2016 10:50'!
codeBrace: elements

	^ZBraceNode new elements: elements! !

!ZDecompilerConstructor methodsFor: 'constructor' stamp: 'jmv 5/22/2016 10:50'!
codeCascade: receiver messages: messages

	^ (ZBraceNode new matchBraceStreamReceiver: receiver messages: messages)
		ifNil: [ZCascadeNode new receiver: receiver messages: messages]! !

!ZDecompilerConstructor methodsFor: 'constructor' stamp: 'jmv 5/22/2016 10:53'!
codeConstants
	"Answer with an array of the objects representing self, true, false, nil,
	-1, 0, 1, 2."

	^(Array with: NodeSelf with: NodeTrue with: NodeFalse with: NodeNil)
		, ((-1 to: 2) collect: [:i | ZLiteralNode new key: i code: LdMinus1 + i + 1])! !

!ZDecompilerConstructor methodsFor: 'constructor' stamp: 'jmv 5/22/2016 10:49'!
codeEmptyBlock
	^ ZBlockNode withJust: NodeNil! !

!ZDecompilerConstructor methodsFor: 'constructor' stamp: 'jmv 5/22/2016 10:55'!
codeInst: index

	^ZInstanceVariableNode new
		name: (instVars at: index + 1 ifAbsent: ['unknown', index asString])
		index: index + 1! !

!ZDecompilerConstructor methodsFor: 'constructor' stamp: 'jmv 5/22/2016 10:58'!
codeMessage: receiver selector: selector arguments: arguments
	| symbol node |
	symbol := selector key.
	(node := ZBraceNode new
			matchBraceWithReceiver: receiver
			selector: symbol
			arguments: arguments) ifNotNil: [^ node].
	(node := self decodeIfNilWithReceiver: receiver
			selector: symbol
			arguments: arguments) ifNotNil: [^ node].
	^ ZMessageNode new
			receiver: receiver selector: selector
			arguments: arguments
			precedence: symbol precedence! !

!ZDecompilerConstructor methodsFor: 'constructor' stamp: 'jmv 5/22/2016 10:57'!
codeMethod: selector block: block tempVars: vars primitive: primitive class: class

	| node methodTemps arguments temporaries |
	node := self codeSelector: selector code: nil.
	tempVars := vars.
	methodTemps := tempVars select: [:t | t scope >= 0].
	arguments := methodTemps copyFrom: 1 to: nArgs.
	temporaries := methodTemps copyFrom: nArgs + 1 to: methodTemps size.
	block
		arguments: arguments;
		temporaries: temporaries.
	^ZMethodNode new
		selector: node
		arguments: arguments
		precedence: selector precedence
		temporaries: temporaries
		block: block
		encoder: (ZEncoder new initScopeAndLiteralTables
					temps: tempVars
					literals: literalValues
					class: class)
		primitive: primitive! !

!ZDecompilerConstructor methodsFor: 'constructor' stamp: 'jmv 5/22/2016 10:53'!
codeSelector: sel code: code

	^ZSelectorNode new key: sel code: code! !

!ZDecompilerConstructor methodsFor: 'constructor' stamp: 'jmv 5/22/2016 10:56'!
codeTemp: index

	^ ZTempVariableNode new
		name: 'temp' , (index + 1) printString
		index: index
		type: LdTempType
		scope: 0! !

!ZDecompilerConstructor methodsFor: 'constructor' stamp: 'jmv 5/22/2016 10:56'!
codeTemp: index named: tempName

	^ ZTempVariableNode new
		name: tempName
		index: index
		type: LdTempType
		scope: 0! !

!ZDecompilerConstructor methodsFor: 'constructor' stamp: 'jmv 5/22/2016 10:58'!
decodeIfNilWithReceiver: receiver selector: selector arguments: arguments
	receiver ifNil: [ ^nil ].		"For instance, when cascading"
	selector == #ifTrue:ifFalse:
		ifFalse: [^ nil].
	(receiver isMessage: #==
				receiver: nil
				arguments: [:argNode | argNode == NodeNil])
		ifFalse: [^ nil].
	^ (ZMessageNode new
			receiver: receiver
			selector: (ZSelectorNode new key: #ifTrue:ifFalse: code: #macro)
			arguments: arguments
			precedence: 3)
		noteSpecialSelector: #ifNil:ifNotNil:! !


!ZDecompilerConstructorForClosures methodsFor: 'constructor' stamp: 'jmv 5/22/2016 10:57'!
codeMethod: selector block: block tempVars: vars primitive: primitive class: class

	| blockNode selectorNode visibleTemps invisibleTemps arguments temporaries |
	selectorNode := self codeSelector: selector code: nil.
	tempVars := vars.
	visibleTemps := OrderedCollection new.
	invisibleTemps := OrderedCollection new.
	tempVars do: [:t|
				   ((t isIndirectTempVector or: [t scope >= 0])
						ifTrue: [visibleTemps]
						ifFalse: [invisibleTemps]) addLast: t].
	arguments := visibleTemps copyFrom: 1 to: nArgs.
	temporaries := visibleTemps copyFrom: nArgs + 1 to: visibleTemps size.
	block
		arguments: arguments;
		temporaries: temporaries.
	blockNode := ZMethodNode new
		selector: selectorNode
		arguments: arguments
		precedence: selector precedence
		temporaries: temporaries
		block: block
		encoder: (ZEncoderForV3PlusClosures new initScopeAndLiteralTables
					temps: visibleTemps, invisibleTemps
					literals: literalValues
					class: class)
		primitive: primitive
		properties: method properties copy.
	blockNode properties method: blockNode.
	^blockNode! !

!ZDecompilerConstructorForClosures methodsFor: 'constructor' stamp: 'jmv 5/22/2016 10:56'!
codeRemoteTemp: index remoteTemps: tempVector

	^(ZRemoteTempVectorNode new
		name: 'remoteVar', index printString
		index: index
		type: LdTempType
		scope: 0)
			remoteTemps: tempVector;
			yourself! !


!ZEncoder methodsFor: 'results' stamp: 'jmv 5/22/2016 10:57'!
tempNames 

	^ self tempNodes collect:
		[:node | (node isMemberOf: ZMessageAsTempNode)
					ifTrue: [scopeTable keyAtValue: node]
					ifFalse: [node key]]! !

!ZEncoder methodsFor: 'temps' stamp: 'jmv 5/22/2016 10:56'!
fixTemp: name
	| node |
	node := scopeTable at: name ifAbsent: [].
	node class ~~ ZTempVariableNode ifTrue:
		[self error: 'can only fix a floating temp var'].
	node index: nTemps.
	nTemps := nTemps + 1.
	^node! !

!ZEncoder methodsFor: 'temps' stamp: 'jmv 5/22/2016 10:56'!
floatTemp: node
	(node ~~ (scopeTable at: node name ifAbsent: [])
	or: [node class ~~ ZTempVariableNode
	or: [node code ~= (node code: nTemps - 1 type: LdTempType)]]) ifTrue:
		[self error: 'can only float the last allocated temp var'].
	nTemps := nTemps - 1! !

!ZEncoder methodsFor: 'temps' stamp: 'jmv 5/22/2016 10:56'!
newTemp: name

	nTemps := nTemps + 1.
	^ ZTempVariableNode new
		name: name
		index: nTemps - 1
		type: LdTempType
		scope: 0! !

!ZEncoder methodsFor: 'temps' stamp: 'jmv 5/22/2016 10:55'!
newUndeclaredTemp: name
	^ZUndeclaredVariableNode new name: name! !

!ZEncoder methodsFor: 'encoding' stamp: 'jmv 5/22/2016 10:53'!
encodeLiteral: object

	^self
		name: object
		key: (class literalScannedAs: object notifying: self)
		class: ZLiteralNode
		type: LdLitType
		set: litSet! !

!ZEncoder methodsFor: 'encoding' stamp: 'jmv 5/22/2016 10:54'!
encodeSelector: aSelector
	"Don't call it 'selector'. It would be shadowed."
	^self
		name: aSelector
		key: aSelector
		class: ZSelectorNode
		type: SendType
		set: selectorSet! !

!ZEncoder methodsFor: 'encoding' stamp: 'jmv 5/22/2016 11:06'!
undeclared: name
	| sym |
	requestor interactive ifTrue:
		[requestor requestor == #error: ifTrue:
			[requestor error: 'Undeclared'].
		 ^self notify: 'Undeclared'].
	"Allow knowlegeable clients to squash the undeclared warning if they want (e.g.
	 Diffing pretty printers that are simply formatting text).  As this breaks
	 compilation it should only be used by clients that want to discard the result
	 of the compilation.  To squash the warning use e.g.
		[Compiler format: code in: class notifying: nil decorated: false]
			on: UndeclaredVariableWarning
			do: [:ex| ex resume: false]"
	sym := name asSymbol.
	^(ZUndeclaredVariableWarning new name: name selector: selector class: class) signal
		ifTrue:
			[Undeclared at: sym put: nil.
			self global: (Undeclared associationAt: sym) name: sym]
		ifFalse:
			[self global: (Association key: sym) name: sym]! !

!ZEncoder methodsFor: 'private' stamp: 'jmv 5/22/2016 10:56'!
global: ref name: name

	^self
		name: name
		key: ref
		class: ZLiteralVariableNode
		type: LdLitIndType
		set: litIndSet! !

!ZEncoder methodsFor: 'accessing' stamp: 'jmv 5/22/2016 10:57'!
methodNodeClass
	^ZMethodNode! !

!ZEncoder methodsFor: 'initialization' stamp: 'jmv 5/22/2016 10:57'!
init: aClass context: aContext notifying: req
	requestor := req.
	class := aClass.
	nTemps := 0.
	supered := false.
	self initScopeAndLiteralTables.
	"No Tweak Fields in Cuis"
	class variablesAndOffsetsDo: [ :variable "<String>" :offset "<Integer>" |
		scopeTable
			at: variable
			put: (offset >= 0
					ifTrue: [ZInstanceVariableNode new
								name: variable index: offset]
					ifFalse: [ZMaybeContextInstanceVariableNode new
								name: variable index: offset negated])].
	aContext ifNotNil: [
		| homeNode |
		homeNode := self bindTemp: self doItInContextName.
		"0th temp = aContext passed as arg"
		aContext tempNames withIndexDo: [ :variable :index |
			variable ~= self doItInContextName ifTrue: [
				scopeTable
					at: variable
					put: (ZMessageAsTempNode new
						receiver: homeNode
						selector: #namedTempAt:
						arguments: (Array with: (self encodeLiteral: index))
						precedence: 3
						from: self)]]].
	sourceRanges := Dictionary new: 32.
	globalSourceRanges := OrderedCollection new: 32! !


!ZBytecodeEncoder methodsFor: 'temps' stamp: 'jmv 5/22/2016 11:05'!
bindAndJuggle: name
	"This is used to insert a new temp and reorcder temps on editing.
	 It doesn't really work for closure compilation since we have multiple
	 locations for temps.  Simply signal a reparse is necessary."

	ZReparseAfterSourceEditing signal! !


!ZVariableNode class methodsFor: 'class initialization' stamp: 'jmv 5/22/2016 11:04'!
initClassCachedState
	"
	ZVariableNode initialize
	"
	| encoder |
	encoder := ZEncoder new.
	StdVariables := Dictionary new: 16.
	encoder
		fillDict: StdVariables
		with: ZVariableNode
		mapping: #('self' 'thisContext' 'super' 'nil' 'false' 'true' )
		to: (Array with: LdSelf with: LdThisContext with: LdSuper)
				, (Array with: LdNil with: LdFalse with: LdTrue).
	StdSelectors := Dictionary new: 64.
	encoder
		fillDict: StdSelectors
		with: ZSelectorNode
		mapping: ((1 to: Smalltalk specialSelectorSize) collect: 
							[:i | Smalltalk specialSelectorAt: i])
		to: (SendPlus to: SendPlus + 31).
	StdLiterals := ZLiteralDictionary new: 16.
	encoder
		fillDict: StdLiterals
		with: ZLiteralNode
		mapping: #(-1 0 1 2 )
		to: (LdMinus1 to: LdMinus1 + 3).
	encoder initScopeAndLiteralTables.

	NodeNil := encoder encodeVariable: 'nil'.
	NodeTrue := encoder encodeVariable: 'true'.
	NodeFalse := encoder encodeVariable: 'false'.
	NodeSelf := encoder encodeVariable: 'self'.
	NodeThisContext := encoder encodeVariable: 'thisContext'.
	NodeSuper := encoder encodeVariable: 'super'! !


!ZLiteralVariableNode methodsFor: 'code generation' stamp: 'jmv 5/22/2016 10:58'!
sizeCodeForValue: encoder
	self reserve: encoder.
	(key isVariableBinding and: [key isSpecialReadBinding]) 
		ifFalse:
			[^encoder sizePushLiteralVar: index].
	readNode := ZMessageNode new 
		receiver: (encoder encodeLiteral: key)
		selector: (encoder encodeSelector: #value)
		arguments: #()
		precedence: #value precedence.
	^readNode sizeCodeForValue: encoder! !


!ZRemoteTempVectorNode methodsFor: 'code generation (closures)' stamp: 'jmv 5/22/2016 10:58'!
nodeToInitialize: encoder
	^ZAssignmentNode new
		variable: self
		value: (encoder supportsClosureOpcodes
					ifTrue: [ZNewArrayNode new numElements: remoteTemps size]
					ifFalse:
						[ZMessageNode new
							receiver: (encoder encodeVariable: 'Array')
							selector: #new:
							arguments: (Array with: (encoder encodeLiteral: remoteTemps size))
							precedence: 3
							from: encoder])! !


!ZMessageNode methodsFor: 'code generation (closures)' stamp: 'jmv 5/22/2016 10:47'!
analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>"
	"Assignments within optimized loops are tricky.  Because a loop repeats a
	 write to a temporary in an optimized loop effectively occurs after the loop.
	 To handle this collect the set of temps assigned to in optimized loops and
	 add extra writes after traversing the optimized loop constituents."
	| writtenToTemps |
	self isOptimizedLoop ifTrue:
		[{ receiver }, arguments do:
			[:node|
			(node notNil and: [node isBlockNode and: [node optimized]]) ifTrue:
				[assignmentPools at: node put: Set new]]].
	"receiver is nil in cascades"
	receiver == nil ifFalse:
		[receiver analyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools].
	arguments do:
		[:node|
		node == nil ifFalse: "last argument of optimized to:do: can be nil"
			[node analyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools]].
	"Add assignments representing subsequent iterations
	 and redo the closure analysis for the written-to temps."
	self isOptimizedLoop ifTrue:
		[writtenToTemps := Set new.
		 { receiver }, arguments do:
			[:node|
			(node notNil and: [node isBlockNode and: [node optimized]]) ifTrue:
				[(assignmentPools removeKey: node) do:
					[:temp|
					temp isBlockArg ifFalse: "ignore added assignments to to:do: loop args"
						[writtenToTemps add: temp.
						 temp addWriteWithin: node at: rootNode locationCounter]]]].
		 writtenToTemps isEmpty ifFalse:
			[(writtenToTemps asSortedCollection: ZParseNode tempSortBlock) do:
				[:each| each analyseClosure: rootNode].
			 (writtenToTemps collect: [:each| each definingScope]) do:
				[:blockNode|
				blockNode ifHasRemoteTempNodeEnsureInitializationStatementExists: rootNode]]]! !

!ZMessageNode methodsFor: 'printing' stamp: 'jmv 5/22/2016 10:48'!
printToDoOn: aStream indent: level

	| limitNode |
	self printReceiver: receiver on: aStream indent: level.

	(arguments last == nil or: [(arguments last isMemberOf: ZAssignmentNode) not])
		ifTrue: [limitNode := arguments first]
		ifFalse: [limitNode := arguments last value].
	(selector key = #to:by:do:
	 and: [(arguments at: 2) isConstantNumber
	 and: [(arguments at: 2) key = 1]])
		ifTrue: [self printKeywords: #to:do:
					arguments: (Array with: limitNode with: (arguments at: 3))
					on: aStream indent: level]
		ifFalse: [self printKeywords: selector key
					arguments: (Array with: limitNode) , arguments allButFirst
					on: aStream indent: level]! !

!ZMessageNode methodsFor: 'printing' stamp: 'jmv 5/22/2016 10:54'!
printWithClosureAnalysisWhileOn: aStream indent: level

	self printWithClosureAnalysisReceiver: receiver on: aStream indent: level.
	(arguments isEmpty not
	 and: [arguments first isJust: NodeNil]) ifTrue:
			[selector := ZSelectorNode new
							key:
									(selector key == #whileTrue:
										ifTrue: [#whileTrue]
										ifFalse: [#whileFalse])
							code: #macro.
			arguments := Array new].
	self printWithClosureAnalysisKeywords: selector key arguments: arguments
		on: aStream indent: level! !

!ZMessageNode methodsFor: 'macro transformations' stamp: 'jmv 5/22/2016 10:58'!
toDoFromWhileWithInit: initStmt
	"Return nil, or a to:do: expression equivalent to this whileTrue:"
	| variable increment limit toDoBlock body test |
	(selector key == #whileTrue:
	 and: [initStmt isAssignmentNode
	 and: [initStmt variable isTemp]]) ifFalse:
		[^nil].
	body := arguments last statements.
	variable := initStmt variable.
	increment := body last toDoIncrement: variable.
	(increment == nil
	 or: [receiver statements size ~= 1]) ifTrue:
		[^nil].
	test := receiver statements first.
	"Note: test chould really be checked that <= or >= comparison
	jibes with the sign of the (constant) increment"
	(test isMessageNode
	 and: [(limit := test toDoLimit: variable) notNil]) ifFalse:
		[^nil].
	"The block must not overwrite the limit"
	(limit isVariableNode and: [body anySatisfy: [:e | e isAssignmentNode and: [e variable = limit]]])
		ifTrue: [^nil]. 
	toDoBlock := ZBlockNode statements: body allButLast returns: false.
	toDoBlock arguments: (Array with: variable).
	variable scope: -1.
	variable beBlockArg.
	^ZMessageNode new
		receiver: initStmt value
		selector: (ZSelectorNode new key: #to:by:do: code: #macro)
		arguments: (Array with: limit with: increment with: toDoBlock)
		precedence: precedence! !

!ZMessageNode methodsFor: 'macro transformations' stamp: 'jmv 5/22/2016 10:49'!
transformAnd: encoder
	(self transformBoolean: encoder)
		ifTrue: 
			[arguments := 
				Array 
					with: ((arguments at: 1) noteOptimizedIn: self)
					with: ((ZBlockNode withJust: NodeFalse) noteOptimizedIn: self).
			^true]
		ifFalse: 
			[^false]! !

!ZMessageNode methodsFor: 'macro transformations' stamp: 'jmv 5/22/2016 10:50'!
transformCase: encoder

	| caseNode |
	caseNode := arguments first.
	(caseNode isMemberOf: ZBraceNode) ifFalse: [ ^false ].
	(caseNode blockAssociationCheck: encoder) ifFalse: [ ^false ].
	(arguments size = 1 or: [
		self checkBlock: arguments last as: 'otherwise arg' from: encoder ]) ifFalse: [
			^false ].
	 caseNode elements do: [ :messageNode |
		messageNode receiver noteOptimizedIn: self.
		messageNode arguments first noteOptimizedIn: self ].
	 arguments size = 2 ifTrue: [ arguments last noteOptimizedIn: self ].
	 ^true! !

!ZMessageNode methodsFor: 'macro transformations' stamp: 'jmv 5/22/2016 10:49'!
transformIfFalse: encoder
	(self transformBoolean: encoder)
		ifTrue: 
			[arguments := 
				Array 
					with: ((ZBlockNode withJust: NodeNil) noteOptimizedIn: self)
					with: ((arguments at: 1) noteOptimizedIn: self).
			^true]
		ifFalse:
			[^false]! !

!ZMessageNode methodsFor: 'macro transformations' stamp: 'jmv 5/22/2016 10:54'!
transformIfFalseIfTrue: encoder
	^(self checkBlock: (arguments at: 1) as: 'False arg' from: encoder)
	   and: [(self checkBlock: (arguments at: 2) as: 'True arg' from: encoder)
	   and: [selector := ZSelectorNode new key: #ifTrue:ifFalse: code: #macro.
			arguments swap: 1 with: 2.
			arguments do: [:arg| arg noteOptimizedIn: self].
			true]]! !

!ZMessageNode methodsFor: 'macro transformations' stamp: 'jmv 5/22/2016 10:54'!
transformIfNil: encoder

	"vb: Removed the original transformBoolean: which amounds to a test we perform in each of the branches below."
	(MacroSelectors at: special) = #ifNotNil: ifTrue:
		[(self checkBlock: arguments first as: 'ifNotNil arg' from: encoder maxArgs: 1) ifFalse:
			[^false].

		"Transform 'ifNotNil: [stuff]' to 'ifNil: [nil] ifNotNil: [stuff]'.
		Slightly better code and more consistent with decompilation."
		self noteSpecialSelector: #ifNil:ifNotNil:.
		selector := ZSelectorNode new key: (MacroSelectors at: special) code: #macro.
		arguments := Array
						with: ((ZBlockNode withJust: NodeNil) noteOptimizedIn: self)
						with: (arguments first noteOptimizedIn: self).
		(self transform: encoder) ifFalse:
			[self error: 'compiler logic error'].
		^true].
	(self checkBlock: arguments first as: 'ifNil arg' from: encoder) ifFalse:
		[^false].
	arguments first noteOptimizedIn: self.
	^true! !

!ZMessageNode methodsFor: 'macro transformations' stamp: 'jmv 5/22/2016 10:58'!
transformIfNilIfNotNil: encoder
	"vb: Changed to support one-argument ifNotNil: branch. In the 1-arg case we
	 transform the receiver to
		(var := receiver)
	 which is further transformed to
		(var := receiver) == nil ifTrue: .... ifFalse: ...
	 This does not allow the block variable to shadow an existing temp, but it's no different
	 from how to:do: is done."
	| ifNotNilArg |
	ifNotNilArg := arguments at: 2.
	((self checkBlock: (arguments at: 1) as: 'Nil arg' from: encoder)
	  and: [self checkBlock: ifNotNilArg as: 'NotNil arg' from: encoder maxArgs: 1]) ifFalse:
		[^false].

	ifNotNilArg numberOfArguments = 1 ifTrue:
		[receiver := ZAssignmentNode new
						variable: ifNotNilArg firstArgument
						value: receiver].

	selector := ZSelectorNode new key: #ifTrue:ifFalse: code: #macro.
	receiver := ZMessageNode new
					receiver: receiver
					selector: #==
					arguments: (Array with: NodeNil)
					precedence: 2
					from: encoder.
	arguments do: [:arg| arg noteOptimizedIn: self].
	^true! !

!ZMessageNode methodsFor: 'macro transformations' stamp: 'jmv 5/22/2016 10:58'!
transformIfNotNilIfNil: encoder
	"vb: Changed to support one-argument ifNotNil: branch. In the 1-arg case we
	 transform the receiver to
		(var := receiver)
	 which is further transformed to
		(var := receiver) == nil ifTrue: .... ifFalse: ...
	 This does not allow the block variable to shadow an existing temp, but it's no different
	 from how to:do: is done."
	| ifNotNilArg |
	ifNotNilArg := arguments at: 1.
	((self checkBlock: ifNotNilArg as: 'NotNil arg' from: encoder maxArgs: 1)
	  and: [self checkBlock: (arguments at: 2) as: 'Nil arg' from: encoder]) ifFalse:
		[^false].

	ifNotNilArg numberOfArguments = 1 ifTrue:
		[receiver := ZAssignmentNode new
						variable: ifNotNilArg firstArgument
						value: receiver].

	selector := ZSelectorNode new key: #ifTrue:ifFalse: code: #macro.
	receiver := ZMessageNode new
					receiver: receiver
					selector: #==
					arguments: (Array with: NodeNil)
					precedence: 2
					from: encoder.
	arguments swap: 1 with: 2.
	arguments do: [:arg| arg noteOptimizedIn: self].
	^true! !

!ZMessageNode methodsFor: 'macro transformations' stamp: 'jmv 5/22/2016 10:49'!
transformIfTrue: encoder
	(self transformBoolean: encoder)
		ifTrue: 
			[arguments := 
				Array 
					with: ((arguments at: 1) noteOptimizedIn: self)
					with: ((ZBlockNode withJust: NodeNil) noteOptimizedIn: self).
			^true]
		ifFalse: 
			[^false]! !

!ZMessageNode methodsFor: 'macro transformations' stamp: 'jmv 5/22/2016 10:49'!
transformOr: encoder
	(self transformBoolean: encoder)
		ifTrue: 
			[arguments := 
				Array 
					with: ((ZBlockNode withJust: NodeTrue) noteOptimizedIn: self)
					with: ((arguments at: 1) noteOptimizedIn: self).
			^true]
		ifFalse: 
			[^false]! !

!ZMessageNode methodsFor: 'macro transformations' stamp: 'jmv 5/22/2016 10:58'!
transformToDo: encoder
	" var := rcvr. L1: [var <= arg1] Bfp(L2) [block body. var := var + inc] 
Jmp(L1) L2: "
	| limit increment block initStmt test incStmt limitInit blockVar myRange |
	"First check for valid arguments"
	((arguments last isMemberOf: ZBlockNode)
	  and: [arguments last numberOfArguments = 1
	  and: [arguments last firstArgument isVariableReference "As with debugger remote vars"]]) ifFalse:
		[^false].
	arguments size = 3
		ifTrue: [increment := arguments at: 2.
				(increment isConstantNumber
				 and: [increment literalValue ~= 0]) ifFalse: [^false]]
		ifFalse: [increment := encoder encodeLiteral: 1].
	limit := arguments at: 1.
	arguments size < 3 ifTrue:   "transform to full form"
		[selector := ZSelectorNode new key: #to:by:do: code: #macro].

	"Now generate auxiliary structures"
	myRange := encoder rawSourceRanges at: self ifAbsent: [1 to: 0].
	block := arguments last.
	blockVar := block firstArgument.
	initStmt := ZAssignmentNode new variable: blockVar value: receiver.
	limit isConstantNumber
		ifTrue: [limitInit := nil]
		ifFalse:  "Need to store limit in a var"
			[limit := encoder bindBlockArg: blockVar key, 'LimiT' within: block.
			 limit scope: -2.  "Already done parsing block; flag so it won't print"
			 block addArgument: limit.
			 limitInit := ZAssignmentNode new
							variable: limit
							value: arguments first].
	test := ZMessageNode new
				receiver: blockVar
				selector: (increment key > 0 ifTrue: [#<=] ifFalse: [#>=])
				arguments: (Array with: limit)
				precedence: precedence from: encoder
"				sourceRange: (myRange first to: blockRange first)."
				sourceRange: myRange.
	incStmt := ZAssignmentNode new
				variable: blockVar
				value: (ZMessageNode new
							receiver: blockVar selector: #+
							arguments: (Array with: increment)
							precedence: precedence from: encoder)
				from: encoder
				sourceRange: (myRange last to: myRange last).
	arguments := (Array with: limit with: increment with: block),
					(Array with: initStmt with: test with: incStmt with: limitInit).
	block noteOptimizedIn: self.
	^true! !

!ZMessageNode methodsFor: 'macro transformations' stamp: 'jmv 5/22/2016 10:54'!
transformWhile: encoder
	(self checkBlock: receiver as: 'receiver' from: encoder) ifFalse:
		[^false].
	arguments size = 0 ifTrue:  "transform bodyless form to body form"
		[selector := ZSelectorNode new
						key: (special = 10 ifTrue: [#whileTrue:] ifFalse: [#whileFalse:])
						code: #macro.
		 arguments := Array with: ((ZBlockNode withJust: NodeNil) noteOptimizedIn: self).
		 receiver noteOptimizedIn: self.
		 ^true].
	^(self transformBoolean: encoder)
	   and: [receiver noteOptimizedIn: self.
			arguments first noteOptimizedIn: self.
			true]! !

!ZMessageNode methodsFor: 'initialization' stamp: 'jmv 5/22/2016 10:54'!
receiver: rcvr selector: aSelector arguments: args precedence: p from: encoder 
	"Compile."

	self receiver: rcvr
		arguments: args
		precedence: p.
	originalSelector := aSelector.
	self noteSpecialSelector: aSelector.
	(self transform: encoder)
		ifTrue: 
			[selector isNil ifTrue:
				[selector := ZSelectorNode new 
								key: (MacroSelectors at: special)
								code: #macro]]
		ifFalse: 
			[selector := encoder encodeSelector: aSelector.
			rcvr == NodeSuper ifTrue: [encoder noteSuper]].
	self pvtCheckForPvtSelector: encoder! !


!ZMessageAsTempNode methodsFor: 'access to remote temps' stamp: 'jmv 5/22/2016 10:57'!
asStorableNode: encoder
	"This node is a message masquerading as a temporary variable.
	It currently has the form {homeContext tempAt: offset}.
	We need to generate code for {expr storeAt: offset inTempFrame: homeContext},
	where the expr, the block argument, is already on the stack.
	This, in turn will get turned into {homeContext tempAt: offset put: expr}
	at runtime if nobody disturbs storeAt:inTempFrame: in Object (not clean)"
	^ ZMessageAsTempNode new
		receiver: nil  "suppress code generation for receiver already on stack"
		selector: #storeAt:inTempFrame:
		arguments: (arguments copyWith: receiver)
		precedence: precedence
		from: encoder! !

!ZMessageAsTempNode methodsFor: 'access to remote temps' stamp: 'jmv 5/22/2016 10:57'!
store: expr from: encoder 
	"ctxt tempAt: n -> ctxt tempAt: n put: expr (see Assignment).
	For assigning into temps of a context being debugged."

	selector key ~= #namedTempAt: 
		ifTrue: [^self error: 'cant transform this message'].
	^ ZMessageAsTempNode new
		receiver: receiver
		selector: #namedTempAt:put:
		arguments: (arguments copyWith: expr)
		precedence: precedence
		from: encoder! !


!ZMethodNode methodsFor: 'code generation' stamp: 'jmv 5/22/2016 11:05'!
generate: trailer 
	"The receiver is the root of a parse tree. Answer a CompiledMethod.
	 The argument, trailer, is arbitrary but is typically either the reference
	 to the source code that is stored with every CompiledMethod, or an
	 encoding of the method's temporary names."

	| primErrNode blkSize nLits literals stack method |
	self generate: trailer ifQuick:
			[:m |
			  m	literalAt: 2 put: encoder associationForClass;
				properties: properties.
			^m].
	primErrNode := self primitiveErrorVariableName ifNotNil:
						[encoder fixTemp: self primitiveErrorVariableName].
	encoder supportsClosureOpcodes ifTrue:
		[self ensureClosureAnalysisDone.
		 encoder rootNode: self. "this is for BlockNode>>sizeCodeForClosureValue:"].
	blkSize := (block sizeCodeForEvaluatedValue: encoder)
				+ (primErrNode
					ifNil: [0]
					ifNotNil: [primErrNode sizeCodeForStore: encoder "The VM relies on storeIntoTemp: (129)"]).
	method := CompiledMethod
				newBytes: blkSize
				trailerBytes: trailer 
				nArgs: arguments size
				nTemps: (encoder supportsClosureOpcodes
							ifTrue: [| locals |
									locals := arguments,
											  temporaries,
											  (primErrNode
												ifNil: [#()]
												ifNotNil: [{primErrNode}]).
									encoder
										noteBlockExtent: block blockExtent
										hasLocals: locals.
									locals size]
							ifFalse: [encoder maxTemp])
				nStack: 0
				nLits: (nLits := (literals := encoder allLiterals) size)
				primitive: primitive.
	nLits > 255 ifTrue:
		[^self error: 'Too many literals referenced'].
	1 to: nLits do: [:lit | method literalAt: lit put: (literals at: lit)].
	encoder streamToMethod: method.
	stack := ZParseStack new init.
	primErrNode ifNotNil: [primErrNode emitCodeForStore: stack encoder: encoder].
	stack position: method numTemps.
	block emitCodeForEvaluatedValue: stack encoder: encoder.
	stack position ~= (method numTemps + 1) ifTrue:
		[^self error: 'Compiler stack discrepancy'].
	encoder methodStreamPosition ~= (method size - trailer size) ifTrue:
		[^self error: 'Compiler code size discrepancy'].
	method needsFrameSize: stack size - method numTemps.
	method properties: properties.
	^method! !

!ZMethodNode methodsFor: 'code generation' stamp: 'jmv 5/22/2016 11:35'!
parserClass
	"Which parser produces this class of parse node"

	^ ZParser! !

!ZMethodNode methodsFor: 'code generation' stamp: 'jmv 5/22/2016 10:54'!
selectorNode
	"Answer a SelectorNode for the message selector of the method represented by the receiver."

	^(selectorOrFalse isMemberOf: ZSelectorNode)
		ifTrue: [selectorOrFalse]
		ifFalse: [ZSelectorNode new key: selectorOrFalse]! !

!ZMethodNode methodsFor: 'printing' stamp: 'jmv 5/22/2016 11:35'!
printPragmasOn: aStream
	properties ifNil: [^self].
	properties pragmas do: [ :pragma |
		"Primitives are printed in printPrimitiveOn:; skip these"
		(ZParser primitivePragmaSelectors includes: pragma keyword) ifFalse:
			[aStream newLineTab: 1.
			 pragma printOn: aStream]]! !


!ZParseNodeVisitor methodsFor: 'visiting' stamp: 'jmv 5/22/2016 10:54'!
visitFutureNode: aFutureNode
	aFutureNode receiver accept: self.
	(aFutureNode originalSelector isKindOf: ZSelectorNode) ifTrue:
		[aFutureNode originalSelector accept: self]! !


!ZScanner methodsFor: 'expression types' stamp: 'jmv 5/22/2016 11:33'!
scanAllTokenPositionsInto: aBlock
	"Evaluate aBlock with the start and end positions of all separate non-white-space tokens, including comments."

	| lastMark |
	lastMark := 1.
	[currentComment ifNotNil:
		[currentComment do:
			[:cmnt| | idx |
			 idx := source originalContents indexOfSubCollection: cmnt startingAt: lastMark.
			 (idx > 0 and: [idx < mark]) ifTrue:
				[aBlock value: idx - 1 value: (lastMark := idx + cmnt size)]].
		 currentComment := nil].
	mark ifNotNil:
		[(token == #- 
		  and: [(self typeTableAt: hereChar) == #xDigit]) ifTrue:
			[| savedMark |
			 savedMark := mark.
			 self scanToken.
			 token := token negated.
			 mark := savedMark].
		"Compensate for the fact that the parser uses two character lookahead.  Normally we must
		  remove the extra two characters.  But this mustn't happen for the last token at the end of stream."
		 aBlock
			value: mark
			value: (source position - (aheadChar = ZScanner doItCharacterValue ifTrue: [hereChar = ZScanner doItCharacterValue asCharacter ifTrue: [0] ifFalse: [1]] ifFalse: [2]))].
	 (tokenType == #rightParenthesis
	  or: [tokenType == #doIt]) ifTrue:
		[^self].
	tokenType == #leftParenthesis
		ifTrue: 
			[self scanToken; scanAllTokenPositionsInto: aBlock]
		ifFalse: 
			[(tokenType == #word or: [tokenType == #keyword or: [tokenType == #colon]])
				ifTrue: 
					[self scanLitWord.
					 token == #true ifTrue: [token := true].
					 token == #false ifTrue: [token := false].
					 token == #nil ifTrue: [token := nil]]
				ifFalse:
					[(token == #- 
					  and: [(self typeTableAt: hereChar) == #xDigit])
						ifTrue: 
							[self scanToken.
							 token := token negated]]].
		self scanToken ] repeat! !

!ZScanner methodsFor: 'expression types' stamp: 'jmv 5/22/2016 11:34'!
step

	| c |
	c := hereChar.
	hereChar := aheadChar.
	source atEnd
		ifTrue: [aheadChar := ZScanner doItCharacterValue asCharacter "doit"]
		ifFalse: [aheadChar := source next].
	^c! !

!ZScanner methodsFor: 'multi-character scans' stamp: 'jmv 5/22/2016 11:34'!
xDigit
	"Form a number."

	tokenType := #number.
	(aheadChar = ZScanner doItCharacterValue asCharacter and: [source atEnd
			and:  [source skip: -1. source next ~= ZScanner doItCharacterValue asCharacter]])
		ifTrue: [source skip: -1 "Read off the end last time"]
		ifFalse: [source skip: -2].
	token := [Number readFrom: source] ifError: [:err :rcvr | self offEnd: err].
	self step; step! !

!ZScanner methodsFor: 'multi-character scans' stamp: 'jmv 5/22/2016 11:34'!
xDoubleQuote
    "Collect a comment."
    "wod 1/10/98: Allow 'empty' comments by testing the first character
for $"" rather than blindly adding it to the comment being collected."
    | aStream stopChar |
    stopChar := ZScanner doItCharacterValue asCharacter.
    aStream := WriteStream on: (String new: 200).
    self step.
    [hereChar == $"]
        whileFalse:
            [(hereChar == stopChar and: [source atEnd])
                ifTrue: [^self offEnd: 'Unmatched comment quote'].
            aStream nextPut: self step.].
    self step.
    currentComment == nil
        ifTrue: [currentComment := OrderedCollection with: aStream
contents]
        ifFalse: [currentComment add: aStream contents].
    self scanToken! !

!ZScanner methodsFor: 'multi-character scans' stamp: 'jmv 5/22/2016 11:33'!
xLetter
	"Form a word or keyword."

	| type |
	buffer reset.
	[(type := self typeTableAt: hereChar) == #xLetter
		or: [type == #xDigit
		or: [type == #xUnderscore]]] whileTrue:
			["open code step for speed"
			buffer nextPut: hereChar.
			hereChar := aheadChar.
			aheadChar := source atEnd
							ifTrue: [ZScanner doItCharacterValue asCharacter "doit"]
							ifFalse: [source next]].
	tokenType := (type == #colon or: [type == #xColon and: [aheadChar ~~ $=]])
					ifTrue: 
						[buffer nextPut: self step.
						"Allow any number of embedded colons in literal symbols"
						[(self typeTableAt: hereChar) == #xColon] whileTrue:
							[buffer nextPut: self step].
						#keyword]
					ifFalse: 
						[#word].
	token := buffer contents! !

!ZScanner methodsFor: 'multi-character scans' stamp: 'jmv 5/22/2016 11:34'!
xSingleQuote
	"String."

	self step.
	buffer reset.
	[hereChar = $' 
		and: [aheadChar = $' 
				ifTrue: [self step. false]
				ifFalse: [true]]]
		whileFalse: 
			[buffer nextPut: self step.
			(hereChar = ZScanner doItCharacterValue asCharacter and: [source atEnd])
				ifTrue: [^self offEnd: 'Unmatched string quote']].
	self step.
	token := buffer contents.
	tokenType := #string! !

!ZScanner methodsFor: 'initialization' stamp: 'jmv 5/22/2016 11:34'!
initScanner

	buffer := WriteStream on: (String new: 40).
	typeTable := ZScanner typeTable.
	isForTokenization := false! !


!ZScanner class methodsFor: 'testing' stamp: 'jmv 5/22/2016 11:34'!
findSelectorIn: aString

    | result |
    result _ ZScanner new
        scan: (ReadStream on:  aString);
        scanForFindSelectorUpTo: #notATerminator.
    ^result! !

!ZScanner class methodsFor: 'cached class state' stamp: 'jmv 5/22/2016 11:34'!
initTypeTable
	| newTable |
	newTable := Array new: 256 withAll: #xIllegal. "default"
	newTable atAll: #(9 10 12 13 32 ) put: #xDelimiter. "tab lf ff cr space"

	1 to: 255
		do: [:index |
			(Character value: index) isValidInIdentifiers
				ifTrue: [
					"Digits and $_ are overwritten later"
					newTable at: index put: #xLetter ]].

	newTable atAll: ($0 asciiValue to: $9 asciiValue) put: #xDigit.
	newTable atAll:  '!!%&+-*/\±×÷¬­¯,<=>”•–—˜™š›œ«»?@~€‚ƒŽ‘’“žŸ°·' asByteArray put: #xBinary.

	newTable at: ZScanner doItCharacterValue put: #doIt.
	newTable at: $" asciiValue put: #xDoubleQuote.
	newTable at: $# asciiValue put: #xLitQuote.
	newTable at: $$ asciiValue put: #xDollar.
	newTable at: $' asciiValue put: #xSingleQuote.
	newTable at: $: asciiValue put: #xColon.
	newTable at: $( asciiValue put: #leftParenthesis.
	newTable at: $) asciiValue put: #rightParenthesis.
	newTable at: $. asciiValue put: #period.
	newTable at: $; asciiValue put: #semicolon.
	newTable at: $[ asciiValue put: #leftBracket.
	newTable at: $] asciiValue put: #rightBracket.
	newTable at: ${ asciiValue put: #leftBrace.
	newTable at: $} asciiValue put: #rightBrace.
	newTable at: $^ asciiValue put: #upArrow.
	newTable at: $_ asciiValue put: #xUnderscore.
	newTable at: $| asciiValue put: #verticalBar.
	TypeTable := newTable "bon voyage!!"

	"
	Scanner initTypeTable
	"! !


!ZParser methodsFor: 'pragmas' stamp: 'jmv 5/22/2016 11:34'!
pragmaLiteral: selectorSoFar
	"Read a pragma literal.  As a nicety we allow a variable name (rather
	 than a literal string) as the second argument to primitive:error:"

	(hereType == #string or: [ hereType == #literal or: [ hereType == #number ] ])
		ifTrue: [ ^ self advance ].
	(here == $# and: [ tokenType == #word ])
		ifTrue: [ ^ self advance ].
	(here == #- and: [ tokenType == #number ])
		ifTrue: [ ^ (self advance; advance) negated ].
	(here = 'true' or: [ here = 'false' or: [ here = 'nil' ] ])
		ifTrue: [ ^ (ZScanner new scanTokens: self advance) first ].
	"This nicety allows one to supply a primitive error
	 temp as a variable name, rather than a string."
	((selectorSoFar beginsWith: 'primitive:')
	 and: [(selectorSoFar endsWith: 'error:')
	 and: [hereType == #word]]) ifTrue:
		[^self advance].
	^self expected: 'Literal constant'! !

!ZParser methodsFor: 'error handling' stamp: 'jmv 5/22/2016 11:08'!
notify: string at: location
	requestor
		ifNil: [
			(encoder == self or: [encoder isNil]) ifTrue: [^ self fail  "failure setting up syntax error"].
				ZSyntaxErrorNotification
					inClass: encoder classEncoding
					category: category
					withCode: 
						(source contents
							copyReplaceFrom: location
							to: location - 1
							with: string , ' ->')
					doitFlag: doitFlag
					errorMessage: string
					location: location]
		ifNotNil: [
			requestor
					notify: string , ' ->'
					at: location
					in: source].
	^self fail! !

!ZParser methodsFor: 'scanning' stamp: 'jmv 5/22/2016 11:34'!
advance
	| this |
	prevMark := hereMark.
	prevEnd := hereEnd.
	this := here.
	here := token.
	hereType := tokenType.
	hereMark := mark.
	hereEnd := source position - (source atEnd ifTrue: [hereChar = ZScanner doItCharacterValue asCharacter ifTrue: [0] ifFalse: [1]] ifFalse: [2]).
	self scanToken.
	"Transcript show: 'here: ', here printString, ' mark: ', hereMark printString, ' end: ', hereEnd printString; cr."
	^this! !

!ZParser methodsFor: 'expression types' stamp: 'jmv 5/22/2016 10:49'!
assignment: varNode
	" var ':=' expression => AssignmentNode."
	| loc start |
	(loc := varNode assignmentCheck: encoder at: prevMark + requestorOffset) >= 0
		ifTrue: [^self notify: 'Cannot store into' at: loc].
	start := self startOfNextToken.
	self advance.
	self expression ifFalse: [^self expected: 'Expression'].
	parseNode := ZAssignmentNode new
				variable: varNode
				value: parseNode
				from: encoder
				sourceRange: (start to: self endOfLastToken).
	varNode nowHasDef.
	^true! !

!ZParser methodsFor: 'expression types' stamp: 'jmv 5/22/2016 10:50'!
blockExpression
	"[ ({:var} |) (| {temps} |) (statements) ] => BlockNode."

	| blockNode variableNodes temporaryBlockVariables start |
	blockNode := ZBlockNode new.
	variableNodes := OrderedCollection new.
	start := prevMark + requestorOffset.
	"Gather parameters."
	[self match: #colon] whileTrue:
		[variableNodes addLast: (encoder bindBlockArg: self argumentName within: blockNode)].
	(variableNodes size > 0 & (hereType ~~ #rightBracket) and: [(self match: #verticalBar) not]) ifTrue:
		[^self expected: 'Vertical bar'].

	temporaryBlockVariables := self temporaryBlockVariablesFor: blockNode.
	self statements: variableNodes innerBlock: true blockNode: blockNode.
	blockNode temporaries: temporaryBlockVariables.

	(self match: #rightBracket) ifFalse: [^self expected: 'Period or right bracket'].

	blockNode noteSourceRangeStart: start end: self endOfLastToken encoder: encoder.

	"The scope of the parameters and temporary block variables is no longer active."
	temporaryBlockVariables do: [:variable | variable scope: -1].
	variableNodes do: [:variable | variable scope: -1]! !

!ZParser methodsFor: 'expression types' stamp: 'jmv 5/22/2016 10:50'!
braceExpression
	" { elements } => BraceNode."

	| elements locations loc more |
	elements := OrderedCollection new.
	locations := OrderedCollection new.
	self advance.
	more := hereType ~~ #rightBrace.
	[more]
		whileTrue: 
			[loc := hereMark + requestorOffset.
			self expression
				ifTrue: 
					[elements addLast: parseNode.
					locations addLast: loc]
				ifFalse:
					[^self expected: 'Variable or expression or right brace'].
			(self match: #period)
				ifTrue: [more := hereType ~~ #rightBrace]
				ifFalse: [more := false]].
	parseNode := ZBraceNode new elements: elements sourceLocations: locations.
	(self match: #rightBrace)
		ifFalse: [^self expected: 'Period or right brace'].
	^true! !

!ZParser methodsFor: 'expression types' stamp: 'jmv 5/22/2016 10:51'!
cascade
	" {; message} => CascadeNode."

	| rcvr msgs |
	parseNode canCascade ifFalse:
		[^self expected: 'Cascading not'].
	parseNode ensureCanCascade: encoder.
	rcvr := parseNode cascadeReceiver.
	msgs := OrderedCollection with: parseNode.
	[self match: #semicolon]
		whileTrue: 
			[parseNode := rcvr.
			(self messagePart: 3 repeat: false)
				ifFalse: [^self expected: 'Cascade'].
			parseNode canCascade ifFalse:
				[^self expected: '<- No special messages'].
			parseNode ensureCanCascade: encoder.
				parseNode cascadeReceiver.
			msgs addLast: parseNode].
	parseNode := ZCascadeNode new receiver: rcvr messages: msgs! !

!ZParser methodsFor: 'expression types' stamp: 'jmv 5/22/2016 10:58'!
messagePart: level repeat: repeat

	| start receiver selector args precedence words keywordStart |
	[receiver := parseNode.
	(hereType == #keyword and: [level >= 3])
		ifTrue: 
			[start := self startOfNextToken.
			selector := WriteStream on: (String new: 32).
			args := OrderedCollection new.
			words := OrderedCollection new.
			[hereType == #keyword]
				whileTrue: 
					[keywordStart := self startOfNextToken + requestorOffset.
					selector nextPutAll: self advance.
					words addLast: (keywordStart to: self endOfLastToken + requestorOffset).
					self primaryExpression ifFalse: [^self expected: 'Argument'].
					self messagePart: 2 repeat: true.
					args addLast: parseNode].
			(Symbol hasInterned: selector contents ifTrue: [ :sym | selector := sym])
				ifFalse: [ selector := self correctSelector: selector contents
										wordIntervals: words
										exprInterval: (start to: self endOfLastToken)
										ifAbort: [ ^ self fail ] ].
			precedence := 3]
		ifFalse: [((hereType == #binary or: [hereType == #verticalBar])
				and: [level >= 2])
				ifTrue: 
					[start := self startOfNextToken.
					selector := self advance asSymbol.
					self primaryExpression ifFalse: [^self expected: 'Argument'].
					self messagePart: 1 repeat: true.
					args := Array with: parseNode.
					precedence := 2]
				ifFalse: [hereType == #word
						ifTrue: 
							[start := self startOfNextToken.
							selector := self advance.
							args := #().
							words := OrderedCollection with: (start  + requestorOffset to: self endOfLastToken + requestorOffset).
							(Symbol hasInterned: selector ifTrue: [ :sym | selector := sym])
								ifFalse: [ selector := self correctSelector: selector
													wordIntervals: words
													exprInterval: (start to: self endOfLastToken)
													ifAbort: [ ^ self fail ] ].
							precedence := 1]
						ifFalse: [^args notNil]]].
	parseNode := ZMessageNode new
				receiver: receiver
				selector: selector
				arguments: args
				precedence: precedence
				from: encoder
				sourceRange: (start to: self endOfLastToken).
	repeat]
		whileTrue: [].
	^true! !

!ZParser methodsFor: 'expression types' stamp: 'jmv 5/22/2016 10:50'!
statements: argNodes innerBlock: inner

	^self statements: argNodes innerBlock: inner blockNode: ZBlockNode new! !

!ZParser methodsFor: 'expression types' stamp: 'jmv 5/22/2016 10:56'!
statements: argNodes innerBlock: inner blockNode: theBlockNode

	| stmts returns start |
	"give initial comment to block, since others trail statements"
	theBlockNode comment: currentComment.
	"Very important. Nil the comment, to avoid printing comments multiple times when pretty-printing"
	currentComment _ nil.
	stmts := OrderedCollection new.
	returns := false.
	hereType ~~ #rightBracket ifTrue:
		[[theBlockNode startOfLastStatement: (start := self startOfNextToken).
		  (returns := self matchReturn)
			ifTrue: 
				[self expression ifFalse:
					[^self expected: 'Expression to return'].
				 self addComment.
				 stmts addLast: (parseNode isReturningIf
								ifTrue: [parseNode]
								ifFalse: [ZReturnNode new
											expr: parseNode
											encoder: encoder
											sourceRange: (start to: self endOfLastToken)])]
			ifFalse: 
				[self expression
					ifTrue: 
						[self addComment.
						 stmts addLast: parseNode]
					ifFalse: 
						[self addComment.
						 stmts size = 0 ifTrue: 
							[stmts addLast: 
								(encoder encodeVariable:
									(inner ifTrue: ['nil'] ifFalse: ['self']))]]].
		  returns ifTrue: 
			[self match: #period.
			 (hereType == #rightBracket or: [hereType == #doIt]) ifFalse:
				[^self expected: 'End of block']].
		  returns not and: [self match: #period]] whileTrue].
	theBlockNode
		arguments: argNodes
		statements: stmts
		returns: returns
		from: encoder.
	parseNode := theBlockNode.
	^true! !

!ZParser methodsFor: 'expression types' stamp: 'jmv 5/22/2016 11:06'!
variable

	| varName varStart varEnd result |
	varStart _ self startOfNextToken + requestorOffset.
	varName _ self advance.
	varEnd _ self endOfLastToken + requestorOffset.
	[
		result _ encoder encodeVariable: varName
			sourceRange: (varStart to: varEnd)
			ifUnknown: [ nil ].
		result ifNil: [
			result _ (ZUndeclaredVariableReference new)
				parser: self;
				varName: varName;
				varStart: varStart;
				varEnd: varEnd;
				signal ].
		result isString ] whileTrue: [ varName _ result] .
	^result! !

!ZParser methodsFor: 'error correction' stamp: 'jmv 5/22/2016 11:07'!
correctSelector: proposedKeyword wordIntervals: spots exprInterval: expInt ifAbort: abortAction
	"Correct the proposedKeyword to some selector symbol, correcting the original text if such action is indicated.  abortAction is invoked if the proposedKeyword couldn't be converted into a valid selector.  Spots is an ordered collection of intervals within the test stream of the for each of the keyword parts."

	| correctSelector userSelection |
	"If we can't ask the user, assume that the keyword will be defined later"
	self interactive ifFalse: [^proposedKeyword asSymbol].
	
	"If the requestor is of an exotic kind (like a telnet server) we might not be allowed to open a PupUpMenu for querying the user"
	"
	((requestor isKindOf: Editor) or: [ requestor is: #Morph ])
		ifFalse: [ ^ proposedKeyword asSymbol ].
	"

	userSelection _ requestor selectionInterval.
	requestor selectFrom: spots first first to: spots last last.

	correctSelector _ ZUnknownSelector name: proposedKeyword.
	correctSelector ifNil: [^abortAction value].

	requestor selectInvisiblyFrom: userSelection first to: userSelection last.

	self substituteSelector: correctSelector keywords wordIntervals: spots.
	^(proposedKeyword last ~~ $:
	   and: [correctSelector last == $:])
		ifTrue: [abortAction value]
		ifFalse: [correctSelector]! !

!ZParser methodsFor: 'error correction' stamp: 'jmv 5/22/2016 11:07'!
correctVariable: proposedVariable interval: spot
	"Correct the proposedVariable to a known variable, or declare it as a new
	variable if such action is requested.  We support declaring lowercase
	variables as temps or inst-vars, and uppercase variables as Globals or 
	ClassVars, depending on whether the context is nil (class=UndefinedObject).
	Spot is the interval within the test stream of the variable.
	rr 3/4/2004 10:26 : adds the option to define a new class. "

	"Check if this is an i-var, that has been corrected already (ugly)"

	"Display the pop-up menu"

	| userSelection action |
	(encoder classEncoding instVarNames includes: proposedVariable) ifTrue: [
		^ZInstanceVariableNode new 
			name: proposedVariable
			index: (encoder classEncoding allInstVarNames indexOf: proposedVariable)].

	"First check to see if the requestor knows anything about the variable"
	(requestor bindingOf: proposedVariable)
		ifNotNil: [ :binding | ^ encoder global: binding name: proposedVariable].

	"If we can't ask the user for correction, make it undeclared"
	self interactive ifFalse: [^encoder undeclared: proposedVariable].

	userSelection _ requestor selectionInterval.
	requestor selectFrom: spot first to: spot last.

	"Build the menu with alternatives"
	action _ ZUndeclaredVariable 
				signalFor: self
				name: proposedVariable
				inRange: spot.
	action ifNil: [^self fail].

	"Execute the selected action"
	requestor selectInvisiblyFrom: userSelection first to: userSelection last.
	^action value! !

!ZParser methodsFor: 'error correction' stamp: 'jmv 5/22/2016 10:55'!
declareInstVar: name
	"Declare an instance variable.  Since the variable will get added after any existing
	 inst vars its index is the instSize."
	encoder classEncoding addInstVarName: name.
	"Not needed in Cuis"
	"Smalltalk logChange: encoder classEncoding definition."
	^ZInstanceVariableNode new name: name index: encoder classEncoding instSize
		! !

!ZParser methodsFor: 'error correction' stamp: 'jmv 5/22/2016 11:05'!
declareUndeclaredTemps: methodNode
	"Declare any undeclared temps, declaring them at the smallest enclosing scope."
	| undeclared userSelection blocksToVars |
	(undeclared _ encoder undeclaredTemps) isEmpty ifTrue: [ ^ self ].
	userSelection _ requestor selectionInterval.
	blocksToVars _ IdentityDictionary new.
	undeclared do: [ :var |
		(blocksToVars
			at: (var tag == #method
				ifTrue: [ methodNode block ]
				ifFalse: [ methodNode accept: (ZVariableScopeFinder new ofVariable: var) ])
			ifAbsentPut: [ SortedCollection new ]) add: var name ].
	(blocksToVars removeKey: methodNode block ifAbsent: nil) ifNotNil: [ :rootVars |
		rootVars do: [ :varName |
			self pasteTempAtMethodLevel: varName ]].
	(blocksToVars keys sort: [ :a :b |
		a tempsMark < b tempsMark ]) do: [ :block | | decl |
		decl _ String streamContents: [ :strm |
			(blocksToVars at: block) do: [ :v |
				strm nextPutAll: v; nextPut: $  ]].
		block temporaries isEmpty
			ifTrue: [
				self
					substituteWord: ' | ' , decl , '|'
					wordInterval: (block tempsMark + 1 to: block tempsMark)
					offset: requestorOffset ]
			ifFalse: [
				self
					substituteWord: decl
					wordInterval: (block tempsMark to: block tempsMark - 1)
					offset: requestorOffset ]].
	requestor
		selectInvisiblyFrom: userSelection first
		to: userSelection last + requestorOffset.
	ZReparseAfterSourceEditing signal! !

!ZParser methodsFor: 'error correction' stamp: 'jmv 5/22/2016 11:12'!
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
	ZCompiler evaluate: d2.
	^ encoder
		global: (Smalltalk associationAt: sym)
		name: sym! !

!ZParser methodsFor: 'error correction' stamp: 'jmv 5/22/2016 11:07'!
queryUndefined
	| varStart varName | 
	varName := parseNode key.
	varStart := self endOfLastToken + requestorOffset - varName size + 1.
	requestor selectFrom: varStart to: varStart + varName size - 1.
	(ZUndefinedVariable name: varName) ifFalse: [^ self fail]! !

!ZParser methodsFor: 'error correction' stamp: 'jmv 5/22/2016 11:07'!
removeUnusedTemps
	"Scan for unused temp names, and prompt the user about the prospect of removing each one found"

	| str madeChanges | 
	madeChanges := false.
	str := requestor text asString.
	((tempsMark between: 1 and: str size)
		and: [(str at: tempsMark) = $|]) ifFalse: [^ self].
	encoder unusedTempNames do:
		[:temp | | start end |
		(ZUnusedVariable name: temp) ifTrue:
			[(encoder lookupVariable: temp ifAbsent: []) isUndefTemp
				ifTrue:
					[end := tempsMark.
					["Beginning at right temp marker..."
					start := end - temp size + 1.
					end < temp size or: [temp = (str copyFrom: start to: end)
							"In Cuis, we chose to use 'tokenish not' and not 'isSeparator'"
							and: [(str at: start-1) tokenish not & (str at: end+1) tokenish not]]]
						whileFalse:
							["Search left for the unused temp"
							end := requestor nextTokenFrom: end direction: -1].
					end < temp size ifFalse:
						[(str at: start-1) = $  ifTrue: [start := start-1].
						requestor correctFrom: start to: end with: ''.
						str := str copyReplaceFrom: start to: end with: ''. 
						madeChanges := true.
						tempsMark := tempsMark - (end-start+1)]]
				ifFalse:
					[self inform:
'You''ll first have to remove the\statement where it''s stored into' withNewLines]]].
	madeChanges ifTrue: [ZReparseAfterSourceEditing signal]! !

!ZParser methodsFor: 'public access' stamp: 'jmv 5/22/2016 10:52'!
encoder
	encoder ifNil:
		[encoder := ZEncoderForV3PlusClosures new].
	^encoder! !

!ZParser methodsFor: 'public access' stamp: 'jmv 5/22/2016 11:05'!
parse: sourceStream class: class category: aCategory noPattern: noPattern context: ctxt notifying: req ifFail: aBlock
	"Answer a MethodNode for the argument, sourceStream, that is the root of
	 a parse tree. Parsing is done with respect to the argument, class, to find
	 instance, class, and pool variables; and with respect to the argument,
	 ctxt, to find temporary variables. Errors in parsing are reported to the
	 argument, req, if not nil; otherwise aBlock is evaluated. The argument
	 noPattern is a Boolean that is true if the the sourceStream does not
	 contain a method header (i.e., for DoIts)."
	| methNode repeatNeeded myStream s p |
	category _ aCategory.
	myStream _ sourceStream.
	[
		repeatNeeded _ false.
		p _ myStream position.
		s _ myStream upToEnd.
		myStream position: p.
		self encoder init: class context: ctxt notifying: self.
		self init: myStream notifying: req failBlock: [
			^ aBlock value ].
		doitFlag _ noPattern.
		failBlock _ aBlock.
		[ methNode _ self method: noPattern context: ctxt ]
			on: ZReparseAfterSourceEditing
			do: [ :ex |
				repeatNeeded _ true.
				myStream _ sourceStreamGetter notNil 	"Cuis specific. Do not remove!!"
					ifTrue: [ requestor perform: sourceStreamGetter ]
					ifFalse: [ ReadStream on: requestor text string ]].
		repeatNeeded
	] whileTrue: [
		encoder _ self encoder class new ].
	methNode sourceText: s.
	^ methNode! !

