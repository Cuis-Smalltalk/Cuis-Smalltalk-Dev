'From Cuis 4.2 of 25 July 2013 [latest update: #2157] on 20 January 2015 at 11:43:30.266634 am'!

!Switch commentStamp: '<historical>' prior: 0!
I represent a selection setting and actions to take depending on a change in the setting. An instance has three attributes: state, which is either on or off; on action; and off action. The on and off actions are blocks of code that execute whenever the instance changes state.!


!ChangeList commentStamp: '<historical>' prior: 0!
A ChangeList represents a list of changed methods that reside on a file in fileOut format.  The classes and methods in my list are not necessarily in this image!!  Used as the model for both Version Lists and Changed Methods.

It holds three lists:
	changeList - a list of ChangeRecords
	list - a list of one-line printable headers
	listSelections - a list of Booleans (true = selected, false = not selected) multiple OK.
	listIndex 
Items that are removed (removeDoits, remove an item) are removed from all three lists.
Most recently clicked item is the one showing in the bottom pane.!


!Compiler commentStamp: '<historical>' prior: 0!
The compiler accepts Smalltalk source code and compiles it with respect to a given class. The user of the compiler supplies a context so that temporary variables are accessible during compilation. If there is an error, a requestor is sent the message notify:at:in: so that the error message can be displayed. If there is no error, then the result of compilation is a MethodNode, which is the root of a parse tree whose nodes are kinds of ParseNodes. The parse tree can be sent messages to (1) generate code for a CompiledMethod (this is done for compiling methods or evaluating expressions); (2) pretty-print the code (for formatting); or (3) produce a map from object code back to source code (used by debugger program-counter selection). See also Parser, Encoder, ParseNode.!


!Browser methodsFor: 'accessing' stamp: 'jmv 1/20/2015 11:39'!
contents: input notifying: aRequestor
	"The retrieved information has changed and its source must now be
	 updated. The information can be a variety of things, depending on
	 the list selections (such as templates for class or message definition,
	 methods) or the user menu commands (such as definition, comment,
	 hierarchy).  Answer the result of updating the source."

	| aString aText theClass |
	aString _ input asString.
	aText _ input asText.
	editSelection == #editSystemCategories ifTrue: [ ^ self changeSystemCategories: aString ].
	editSelection == #editClass | (editSelection == #newClass) ifTrue: [ ^ self defineClass: aString notifying: aRequestor ].
	editSelection == #editComment
		ifTrue: [
			theClass _ self selectedClass.
			theClass
				ifNil: [
					self inform: 'You must select a class
before giving it a comment.'.
					^ false].
			theClass comment: aText stamp: Utilities changeStamp.
			self changed: #classCommentText.
			^ true].
	editSelection == #hierarchy ifTrue: [ ^ true ].
	editSelection == #editMessageCategories ifTrue: [ ^ self changeMessageCategories: aString ].
	editSelection == #editMessage | (editSelection == #newMessage)
		ifTrue: [
			^ self okayToAccept
				ifFalse:[ false ]
				ifTrue: [
					(self compileMessage: aText notifying: aRequestor)
						ifTrue: [ self triggerEvent: #annotationChanged ];
						yourself ]].
	editSelection == #none
		ifTrue: [
			self inform: 'This text cannot be accepted
in this part of the browser.'.
			^ false].
	self error: 'unacceptable accept'! !

!Browser methodsFor: 'class functions' stamp: 'jmv 1/20/2015 11:36'!
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
			(self confirm: ((newClassName , ' is an existing class in this system.
Redefining it might cause serious problems.
Is this really what you want to do?') asText makeBoldFrom: 1 to: newClassName size))
				ifFalse: [ ^ false ]].
	"ar 8/29/1999: Use oldClass superclass for defining oldClass
	since oldClass superclass knows the definerClass of oldClass."
	oldClass ifNotNil: [ oldClass _ oldClass superclass ].
	class _ Compiler
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

!Browser methodsFor: 'code pane' stamp: 'jmv 1/20/2015 11:40'!
compileMessage: aText notifying: aRequestor
	"Compile the code that was accepted by the user, placing the compiled method into an appropriate message category.  Return true if the compilation succeeded, else false."

	| fallBackCategory fallBackMessage originalSelectorName result |

	self selectedMessageCategoryName ifNil: [
		self selectOriginalCategoryForCurrentMethod 	
			ifFalse: [ "Select the '--all--' category"
				self messageCategoryListIndex: 1]]. 


	self selectedMessageCategoryName asSymbol == ClassOrganizer allCategory
		ifTrue: [
			"User tried to save a method while the ALL category was selected"
			fallBackCategory _ selectedMessageCategory.
			fallBackMessage _ selectedMessage.
			editSelection == #newMessage
				ifTrue: [
					"Select the 'as yet unclassified' category"
					selectedMessageCategory _ nil.
					(result _ self defineMessageFrom: aText notifying: aRequestor)
						ifNil: [
							"Compilation failure:  reselect the original category & method"
							selectedMessageCategory _ fallBackCategory.
							selectedMessage _ fallBackMessage ]
						ifNotNil: [
							self setSelector: result]]
				ifFalse: [
					originalSelectorName _ self selectedMessageName.
					self setOriginalCategoryIndexForCurrentMethod.
					selectedMessage _ fallBackMessage _ originalSelectorName.
					(result _ self defineMessageFrom: aText notifying: aRequestor)
						ifNotNil: [
							self setSelector: result]
						ifNil: [
							"Compilation failure:  reselect the original category & method"
							selectedMessageCategory _ fallBackCategory.
							selectedMessage _ fallBackMessage.
							^ result notNil]].
			self changed: #messageCategoryList.
			^ result notNil]
		ifFalse: [
			"User tried to save a method while the ALL category was NOT selected"
			^ (self defineMessageFrom: aText notifying: aRequestor) notNil ]! !

!Browser methodsFor: 'message functions' stamp: 'jmv 1/20/2015 11:41'!
defineMessageFrom: aString notifying: aRequestor
	"Compile the expressions in aString. Notify aRequestor if a syntax error occurs. Install the compiled method in the selected class classified under  the currently selected message category name. Answer the selector obtained if compilation succeeds, nil otherwise."
	| selectedMessageName selector category oldMessageList |
	selectedMessageName _ self selectedMessageName.
	oldMessageList _ self messageList.
	selector _ Parser selectorFrom: aString.
	(self metaClassIndicated
		and: [(self selectedClassOrMetaClass includesSelector: selector) not
		and: [Metaclass isScarySelector: selector]])
		ifTrue: ["A frist-time definition overlaps the protocol of Metaclasses"
				(self confirm: ((selector , ' is used in the existing class system.
Overriding it could cause serious problems.
Is this really what you want to do?') asText makeBoldFrom: 1 to: selector size))
				ifFalse: [^nil]].
	selector _ self selectedClassOrMetaClass
				compile: aString
				classified: (category _ self selectedMessageCategoryName)
				notifying: aRequestor.
	selector
		ifNil: [^ nil].
	selector ~~ selectedMessageName
		ifTrue: [
			category = ClassOrganizer nullCategory
				ifTrue: [self changed: #classSelectionChanged.
						self changed: #classList.
						self messageCategoryListIndex: 1].
			self setClassOrganizer.  "In case organization not cached"
			(oldMessageList includes: selector)
				ifFalse: [self changed: #messageList].
			self messageListIndex: (self messageList indexOf: selector)].
	^ selector! !


!CodeFileBrowser methodsFor: 'accessing' stamp: 'jmv 1/20/2015 11:39'!
contents: input notifying: aRequestor 
	"The retrieved information has changed and its source must now be 
	updated. The information can be a variety of things, depending on the 
	list selections (such as templates for class or message definition, methods) 
	or the user menu commands (such as definition, comment, hierarchy). 
	Answer the result of updating the source."

	| aString aText theClass |
	aString _ input asString.
	aText _ input asText.

	editSelection == #editComment 
		ifTrue: [theClass _ self selectedClass.
				theClass ifNil: [self inform: 'You must select a class
before giving it a comment.'.
				^ false].
				theClass comment: aText. ^ true].
	editSelection == #editMessageCategories 
		ifTrue: [^ self changeMessageCategories: aString].

	self inform:'You cannot change the current selection'.
	^false
! !


!MessageSet methodsFor: 'private' stamp: 'jmv 1/20/2015 11:41'!
contents: aString notifying: aRequestor 
	"Compile the code in aString. Notify aRequestor of any syntax errors. 
	Answer false if the compilation fails. Otherwise, if the compilation 
	created a new method, deselect the current selection. Then answer true."

	| category selector class oldSelector |
	self okayToAccept ifFalse: [^ false].
	self setClassAndSelectorIn: [:c :os | class _ c.  oldSelector _ os].
	class ifNil: [^ false].
	(oldSelector notNil and: [oldSelector first isUppercase]) ifTrue:
		[oldSelector == #Comment ifTrue:
			[class comment: aString stamp: Utilities changeStamp.
			self triggerEvent: #annotationChanged.
 			self changed: #clearUserEdits.
			^ false].
		oldSelector == #Definition ifTrue: [
			Compiler
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


!ChangeSorter methodsFor: 'code pane' stamp: 'jmv 1/20/2015 11:41'!
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
			ifFalse: [(Smalltalk actualScannerClass new scanTokens: aString) size > 0 ifTrue: [
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


!Debugger methodsFor: 'accessing' stamp: 'jmv 1/20/2015 11:39'!
contents: aText notifying: aRequestor
	"The retrieved information has changed and its source must now be updated.
	 In this case, the retrieved information is the method of the selected context."
	| result selector classOfMethod category h ctxt newMethod |
	contextStackIndex = 0 ifTrue:
		[^false].
	self selectedContext isExecutingBlock ifTrue:
		[h := self selectedContext activeHome.
		 h ifNil:
			[self inform: 'Method for block not found on stack, can''t edit and continue'.
			 ^false].
		 (self confirm: 'I will have to revert to the method from\which this block originated.  Is that OK?' withNewLines) ifFalse:
			[^false].
		self resetContext: h.
		(result := self contents: aText notifying: aRequestor) ifTrue: [
			self acceptedContentsChanged].
		^result].

	classOfMethod := self selectedClass.
	category := self selectedMessageCategoryName.
	selector :=Parser selectorFrom: aText.
	(selector == self selectedMessageName
	 or: [(self selectedMessageName beginsWith: 'DoIt')
		and: [selector numArgs = self selectedMessageName numArgs]]) ifFalse:
		[self inform: 'can''t change selector'.
		 ^false].
	selector := classOfMethod
				compile: aText
				classified: category
				notifying: aRequestor.
	selector ifNil: [^false]. "compile cancelled"
	newMethod := classOfMethod compiledMethodAt: selector.
	newMethod isQuick ifTrue:
		[self down.
		 self selectedContext jump: (self selectedContext previousPc - self selectedContext pc)].
	ctxt := interruptedProcess popTo: self selectedContext.
	ctxt == self selectedContext
		ifFalse:
			[self inform: 'Method saved, but current context unchanged\because of unwind error. Click OK to see error' withNewLines]
		ifTrue:
			[newMethod isQuick ifFalse:
				[interruptedProcess
					restartTopWith: newMethod;
				 	stepToSendOrReturn].
			contextVariablesInspector object: nil].
	self resetContext: ctxt.
	^true! !

!Debugger methodsFor: 'context stack menu' stamp: 'jmv 1/20/2015 11:34'!
restart
	"Proceed from the initial state of the currently selected context."

	| ctxt |
	self checkContextSelection.
	ctxt _ interruptedProcess popTo: self selectedContext.
	ctxt == self selectedContext ifTrue: [
		interruptedProcess restartTop; stepToSendOrReturn].
	self resetContext: ctxt! !


!Compiler class methodsFor: 'evaluating' stamp: 'jmv 1/20/2015 11:36'!
evaluate: textOrString for: anObject notifying: aRequestor logged: logFlag
	"Compile and execute the argument, textOrString with respect to the class 
	of anObject. If a compilation error occurs, notify aRequestor. If both 
	compilation and execution are successful then, if logFlag is true, log 
	(write) the text onto a system changes file so that it can be replayed if 
	necessary."

	^ self new
				evaluate: textOrString
				in: nil
				to: anObject
				notifying: aRequestor
				ifFail: [^nil]
				logged: logFlag.! !

!Compiler class methodsFor: 'evaluating' stamp: 'jmv 1/20/2015 11:34'!
evaluate: textOrString notifying: aRequestor logged: logFlag 
	"See Compiler|evaluate:for:notifying:logged:. Compilation is carried out 
	with respect to nil, i.e., no object."

	^self evaluate: textOrString for: nil notifying: aRequestor logged: logFlag! !


!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 1/20/2015 11:35'!
fileItIn
	"Make a Stream on the text selection and fileIn it."

	| selection |
	selection _ self selection.
	(ReadWriteStream on: selection string from: 1 to: selection size) fileIn! !


!CodeFileBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 1/20/2015 11:33'!
buildMorphicWindow
	"Create a pluggable version of all the views for a Browser, using Morphic widgets."

	| list1 list2 upperPanes |
	model systemCategoryListIndex: 1.
	list1 _ PluggableListMorph
			model: model 
			listGetter: #systemCategorySingleton
			indexGetter: #indexIsOne 
			indexSetter: #indexIsOne:
			mainView: self
			menuGetter: #codeFileListMenu
			keystrokeAction: #codeFileListKey:from:.
	list1 hideScrollBarsIndefinitely.
	
	list2 _ PluggableListMorph
			model: model 
			listGetter: #messageCategoryList
			indexGetter: #messageCategoryListIndex 
			indexSetter: #messageCategoryListIndex:
			mainView: self
			menuGetter: #messageCategoryMenu
			keystrokeAction: nil.

	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: self buildMorphicClassColumn proportionalWidth: 0.3;
		addAdjusterAndMorph: list2 proportionalWidth: 0.3;
		addAdjusterAndMorph: self buildMorphicMessageList proportionalWidth: 0.4.

	self layoutMorph
		addMorph: list1 fixedHeight: Preferences standardCodeFont height + 10;
		addAdjusterAndMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.
	model changed: #editSelection! !


!Encoder methodsFor: 'error handling' stamp: 'jmv 1/20/2015 11:35'!
requestor: aRequestor

	requestor _ aRequestor! !

