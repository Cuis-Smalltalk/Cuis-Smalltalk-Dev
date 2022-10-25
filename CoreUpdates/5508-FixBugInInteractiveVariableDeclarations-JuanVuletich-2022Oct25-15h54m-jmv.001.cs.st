'From Cuis 6.0 [latest update: #5507] on 25 October 2022 at 3:57:08 pm'!

!CodeProvider class methodsFor: 'services' stamp: 'jmv 10/25/2022 15:50:25'!
normalizeCode: aTextOrString
	"Adapt edited code to be suitable for compilation or logging.
	Answer is a plain text.
	Additionally, if code was modified for better look and edition experience, normalize it.
	This may include reverting to ':=' and/or '_' for assignment, and other replacements of
	ASCII by Unicode symbols done by Shout."

	| answer |
	answer _ aTextOrString asPlainString.
	answer _ answer copyReplaceAll: TextModel arrowStringForAssignment with: '_'.
	answer _ answer copyReplaceAll: TextModel arrowStringForReturn with: '^'.
	^answer! !


!Browser methodsFor: 'accessing' stamp: 'jmv 10/25/2022 15:50:39'!
contents: input notifying: aRequestor
	"The retrieved information has changed and its source must now be
	 updated. The information can be a variety of things, depending on
	 the list selections (such as templates for class or message definition,
	 methods) or the user menu commands (such as definition, comment).
	Answer the result of updating the source."

	| codeString theClass |
	codeString _ CodeProvider normalizeCode: input.
	editSelection == #editSystemCategories ifTrue: [
		^ self changeSystemCategories: codeString ].
	self isEditingClass ifTrue: [
		[
			self defineClass: codeString notifying: aRequestor 
		] on: RecompilationFailure do: [ :ex |
			self inform: ex messageText.
			^ false].
		^ true].
	editSelection == #editComment
		ifTrue: [
			theClass _ self selectedClass.
			theClass
				ifNil: [
					self inform: 'You must select a class
before giving it a comment.'.
					^ false].
			theClass comment: codeString stamp: Utilities changeStamp.
			self changed: #classCommentText.
			^ true].
	editSelection == #editMessageCategories ifTrue: [ ^ self changeMessageCategories: codeString ].
	editSelection == #editMessage | (editSelection == #newMessage)
		ifTrue: [
			^ self okayToAccept
				ifFalse:[ false ]
				ifTrue: [
					(self compileMessage: codeString notifying: aRequestor)
						ifTrue: [ self triggerEvent: #annotationChanged ];
						yourself ]].
	editSelection == #none
		ifTrue: [
			self inform: 'This text cannot be accepted
in this part of the browser.'.
			^ false].
	self error: 'unacceptable accept'! !


!MessageSet methodsFor: 'accessing' stamp: 'jmv 10/25/2022 15:50:43'!
contents: input notifying: aRequestor 
	"Compile the code in aString. Notify aRequestor of any syntax errors. 
	Answer false if the compilation fails. Otherwise, if the compilation 
	created a new method, deselect the current selection. Then answer true."

	| codeString category selector class oldSelector |
	codeString _ CodeProvider normalizeCode: input.
	self okayToAccept ifFalse: [^ false].
	selectedMessage ifNil: [^ false].
	class _ selectedMessage actualClass.
	oldSelector _ selectedMessage methodSymbol.
	(oldSelector notNil and: [oldSelector first isUppercase]) ifTrue: [
		oldSelector == #Comment ifTrue: [
			class comment: codeString stamp: Utilities changeStamp.
			self triggerEvent: #annotationChanged.
 			self changed: #clearUserEdits.
			^ false].
		oldSelector == #Definition ifTrue: [
			Compiler
				evaluate: codeString
				notifying: aRequestor
				logged: true.
			self changed: #clearUserEdits.
 			^ false]].
	"Normal method accept"
	category _ class organization categoryOfElement: oldSelector.
	selector _ class compile: codeString
				classified: category
				notifying: aRequestor.
	selector
		ifNil: [^ false].
	selector == oldSelector ifFalse: [
		self reformulateListNoting: selector].
	self triggerEvent: #annotationChanged.
	^ true! !


!CodeFileBrowser methodsFor: 'accessing' stamp: 'jmv 10/25/2022 15:50:47'!
contents: input notifying: aRequestor 
	"The retrieved information has changed and its source must now be 
	updated. The information can be a variety of things, depending on the 
	list selections (such as templates for class or message definition, methods) 
	or the user menu commands (such as definition, comment, hierarchy). 
	Answer the result of updating the source."

	| codeString theClass |
	codeString _ CodeProvider normalizeCode: input.

	editSelection == #editComment 
		ifTrue: [
			theClass _ self selectedClass.
			theClass ifNil: [
				self inform: 'You must select a class
before giving it a comment.'.
				^ false ].
			theClass comment: codeString. 
			^ true ].
	editSelection == #editMessageCategories 
		ifTrue: [^ self changeMessageCategories: codeString].

	self inform:'You cannot change the current selection'.
	^false! !


!ChangeSorter methodsFor: 'code pane' stamp: 'jmv 10/25/2022 15:50:50'!
contents: input notifying: aRequestor 
	"Compile the code in aString. Notify aRequestor of any syntax errors. 
	Create an error if the category of the selected message is unknown. 
	Answer false if the compilation fails. Otherwise, if the compilation 
	created a new method, deselect the current selection. Then answer true."

	| codeString category selector class oldSelector |
	codeString _ CodeProvider normalizeCode: input.
	(class _ self selectedClassOrMetaClass) ifNil:
		[(myChangeSet preambleString == nil or: [codeString isEmpty]) ifTrue: [ ^ false].
		(codeString count: [:char | char = $"]) odd 
			ifTrue: [self inform: 'unmatched double quotes in preamble']
			ifFalse: [(Scanner new scanTokens: codeString) notEmpty ifTrue: [
				self inform: 'Part of the preamble is not within double-quotes.
To put a double-quote inside a comment, type two double-quotes in a row.
(Ignore this warning if you are including a doIt in the preamble.)']].
		myChangeSet preambleString: codeString.
		self currentSelector: nil.  "forces update with no 'unsubmitted chgs' feedback"
		^ true].
	oldSelector _ self selectedMessageName.
	category _ class organization categoryOfElement: oldSelector.
	selector _ class compile: codeString
				classified: category
				notifying: aRequestor.
	selector ifNil: [^ false].
	(self messageList includes: selector)
		ifTrue: [self currentSelector: selector]
		ifFalse: [self currentSelector: oldSelector].
	self update.
	^ true! !


!Debugger methodsFor: 'accessing' stamp: 'jmv 10/25/2022 15:50:53'!
contents: input notifying: aController
	"The retrieved information has changed and its source must now be updated.
	 In this case, the retrieved information is the method of the selected context."

	| codeString result selector classOfMethod category h newMethod |
	codeString _ CodeProvider normalizeCode: input.

	contextStackIndex = 0 ifTrue: [^false].

	classOfMethod := self selectedClass.
	category := self selectedMessageCategoryName.
	selector :=self selectedClass parserClass selectorFrom: codeString.

	selector ~= self selectedMessageName ifTrue: [
		self inform: 'Can not change the selector in the debugger'.
		^false].	
 	(classOfMethod = UndefinedObject and: [ selector = Scanner doItSelector or: [ selector = Scanner doItInSelector ]]) ifTrue: [
		self inform: 'DoIt and DoItIn: methods can not be changed'.
	 	^false].

	self selectedContext isExecutingBlock ifTrue: [
		h := self selectedContext activeHome.
		h ifNil: [
			self inform: 'Method for block not found on stack, can''t edit and continue'.
			^false].
		(self confirm: 'I will have to revert to the method from\which this block originated.  Is that OK?' withNewLines) ifFalse: [^false].
		self resetContext: h.
		(result := self contents: codeString notifying: aController) ifTrue: [self acceptedContentsChanged].
		^result].
		
	selector := classOfMethod
				compile: codeString
				classified: category
				notifying: aController.
	selector ifNil: [^false]. "compile cancelled"
	newMethod := classOfMethod compiledMethodAt: selector.

	newMethod isQuick ifTrue: [
		contextStackIndex + 1 > contextStack size ifTrue: [
			self inform: 'Can not compile a quick method in the stack base context'.
			^false].
		self down.
		self selectedContext jump: (self selectedContext previousPc - self selectedContext pc)].

	self resetToSelectedContextWith: newMethod.
	
	^true! !


!Behavior methodsFor: 'compiling' stamp: 'jmv 10/25/2022 15:56:23'!
compile: code notifying: requestor 
	"Compile the argument, code, as source code in the context of the 
	receiver and install the result in the receiver's method dictionary. The 
	second argument, requestor, is to be notified if an error occurs. The 
	argument code is either a string or an object that converts to a string or 
	a PositionableStream. This method also saves the source code."
	
	| methodAndNode updatedCode |
	methodAndNode _ self
		basicCompile: code
		notifying: requestor
		trailer: self defaultMethodTrailer
		ifFail: [^nil].
	"Re-fetching source code may be needed if, for example, some variable declaration was corrected during compilation."
	updatedCode _ requestor ifNil: [code] ifNotNil: [CodeProvider normalizeCode: requestor text].
	methodAndNode method putSource: updatedCode fromParseNode: methodAndNode node inFile: 2
			withPreamble: [ :f | f newLine; nextPut: $!!; nextChunkPut: 'Behavior method'; newLine].
	self addSelectorSilently: methodAndNode selector withMethod: methodAndNode method.
	^ methodAndNode selector! !


!ClassDescription methodsFor: 'compiling' stamp: 'jmv 10/25/2022 15:56:33'!
compile: code classified: category withStamp: changeStamp notifying: requestor logSource: logSource
	
	| methodAndNode selector isExistingMethod updatedCode |	
	methodAndNode _ self basicCompile: code notifying: requestor 
							trailer: self defaultMethodTrailer ifFail: [^nil].
	selector _ methodAndNode selector.
	isExistingMethod _ self includesSelector: selector.
	isExistingMethod
		ifTrue: [
			(self theNonMetaClass isOkToChangeMethod: selector isMeta: self isMeta)
				ifFalse: [self error: 'Method modification not allowed']]
		ifFalse: [
			(self theNonMetaClass isOkToAddMethod: selector isMeta: self isMeta)
				ifFalse: [self error: 'Method addition not allowed']].
	logSource ifTrue: [
		"Re-fetching source code may be needed if, for example, some variable declaration was corrected during compilation."
		updatedCode _ requestor ifNil: [code] ifNotNil: [CodeProvider normalizeCode: requestor text].
		self logMethodSource: updatedCode forMethodWithNode: methodAndNode 
			inCategory: category withStamp: changeStamp notifying: requestor.
	].
	self addAndClassifySelector: selector withMethod: methodAndNode 
		method inProtocol: category notifying: requestor.
	^ methodAndNode selector! !

!methodRemoval: CodeProvider #normalizeCode: stamp: 'jmv 10/25/2022 15:55:32'!
CodeProvider removeSelector: #normalizeCode:!
