'From Cuis7.9 [latest update: #8130] on 5 August 2026 at 11:06:23 am'!

!CompiledMethod methodsFor: 'accessing-pragmas & properties' stamp: 'jmv 5/Aug/2026, 1:33:00 pm (UTC)'!
methodProperties: anArrayOfAssociations
	| pragmas |
	pragmas := self pragmas.
	self removeProperties.
	pragmas do: [ :each | self addPragmaOrProperty: each ].
	anArrayOfAssociations do: [ :each | self addPragmaOrProperty: each ].
! !


!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 5/Aug/2026, 1:59:14 pm (UTC)'!
methodChanged: method
	self
		methodChangedFrom: nil
		to: method
		selector: method selector
		inClass: method methodClass.
! !


!CodeProvider methodsFor: 'what to show' stamp: 'jmv 3/Aug/2026, 6:35:17 pm (UTC)'!
showProperties
	self contentsSymbol: #methodProperties! !

!CodeProvider methodsFor: 'what to show' stamp: 'jmv 3/Aug/2026, 6:32:48 pm (UTC)'!
showingProperties
	"Answer whether the receiver is currently showing source code"

	^ self contentsSymbol == #methodProperties
! !

!CodeProvider methodsFor: 'what to show' stamp: 'jmv 5/Aug/2026, 12:48:15 pm (UTC)'!
showingPropertiesString
	"Answer a string telling whether the receiver is showing plain source"

	^ self showingProperties asMenuItemTextPrefix,
		'properties'! !


!CodeProvider methodsFor: 'annotation parts' stamp: 'jmv 5/Aug/2026, 1:14:54 pm (UTC)'!
propertiesOf: aClass selector: aSelector printOn: strm
	"Print the #properties method annotation."

	(aClass compiledMethodAt: aSelector ifAbsent: nil) ifNotNil: [ :method |
		method propertiesAsArray ifNotEmpty: [ :it |
			strm atStart ifFalse: [ strm nextPutAll: self annotationSeparator ].
			it storePropertiesOn: strm ]].
! !

!CodeProvider methodsFor: 'controls' stamp: 'jmv 3/Aug/2026, 6:34:48 pm (UTC)'!
whatToShowSubmenuSpec
	" 'What to Show' SubMenu"
	^{
		{3010. 	'-----'}.
		{3020.	 self showingSourceString. 			nil. 	#codeProvider. 	#showSource. 				nil. 'the textual source code as written'}.
		{3021.	 self showingPropertiesString. 			nil. 	#codeProvider. 	#showProperties. 			nil. 'the method properties dictionary'}.
		{3030. 	'-----'}.
		{3040.	self showingPrettyPrintString. 			nil. 	#codeProvider. 	#showPrettyPrint. 			nil. 'the method source presented in a standard text format'}.
		{3050. 	'-----'}.
		{3060.	self showingLineDiffsString. 			nil. 	#codeProvider. 	#showLineDiffs. 			nil. 'the textual source lines diffed from its prior version'}.
		{3070.	self showingWordDiffsString. 			nil. 	#codeProvider. 	#showWordDiffs. 			nil. 'the textual source words diffed from its prior version'}.
		{3080.	self showingPrettyLineDiffsString. 		nil. 	#codeProvider. 	#showPrettyLineDiffs. 		nil. 'formatted source lines diffed from formatted prior version'}.
		{3090.	self showingPrettyWordDiffsString. 	nil. 	#codeProvider. 	#showPrettyWordDiffs. 	nil. 'formatted source words diffed from formatted prior version'}.
		{3100. 	'-----'}.
		{3110.	self showingDecompileString. 			nil. 	#codeProvider. 	#showDecompile. 			nil. 'source code decompiled from byteCodes'}.
		{3120.	self showingByteCodesString. 			nil. 	#codeProvider. 	#showByteCodes. 			nil. 'the bytecodes that comprise the compiled method'}.
	}.
! !


!Browser methodsFor: 'accessing' stamp: 'jmv 5/Aug/2026, 2:04:13 pm (UTC)'!
contents: input notifying: aRequestor
	"The retrieved information has changed and its source must now be
	 updated. The information can be a variety of things, depending on
	 the list selections (such as templates for class or message definition,
	 methods) or the user menu commands (such as definition, comment).
	Answer the result of updating the source."

	| codeString theClass |
	codeString := input string.
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
			theClass := self selectedClass.
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
			self okayToAccept
				ifFalse: [ ^false ]
				ifTrue: [
					"Accept Method Properties"
					self showingProperties ifTrue: [
						self selectedClassOrMetaClass compiledMethodAt: self selectedMessageName ifPresent: [ :method |
							"Keep any Pragmas, but use only newly specified Properties"
							(Compiler evaluate: codeString) ifNotNil: [ :props | 
								props isArray ifTrue: [
									method methodProperties: props.
									SystemChangeNotifier uniqueInstance methodChanged: method.
									self triggerEvent: #annotationChanged.
									^ true ]]] ifAbsent: nil.
						^false ].
					"Accept Method Source"
					^ (self compileMessage: codeString notifying: aRequestor)
						ifTrue: [ self triggerEvent: #annotationChanged ];
						yourself ]].
	editSelection == #none
		ifTrue: [
			self inform: 'This text cannot be accepted
in this part of the browser.'.
			^ false].
	self error: 'unacceptable accept'.
! !

!Browser methodsFor: 'message list' stamp: 'jmv 5/Aug/2026, 2:04:19 pm (UTC)'!
selectedMessage
	"Answer a copy of the source code for the selected message."

	| class selector method |

	self showingDecompile ifTrue: [
		^ self decompiledSource ].

	class := self selectedClassOrMetaClass.
	selector := self selectedMessageName.
	method := class compiledMethodAt: selector ifAbsent: [^ ''].	"method deleted while in another project"
	currentCompiledMethod := method.

	self showingProperties ifTrue: [
		^String streamContents: [ :strm |
			strm
				nextPutAll: '"The following Smalltalk expression evaluates to the Properties for this method'; newLine;
				nextPutAll: 'It must have the form of an Array of associations. For instance something like:'; newLine;
				nextPutAll: '{#propertyName1 -> 3.141592. #propertyName2 -> #stuff. #propertyName3 -> #(#toto #toton #totona)}'; newLine;
				nextPutAll: 'You can edit it and save as needed"'; newLine.
			currentCompiledMethod propertiesAsArray storePropertiesOn: strm ]].
	
	^ self sourceStringPrettifiedAndDiffed! !


!MethodSet methodsFor: 'accessing' stamp: 'jmv 5/Aug/2026, 1:59:44 pm (UTC)'!
contents: input notifying: aRequestor 
	"Compile the code in aString. Notify aRequestor of any syntax errors. 
	Answer false if the compilation fails. Otherwise, if the compilation 
	created a new method, deselect the current selection. Then answer true."

	| codeString category selector class oldSelector |
	codeString := input string.
	self okayToAccept ifFalse: [^ false].
	selectedMessage ifNil: [^ false].
	class := selectedMessage actualClass.
	oldSelector := selectedMessage methodSymbol.
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

	"Accept Method Properties"
	self showingProperties ifTrue: [
		class compiledMethodAt: oldSelector ifPresent: [ :method |
			"Keep any Pragmas, but use only newly specified Properties"
			(Compiler evaluate: codeString) ifNotNil: [ :props | 
				props isArray ifTrue: [
					method methodProperties: props.
					SystemChangeNotifier uniqueInstance methodChanged: method.
					self triggerEvent: #annotationChanged.
					^ true ]]] ifAbsent: nil.
		^false ].

	"Normal method accept"
	category := class organization categoryOfElement: oldSelector.
	selector := class compile: codeString
				classified: category
				notifying: aRequestor.
	selector
		ifNil: [^ false].
	selector == oldSelector ifFalse: [
		self reformulateListNoting: selector].
	shouldAutoHighlight := self showingSource.
	self triggerEvent: #annotationChanged.
	^ true! !

!MethodSet methodsFor: 'message list' stamp: 'jmv 5/Aug/2026, 1:22:24 pm (UTC)'!
selectedMessage
	"Answer the source method for the currently selected message."

	| class selector |
	selectedMessage ifNil: [^ 'Class vanished'].
	
	class := selectedMessage actualClass.
	selector := selectedMessage methodSymbol.
	selector ifNil: [ ^'prims'].

	selector first isUppercase ifTrue: [
		selector == #Comment ifTrue: [
			currentCompiledMethod := class organization commentRemoteStr.
			^ class smalltalkComment ].
		selector == #Definition ifTrue: [
			^ class definition ].].

	(class notNil and: [ class includesSelector: selector]) ifFalse: [
		currentCompiledMethod := nil.
		^ 'Missing'].

	self showingDecompile ifTrue: [
		^ self decompiledSource ].

	currentCompiledMethod := class compiledMethodAt: selector ifAbsent: nil.

	self showingProperties ifTrue: [
		^String streamContents: [ :strm |
			strm
				nextPutAll: '"The following Smalltalk expression evaluates to the Properties for this method'; newLine;
				nextPutAll: 'It must have the form of an Array of associations. For instance something like:'; newLine;
				nextPutAll: '{#propertyName1 -> 3.141592. #propertyName2 -> #stuff. #propertyName3 -> #(#toto #toton #totona)}'; newLine;
				nextPutAll: 'You can edit it and save as needed"'; newLine.
			currentCompiledMethod propertiesAsArray storePropertiesOn: strm ]].

	^self sourceStringPrettifiedAndDiffed
! !

