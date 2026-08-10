'From Cuis7.9 [latest update: #8135] on 10 August 2026 at 12:09:07 pm'!

!ClassDescription methodsFor: 'compiling' stamp: 'jmv 10/Aug/2026, 3:08:13 pm (UTC)'!
compileProperties: aString for: method
	"Keep any Pragmas, but use only newly specified Properties"

	(Compiler evaluate: aString) ifNotNil: [ :props | 
		props isArray ifTrue: [
			method methodProperties: props.
			SystemChangeNotifier uniqueInstance methodChanged: method.
			"Save to Changes and User Changes files."
			method
				putSource: method sourceCode
				class: method methodClass
				category: (method methodClass organization categoryOfElement: method selector)
				withStamp: method authorAndStampAsSaved
				inFile: 2
				priorMethod: method.
			^ true ]].
	^false! !


!Browser methodsFor: 'accessing' stamp: 'jmv 10/Aug/2026, 2:49:59 pm (UTC)'!
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
						^self selectedClassOrMetaClass compiledMethodAt: self selectedMessageName
						ifPresent: [ :method | | answer |
							answer := self selectedClassOrMetaClass compileProperties: codeString for: method.
							self triggerEvent: #annotationChanged.
							answer ]
						ifAbsent: [false]].
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


!MethodSet methodsFor: 'accessing' stamp: 'jmv 10/Aug/2026, 2:42:09 pm (UTC)'!
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
		^class compiledMethodAt: oldSelector
			ifPresent: [ :method | | answer |
				answer := class compileProperties: codeString for: method.
				self triggerEvent: #annotationChanged.
				answer ]
			ifAbsent: [false]].

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

