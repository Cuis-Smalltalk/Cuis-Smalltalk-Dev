'From Cuis7.9 [latest update: #8290] on 27 October 2026 at 2:09:53 pm'!

!Browser methodsFor: 'code pane' stamp: 'jmv 27/Oct/2026, 5:09:18 pm (UTC)'!
compileMethod: aText notifying: aRequestor
	"Compile the code that was accepted by the user, placing the compiled method into an appropriate message category.  Return true if the compilation succeeded, else false."

	"If no category (not even the '-- all --' category) is selected, select one."
	self selectedMessageCategoryName ifNil: [
		self selectOriginalCategoryForCurrentMethod 	
			ifFalse: [ "Select the '--all--' category"
				self messageCategoryListIndex: 1]]. 

	^(self defineMethodFrom: aText notifying: aRequestor) notNil.! !

!Browser methodsFor: 'message functions' stamp: 'jmv 27/Oct/2026, 5:09:10 pm (UTC)'!
defineMethodFrom: aString notifying: aRequestor
	"Compile the expressions in aString. Notify aRequestor if a syntax error occurs. Install the compiled method in the selected class classified under  the currently selected message category name. Answer the selector obtained if compilation succeeds, nil otherwise."
	| selectedMessageName selector category oldMessageList scarySelector |
	selectedMessageName := self selectedMessageName.
	oldMessageList := self messageList.
	 self metaClassIndicated ifTrue: [
		scarySelector := self selectedClassOrMetaClass parserClass selectorFrom: aString.
		((self selectedClassOrMetaClass includesSelector: scarySelector) not
			and: [Metaclass isScarySelector: scarySelector])
			ifTrue: ["A frist-time definition overlaps the protocol of Metaclasses"
					(self confirm: (scarySelector bold, ' is used in the existing class system.
Overriding it could cause serious problems.
Is this really what you want to do?'))
					ifFalse: [^nil]]].
	category := self selectedMessageCategoryName ifNotNil: [ :n | n asSymbol ].
	category == ClassOrganizer allCategory ifTrue: [
		"Current category for existing methods or if a method was selected,
		nil for new methods when no previous method was selected."
		category := self categoryOfCurrentMethod ].
	selector := self selectedClassOrMetaClass
				compile: aString
				classified: category
				notifying: aRequestor.
	selector
		ifNil: [^ nil].
	selector ~~ selectedMessageName
		ifTrue: [
			self setClassOrganizer.  "In case organization not cached"
			(oldMessageList includes: selector)
				ifFalse: [self changed: #messageList].
			self messageListIndex: (self messageList indexOf: selector)].
	^ selector! !


!Browser methodsFor: 'accessing' stamp: 'jmv 27/Oct/2026, 5:08:51 pm (UTC)'!
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
					^ (self compileMethod: codeString notifying: aRequestor)
						ifTrue: [ self triggerEvent: #annotationChanged ];
						yourself ]].
	editSelection == #none
		ifTrue: [
			self inform: 'This text cannot be accepted
in this part of the browser.'.
			^ false].
	self error: 'unacceptable accept'.
! !

!methodRemoval: Browser #compileMessage:notifying: stamp: 'jmv 27/Oct/2026, 5:08:58 pm (UTC)'!
Browser removeSelector: #compileMessage:notifying:!
!methodRemoval: Browser #defineMessageFrom:notifying: stamp: 'jmv 27/Oct/2026, 5:09:24 pm (UTC)'!
Browser removeSelector: #defineMessageFrom:notifying:!
