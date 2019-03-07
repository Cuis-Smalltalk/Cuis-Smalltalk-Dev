'From Cuis 5.0 of 7 November 2016 [latest update: #3640] on 7 March 2019 at 12:04:56 pm'!
!classDefinition: #RecompilationFailure category: #'Exceptions Kernel'!
Error subclass: #RecompilationFailure
	instanceVariableNames: 'class selector'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Exceptions Kernel'!

!RecompilationFailure commentStamp: 'jmv 3/7/2019 11:39:10' prior: 0!
Recompilation of some existing method failed.

The usual handling of this exception should be to cancel the action that triggered recompilation. If not handled, a debugger will open.!


!RecompilationFailure methodsFor: 'accessing' stamp: 'jmv 3/7/2019 12:02:40'!
messageText
	^ 'In method ', class name,'>>', selector asString,' ', messageText! !

!RecompilationFailure methodsFor: 'private' stamp: 'jmv 3/7/2019 11:28:08'!
class: aClass selector: aSymbol messageText: aString
	class _ aClass.
	selector _ aSymbol.
	messageText _ aString! !


!RecompilationFailure class methodsFor: 'instance creation' stamp: 'jmv 3/7/2019 11:28:18'!
class: aClass selector: aSymbol messageText: aString
	^ self new class: aClass selector: aSymbol messageText: aString! !


!Browser methodsFor: '*LiveTyping' stamp: 'jmv 3/7/2019 12:03:38'!
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
	editSelection == #editClass | (editSelection == #newClass) ifTrue: [
		[
			self defineClass: aString notifying: aRequestor 
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


!Encoder methodsFor: 'private' stamp: 'jmv 3/7/2019 12:02:44'!
warnAboutShadowed: name
	| msg fullMsg |
	msg _ 'There already exists a variable named ', name, ' '.
	fullMsg _ class name,'>>', selector asString, ' ', msg.
	requestor addWarning: fullMsg.
	Transcript newLine; show: fullMsg.
	InMidstOfFileinNotification signal
		ifFalse: [
			requestor interactive
				ifTrue: [
					self notify: msg]
				ifFalse: [
					(RecompilationFailure
						class: class
						selector: selector
						messageText: msg, String newLineString, 'Please use a different name') signal ]]! !


