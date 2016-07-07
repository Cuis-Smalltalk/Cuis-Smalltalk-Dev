'From Cuis 4.2 of 25 July 2013 [latest update: #2841] on 5 July 2016 at 10:09:46.779514 am'!

!TestCase methodsFor: 'testing' stamp: 'jmv 7/4/2016 22:41'!
is: aSymbol
	^aSymbol == #TestCase or: [ super is: aSymbol ]! !


!Timespan methodsFor: 'testing' stamp: 'jmv 7/4/2016 22:42'!
is: aSymbol
	^aSymbol == #Timespan or: [ super is: aSymbol ]! !


!TestRunner methodsFor: 'updating' stamp: 'jmv 7/4/2016 22:41'!
update: aParameter 
	"updates come in from another thread"
	(aParameter is: #TestCase)
		ifTrue: [
			WorldState addDeferredUIMessage: [
				completedTests _ completedTests + 1.
				progressLabel _ aParameter printString.
				self changed: #progress ]]
		ifFalse: [ super update: aParameter ]! !


!Interval class methodsFor: 'instance creation' stamp: 'jmv 7/4/2016 22:17'!
integersFrom: start to: stop count: n
	"Answer an instance of me, starting at start, ending at 
	stop, and with n elements. All the elements will be integer numbers."

	| answer |
	answer _ self from: start asInteger to: stop asInteger count: n.
	answer increment isFraction
		ifTrue: [answer _ answer collect: [ :each | each rounded ]].
	^answer! !


!Compiler methodsFor: 'private' stamp: 'jmv 7/4/2016 22:32'!
from: textOrStream class: aClass context: aContext notifying: req

	sourceStream _ (textOrStream is: #Stream)
		ifTrue: [ textOrStream ]
		ifFalse: [ ReadStream on: textOrStream asString ].
	class _ aClass.
	context _ aContext.
	requestor _ req! !


!Morph methodsFor: 'menus' stamp: 'jmv 7/5/2016 10:07'!
maybeAddCollapseItemTo: aMenu
	"If appropriate, add a collapse item to the given menu"

	owner ifNotNil: [
		owner isWorldMorph ifTrue: [
			aMenu add: 'collapse' target: self action: #collapse ]]! !

!Morph methodsFor: 'printing' stamp: 'jmv 7/5/2016 10:03'!
printOn: aStream 
	"Add the identity of the receiver to a stream"
	owner ifNil: [
		aStream isText
			ifTrue: [
				aStream withAttribute: (TextAnchor new anchoredFormOrMorph: self) do: [aStream nextPut: $*].
				^ self]].
	super printOn: aStream. "a(n) className"
	aStream 
		nextPut: $(;
		print: self identityHash;
		nextPut: $).
	self valueOfProperty: #morphName ifPresentDo: [ :x | aStream nextPutAll: x asString]! !

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 7/5/2016 10:06'!
comeToFront
	owner firstSubmorph == self 
		ifFalse: [owner addMorphFront: self]! !


!SystemWindow methodsFor: 'top window' stamp: 'jmv 7/5/2016 10:08'!
activateAndSendTopToBack: aBoolean
	"Bring me to the front and make me able to respond to mouse and keyboard"

	| oldTop |
	owner 
		ifNil: [^self	"avoid spurious activate when drop in trash"].
	oldTop _ TopWindow.
	TopWindow _ self.

	oldTop ifNotNil: [
		oldTop redrawNeeded.
		aBoolean ifTrue: [
			| bottomWindow |
			bottomWindow _ oldTop owner submorphs reverse detect: [:one | one is: #SystemWindow].
			oldTop owner addMorph: oldTop behind: bottomWindow]].

	owner firstSubmorph == self 
		ifFalse: [
			"Bring me to the top if not already"
			owner addMorphFront: self].
	self redrawNeeded.

	"Set keyboard focus"
	self world ifNotNil: [ :w |
		w activeHand newKeyboardFocus: self submorphToFocusKeyboard ]! !


!Timespan methodsFor: 'squeak protocol' stamp: 'jmv 7/4/2016 22:42'!
includes: aDateAndTime


	^ (aDateAndTime is: #Timespan)
			ifTrue: [ (self includes: aDateAndTime start)
						and: [ self includes: aDateAndTime end ] ]
			ifFalse: [ aDateAndTime between: start and: self end ]! !

