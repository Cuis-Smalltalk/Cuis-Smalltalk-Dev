'From Cuis 4.1 of 12 December 2012 [latest update: #1519] on 21 December 2012 at 1:55:53 pm'!

!ClosureTests methodsFor: 'testing' stamp: 'jmv 12/20/2012 21:41'!
testIsClean
	"
	ClosureTests new testIsClean
	"
	| tempVar |
	tempVar _ 1.
	self assert: [ 3 + 4 ] isClean.
	self assert: [ :a | a * 2 ] isClean.
	self assert: [ Smalltalk size ] isClean.
	self assert: [ :blockArg | blockArg printString ] isClean.
	self assert: [ | blockTemp | blockTemp printString ] isClean.
	self assert: [ | blockTemp | blockTemp _ 7 ] isClean.
	self deny: [ | outerBlockTemp | [ outerBlockTemp printString ] isClean ] value.
	self deny: [ | outerBlockTemp | [ outerBlockTemp _ 7 ] isClean ] value.
	self deny: [ tempVar + 1 ] isClean.
	self deny: [ tempVar _ 1 ] isClean.
	self deny: [ ivar + 1 ] isClean.
	self deny: [ ivar _ 1 ] isClean.
	self deny: [ ^ true ] isClean.
	self deny: [ self printString ] isClean.
	self deny: [ ^ self ] isClean.
	self deny: [ ClassVar + 1 ] isClean.
	self deny: [ ClassVar _ 1 ] isClean! !


!CodeWindow methodsFor: 'misc' stamp: 'jmv 12/20/2012 21:45'!
sendQuery: querySelector to: queryPerformer
	"Apply a query to the primary selector associated with the current context.  If no such selection exists, obtain one by user type-in. Then send querySelector to queryPerformer with the selector as its argument."

	| aSelector aString |
	aSelector _ model selectedMessageName ifNil: [
		aString _ FillInTheBlankMorph request: 'Type selector:' initialAnswer: 'flag:'.
		^ aString isEmptyOrNil ifFalse: [
			(Symbol hasInterned: aString ifTrue: [ :aSymbol |
				queryPerformer perform: querySelector with: aSymbol])
					ifFalse: [ self inform: 'no such selector' ]]].

	queryPerformer perform: querySelector with: aSelector! !

!CodeWindow methodsFor: 'misc' stamp: 'jmv 12/20/2012 21:46'!
useSelector: incomingSelector orGetSelectorAndSendQuery: querySelector to: queryPerformer
	"If incomingSelector is not nil, use it, else obtain a selector from user type-in.   Using the determined selector, send the query to the performer provided."

	| aSelector |
	incomingSelector
		ifNotNil: [
			queryPerformer perform: querySelector with: incomingSelector]
		ifNil: [
			aSelector _ FillInTheBlankMorph request: 'Type selector:' initialAnswer: 'flag:'.
			aSelector isEmptyOrNil ifFalse: [
				(Symbol hasInterned: aSelector ifTrue: [ :aSymbol |
					queryPerformer perform: querySelector with: aSymbol])
						ifFalse: [ self inform: 'no such selector']]]! !


!SystemDictionary methodsFor: 'image format' stamp: 'jmv 12/20/2012 21:40'!
imageFormatVersion
	"Answer an integer identifying the type of image in memory. The image version number may
	identify the format of the image (e.g. 32 or 64-bit word size) or specific requirements
	of the image (e.g. block closure support required). This invokes an optional primitive
	that may not be available on all virtual machines."

	"
	Smalltalk imageFormatVersion
	"

	<primitive: 'primitiveImageFormatVersion'>

	"Cog provides a VM parameter"
	^[Smalltalk vmParameterAt: 41]
		on: Error
		do: [self notify: 'This virtual machine does not support the optional ',
				'primitive #primitiveImageFormatVersion'.
			nil]
! !

