'From Cuis 4.2 of 25 July 2013 [latest update: #2391] on 29 June 2015 at 3:10:17.921275 pm'!

!CodePackageList methodsFor: 'as yet unclassified' stamp: 'jmv 6/29/2015 15:08'!
editorClassFor: textGetter
	textGetter = #description ifTrue: [
		^SmalltalkEditor ].		"To enable saving"
	^TextEditor! !


!Object methodsFor: 'testing' stamp: 'jmv 6/29/2015 14:57'!
is: aSymbol
	"A means for cleanly replacing isXXX like methods.
	Please use judiciously!!
	aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc.
	
	A few comments:
	
		- Good for kernel tests
		- Good for tests defined in the same package as the receiver
		- Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases
		
		- In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching.
		
		- if a class happens to answer true for several Symbols, consider implementing it like:
			^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol
		"
	
	"Enable this to log improper calls to the Transcript..."
	"
	aSymbol class == Symbol ifFalse: [ thisContext sender sender print. aSymbol print ].
	"
	^false! !


!CodeProvider methodsFor: 'misc' stamp: 'jmv 6/29/2015 15:01'!
okayToAccept
	"Answer whether it is okay to accept the receiver's input"

	self showingByteCodes ifTrue: [
		self inform: 
'Sorry, you can only submit changes here 
when you are showing source.'.
		^ false].

	self showingDocumentation ifTrue: [
		self inform: 
'Sorry, you can only submit changes here 
when you are showing source.'.
		^ false].

	self showingAnyKindOfDiffs ifTrue: [
		^ SelectionMenu confirm: 
'Caution!!  You are "showing diffs" here, so 
there is a danger that some of the text in the
code pane is contaminated by the "diff" display'
		trueChoice: 'accept anyway -- I''ll take my chances' falseChoice: 'um, let me reconsider'
	].

	^ true! !

!CodeProvider methodsFor: 'user interface support' stamp: 'jmv 6/29/2015 15:03'!
editorClassFor: textGetter
	textGetter = #acceptedContents
		ifFalse: [ ^super editorClassFor: textGetter ].
	currentCompiledMethod ifNotNil: [ :cm |
		^cm compilerClass editorClass ].
	^SmalltalkEditor! !


!FileList methodsFor: 'user interface support' stamp: 'jmv 6/29/2015 15:05'!
editorClassFor: textGetter
	textGetter = #acceptedContents
		ifFalse: [ ^super editorClassFor: textGetter ].
	^SmalltalkEditor! !

!methodRemoval: CodeProvider #isRefusingToAccept!
CodeProvider removeSelector: #isRefusingToAccept!
