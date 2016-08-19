'From Cuis 4.2 of 25 July 2013 [latest update: #2773] on 25 May 2016 at 4:10:14.677277 pm'!

!Compiler methodsFor: 'Cuis protocol' stamp: 'jmv 5/24/2016 23:21'!
sourceStreamGetter: aSymbol
	"Cuis specific. Do not remove!!"! !


!Compiler class methodsFor: 'Cuis protocol' stamp: 'jmv 5/24/2016 23:18'!
autoCompleterClass
	^SmalltalkCompleter! !

!Compiler class methodsFor: 'Cuis protocol' stamp: 'jmv 5/24/2016 23:27'!
editorClass
	^SmalltalkEditor! !

!Compiler class methodsFor: 'Cuis protocol' stamp: 'jmv 5/24/2016 23:39'!
textStylerClass
	^SHTextStylerST80! !


!Encoder methodsFor: 'error handling' stamp: 'jmv 5/24/2016 23:44'!
requestor: aRequestor

	requestor _ aRequestor! !


!Scanner class methodsFor: 'Cuis Protocol' stamp: 'jmv 5/24/2016 23:19'!
doItCharacterValue
	^26! !


!Parser methodsFor: 'Cuis Protocol' stamp: 'jmv 5/24/2016 23:22'!
sourceStreamGetter: aSymbol
	"Cuis specific. Do not remove!!"! !


!Parser class methodsFor: 'Cuis Protocol' stamp: 'jmv 5/24/2016 23:20'!
selectorFrom: methodSource
	^ZParser selectorFrom: methodSource! !

