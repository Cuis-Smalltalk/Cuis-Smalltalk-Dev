'From Cuis 5.0 of 7 November 2016 [latest update: #3672] on 18 March 2019 at 6:45:12 pm'!

!TextModel methodsFor: 'testing' stamp: 'jmv 3/18/2019 18:43:10'!
is: aSymbol
	^ aSymbol == #canSaveContents or: [ super is: aSymbol ]! !


!SystemWindow methodsFor: 'user interface' stamp: 'jmv 3/18/2019 18:43:17'!
hasSaveAs
	"Returns true if the window has a model which can be saved to a file"
	^model is: #canSaveContents! !

