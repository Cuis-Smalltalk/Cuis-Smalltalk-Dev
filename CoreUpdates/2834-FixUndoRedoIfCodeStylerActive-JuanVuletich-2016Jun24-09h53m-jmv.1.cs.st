'From Cuis 4.2 of 25 July 2013 [latest update: #2832] on 24 June 2016 at 10:10:05.928602 am'!

!TextModel methodsFor: 'accessing' stamp: 'jmv 6/24/2016 10:09'!
basicActualContents: aTextOrString
	"Do not throw events... Not nice... Use with care... Clean some day..."
	| prevContents |
	prevContents _ actualContents.
	actualContents _ aTextOrString ifNotNil: [ aTextOrString asText ].
	actualContents = prevContents ifFalse: [	"Compares only characters, not attributes"
		undoRedoCommands resetToStart.
		lastEditTimeStamp _ nil ]! !

