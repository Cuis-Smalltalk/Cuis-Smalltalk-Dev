'From Cuis 4.2 of 25 July 2013 [latest update: #1932] on 9 January 2014 at 11:29:10.277044 pm'!

!ChangeList methodsFor: 'viewing access' stamp: 'jmv 1/9/2014 22:57'!
annotation
	"Answer the string to be shown in an annotation pane.  Make plain that the annotation is associated with the current in-image version of the code, not of the selected disk-based version, and if the corresponding method is missing from the in-image version, mention that fact."

	| change count selectedCount ann1 ann2 |
	change _ self currentChange.
	
	change isNil ifTrue: [
		count _ listSelections size.
		selectedCount _ listSelections count: [ :flag | flag ].
		^ 'Total items: ', count printString, ' - Selected items: ', selectedCount printString ].

	change changeType == #classDefinition ifTrue: [
		ann1 _ change isMetaClassChange ifTrue: [ 'Metaclass' ] ifFalse: [ 'Class' ].
		ann2 _ (Smalltalk includesKey: change changeClassName) ifTrue: [ ' already exists' ] ifFalse: [ ' not in system' ].
		^ann1, ann2 ].
	
	(self selectedMessageName isNil or: [self selectedClassOrMetaClass isNil])
		ifTrue: [^ '------'].

	^ change methodSelector notNil
		ifFalse: [ super annotation]
		ifTrue: [
			(self isNewMethod: change)
				ifTrue: [
					String streamContents: [ :strm | | sel |
						sel _ change methodSelector.
						strm
							nextPutAll: change changeClassName;
							nextPutAll: ' >> ';
							nextPutAll: sel;
							nextPutAll: ' is not present in the system. It has '.
							count _ Smalltalk numberOfImplementorsOf: sel.
							count = 1
								ifTrue: [strm nextPutAll: '1 implementor.']
								ifFalse: [count printOn: strm. strm nextPutAll: ' implementors.' ].
							strm nextPutAll: ' and '.
							count _ Smalltalk numberOfSendersOf: sel.
							count = 1
								ifTrue: [strm nextPutAll: '1 sender']
								ifFalse: [count printOn: strm. strm nextPutAll: ' senders' ].
						]
					]
				ifFalse: [
					'current version: ', super annotation]]! !

