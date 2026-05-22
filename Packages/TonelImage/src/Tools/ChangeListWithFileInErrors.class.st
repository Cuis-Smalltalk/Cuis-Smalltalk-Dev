"
This class is used to keep errors when filing in changes.
I could have use ChangeList directly, selecting changes with errors, then removing them, etc., but it had some problems and that solution is more a hack.
So, instances of this class will keep errors when filing in a change, and it allows the posibility to show the change with the error in a change list window.

A doit change that signaled a MessageNotUnderstood is assume to not be an error becuase those kinds of things are evaluations in specific contexts that will obiously generate errors.
All doits with errors could be assume not to be errors, but I limited to MNU type of errors to avoid filtering errors that should be shown.
"
Class {
	#name : 'ChangeListWithFileInErrors',
	#superclass : 'ChangeList',
	#instVars : [
		'errors'
	],
	#category : 'Tools-Changes',
	#package : 'Tools',
	#tag : 'Changes'
}

{ #category : 'showing filein errors' }
ChangeListWithFileInErrors >> failedFileInChangesLabel [

	^'Changes that failed to file in'
]

{ #category : 'filing in' }
ChangeListWithFileInErrors >> fileInAllKeepingErrors [

	errors := Dictionary new.
	changeList do: [ :change | self fileInKeepingError: change ].

]

{ #category : 'filing in' }
ChangeListWithFileInErrors >> fileInKeepingError: change [
	
	[ change fileIn ]
		on: Error 
		do: [ :anError | (self hasToKeep: anError for: change) ifTrue: [ errors at: change put: anError ]]
]

{ #category : 'testing' }
ChangeListWithFileInErrors >> hasFileInErrors [

	^errors notEmpty
]

{ #category : 'testing' }
ChangeListWithFileInErrors >> hasToKeep: anError for: change [

	^(change isDoIt and: [ anError isKindOf: MessageNotUnderstood ]) not
]

{ #category : 'showing filein errors' }
ChangeListWithFileInErrors >> ifFiledInWithErrorAdd: aChange at: anIndex to: newChangeList and: newList  [
	
	(self wasFiledInWithError: aChange) ifTrue: [ 
		newChangeList add: aChange.
		newList add: ((list at: anIndex) contractTo: 40), ' | ', (errors at: aChange) printString ]
]

{ #category : 'initialization-release' }
ChangeListWithFileInErrors >> initialize [

	super initialize.
	errors := Dictionary new.
]

{ #category : 'showing filein errors' }
ChangeListWithFileInErrors >> removeSucessfullyFiledInChanges [

	| newChangeList newList |

	newChangeList := OrderedCollection new.
	newList := OrderedCollection new.

	changeList withIndexDo: [ :aChange :anIndex | self ifFiledInWithErrorAdd: aChange at: anIndex to: newChangeList and: newList ].
			
	changeList := newChangeList.
	list := newList.
	listIndex := 0.
	self clearSelections.
	self changed: #list.
]

{ #category : 'showing filein errors' }
ChangeListWithFileInErrors >> showChangesWithFileInErrors [

	self removeSucessfullyFiledInChanges.
	ChangeListWindow open: self label: self failedFileInChangesLabel


]

{ #category : 'showing filein errors' }
ChangeListWithFileInErrors >> wasFiledInWithError: aChange [
	
	^errors includesKey: aChange
]
