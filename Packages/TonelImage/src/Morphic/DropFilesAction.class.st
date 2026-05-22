Class {
	#name : 'DropFilesAction',
	#superclass : 'Object',
	#instVars : [
		'dropFilesEvent',
		'selectedFileEntry',
		'shouldAskToStop',
		'stopHereBlock'
	],
	#category : 'Morphic-Kernel',
	#package : 'Morphic',
	#tag : 'Kernel'
}

{ #category : 'instance creation' }
DropFilesAction class >> for: aDropFilesEvent [

	^self new initializeFor: aDropFilesEvent 
	
]

{ #category : 'evaluating - private' }
DropFilesAction >> createMenuFor: options [

	| icons lines labels |

	"options is a small collection, no problem to collect twice - Hernan"	
	labels := options collect: [ :option | option label ].
	icons := options collect: [ :option | option iconSpec ].

	shouldAskToStop 
		ifTrue: [
			lines := Array with: labels size.
			labels add: 'stop here'.
			icons add: #cancelIcon ]
		ifFalse: [ lines := #() ].
		
	^PopUpMenu labelArray: labels lines: lines icons: icons
]

{ #category : 'evaluating - private' }
DropFilesAction >> fileNamedDropped: aFileName  [

	| options selectionIndex menu |
	
	selectedFileEntry := aFileName asFullFileEntry.
	options := FileList itemsForFileEntry: selectedFileEntry.
	options isEmpty ifTrue: [ ^self inform: 'No action found for ', selectedFileEntry name ].
	menu := self createMenuFor: options.
		
	selectionIndex := menu startUpWithCaption: 'Select action for ', selectedFileEntry name.
	
	selectionIndex = 0 ifTrue: [ ^self ].
	(options isInBounds: selectionIndex) ifTrue: [ ^self performService: (options at: selectionIndex) ].
	"The only available option is 'stop here'. This could change if #createMenuFor: changes - Hernan"
	stopHereBlock value
	

]

{ #category : 'FileList protocol' }
DropFilesAction >> fullName [
	
	^selectedFileEntry name 
]

{ #category : 'initialization' }
DropFilesAction >> initializeFor: aDropFilesEvent  [

	dropFilesEvent := aDropFilesEvent.
	shouldAskToStop := aDropFilesEvent numberOfFiles > 1
]

{ #category : 'evaluating - private' }
DropFilesAction >> performService: aService [

	aService 
		argumentProvider: self;
		performService 

]

{ #category : 'FileList protocol' }
DropFilesAction >> selectedFileEntry [
	
	^selectedFileEntry
]

{ #category : 'evaluating' }
DropFilesAction >> value [

	stopHereBlock := [ ^self ].
	dropFilesEvent fileNamesDo: [ :fileName | self fileNamedDropped: fileName ]
		

]
