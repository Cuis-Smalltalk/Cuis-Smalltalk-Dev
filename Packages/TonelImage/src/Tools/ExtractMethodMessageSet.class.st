Class {
	#name : 'ExtractMethodMessageSet',
	#superclass : 'MethodSet',
	#instVars : [
		'finder',
		'selectedIndex'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
ExtractMethodMessageSet class >> finder: aFinder  [
	
	^(self messageList: aFinder methodsToReplace) initializeFinder: aFinder
]

{ #category : 'message list' }
ExtractMethodMessageSet >> doNotRefreshSelection [
	
	selectedIndex := 0
]

{ #category : 'initialization' }
ExtractMethodMessageSet >> initialize [

	selectedIndex := 0.
	super initialize 
]

{ #category : 'initialization' }
ExtractMethodMessageSet >> initializeFinder: aFinder  [
	
	finder := aFinder.
]

{ #category : 'message list' }
ExtractMethodMessageSet >> messageListIndex [

	^selectedIndex 
]

{ #category : 'message list' }
ExtractMethodMessageSet >> messageListIndex: anIndex [

	selectedIndex := anIndex.
	^super messageListIndex: anIndex 
]

{ #category : 'source code ranges' }
ExtractMethodMessageSet >> messageSendsRangesOf: aSelector [

	| replacement |
	
	replacement := finder replacementsAt: self messageListIndex ifAbsent: [ ^#()  ].
	
	^Array with: replacement intervalToExtract.
]

{ #category : 'message list' }
ExtractMethodMessageSet >> removeMessageFromBrowserKeepingLabel [

	| newIndex |

	selectedMessage ifNil: [ ^nil ].
	messageList removeIndex: selectedIndex.
	finder removeReplacementAt: selectedIndex.
	self changed: #messageList.
	
	newIndex := selectedIndex > messageList size 
		ifTrue: [ selectedIndex - 1 ]
		ifFalse: [ selectedIndex ].
	self messageListIndex: newIndex.
]
