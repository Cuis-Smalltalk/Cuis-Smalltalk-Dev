"
A TimeProfileBrowser is a browser visualizing the runtime profile of an executed Smalltalk block.  It is useful for finding performance bottlenecks in code. When optimizing code it can
be hard to know what methods actually constitute the bulk of the execution time. Is it a few
methods that take very long time to execute or is it perhaps a single method that gets executed a thousand times?

The block is first spied on using a MessageTally instance (which has even more funtionality than used by the TimeProfileBrowser) which samples the block during it's execution and collects the amount of time approximately spent in the methods executed. Then the methods are shown in the browser with their relative execution time in percent.

Example:
TimeProfileBrowser onBlock: [20 timesRepeat:  [Transcript show: 100 factorial printString]]

"
Class {
	#name : 'TimeProfileBrowser',
	#superclass : 'MethodSet',
	#instVars : [
		'tally',
		'talliesList'
	],
	#category : 'Tools-Profiling',
	#package : 'Tools',
	#tag : 'Profiling'
}

{ #category : 'instance creation' }
TimeProfileBrowser class >> onBlock: block [
	"Open a profile browser on the given block, thereby running the block and 
	 collecting the message tally."
	"TimeProfileBrowser onBlock: [20 timesRepeat: 
			[Transcript show: 100 factorial printString]]"

	| inst result |
	inst := self new.
	result := inst runBlock: block.
	MethodSetWindow open: inst label: 'Time Profile'.
	^ result
]

{ #category : 'instance creation' }
TimeProfileBrowser class >> spyOn: block [
	"Open a profile browser on the given block, thereby running the block and 
	 collecting the message tally."
	"
	TimeProfileBrowser spyOn:  [20 timesRepeat: 
			[Transcript show: 100 factorial printString]]
	"

	^self onBlock: block
]

{ #category : 'private' }
TimeProfileBrowser >> initializeMessageList: anArray talliesList: anotherArray [

	messageList := anArray.
	talliesList := anotherArray.
	selectedMessage := talliesList isEmpty ifFalse: [ talliesList first ]
]

{ #category : 'message list' }
TimeProfileBrowser >> messageListIndex [
	"Answer the index of the selected message selector into the currently 
	selected message category."

	selectedMessage ifNil: [ ^0 ].
	^talliesList indexOf: selectedMessage
]

{ #category : 'message list' }
TimeProfileBrowser >> messageListIndex: anInteger [

	"Set the index of the selected item to be anInteger."
	
	selectedMessage := (talliesList isInBounds: anInteger) ifTrue: [ talliesList at: anInteger ].
	self changed: #messageListIndex.	 "update my selection"
	self changed: #editSelection.
	self acceptedContentsChanged
]

{ #category : 'private' }
TimeProfileBrowser >> runBlock: aBlock [
"
	TimeProfileBrowser spyOn:  [20 timesRepeat: 
			[Transcript show: 100 factorial printString]]
"
	| result linesStream talliesStream textStream |

	tally := AndreasSystemProfiler new.
	tally observedProcess: Processor activeProcess.
	result := tally spyOn: aBlock.

	textStream := DummyStream on: nil.
	linesStream := WriteStream on: #().
	talliesStream := WriteStream on: #().
	
	tally reportTextOn: textStream linesOn: linesStream talliesOn: talliesStream.
	self initializeMessageList: linesStream contents talliesList: talliesStream contents.

	self changed: #messageList.
	self changed: #messageListIndex.
	self triggerEvent: #decorateButtons.
	^result
]
