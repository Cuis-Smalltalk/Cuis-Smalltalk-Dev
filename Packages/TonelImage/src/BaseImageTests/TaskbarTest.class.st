"
Tests for the Taskbar.
"
Class {
	#name : 'TaskbarTest',
	#superclass : 'TestCase',
	#instVars : [
		'taskbar',
		'needsDelete'
	],
	#category : 'BaseImageTests-Tools-Taskbar',
	#package : 'BaseImageTests',
	#tag : 'Tools-Taskbar'
}

{ #category : 'Running' }
TaskbarTest >> setUp [

	super setUp.
	needsDelete := UISupervisor ui taskbar isNil.
	taskbar := UISupervisor ui hideTaskbar; showTaskbar; taskbar.
	taskbar screenSizeChanged.
	"taskbar world runStepMethods"
]

{ #category : 'Running' }
TaskbarTest >> tearDown [

	needsDelete ifTrue: [
		UISupervisor ui hideTaskbar ]
]

{ #category : 'test cases' }
TaskbarTest >> testClassSingleton [

	self should: [ taskbar == UISupervisor ui taskbar ].

]

{ #category : 'test cases' }
TaskbarTest >> testHorizontalAlignment [

	self should: [ taskbar positionInWorld x = 0 ]
]

{ #category : 'test cases' }
TaskbarTest >> testWidth [
	
	self should: [ taskbar morphWidth = UISupervisor ui morphWidth ]
]
