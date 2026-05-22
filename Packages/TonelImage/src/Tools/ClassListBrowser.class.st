"
A ClassListBrowser displays the code for an arbitrary list of classes.

ClassListBrowser example1.  "all classes that have the string 'Pluggable' in their names"
ClassListBrowser example2.  "all classes whose names start with the letter S"
ClassListBrowser example3.  "all variable classes"
ClassListBrowser example4.  "all classes with more than 100 methods"
ClassListBrowser example5.  "all classes that lack class comments"
ClassListBrowser example6.  "all classes that have class instance variables"

ClassListBrowser new initForClassesNamed: #(Browser Boolean) title: 'Browser and Boolean!'.

"
Class {
	#name : 'ClassListBrowser',
	#superclass : 'HierarchyBrowser',
	#instVars : [
		'defaultTitle'
	],
	#category : 'Tools-Browser',
	#package : 'Tools',
	#tag : 'Browser'
}

{ #category : 'initialization' }
ClassListBrowser >> initForClassesNamed: nameList title: aTitle [
	"Initialize the receiver for the class-name-list and title provided"

	self systemOrganizer: SystemOrganization.
	metaClassIndicated := false.
	defaultTitle := aTitle.
	classList := nameList copy
]

{ #category : 'title' }
ClassListBrowser >> labelString [
	"Answer the label strilng to use on the browser"

	^ defaultTitle ifNil: [super labelString]
]
