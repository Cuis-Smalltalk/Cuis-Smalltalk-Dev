Class {
	#name : 'DynamicTypingSmalltalkCompleterTest',
	#superclass : 'SmalltalkCompleterTest',
	#category : 'BaseImageTests-Tools-Autocompletion',
	#package : 'BaseImageTests',
	#tag : 'Tools-Autocompletion'
}

{ #category : 'testing' }
DynamicTypingSmalltalkCompleterTest class >> isAbstract [

	^DynamicTypingSmalltalkCompleter isForCurrentTypeSystem not
]

{ #category : 'tests' }
DynamicTypingSmalltalkCompleterTest >> test023_AutocompletingMessagesFor_UnaryMessages [
	
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 self class '.
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 1 + self class '.	
]

{ #category : 'tests' }
DynamicTypingSmalltalkCompleterTest >> test024_AutocompletingMessagesFor_RightParenthesis [
	
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 (1) '.
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 1 < (1) '.
]

{ #category : 'tests' }
DynamicTypingSmalltalkCompleterTest >> test026_AutocompletingMessagesFor_Cascade [
	
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 self class; '.
	
]
