"
Tally for assembling system profiles. It's a subclass of Link so receivers actually holds the first element in a linked list. This is good for making the profiling overhead low and relatively constant (no OrderedCollections to grow).
"
Class {
	#name : 'QSystemTally',
	#superclass : 'QAbstractTally',
	#instVars : [
		'class',
		'method',
		'blockNesting',
		'wasInPrimitive'
	],
	#category : 'Tools-Profiling',
	#package : 'Tools',
	#tag : 'Profiling'
}

{ #category : 'LICENSE' }
QSystemTally class >> LICENSE [
	^'Project Squeak

	In Memory of Andreas Raab.  Author, Friend, Colleague. 	http://forum.world.st/In-Memory-of-Andreas-Raab-td4663424.html
	Copyright (c) 2005-2013, 3D Immersive Collaboration Consulting, LLC., All Rights Reserved

	Redistributions in source code form must reproduce the above
	copyright and this condition.

Licensed under MIT License (MIT)
Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'
]

{ #category : 'MethodReference compatibility' }
QSystemTally >> actualClass [
	^method methodClass
]

{ #category : 'accessing' }
QSystemTally >> actualReceiverClass [
	^class
]

{ #category : 'accessing' }
QSystemTally >> blockNestingCount [
	^blockNesting
]

{ #category : 'tallying' }
QSystemTally >> blockNestingCountOf: context [
	| count nest |
	count := 0.
	nest := context.
	[nest closure notNil] whileTrue:
		[count := count + 1.
		 nest := nest closure outerContext].
	^count
]

{ #category : 'initialization' }
QSystemTally >> class: aClass method: aCompiledMethod nesting: blockNestingCount [
	class := aClass.
	method := aCompiledMethod.
	blockNesting := blockNestingCount
]

{ #category : 'initialization' }
QSystemTally >> initialize [
	super initialize.
	wasInPrimitive := false
]

{ #category : 'accessing' }
QSystemTally >> method [
	"Answer the CompiledMethod associated with this tally"
	^method
]

{ #category : 'MethodReference compatibility' }
QSystemTally >> methodSymbol [
	^method selector
]

{ #category : 'report' }
QSystemTally >> printOn: aStream [
	blockNesting timesRepeat: [
		aStream nextPutAll: '[] in '].
	aStream print: class; nextPutAll: '>>'; print: (method ifNotNil:[method selector]).
	aStream nextPutAll: ' -- '; print: tally
]

{ #category : 'report' }
QSystemTally >> printOn: textStream linesOn: linesStream talliesOn: talliesStreams tabs: tabsAndTreeLines total: total totalTime: totalTime parent: parentTally [

	| aSelector aClass percentage line |
	line := String streamContents: [ :lineStream |
		tabsAndTreeLines do: [ :tabOrLineChar | lineStream nextPutAll: tabOrLineChar ].
		percentage := tally asFloat / total * 100.0.
		percentage printOn: lineStream fractionDigits: 2.
		lineStream nextPutAll: '% ('.
		percentage * totalTime / 100 printOn: lineStream fractionDigits: 1.
		lineStream nextPutAll: ' ms) '.
		aSelector := class selectorAtMethod: method setClass: [ :c | aClass := c].
			blockNesting > 0 ifTrue: [
				lineStream
					next: blockNesting put: $[;
					next: blockNesting put: $];
					space ].
		lineStream
			nextPutAll: class name;
			nextPutAll: (aClass == class 
					ifTrue: ['>>']
					ifFalse: ['(' , aClass name , ')>>']);
			nextPutAll: aSelector.
		wasInPrimitive ifTrue: [
			self flag: #profilerFriendlyCall:.
			parentTally methodSymbol == #profilerFriendlyCall:
				ifTrue: [
					lineStream nextPutAll: '   -- primitive (reported properly)' ]
				ifFalse: [
					lineStream nextPutAll: '   -- primitive (real sender possibly omitted, see #profilerFriendlyCall:)' ]
			].
		].
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStreams nextPut: self
]

{ #category : 'tallying' }
QSystemTally >> tally: context inProcess: aProcess by: count [
	"Explicitly tally the specified context and its stack."
	| sender senderNode |

	"Add to this node if appropriate"
	(context receiver class == class and: [ context method == method and: [ aProcess == process | process isNil and: [ (self blockNestingCountOf: context) = blockNesting ]]]) ifTrue: [
		self wasInPrimitive ifFalse: [
			self addTallies: count.
			^self ]].

	sender := context sender.
	"No sender means end of recursion. Add new root to the tree."
	sender ifNil: [
		self addTallies: count.
		^self tallyChild: context inProcess: aProcess by: count ].

	"Find the node for the sending context (or add it if necessary).
	Then, add us."
	senderNode := self tally: sender inProcess: aProcess by: count.
	^senderNode tallyChild: context inProcess: aProcess by: count
]

{ #category : 'tallying' }
QSystemTally >> tallyChild: context inProcess: aProcess by: count [
	"Find a suitable child, or add a new one if needed."
	| aMethod node nesting |
	aMethod := context method.
	nesting := self blockNestingCountOf: context.
	node := receivers.
	[ node == nil ] whileFalse: [
		(context receiver class == node actualReceiverClass and: [aMethod == node method and: [ node process == aProcess and: [ nesting = node blockNestingCount ]]]) ifTrue: [
			node wasInPrimitive ifFalse: [
				^node addTallies: count ]].
		node := node nextLink ].
	node := QSystemTally new
				class: context receiver class
				method: aMethod
				nesting: nesting.
	node process: aProcess.
	node nextLink: receivers.
	receivers := node.
	^node addTallies: count
]

{ #category : 'accessing' }
QSystemTally >> wasInPrimitive [
	^ wasInPrimitive
]

{ #category : 'initialization' }
QSystemTally >> wasInPrimitive: aBoolean [
	wasInPrimitive := aBoolean
]
