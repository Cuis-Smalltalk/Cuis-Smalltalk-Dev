"
MethodChangeRecords are used to record method changes.  Here is a simple summary of the relationship between the changeType symbol and the recording of prior state

					|	prior == nil			|	prior not nil	
	---------	|----------------------------	|--------------------
			add		|	add					|	change
	---------	|----------------------------	|--------------------
			remove	|	addedThenRemoved	|	remove

Structure:
changeType		symbol -- as summarized above
currentMethod	method
				This is the current version of the method.
				It can be used to assert this change upon entry to a layer. 
infoFromRemoval -- an array of size 2.
				The first element is the source index of the last version of the method.
				The second element is the category in which it was defined, so it
				can be put back there if re-accepted from a version browser.

Note that the above states each have an associated revoke action:
	add --> remove
	change --> change back
	remove --> add back
	addedThenRemoved --> no change
However all of these are accomplished trivially by restoring the original method dictionary.
"
Class {
	#name : 'MethodChangeRecord',
	#superclass : 'Object',
	#instVars : [
		'changeType',
		'infoFromRemoval',
		'stamp'
	],
	#category : 'Tools-Changes',
	#package : 'Tools',
	#tag : 'Changes'
}

{ #category : 'change type' }
MethodChangeRecord >> changeType [

	^ changeType
]

{ #category : 'method' }
MethodChangeRecord >> methodInfoFromRemoval [
	"Return an array with the source index of the last version of the method,
	and the category in which it was defined (so it can be put back there if
	re-accepted from a version browser)."

	(changeType == #remove or: [changeType == #addedThenRemoved])
		ifTrue: [^ infoFromRemoval]
		ifFalse: [^ nil]
]

{ #category : 'change type' }
MethodChangeRecord >> noteChangeType: newChangeType [

	stamp := Utilities changeStamp.
	
	"Change of an added method, is still an add"
	(changeType == #add and: [ newChangeType == #change ])
		ifTrue: [ ^self ].

	"Change of an added method, is still an add"
	(changeType == #addedThenRemoved and: [ newChangeType == #change ])
		ifTrue: [ 
			changeType := #add.
			^self ].

	changeType := newChangeType.
]

{ #category : 'method' }
MethodChangeRecord >> noteMethodInfoFromRemoval: info [
	"Store an array with the source index of the last version of the method,
	and the category in which it was defined (so it can be put back there if
	re-accepted from a version browser)."

	infoFromRemoval := info
]

{ #category : 'printing' }
MethodChangeRecord >> printOn: strm [

	super printOn: strm.
	strm nextPutAll: ' ('; print: changeType; nextPutAll: ')'
]

{ #category : 'initialization' }
MethodChangeRecord >> priorMethod: ignored [

	"We do not save original versions of changed methods because we only
	revoke changes at the level of entire classes, and that is done by
	restoration of the entire methodDictionary."
]

{ #category : 'stamp' }
MethodChangeRecord >> stamp [

	^stamp 
]
