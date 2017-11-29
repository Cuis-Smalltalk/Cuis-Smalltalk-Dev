'From Cuis 5.0 of 7 November 2016 [latest update: #3224] on 29 November 2017 at 11:59:54 am'!
!classDefinition: #Clipboard category: #'System-Support'!
Object subclass: #Clipboard
	instanceVariableNames: 'contents recent contentsOriginalObjectWeakly '
	classVariableNames: 'Default '
	poolDictionaries: ''
	category: 'System-Support'!

!Clipboard methodsFor: 'accessing' stamp: 'jmv 11/29/2017 11:42:16'!
contentsOriginalObject
	"If not nil, the original object (not a copy!!) of what was stored in the clipboard. See #storeObject: Use with care"

	^ contentsOriginalObjectWeakly at: 1! !


!Clipboard class methodsFor: 'default clipboard' stamp: 'jmv 11/29/2017 11:42:25'!
contentsOriginalObject
	"If not nil, the original object (not a copy!!) of what was stored in the clipboard. See #storeObject: Use with care"

	^ self default contentsOriginalObject! !


!Object methodsFor: 'inspecting' stamp: 'jmv 11/29/2017 11:49:58'!
copyToClipboard

	Clipboard storeObject: self! !


!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 11/29/2017 11:55:10'!
paste
	| objectName |
	
	model canBindVariables ifTrue: [
		"Not a copy!!!!!!"
		Clipboard contentsOriginalObject ifNotNil: [ :object |
			objectName _ model nameForObject: object.
			(model bindingOf: objectName) value: object.
			self replaceSelectionWith: objectName.
			^ self ]].
	^ super paste! !


!Clipboard methodsFor: 'accessing' stamp: 'jmv 11/29/2017 11:36:45'!
storeObject: anObject
	"Set new contents on the clipboard.  Also export to OS.
	anObject can be a:
		String
		Text
		Form
		Morph
		Object.
	OS clipboard supports String. Other formats might be supported if ExtendedClipboardInterface is present and operative."

	| primitiveFormat id |

	"Store a copy of the object. This is appropriate in case the original object is modified after being copied to the clipboard.
	Another copy must be made again when pasting, as the same object could be pasted many times.
	Besides, store the original object, but weakly (so we don't prevent it GCed). The original object might be used in workspaces."
	(anObject isString or: [ anObject is: #Text])
		ifTrue: [
			contents _ anObject withCuisLineEndings.
			contentsOriginalObjectWeakly at: 1 put: nil ]
		ifFalse: [
			contents _ anObject copyForClipboard.
			contentsOriginalObjectWeakly at: 1 put: anObject ].
	
	self noteRecentClipping: contents.

	"Store on OS clipboard using ExtendedClipboardInterface if present"
	self extendedClipboardInterface ifNotNil: [ :interface |
		interface canStore ifTrue: [
			id _ self idFor: contents.
			contents isString
				ifTrue: [ ^interface storeString: contents id: id ].
			(contents is: #Text)
				ifTrue: [ ^interface storeText: contents id: id ].
			(contents is: #Form)
				ifTrue: [ ^interface storeForm: contents id: id ].
			(contents is: #Morph)
				ifTrue: [ ^interface storeForm: (contents imageForm: 32) id: id ].
			^interface storeString: contents asString id: id ]].

	"Otherwise use the clipboard primitives in the VM"
	"The VM uses UTF-8 for clipboard"
	primitiveFormat _ (self stringOrIdFor: contents) asUtf8: true.
	self primitiveClipboardString: primitiveFormat! !

!Clipboard methodsFor: 'initialization' stamp: 'jmv 11/29/2017 11:34:38'!
initialize
	contents _ nil.
	contentsOriginalObjectWeakly _ WeakArray new: 1.
	recent _ OrderedCollection new.! !


!Morph methodsFor: 'copying' stamp: 'jmv 11/29/2017 11:51:04'!
copyForClipboard
	"Some subclasses might need specific behavior..."

	self okayToDuplicate ifFalse: [ ^ nil ].
	^self copy! !

!Morph methodsFor: 'menus' stamp: 'jmv 11/29/2017 11:52:10'!
addCopyItemsTo: aMenu 
	"Add copy-like items to the halo menu"

	| subMenu |
	subMenu := MenuMorph new defaultTarget: self.
	subMenu add: 'copy to clipboard (c)' action: #copyToClipboard:.
	aMenu add: 'copy & print...' subMenu: subMenu! !

!Morph methodsFor: 'meta-actions' stamp: 'jmv 11/29/2017 11:46:23'!
copyToClipboard: evt
	self copyToClipboard! !

!classDefinition: #Clipboard category: #'System-Support'!
Object subclass: #Clipboard
	instanceVariableNames: 'contents recent contentsOriginalObjectWeakly'
	classVariableNames: 'Default'
	poolDictionaries: ''
	category: 'System-Support'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Clipboard releaseClassCachedState!

