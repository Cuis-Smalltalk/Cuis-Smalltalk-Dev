'From Cuis7.9 [latest update: #8382] on 17 November 2026 at 5:10:06 pm'!

!CodeHistory methodsFor: 'add / update' stamp: 'jmv 17/Nov/2026, 8:09:41 pm (UTC)'!
addMethodVersion: newMethod libraryIndex: libIndex positionIndex: posIndex oldVersionOrNil: oldMethod
	"A new method (possibly a new version of oldMethod) was saved to .changes file and is about to be installed.
	Add it to CodeHistory.
	newMethod may be
	- entirely new (oldMethod is nil)
	- replacement for an existing method: oldMethod
	- re-addition of a previously existing and removed method (oldMethod is nil)
	In any case posIndex=nil means there's no source code or authorAndStamp
jmv: problem here: we can not retrieve method properties to go back to some version.
We need to point to some library that can hold that information.
Maybe encode as 16rFC000000+methodNumber? Or use selector as index in a dictionary in the library?
	Note: newMethod is still not installed, the old method, if existing is still installed."

	| newItem prevItem |
	
	"If re-added, take the entry for the removal. We don't have the old method, but its entry
	in CodeHistory was stored in removals. Retrieve it and remove it from removals."
	prevItem := oldMethod
		ifNotNil: [ oldMethod historyPointer ]
		ifNil: [ removals removeKey: newMethod asMethodReference ifAbsent: [0] ].
	
	"Add new entry to CodeHistory"
	newItem := self addLibraryIndex: libIndex positionIndex: (posIndex ifNil: [ 16rFFFFFFFF]) prevItem: prevItem nextItem: 0.

	"If method already in CodeHistory, update its previous entry to point to new entry as nextItem"
	(prevItem > 0 and: [
	prevItem < 16rFC000000]) ifTrue: [
		self at: prevItem updateNextItem: newItem ].

	"Make method point to the entry just added"
	newMethod historyPointer: newItem.! !

