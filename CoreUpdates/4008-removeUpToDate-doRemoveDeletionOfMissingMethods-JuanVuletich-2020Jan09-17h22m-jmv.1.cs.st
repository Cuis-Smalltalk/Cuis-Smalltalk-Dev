'From Cuis 5.0 [latest update: #4007] on 9 January 2020 at 5:25:46 pm'!

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 1/9/2020 17:25:05'!
removeUpToDate
	"Remove all up to date version of entries from the receiver"

	| newChangeList newList |
	newChangeList _ OrderedCollection new.
	newList _ OrderedCollection new.
	changeList with: list do: [ :chRec :strNstamp | | keep cls name |
		keep _ chRec isClassDeletion not or: [ chRec changeClass notNil ]. "If a class deletion, and class already gone, don't keep it"
		keep ifTrue: [
			(cls _ chRec changeClass) ifNotNil: [ | sel str |
				str _ chRec string.
				sel _ chRec methodSelector.
				keep _ chRec isMethodDeletion
					ifTrue: [cls includesSelector: sel]
					ifFalse: [(cls sourceCodeAt: sel ifAbsent: nil)  ~= str]]].
		(chRec changeType == #classComment and: [
				name _ chRec changeClassName.
				Smalltalk includesKey: name]) ifTrue: [
			cls _ Smalltalk at: name.
			keep _ cls organization classComment ~= chRec text ].
		(chRec changeType == #classDefinition and: [
				name _ chRec changeClassName.
				Smalltalk includesKey: name]) ifTrue: [
			cls _ Smalltalk at: name.
			chRec isMetaClassChange ifTrue: [ cls _ cls class ].
			keep _ cls definition ~= chRec text ].
		keep ifTrue: [
			newChangeList add: chRec.
			newList add: strNstamp]].
	newChangeList size < changeList size ifTrue: [
		changeList _ newChangeList.
		list _ newList.
		listIndex _ 0.
		self clearSelections ].
	self changed: #list! !

