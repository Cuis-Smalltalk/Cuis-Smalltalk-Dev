'From Cuis 5.0 of 7 November 2016 [latest update: #3209] on 12 November 2017 at 9:07:35 pm'!

!Morph methodsFor: 'accessing - properties' stamp: 'jmv 11/12/2017 20:56:19'!
objectsForWorkspace
	^{self}! !


!InspectorWindow methodsFor: 'accessing' stamp: 'jmv 11/12/2017 21:00:55'!
objectsForWorkspace
	^{self. model object. model selection}! !


!ObjectExplorerWindow methodsFor: 'accessing' stamp: 'jmv 11/12/2017 21:00:29'!
objectsForWorkspace
	^{self. model rootObject. model object}! !


!WorkspaceWindow methodsFor: 'drag n drop' stamp: 'jmv 11/12/2017 20:57:38'!
nameForObject: object
	"Answer a name suitable for a Workspace variable"
	object name ifNotNil: [ :displayName |
		^displayName asIdentifier: false ].
	^ object class name asLowercase , object identityHash asString! !


!WorkspaceWindow methodsFor: 'drag n drop' stamp: 'jmv 11/12/2017 20:44:49'!
allowsMorphDrop
	"Answer whether we accept dropping morphs. Workspaces always accept drops.
	Regular morphs are inserted in the text.
	Inspectors and Explorers generate workspace variables referencing the inspected object(s.)"

	^ true! !

!WorkspaceWindow methodsFor: 'drag n drop' stamp: 'jmv 11/12/2017 21:05:37'!
wantsDroppedMorph: aMorph event: evt
	"Dropping a morph on my window means:
  	    display exteral name for aMorph in Workspace
	    add a binding from name to aMorph
	    and rejecting the drop."
	
	| textModelMorph |
	textModelMorph := self layoutMorph submorphs at: 1.
	aMorph objectsForWorkspace do: [ :object | | objectName |
		objectName _ self nameForObject: object.
		(self model bindingOf: objectName) value: object.
		textModelMorph editor afterSelectionInsertAndSelect: String newLineString, objectName , '. ' ].
	"send aMorph back to original position"
	evt isNil ifFalse: [ evt wasHandled: true ]. 
	"Short circuit"
	aMorph rejectDropMorphEvent: evt.
	
	^ false ! !

!methodRemoval: ObjectExplorerWindow #objectForWorkspace!
ObjectExplorerWindow removeSelector: #objectForWorkspace!
!methodRemoval: InspectorWindow #objectForWorkspace!
InspectorWindow removeSelector: #objectForWorkspace!
!methodRemoval: Morph #nameForWorkspace!
Morph removeSelector: #nameForWorkspace!
!methodRemoval: Morph #objectForWorkspace!
Morph removeSelector: #objectForWorkspace!
